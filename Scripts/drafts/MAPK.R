# Metricas 

## Starting 

# Clean environment
rm(list=ls())
gc()
# Disable scientific notation
options(scipen=999)
# Change prompt
options(prompt="NPB_V0.3> ", continue=" ") 

# Load utilities functions (change wd, auxiliary scripts...)
source("Scripts/utiles.R")
# Set up paths
set_environment() 

pred_npb <- fread("Results/Performance/201808/20181130_npb/pred_npb.csv")

products <- names(pred_npb)[str_detect(names(pred_npb), "target")]
products <- sapply(strsplit(products, "_"), "[[", 2)

# calculate confucion matrix and 
confusion_matrix_list <- list()
for(i in 1:length(products)){
  response <-
    pred_npb[, get(paste0("target_", products[i]))]
  
  predictor <-
    pred_npb[, get(paste0("pred_", products[i]))]
  rc <- roc(response = response, predictor = predictor)
  # plot(rc, asp = NA, main = "ROC Xgboost")
  # rc$auc
  pUmbral <- coords(rc, "best", ret = "threshold")
  
  errorClasificacion <- misClassError(response, predictor, threshold = pUmbral)
  
  predictedClass <- ifelse(predictor >= pUmbral,1,0)
  matrizConfusion <-
    caret::confusionMatrix(
      data = as.factor(predictedClass),
      reference = as.factor(response)
    )
  confusion_matrix_list[[i]] <- matrizConfusion
  pred_npb[, var := predictedClass ]
  setnames(pred_npb, "var", paste0("pred_num_",  products[i]))
}

names(pred_npb)
names(confusion_matrix_list) <- products
(confusion_matrix_list)

# Export
fwrite(pred_npb, "Tesis/pred_npb_tesis.csv")
saveRDS(confusion_matrix_list, "Tesis/confusion_mtrx_products.RDS")

####################################################
# could starting from here

pred_npb <- fread("Tesis/pred_npb_tesis.csv")
(pred_npb)
targets <- names(pred_npb)[grepl("target_", names(pred_npb))]
products <- names(pred_npb)[str_detect(names(pred_npb), "target")]
products <- sapply(strsplit(products, "_"), "[[", 2)

################################################### 

### Perfect MAP@K ### 
# recomend every first product each client bought

pred_npb[, count := rowSums(pred_npb[, mget(targets)])]
pred_npb[, .N, by = count][order(-N)]
pred_npb[count > 0, .N]/nrow(pred_npb)

#####################
###### METRICS ######
#####################

#####################
####### MAP@K #######
#####################

# product adquicision matrix (only clients who agregate products)

cols <- c("llave", targets)
products_acq <- pred_npb[, mget(cols)]
names(products_acq)[-1] <- products
products_acq  <- melt(products_acq , id.vars = c("llave"))
products_acq <- products_acq[products_acq$value > 0]
setkey(products_acq, "llave")
# products_acq[llave == "631322"]
clients_adq <- unique(products_acq$llave) 

products_acq_list <- list()
#making list 
for(i in 1:length(clients_adq)){
  products_acq_list[[i]] <- products_acq[llave == clients_adq[i], variable]
}
names(products_acq_list) <- clients_adq


# product propencity matrix
probs <- names(pred_npb)[grepl("pred_" , names(pred_npb))]
probs <- probs[probs %in% paste0("pred_", products)]
cols <- c("llave", probs)
products_pred <- pred_npb[, mget(cols)]
names(products_pred)[-1] <- products
products_pred  <- melt(products_pred , id.vars = c("llave"))
products_pred[, pred_order := rank(-value), by = .(llave) ]
setkey(products_pred, "llave", "pred_order")
# products_pred[llave == "631322"]

#clients who don't agregate any product
loss_clients <-
  products_pred$llave[products_pred$llave %!in% products_acq$llave]
loss_clients <- unique(loss_clients)

# making list 
products_pred_cl <- copy(products_pred[llave %in% clients_adq])
products_pred_list <- list()
#making list 
for(i in 1:length(clients_adq)){
  products_pred_list[[i]] <- products_pred_cl[llave == clients_adq[i], variable]
}
names(products_pred_list) <- clients_adq

### compare 
length(products_acq_list) == length(products_pred_list)

## Adding clients that do not agregate products

products_acq_list_loss <- list()
for(i in 1:length(loss_clients)){
  products_acq_list_loss[[i]] <- ""
}

products_pred_list_loss <- list()
for(i in 1:length(loss_clients)) {
  products_pred_list_loss[[i]] <- products
}


length(products_acq_list_loss) == length(products_pred_list_loss)

## MAPK

actual <- c(products_acq_list, products_acq_list_loss)
predicted <- c(products_pred_list, products_pred_list_loss)

for(i in 1:9){
  print(mapk(i, actual, predicted))
}

# change matrix results 

response <- melt(pred_npb[, mget(paste0("target_", products))])
predictor <- melt(pred_npb[, mget(paste0("pred_num_", products))])

##############################
####### ACCURACY #######
##############################
# the proportion of elements in actual that are equal to the corresponding
# element in predicted
accuracy(response$value, predictor$value)

########################
####### PRECISON #######
########################
# precision computes proportion of observations predicted to be in the positive
# class (i.e. the element in predicted equals 1) that actually belong to the
# positive class (i.e. the element in actual equals 1)
Metrics::precision(response$value, predictor$value)

######################
####### RECALL #######
######################
# recall computes proportion of observations in the positive class (i.e. the
# element in actual equals 1) that are predicted to be in the positive class
# (i.e. the element in predicted equals 1)
Metrics::recall(response$value, predictor$value)





