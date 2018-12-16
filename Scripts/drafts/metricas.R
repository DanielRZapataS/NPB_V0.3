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

pred_npb <- fread("Results/Performance/201807/20181212_tesis_npb/pred_npb.csv")

products <- names(pred_npb)[str_detect(names(pred_npb), "target")]
products <- sapply(strsplit(products, "_"), "[[", 2)

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

fwrite(pred_npb, "Scripts/drafts/pred_npb_tesis.csv")

names(confusion_matrix_list) <- products

# changematrix results 

response <- melt(pred_npb[, mget(paste0("target_", products))])
predictor <- melt(pred_npb[, mget(paste0("pred_num_", products))])

# precision
npb_presicion <- precision(response$value, predictor$value)

# recall 

npb_recall <- recall(response$value, predictor$value)


## oferta tarjeta 

tarjeta_presicion <-
  precision(c(pred_npb$target_tcredito, rep(0, nrow(pred_npb)*8)),
            rep(1, nrow(pred_npb)*9))





