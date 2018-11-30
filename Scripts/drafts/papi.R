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

# install.packages("foreach")
library(foreach)
library(doMC)
library(Metrics)

# load model predictins on test 
models <-
  c(
    "20181128_tcredito",
    "20181128_crediservice",
    "20181128_ahorros",
    "20181128_cdt",
    "20181128_vehiculo",
    "20181128_libranza",
    "20181128_libredestino",
    "20181129_nomina",
    "20181129_vivienda"
  )

models_files <- list.files(models_path)
models_files <- models_files[models_files %in% models]
products <- sapply(strsplit(models_files, "_"), "[[", 2)
models_files_table <- data.table(models = models_files, products = products) 

results <- list()
for(i in 1:length(models_files) ){
  results[[i]] <- fread(
    os.path.join(models_path, models_files[i], "pred_test.csv"))
}
# results


for(i in 1:length(products)){
  results[[i]]$product <-  products[i]
}
preds <- rbindlist(results)

preds <-
  dcast(preds, llave + periodo ~ product, value.var = c("target", "pred"))
# preds
targets <- names(preds)[grepl("target_", names(preds))]
preds[, count := rowSums(preds[, mget(targets)])]
probs <- names(preds)[grepl("pred_", names(preds))]
preds[, prob_mean :=  rowSums(preds[, mget(probs)])]
preds[, prob_mean := prob_mean / 9]

preds[order(-count)]
preds[, .N, by = count]


############ 

actual <- list(c('a', 'b'), c('a'), c('x', 'y', 'b'))
actual <- list()
predicted <- list(c('a', 'c', 'd'))
mapk(2, actual, predicted)

k <- 1

products <- names(preds)[grepl("pred_", names(preds))]
products <- sapply(strsplit(products, "_"), "[[", 2)

preds
cols <- c("llave", targets)
products_acq <- preds[, mget(cols)]
names(products_acq)[-1] <- products

products_acq  <- melt(products_acq , id.vars = c("llave"))
products_acq <- products_acq[products_acq$value > 0]
setkey(products_acq, llave)
products_acq
# products_acq <- dcast(products_acq, llave ~ variable, value.var = "value")

cols <- c("llave", probs)

products_pred <- preds[, mget(cols)]
names(products_pred)[-1] <- products
products_pred  <- melt(products_pred , id.vars = c("llave"))
products_pred[, pred_order := rank(-value), by = .(llave) ]

setkey(products_pred, llave, pred_order)
head(products_pred, 20)

## 

products_acq$llave[products_acq$llave %!in% products_pred$llave]
loss_clients <- products_pred$llave[products_pred$llave %!in% products_acq$llave]
loss_clients <- unique(loss_clients)
## getting clientes loss on previous filter 

products_acq <- rbindlist(list(products_acq,
                               data.table( 
                                 llave = loss_clients,
                                 variable = "", 
                                 value = NA
                                 ) ))
products_pred$llave[products_pred$llave %!in% products_acq$llave]

setkey(products_acq, llave)
products_acq

## APK 

products_rec <- merge(products_acq[, .(llave, acq = variable)],
                      products_pred[, .(llave, pred = variable)],
                      by = "llave" )

apk <- products_rec[, .(apk = apk(1, acq, pred)), by = llave]
mean(apk$apk)

products_rec <- merge(products_acq[value == 1, .(llave, acq = variable)],
                      products_pred[, .(llave, pred = variable)],
                      by = "llave" )
apk <- products_rec[, .(apk = apk(1, acq, pred)), by = llave]
mean(apk$apk)

## 
products_rec <- merge(products_acq[, .(llave, acq = variable)],
                      products_pred[, .(llave, pred = variable)],
                      by = "llave" )
mape <- c()
for(i in 1:9){
  apk <- products_rec[, .(apk = apk(i, acq, pred)), by = llave]
  mape[i] <-  mean(apk$apk)
}

mape

mape_aju <- c()
for(i in 1:9){
  apk <- products_rec[acq != "", .(apk = apk(i, acq, pred)), by = llave]
  mape_aju[i] <-  mean(apk$apk)
}

mape_aju




                      
                      
                      