# read data
tcredito_path <- os.path.join(master_path, "tcredito")
tcredito_master <- list.files(tcredito_path)
load(os.path.join(tcredito_path, tcredito_master))
tcredito[, periodo := as.Date(periodo)]
# target 
tcredito[, target := ifelse(pr_tcredito_2monthsFurther - pr_tcredito > 0, 1, 0)]
tcredito[, sum(target, na.rm = T), by = periodo]
tcredito[, pr_tcredito_2monthsFurther := NULL]
tcredito[is.na(tcredito)] <- 0

train_cut <- max(train_months)
train_cut <- paste(substr(train_cut, 1,4), substr(train_cut, 5,6), "01", sep ="-")
train_cut_min <- min(train_months)
train_cut_min <- paste(substr(train_cut_min, 1,4), paste0( "0",as.numeric(substr(train_cut_min, 5,6)) + 3), "01", sep ="-")

test_cut1 <- test_months[1]
test_cut1 <- paste(substr(test_cut1, 1,4), substr(test_cut1, 5,6), "01", sep ="-")

test_cut2 <- test_months[2]
test_cut2 <- paste(substr(test_cut2, 1,4), substr(test_cut2, 5,6), "01", sep ="-")

score1 <- dates_to_score[1]
score1 <- paste(substr(score1, 1,4), substr(score1, 5,6), "01", sep ="-")

score2 <- dates_to_score[2]
score2 <- paste(substr(score2, 1,4), substr(score2, 5,6), "01", sep ="-")

test1 <- tcredito[periodo == test_cut1]
test2 <- tcredito[periodo == test_cut2]

prediccion1 <- tcredito[periodo == score1]
prediccion2 <- tcredito[periodo == score2]


tcredito <- tcredito[periodo >= train_cut_min & 
                        periodo <= train_cut]

prediccion1[, target := NULL]
prediccion2[, target := NULL]

# there's a bunch of features related to the products, and thus they have similar
# names. Separate them out to keep things straight

crm.vars <- names(tcredito)[!grepl("pr_*", names(tcredito))]
no.crm.vars <- c("month.id", "llave", "month", "year", 
                 "num.transactions", "target", "total_products")
crm.vars <- crm.vars[crm.vars %!in% no.crm.vars]
lagged.vars <- names(tcredito)[grepl("month_ago", names(tcredito))]
last.owned.vars <- names(tcredito)[grepl("*last.owned", names(tcredito))]
owned.within.vars <- names(tcredito)[grepl("owned.within", names(tcredito))]
purchase.count.vars <- names(tcredito)[grepl("purchase.count", names(tcredito))]

sapply(tcredito[, mget(crm.vars)], class)

categorical.cols <- c(crm.vars[sapply(tcredito[, mget(crm.vars)], is.factor)],
                      "month", "year")
categorical.cols <- categorical.cols[categorical.cols != "birthdate" ]
categorical.cols <- c(categorical.cols, lagged.vars, owned.within.vars)


products.interest <- c("nomina", "tcredito", "ahorros",
                       "libranza", "otros", "libredestino",
                       "crediservice", "cdt")

products.interest <- names(tcredito)[which(rowSums(
  sapply(products.interest, grepl,  names(tcredito))) == 1)]

categorical.cols <- categorical.cols[categorical.cols %in% products.interest]

numeric.cols <- c(crm.vars[sapply(tcredito[, mget(crm.vars)], is.numeric)], 
                  purchase.count.vars[purchase.count.vars %in% products.interest],
                  "total_products", "num.transactions")


# ## making sub-sub sample 
# cedulas <- unique(tcredito$llave)
# length(cedulas)
# n <- length(cedulas)/12 %>% round()
# mysample <- sample(cedulas, n, replace = FALSE )
# length(mysample)
# 
# tcreditoSample <- tcredito[llave %in% mysample]
# tcreditoSample


# one-hot encode the categorical features
ohe <- dummyVars( ~ ., data = tcredito[, mget(categorical.cols)])
ohe <-
  as(data.matrix(predict(ohe, tcredito[, mget(categorical.cols)])), "dgCMatrix")
ohe.test1 <- dummyVars( ~ ., data = test1[, mget(categorical.cols)])
ohe.test1 <-
  as(data.matrix(predict(ohe.test1, test1[, mget(categorical.cols)])), "dgCMatrix")
ohe.test2 <- dummyVars( ~ ., data = test2[, mget(categorical.cols)])
ohe.test2 <-
  as(data.matrix(predict(ohe.test2, test2[, mget(categorical.cols)])), "dgCMatrix")
# pred
ohe.prediccion1 <- dummyVars( ~ ., data = prediccion1[, mget(categorical.cols)])
ohe.prediccion1 <-
  as(data.matrix(predict(ohe.prediccion1, prediccion1[, mget(categorical.cols)])), "dgCMatrix")
ohe.prediccion2 <- dummyVars( ~ ., data = prediccion2[, mget(categorical.cols)])
ohe.prediccion2 <-
  as(data.matrix(predict(ohe.prediccion2, prediccion2[, mget(categorical.cols)])), "dgCMatrix") 


target <- list()
target <- as(data.matrix(tcredito$target),'dgCMatrix')
target.test1 <- test1$target
target.test2 <- test2$target


# remember the id's for people and months for later since all that actually goes
# into xgboost is the raw feature data
save.id       <- tcredito$llave
save.month.id <- tcredito$month.id
save.month    <- tcredito$month
save.id.test1       <- test1$llave
save.month.id.test1 <- test1$month.id
save.id.test2       <- test2$llave
save.month.id.test2 <- test2$month.id
save.id.prediccion1       <- prediccion1$llave
save.month.id.prediccion1 <- prediccion1$month.id
save.id.prediccion2       <- prediccion2$llave
save.month.id.prediccion2 <- prediccion2$month.id


tcredito         <- cbind(ohe,data.matrix(tcredito[,mget(numeric.cols)]))
test1       <- cbind(ohe.test1,data.matrix(test1[,mget(numeric.cols)]))
test2       <- cbind(ohe.test2,data.matrix(test2[,mget(numeric.cols)]))
prediccion1       <- cbind(ohe.prediccion1,data.matrix(prediccion1[,mget(numeric.cols)]))
prediccion2       <- cbind(ohe.prediccion2,data.matrix(prediccion2[,mget(numeric.cols)]))

dtrain <- xgb.DMatrix(data = tcredito, label=target)
model <- xgboost(
  data = dtrain,
  nround = 100,
  objective = "binary:logistic",
  verbose = 2 ,
  print_every_n = 10
)

# save model to binary local file

xgb.save(model, "Models/xgboost.model")
model <-  xgb.load("Models/xgboost.model")

## VARIABLES 
importance_matrix <- xgb.importance(model = model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

# xgb.dump(model, with_stats = T)
# 
# xgb.plot.tree(model = model)




## Test1 ## 

preds <- predict(model,test1)
# size of the prediction vector
print(length(preds))
# limit display of predictions to the first 10
print(head(preds))

prediction <- as.numeric(preds > 0.5)
print(head(prediction))

err <- mean(as.numeric(preds > 0.5) != target.test1)
print(paste("test-error=", err))

rc <- roc(response = target.test1, predictor = preds)
plot(rc, asp = NA, main = "ROC Xgboost")
rc$auc
pUmbral <- coords(rc, "best", ret = "threshold")

#pUmbral <- pUmbral + 0.006

errorClasificacion <- misClassError(target.test1, preds, threshold = pUmbral)

predictedClass <- ifelse(preds >= pUmbral,1,0)
matrizConfusion <-
  caret::confusionMatrix(
    data = as.factor(predictedClass),
    reference = as.factor(target.test1)
  )
matrizConfusion 

uplift <- calculate_lift_table(target.test1, preds, save.id.test1)

gaintable <- uplift$gaintable
predictions <- uplift$predictin

fwrite(gaintable, paste0("Results/upliftTest", test_months[1],".csv"))
fwrite(predictions, paste0("Results/predictionTest",  test_months[1],".csv"))

## Test 2 ## 
preds <- predict(model,test2)
# size of the prediction vector
print(length(preds))
# limit display of predictions to the first 10
print(head(preds))

prediction <- as.numeric(preds > 0.5)
print(head(prediction))

err <- mean(as.numeric(preds > 0.5) != target.test2)
print(paste("test-error=", err))

rc <- roc(response = target.test2, predictor = preds)
plot(rc, asp = NA, main = "ROC Xgboost")
rc$auc
pUmbral <- coords(rc, "best", ret = "threshold")

#pUmbral <- pUmbral + 0.006

errorClasificacion <- misClassError(target.test2, preds, threshold = pUmbral)

predictedClass <- ifelse(preds >= pUmbral,1,0)
matrizConfusion <-
  caret::confusionMatrix(
    data = as.factor(predictedClass),
    reference = as.factor(target.test2)
  )
matrizConfusion 

uplift <- calculate_lift_table(target.test2, preds, save.id.test2)

gaintable <- uplift$gaintable
predictions <- uplift$predictin

fwrite(gaintable, paste0("Results/upliftTest", test_months[2],".csv"))
fwrite(predictions, paste0("Results/predictionTest",  test_months[2],".csv"))
########################### 

preds.septiembre <- predict(model,prediccion2)
predsmtrix <- data.table(id = save.id.prediccion2,
                         pred = preds.septiembre)
predsmtrix[,bucket :=  ntile(-pred, 10)]
setkey(predsmtrix, bucket)
fwrite(predsmtrix, "Results/prediction_201809.csv")

###########################

## Delecting variables ##

tcredito1 <- as.matrix(tcredito)
tcredito1 <- data.table(tcredito1)
test11 <- as.matrix(test1)
test11 <- data.table(test11)
test21 <- as.matrix(test2)
test21 <- data.table(test21)

selectedvars <- importance_matrix[Gain > 0.01, Feature] 
tcredito1 <- tcredito1[, mget(selectedvars)]
test11 <- test11[, mget(selectedvars)]
test21 <- test21[, mget(selectedvars)]

tcredito1 <- as(data.matrix(tcredito1),'dgCMatrix')
test11 <- as(data.matrix(test11),'dgCMatrix')
test21 <- as(data.matrix(test21),'dgCMatrix')

dtrain1 <- xgb.DMatrix(data = tcredito1, label=target)
model1 <- xgboost(
  data = dtrain1,
  nthread = 4,
  nround = 100,
  objective = "binary:logistic",
  verbose = 2 ,
  print_every_n = 10
)

# save model to binary local file
xgb.save(model1, "Models/xgboost2.model")

## VARIABLES 
importance_matrix <- xgb.importance(model = model1)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

xgb.dump(model, with_stats = T)

xgb.plot.tree(model = model)

fwrite(importance_matrix, "Results/importance_matrix.csv")
## Test1 ## 

preds <- predict(model1,test11)
# size of the prediction vector
print(length(preds))
# limit display of predictions to the first 10
print(head(preds))

prediction <- as.numeric(preds > 0.5)
print(head(prediction))

err <- mean(as.numeric(preds > 0.5) != target.test1)
print(paste("test-error=", err))

rc <- roc(response = target.test1, predictor = preds)
plot(rc, asp = NA, main = "ROC Xgboost")
rc$auc
pUmbral <- coords(rc, "best", ret = "threshold")

#pUmbral <- pUmbral + 0.006

errorClasificacion <- misClassError(target.test1, preds, threshold = pUmbral)

predictedClass <- ifelse(preds >= pUmbral,1,0)
matrizConfusion <-
  caret::confusionMatrix(
    data = as.factor(predictedClass),
    reference = as.factor(target.test1)
  )
matrizConfusion 

uplift <- calculate_lift_table(target.test1, preds, save.id.test1)

gaintable <- uplift$gaintable
predictions <- uplift$predictin

fwrite(gaintable, paste0("Results/upliftTestV", test_months[1],".csv"))
fwrite(predictions, paste0("Results/predictionTestV",  test_months[1],".csv"))

## Test 2 ## 
preds <- predict(model1,test21)
# size of the prediction vector
print(length(preds))
# limit display of predictions to the first 10
print(head(preds))

prediction <- as.numeric(preds > 0.5)
print(head(prediction))

err <- mean(as.numeric(preds > 0.5) != target.test2)
print(paste("test-error=", err))

rc <- roc(response = target.test2, predictor = preds)
plot(rc, asp = NA, main = "ROC Xgboost")
rc$auc
pUmbral <- coords(rc, "best", ret = "threshold")

#pUmbral <- pUmbral + 0.006

errorClasificacion <- misClassError(target.test2, preds, threshold = pUmbral)

predictedClass <- ifelse(preds >= pUmbral,1,0)
matrizConfusion <-
  caret::confusionMatrix(
    data = as.factor(predictedClass),
    reference = as.factor(target.test2)
  )
matrizConfusion 

uplift <- calculate_lift_table(target.test2, preds, save.id.test2)

gaintable <- uplift$gaintable
predictions <- uplift$predictin

fwrite(gaintable, "Results/Tables/upliftTest21Sample.csv")
fwrite(predictions, "Results/Tables/predictionTest21Sample.csv")




