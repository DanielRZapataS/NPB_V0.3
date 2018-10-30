train_months
test_months
model_alias_modeling
model_type_modeling

master <- get.path(master_path, "master") %>% readRDS()

print("Creating target variable")
var_target <- paste0("pr_", model_type_modeling)
target <-
    master[, .(llave,
              month.id = month.id - 2,
              var_target_2monthsFurther = get(var_target))]
master <- merge(master, target, by=c("llave", "month.id"), all.x = TRUE)
master[, target := ifelse(var_target_2monthsFurther - get(var_target) > 0, 1, 0)]
master[, var_target_2monthsFurther := NULL] 
master[is.na(master)] <- 0
rm(target)
gc()
# converting cutting months
train_cut_max <- max(train_months)
train_cut_max <- as.Date(paste0(as.character(train_cut_max), '01'), format='%Y%m%d')
train_cut_min <- min(train_months)
train_cut_min <- as.Date(paste0(as.character(train_cut_min), '01'), format='%Y%m%d')
test_cut <- as.Date(paste0(as.character(test_months), '01'), format='%Y%m%d')

# divinding master table
test <- master[periodo == test_cut]
master <- master[periodo >= train_cut_min & 
                       periodo <= train_cut_max]

# Classifing variables into categories 
# there's a bunch of features related to the products, and thus they have similar
# names. Separate them out to keep things straight

id_variables <- c("llave", "periodo", "month.id", "month", "year", "target")
products_variables <- names(master)[grepl("pr_", names(master))]
products_variables <- c(products_variables, "total_products", "num.transactions")
crm_vars <- names(master)[names(master) %!in% c(id_variables, products_variables)]

categorical_cols <- c(crm_vars[sapply(master[, mget(crm_vars)], is.factor)],
                                         "month", "year")
categorical_cols <- categorical_cols[categorical_cols %!in% c("bb_seg_comercial", "aa_cod_ocupacion")]

numeric_cols <- c(crm_vars[!(sapply(master[, mget(crm_vars)], is.factor))],
                  products_variables, c("bb_seg_comercial", "aa_cod_ocupacion"))


# one-hot encode the categorical features
ohe <- dummyVars( ~ ., data = master[, mget(categorical_cols)])
ohe <-
  as(data.matrix(predict(ohe, master[, mget(categorical_cols)])), "dgCMatrix")
ohe_test <- dummyVars( ~ ., data = test[, mget(categorical_cols)])
ohe_test <-
  as(data.matrix(predict(ohe_test, test[, mget(categorical_cols)])), "dgCMatrix")

# separate target
target_train_dmatrix <- as(data.matrix(master$target),'dgCMatrix')
target_test_dmatrix <- test$target


# data to train and predict
master_dmatrix         <- cbind(ohe,data.matrix(master[,mget(numeric_cols)]))
test_dmatrix       <- cbind(ohe_test,data.matrix(test[,mget(numeric_cols)]))
rm(ohe, ohe_test)
gc()

dtrain <- xgb.DMatrix(data = master_dmatrix, label = target_train_dmatrix)
rm(master_dmatrix,target_train_dmatrix)
gc()

model <- xgboost(
  data = dtrain,
  nround = 100,
  objective = "binary:logistic",
  verbose = 2 ,
  print_every_n = 10
)

# save model to binary local file
model_alias_path <- os.path.join(models_path, model_alias_modeling)
dir.create(model_alias_path)
xgb.save(model, os.path.join(model_alias_path, paste0(model_alias_modeling, ".model")))

importance_matrix <- xgb.importance(model = model)
fwrite(importance_matrix,
       os.path.join(
         model_alias_path,
         paste0(model_alias_modeling, "_important_variables.csv")
       ))

# Predict train and test
master[, pred := predict(model, dtrain)]
fwrite(master[, .(llave, periodo, target, pred)], 
       os.path.join(model_alias_path, "pred_train.csv"))

test[, pred := predict(model, test_dmatrix)]
fwrite(master[, .(llave, periodo, target, pred)], 
       os.path.join(model_alias_path, "pred_train.csv"))






