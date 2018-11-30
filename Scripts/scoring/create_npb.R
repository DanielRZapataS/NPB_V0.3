#' Title
#'
#' @param models_path : where product models are allocated
#' @param models : models alias to include in the next product to buy
#' @param date_to_predict : month to make prediction
#' @param performance : if month to predit have a historical target TRUE els FALSE
#' @param results_path : where results are allocated
#' @param model_alias_npb : models alias to ensample product models
#'
#' @return : return on results pth the next product model
create_npb <- function(models_path,
                       models,
                       date_to_predict,
                       performance_npb,
                       results_path,
                       model_alias_npb) {
  
  models_files <- list.files(models_path)
  models_files <- models_files[models_files %in% models]
  products <- sapply(strsplit(models_files, "_"), "[[", 2)
  
  if(sum(duplicated(products))){stop("There is duplicated models products")}
  
  models_files_table <- data.table(models = models_files, products = products) 
  
  print(paste("Making Next Product to buy", date_to_predict))
  ifelse(performance_npb,
         results_path <- os.path.join(results_path, "Performance"),
         results_path <- os.path.join(results_path, "Prediction"))
  
  dir.create(os.path.join(results_path, date_to_predict))
  results_alias_path <-
    os.path.join(results_path, date_to_predict ,model_alias_npb)
  dir.create(results_alias_path)
  
  print("Upload master table")
  master <- get.path(master_path, "master") %>% readRDS()
  non_variables <- names(master)[grepl("last.owned", names(master))]
  selected_var <- names(master)[names(master) %!in% non_variables]
  master <- master[,  mget(selected_var)]
  
  test_cut <-
    as.Date(paste0(as.character(date_to_predict), '01'), format = '%Y%m%d')
  months_cut <- c(test_cut, floor_date(as.Date(test_cut) + months(2), "month"))
  master <- master[periodo %in% months_cut] 
  
  if(performance_npb){
    for(i in 1:nrow(models_files_table)){
      print(paste0("Creating target variable ", products[i]))
      var_target <- paste0("pr_", products[i])
      target <-
        master[, .(llave,
                   month.id = month.id - 2,
                   var_target_2monthsFurther = get(var_target))]
      master <-
        merge(master,
              target,
              by = c("llave", "month.id"),
              all.x = TRUE)
      master[, target := ifelse(var_target_2monthsFurther - get(var_target) > 0, 1, 0)]
      master[, var_target_2monthsFurther := NULL]
      setnames(master, "target", paste0("target_", products[i]))
      rm(target)
      gc()
      
    }
  }
  
  
  # create train and test tables
  print("Creating score tables")
  # divinding master table
  test <- master[periodo == test_cut]
  test[is.na(test)] <- 0
  rm(master)
  gc()
  
  
  # Classifing variables into categories
  id_variables <-
    c("llave", "periodo", "month.id", "month", "year", paste0("target_", products))
  products_variables <- names(test)[grepl("pr_", names(test))]
  products_variables <-
    c(products_variables, "total_products", "num.transactions")
  crm_vars <-
    names(test)[names(test) %!in% c(id_variables, products_variables)]
  
  categorical_cols <-
    c(crm_vars[sapply(test[, mget(crm_vars)], is.factor)],
      "month", "year")
  categorical_cols <-
    categorical_cols[categorical_cols %!in% c("bb_seg_comercial", "aa_cod_ocupacion")]
  
  numeric_cols <-
    c(crm_vars[!(sapply(test[, mget(crm_vars)], is.factor))],
      products_variables,
      c("bb_seg_comercial", "aa_cod_ocupacion"))
  
  # one-hot encode the categorical features
  ohe_test <- dummyVars( ~ ., data = test[, mget(categorical_cols)])
  ohe_test <- predict(ohe_test, test[, mget(categorical_cols)])
  ohe_cols <- colnames(ohe_test)
  ohe_test <- as(data.matrix(ohe_test), "dgCMatrix")
  
  # data to train and predict
  test_dmatrix       <-
    cbind(ohe_test, data.matrix(test[, mget(numeric_cols)]))
  rm( ohe_test)
  gc()
  
  # load models and make predictions
  models <- models_files_table$models
  for(i in 1:nrow(models_files_table)){
    print(paste0("load model ", models[i]))
    model_alias_path <-
      os.path.join(models_path, models[i])
    model <-  xgb.load(os.path.join(model_alias_path, paste0(models[i], ".model")))
    print(paste0("predicting ", products[i], " based on model ", models[i]))
    test[, pred := predict(model, test_dmatrix)]
    setnames(test, "pred", paste0("pred_", products[i]))
    
  }
  
  
  if(performance_npb){
    fwrite(test[, mget(c("llave", "periodo", paste0("target_", products), 
                         paste0("pred_", products)))],
           os.path.join(results_alias_path, "pred_npb.csv"))
    targets <- names(test)[grepl("target_", names(test))]
    test[, count := rowSums(test[, mget(targets)])]
    fwrite(test[, .N, by = count][order(-N)],
           os.path.join(results_alias_path, "count_npb.csv"))
    # products
    products <- names(test)[grepl("pred_", names(test))]
    products <- sapply(strsplit(products, "_"), "[[", 2)
    
    # product adquicision matrix (only clients who agregate products)
    cols <- c("llave", targets)
    products_acq <- test[, mget(cols)]
    names(products_acq)[-1] <- products
    products_acq  <- melt(products_acq , id.vars = c("llave"))
    products_acq <- products_acq[products_acq$value > 0]
    
    # product propencity matrix
    probs <- names(test)[grepl("pred_", names(test))]
    cols <- c("llave", probs)
    products_pred <- test[, mget(cols)]
    names(products_pred)[-1] <- products
    products_pred  <- melt(products_pred , id.vars = c("llave"))
    products_pred[, pred_order := rank(-value), by = .(llave) ]
    
    #clients who don't agregate any product
    loss_clients <- products_pred$llave[products_pred$llave %!in% products_acq$llave]
    loss_clients <- unique(loss_clients)
    
    ## getting clientes loss on previous filter 
    products_acq <- rbindlist(list(products_acq,
                                   data.table( 
                                     llave = loss_clients,
                                     variable = "", 
                                     value = NA
                                   ) ))
    
    products_rec <- merge(products_acq[, .(llave, acq = variable)],
                          products_pred[, .(llave, pred = variable)],
                          by = "llave" )
    
    mape <- c()
    for(i in 1:9){
      apk <- products_rec[, .(apk = apk(i, acq, pred)), by = llave]
      mape[i] <-  mean(apk$apk)
    }
    
    mape <- data.table(k = 1:9, mape = mape)
    mape_aju <- c()
    for(i in 1:9){
      apk <- products_rec[acq != "", .(apk = apk(i, acq, pred)), by = llave]
      mape_aju[i] <-  mean(apk$apk)
    }
    mape_aju <- data.table(k = 1:9, mape_ajustado = mape_aju)
    
    fwrite(mape, os.path.join(results_alias_path, "mape.csv"))
    fwrite(mape_aju, os.path.join(results_alias_path, "mape_ajustado.csv"))
    
    
  }else{
    fwrite(test[, mget(c("llave", "periodo", paste0("pred_", products)))],
           os.path.join(results_alias_path, "pred_npb.csv"))
    
    products <- names(test)[grepl("pred_", names(test))]
    products <- sapply(strsplit(products, "_"), "[[", 2)
    
    # product propencity matrix
    probs <- names(test)[grepl("pred_", names(test))]
    cols <- c("llave", probs)
    products_pred <- test[, mget(cols)]
    names(products_pred)[-1] <- products
    products_pred  <- melt(products_pred , id.vars = c("llave"))
    products_pred[, pred_order := rank(-value), by = .(llave) ]
    fwrite(products_pred, os.path.join(results_alias_path, "pred_npb_long.csv"))
  }
  
}



