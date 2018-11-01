

#' Score monthly table
#'
#' @param model_alias_score : Model previously made to use to score (character)
#' @param date_to_score : Month to score (character)
#' @param model_type_score : product of model choseen to score   
#' @param performance_calculation : use if exist a target to compare (logical)
#'
#' @return
#' @export
#'
#' @examples
score_mensual <- function(model_alias_score,
                          date_to_score,
                          model_type_score,
                          performance_calculation ) {
  print(paste("scoring month", date_to_score))
  print("Upload master table")
  master <- get.path(master_path, "master") %>% readRDS()
  
  print("Creating target variable")
  var_target <- paste0("pr_", model_type_score)
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
  master[is.na(master)] <- 0
  rm(target)
  gc()
  
  # create train and test tables
  print("Creating score tables")
  # converting cutting months
  test_cut <-
    as.Date(paste0(as.character(date_to_score), '01'), format = '%Y%m%d')
  
  # divinding master table
  test <- master[periodo == test_cut]
  rm(master)
  gc()
  
  # Classifing variables into categories
  # there's a bunch of features related to the products, and thus they have similar
  # names. Separate them out to keep things straight
  
  id_variables <-
    c("llave", "periodo", "month.id", "month", "year", "target")
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
  
  
  # save model to binary local file
  print("load model")
  model_alias_path <-
    os.path.join(models_path, model_alias_scoring)
  model <-  xgb.load(os.path.join(model_alias_path, paste0(model_alias_scoring, ".model")))
  
  test[, pred := predict(model, test_dmatrix)]
  
  results_alias_path <-
    os.path.join(results_path, model_alias_scoring)
  dir.create(results_alias_path)
  
  if(performance_calculation){
    test[,bucket :=  ntile(-pred, 10)]
    setkey(test, bucket)
    fwrite(test[, .(llave, periodo, target, pred, bucket)],
           os.path.join(results_alias_path, "pred_score.csv"))
    # metrics model
    print("Making metrics model")
    cols <- c(ohe_cols, numeric_cols)
    
    performanceReport(test,
                      path = results_path,
                      modelFolder = model_alias_scoring,
                      alias = "score")
    importance_matrix <-
      xgb.importance(feature_names = cols, model = model)
    
    no_quantil <-
      c("llave", "month.id", "aa_cod_ciiu", "departamento")
    quantil <- names(test)[names(test) %!in% no_quantil]
    exportQuantile(
      dt = test[, mget(quantil)],
      mostImp = importance_matrix ,
      outputPath = os.path.join(results_path, model_alias_scoring, "quantile_score.csv")
    )
  
    
  }else{
    test[,bucket :=  ntile(-pred, 10)]
    setkey(test, bucket)
    fwrite(test[, .(llave, periodo, pred, bucket)],
           os.path.join(results_alias_path, "pred_score.csv"))
  }
}


