# Sets the locale depending on the system that executes the code
if(Sys.info()["sysname"] == "Linux"){
  Sys.setlocale(category = "LC_TIME", locale = "en_US.utf-8")
} else if(Sys.info()["sysname"] == "Windows"){
  Sys.setlocale(category = "LC_ALL", locale = "English")
} else{
  stop(paste("Configure locales for system with name:", Sys.info()["sysname"]))
}


#' Function which is intended for printing strings in the R console using the C syntax
pprint <- function(...){cat(sprintf(...), "\n")}

#' Canonizes a path given a kind of slash.
#' @param path: path to be canonized (character)
#' @param slash: slash symbol to be used to build the path (character; "/" by default for
#' assuring multi-plataform compatibilities) (character)
#' @return: the path canonized (character)
normalize_path = function(path, slash="/"){
  path = sub(pattern = "\\/\\/", replacement = slash, path)
  path = sub(pattern = "\\/", replacement = slash, path)
  path = sub(pattern = "\\\\", replacement = slash, path)
  return(path)
}

#' Builds a path from chunks
#' @params ...: All the chunks of paths to be loaded. 
#' @return: the path joined and normalized (character)
os.path.join <- function(...){
  normalize_path(file.path(...), slash = "/")
}

#' Loads in the global environment all the paths parametrized in the settings.json file
#' @return: None (void)
load_paths <- function(){
  paths <- fromJSON("settings.json")$path
  data_path    <<- paths$data_path
  data_cat <- paths$data_cat
  dictionary_path <<- os.path.join(data_path, data_cat[1])
  original_path       <<- os.path.join(data_path, data_cat[2])
  staging_path   <<- os.path.join(data_path, data_cat[3])
  meta_path <<- os.path.join(data_path, data_cat[4])
  master_path <<- os.path.join(data_path, data_cat[5])
  feature_path <<- os.path.join(data_path, "feature_purchase_frequency")
  results_path   <<- paths$results_path
  log_path <<- paths$log_path
  scripts_path   <<- paths$scripts_path
}

load_model_parameters <- function(){
  modeling <- fromJSON("settings.json")$modeling
  train_months <<- modeling$train_months
  test_months <<- modeling$test_months
  model_alias_modeling <<- modeling$model_alias
  model_type_modeling <<- modeling$model_type
}

load_scoring_parameters <- function(){
  scoring <- fromJSON("settings.json")$scoring
  model_alias_scoring <<- scoring$model_alias
  dates_to_score <<- scoring$dates_to_score
  model_type_score <<- scoring$model_type_score
  performance_calculation <<- scoring$performance_calculation
}

load_creation_parameters <- function(){
  month_process <- fromJSON("settings.json")$month_process
  month_to_create <<- month_process$month_to_create
  model_type_creation <<- month_process$model_type_creation
  
}


load_common_libraries <- function(){
  import("jsonlite")
  import("data.table") # data handle
  import("dplyr")
  import("ggplot2") # visualizations
  import("stringi") # paquete para manipular strings
  import("stringr") # paquete para manipular strings
  import("rlist") # manipulacion de listas
  import("lubridate")
  import("xgboost")
  import("InformationValue")
  import("caret")
  import("e1071")
  import("DiagrammeR")
  import("pROC")
} 

#' Checks if a library is currently installed, installs it if not, and imports it.
#' @return: None (void)
import <- function(...){
  gb = lapply(..., function(x){
    if(!x %in% installed.packages()){
      pprint("Library '%s' not found. Installing...", x)
      install.packages(x)
    }
    library(x, character.only = TRUE)
    pprint("Library '%s' imported", x)
  })
}

#' Sets the environment of the by importing the necessary modules, loading the 
#' necessary libraries and loading the parametrized paths
#' @return: None (void)
set_environment <- function(){
  '%!in%' <<-  Negate('%in%')
  load_common_libraries()
  load_paths()
  load_model_parameters()
  load_scoring_parameters()
  load_creation_parameters()
  job <<- fromJSON("settings.json")$job
  source(os.path.join(scripts_path, "text_tools.R"))
  source(os.path.join(scripts_path, "file_tools.R"))
  # data transformation functions
  dataTransformation <- "tables_creation"
  import_module(os.path.join(dataTransformation, "cleaning_functions.R"))
  import_module(os.path.join(dataTransformation, "staging_maker.R"))
  import_module(os.path.join(dataTransformation, "feature_purchase_frequency.R"))
  import_module(os.path.join(dataTransformation, "months_since_owned.R"))
  import_module(os.path.join(dataTransformation, "master_maker.R"))
  # modeling functions
  models <- "modeling"
  import_module(os.path.join(models, "models_measures.R"))
  # # utiles functions
  # utiles <- "utiles"
  # import_module(os.path.join(utiles, "text_cleaner.R"))
  # # result function
  # models <- "Results"
  # # import_module(os.path.join(models, "resultsFunctions.R"))
  # 
  # loadDataParameters()
  # # Load configuration file and create log
  config <<- fromJSON("settings.json")
  jsontest = toJSON(config, pretty = TRUE, auto_unbox = TRUE)
  write(jsontest, file = os.path.join(config$paths$log_path, 
                                      paste0("log", Sys.Date(), ".json")))
  
}