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

# Load configuration file and values
config <- fromJSON("settings.json")
products <<- c("tcredito", "crediservice", "ahorros", "cdt", "vehiculo", 
               "libranza", "libredestino", "nomina", "vivienda")
# Start the clock!
tic("NPB")

# First step: Creation job to generate new files
job <<- "Creation"
month_to_create <<- get_month(1)
# Execute orquestador which handles the process to execute.
source(os.path.join("scripts", "orquestador.R")) 

# Second step: Training to generate new models  
job <<- "Training"
model_alias_modeling_vector <- c()
train_months <<- c("201709", get_month(4))
test_month <<- get_month(3)

for(i in 1:length(products)){
  model_alias_modeling <<-
    paste0(today() %>% format(., "%Y%m%d"), "_", products[i])
  model_alias_modeling_vector[i] <-
    paste0(today() %>% format(., "%Y%m%d"), "_", products[i])
  model_type_modeling <<- products[i]
  source(os.path.join("scripts", "orquestador.R"))
  gc()
} 

# Third step: score last month on each model 
job <<- "Scoring"
performance_calculation <<- FALSE
date_to_score <<- get_month(1)

for(i in 1:length(products)){
  model_alias_scoring <<- model_alias_modeling_vector[i]
  model_type_score <<- products[i]
  source(os.path.join("scripts", "orquestador.R"))
  gc()
}

# Fourth step:ensample created models and evaluate prediction
job <<- "Ensamble" 
models <<- model_alias_modeling_vector 
model_alias_npb <<- paste0(today() %>% format(., "%Y%m%d"), "_", "npb")
date_to_predict <<- get_month(3)
performance_npb <<- TRUE
source(os.path.join("scripts", "orquestador.R"))
gc() 

#Fifth step: predict product recomendation
job <<- "Ensamble" 
models <<- model_alias_modeling_vector 
model_alias_npb <<- paste0(today() %>% format(., "%Y%m%d"), "_", "npb")
date_to_predict <<- get_month(1)
performance_npb <<- FALSE
source(os.path.join("scripts", "orquestador.R"))
gc()

toc()
