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

# First step: Creation job to generate new files
job <<- "Creation"
month_to_create <<- today() %>% format(., "%Y%m")
# Execute orquestador which handles the process to execute.
source(os.path.join("scripts", "orquestador.R")) 

# Second step: Training to generate new models  
job <<- "Training"
for(i in 2:length(products)){
  train_months <<- c("201709", get_month(4))
  test_month <<- get_month(3)
  model_alias_modeling <<- paste0(today() %>% format(., "%Y%m%d"), "_", products[i])
  model_type_modeling <<- products[i]
  source(os.path.join("scripts", "orquestador.R")) 
  gc()
}

