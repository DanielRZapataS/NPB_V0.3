#' Make master tables for models 
#' 
#' @param  month_to_create : month to process
#' @param model_type_creation : model type to create; see settings comments 
#' @param staging_path : path field where staging data places
#' @param master_path : path field where master data places
#' @param months_ago : create features indicating whether or not a product was
#owned in each of the past X months. for each lag, match the month with the
#earlier one and through some name manipulation extract whether the product was
#owned or not (numeric)
#' @param months.to.seatch :  get the number of months since each product was
#'   owned. See months_since_owned function
#'
#' @return : models type master table 

master_maker <- function(month_to_create,
                         staging_path,
                         master_path,
                         months_ago = 3,
                         months.to.seatch = 3) {
  
  
  #staging
  files_staging <- list.files(staging_path)
  position <- sapply(str_extract_all(files_staging, "[0-9]+"), "[[", 1) %>% as.numeric
  files_staging <- data.frame(files = files_staging , position = position )
  datos_list <- list()

  for(i in 1:length(files_staging$position)){
    file <- files_staging$files[i]
    print(paste0("Loading staging data: ", file))
    datos_list[[i]] <-  readRDS(os.path.join(staging_path, file ))
  }
  datos <- rbindlist(datos_list, use.names = T)
  rm(datos_list)
  gc()
  
  # eliminate products 
  null.vars <- c("pr_otros", "pr_vehiculo", "pr_vivienda",
                 "pr_fomento","pr_microcredito", "pr_leasing", 
                 "pr_activo_pyme", "pr_constructor")
  datos[, (null.vars) := NULL]
    
  products <- names(datos)[grepl("pr_",names(datos))]
 
  # create a data frame with just the product ownership variables so
  # we can create lag ownership features
  products.owned <- datos %>%
    select(llave,month.id,one_of(products)) %>%
    as.data.table()
  original.month.id <- products.owned$month.id
  # create features indicating whether or not a product was owned in each of the
  # past X months. for each lag, match the month with the earlier one and
  # through some name manipulation extract whether the product was owned or not
  for (month.ago in 1:months_ago){
    print(paste("Collecting data on product ownership",month.ago,"months ago..."))
    products.owned[,month.id:=original.month.id+month.ago]
    #train
    datos <- merge(datos,products.owned,by=c("llave","month.id"),all.x=TRUE)
    change.names <- names(datos)[grepl("\\.y",names(datos))]
    new.names <- gsub("\\.y",paste("_",month.ago,"month_ago",sep=""),change.names)
    names(datos)[grepl("\\.y",names(datos))] <- new.names
    change.names <- names(datos)[grepl("\\.x",names(datos))]
    new.names <- gsub("\\.x","",change.names)
    names(datos)[grepl("\\.x",names(datos))] <- new.names
  }
  rm( products.owned)
  gc()
  # there will be NA values where there isn't a match to the left side since we
  # used all.x=TRUE, assume those correspond to products that were not owned
  datos[is.na(datos)] <- 0
  
  # get the number of months since each product was owned
  datos <- months_since_owned(datos,products, months.to.seatch)

  # datos <- as.data.frame(datos)
  # compute total number of products owned for each month 
  datos[, total_products := rowSums(.SD,na.rm=TRUE), .SDcols = products ]
  
  # save the month id for use creating window ownership features
  # products.owned$month.id <- original.month.id
  
  # windows of product ownership. For each window size look back at previous
  # months and see if the product was ever owned. I do this by adding the value
  # of the ownership variable X months ago for X = 1:window.size then converting
  # to a binary indicator if the value is positive (meaning it was owned at
  # least once)
  
  # for (product in products){
  #   for (window.size in 2:3){
  #     print(paste("Getting ownership for",product,"within last",window.size,"months"))
  #     colname <- paste(product,".owned.within.",window.size,"months",sep="")
  #     datos[[colname]]   <- 0
  #     for (month.ago in 1:window.size){
  #       current.col     <- paste(product,"_",month.ago,"month_ago",sep="")
  #       datos[[colname]]   <- datos[[colname]]  + datos[[current.col]]
  #     }
  #     datos[[colname]]   <- as.integer(datos[[colname]] > 0)
  #   }
  # }
  # 
  # add in purchase frequency feature for each product
  print("Load purchase frequencies")
  purchase.frequencies <- readRDS(get.path(feature_path, month_to_create))
  purchase.frequencies <- data.table(purchase.frequencies)
  # removing products of purchase.feature table 
  null.vars.purchase <- paste0(null.vars, "_purchase.count")
  purchase.frequencies[, (null.vars.purchase) := NULL]
  
  datos   <- merge(datos, purchase.frequencies,by=c("month.id","llave"),all.x = TRUE)
  datos[is.na(datos)] <- 0
  rm(purchase.frequencies)
  gc()
  # characters to factors
  char.cols <- names(datos)[sapply(datos,is.character)]
  datos[ , (char.cols) := lapply(.SD, as.factor), .SDcols = char.cols]
  
  nulls <- c("month.previous.id")
  datos[, (nulls) := NULL]

  datos[, bb_seg_comercial := factor(bb_seg_comercial,
                                  levels = unique(bb_seg_comercial))]
  datos[, aa_cod_ocupacion := factor(aa_cod_ocupacion, 
                                  levels = unique(aa_cod_ocupacion))]
  
  files_master <- list.files(master_path)
  print(paste0("removing past master", files_master))
  file.remove(os.path.join(master_path, files_master))
  
  file_master_name <- paste0("master_", month_to_create , ".rds")
  print(paste0("Saving ", file_master_name))
  saveRDS(datos, file = os.path.join(master_path, file_master_name))

}