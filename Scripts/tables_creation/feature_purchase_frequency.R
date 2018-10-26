#' feature_purchase_frequency
#'
#' @param  month_to_create : month to process 
#' @param staging_path : path folder where staging data places
#' @param feature_path : path folder where feature purchase frequency place
#'
#' @return : models type master table 

feature_purchase_frequency <- function(month_to_create, staging_path, feature_path){
  print("Creating month product purchase frequencies")
  
  # compare original files with staging files to know what to process 
  # depending on month to create
  #staging
  files_staging <- list.files(staging_path)
  position <- sapply(str_extract_all(files_staging, "[0-9]+"), "[[", 1) %>% as.numeric
  files_staging <- data.frame(files = files_staging , position = position )
  #feature
  files_feature <- list.files(feature_path)
  position <- sapply(str_extract_all(files_feature, "[0-9]+"), "[[", 1) %>% as.numeric
  files_feature <- data.frame(files = files_feature , position = position )
  
  #compare 
  comp <-  which(files_staging$position %!in% files_feature$position)
  file_to_process <- files_staging[comp,]
  file_to_process <- file_to_process[file_to_process$position <= month_to_create, ]
  datos_list <- list()
  
  
  for(i in 1:length(file_to_process$position)){
    file <- file_to_process$files[i]
    print(paste0("Loading staging data: ", file))
    datos_list[[i]] <- fread(os.path.join(staging_path, file ))
  }
  datos <- rbindlist(datos_list, use.names = T)
  rm(datos_list)
  gc()
  
  labels <- names(datos)[grepl("pr_",names(datos))]
  cols   <- c("llave","month.id","month.previous.id",labels)
  datos     <- datos[,names(datos) %in% cols,with=FALSE]
  datos     <-
    merge(
      datos,
      datos,
      by.x = c("llave", "month.previous.id"),
      by.y = c("llave", "month.id"),
      all.x = TRUE
    )
  
  datos[is.na(datos)] <- 0
  products <- rep("", nrow(datos))
  num.transactions <- rep(0, nrow(datos))
  purchase.frequencies <-
    data.frame(llave = datos$llave,
               month.id = (datos$month.previous.id + 2))
  for (label in labels){
    colx  <- paste0(label,".x")
    coly  <- paste0(label,".y")
    diffs <- datos[,.(llave,month.id,change=get(colx)-get(coly))]
    num.transactions <- num.transactions + as.integer(diffs$change!=0)
    diffs[diffs<0] <- 0
    setkey(diffs,llave)
    d <- diffs[,.(frequency = cumsum(change)),by=llave]
    purchase.frequencies[[paste(label,"_purchase.count",sep="")]] <- d$frequency
    
  }
  purchase.frequencies$num.transactions <- num.transactions
  purchase.frequencies <- purchase.frequencies %>%
    dplyr::group_by(llave) %>%
    dplyr::mutate(num.transactions = cumsum(num.transactions))
  
  files_feature <- list.files(feature_path)
  print(paste0("removing past purchase frequency ", files_feature))
  file.remove(os.path.join(feature_path, files_feature))
  
  file_purchase_name <- paste0("purchase_frequencies_", month_to_create , ".csv")
  print(paste0("Saving ", file_purchase_name))
  fwrite(purchase.frequencies, file = os.path.join(feature_path, file_purchase_name))
}
