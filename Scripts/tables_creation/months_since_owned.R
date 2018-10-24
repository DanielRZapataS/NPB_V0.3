months_since_owned<- function(datos,products,months.to.search,default.value = 999){
  
  for (product in products){
    print(paste("Finding months since owning",product))
    colname <- paste(product,".last.owned",sep="")
    datos[[colname]] <- default.value
    for (month.ago in seq(months.to.search,1,-1)){
      cur.colname <- paste(product,"_",month.ago,"month_ago",sep="")
      datos[[colname]][datos[[cur.colname]] >= 1] <- month.ago
    }
  }
  return(datos)
  
}
