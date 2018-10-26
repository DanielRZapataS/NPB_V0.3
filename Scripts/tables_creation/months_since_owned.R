#' Search number of months since each product was owned
#'
#' @param datos : data with costmuers holding products each month (data.table)
#' @param products : character vector of products of interest
#' @param months.to.search : month ago to search product holding
#' @param default.value : default number to fill months before months to search
#'
#' @return
#' @export
#'
#' @examples
months_since_owned <-
  function(datos,
           products,
           months.to.search,
           default.value = 999) {
    
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
