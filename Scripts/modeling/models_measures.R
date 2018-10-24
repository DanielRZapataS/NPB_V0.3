calculate_lift_table <- function(depvar, predcol, id.depvar, groups=10) {
  
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  
  helper = data.frame(client.id = id.depvar,
                      target = depvar,
                      pred = predcol)
  helper[,"bucket"] = ntile(-helper[,"pred"], groups)
  
  gaintable <-  helper %>% group_by(bucket)  %>%
    
    summarise_at(vars(target),
                 funs(total = n(), totalresp = sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain = Cumresp / sum(totalresp) * 100,
           Cumlift = Gain / (bucket * (100 / groups)))
  
  return(list(gaintable = gaintable, predictin = helper))
}
