# arguments
dt
path = models_path
modelFolder = model_alias_modeling
alias = "train"

dt <- copy(test)
gini <- auc(dt$target, dt$pred)*2-1
#pintar cada gini en la carpeta modelo


pintaGini(dt$target, dt$pred, 
          os.path.join(path, modelFolder, paste0(modelFolder, "_", alias, "_gini.png")))
actual = dt$target
predicted = dt$pred
filepath = os.path.join(path, modelFolder, paste0(modelFolder, "_", alias, "_gini.png"))
lang = "eng"


#' Plots the ROC curve if a filepath is specified and returns the GINI value
#' @param actual: the target value (logical|integer {0,1} vector)
#' @param predicted: the score value (numeric vector)
#' @param filepath: filepath to store the roc curve plots. If not specified, 
#' the figures will not be generated (character)
#' @param lang: language to use to generate the labels of the plot (character {"eng"|"esp"})
#' @return: the gini value (numeric)
pintaGini <- function(actual, predicted, filepath=NULL, lang = "eng") {
  require(ROCR)
  perf <- performance(pred <- prediction(predicted, actual), "tpr", "fpr")
  gini <- (unlist(performance(pred, measure = "auc")@y.values) - 0.5)/0.5
  ks <- max(unlist(perf@y.values) - unlist(perf@x.values))
  if(!is.null(filepath)){
    if (lang == "eng") {
      yl = "% true positives"
      xl = "% false positives"
    } else {
      yl = "% verdaderos positivos"
      xl = "% falsos positivos"
    }
    png(paste0(filepath), height = 700, width = 700)
    par(mar = c(5.1, 5.1, 4.1, 2.1))
    plot(x = 100 * unlist(perf@x.values), y = 100 * unlist(perf@y.values), type = "l", 
         col = "#00598e", xaxs = "i", lwd = 2, yaxs = "i", xlab = xl, ylab = yl, font.lab = 2, 
         cex.lab = 1.2)
    abline(a = 0, b = 1, lty = 5)
    legend("bottomright", "hi", paste0("Gini = ", round(100 * gini, 1), "%\n", "Ks = ", 
                                       round(100 * ks, 1), "%\n\n"), text.font = 2, cex = 1.2)
    dev.off()
  }
  
  return(gini)
}