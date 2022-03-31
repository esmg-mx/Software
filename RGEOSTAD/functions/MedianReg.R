MedianReg<-function (X, Var, Xlab, Varlab, deltay)
{
 
  plot(Var ~ X, pch = 21, col = "green", bg = "white", cex.lab = 1.4, 
       cex.axis = 1.2, xlab=Xlab, ylab=Varlab)
  abline(h = mean(Var), lty = 5, col = "red")
  lines(loess.smooth(X, Var), col="blue")
  legend(min(X), max(Var), "Mean", cex = 1.0, col = "red", 
         lty = 5, bty = "n")
  legend(min(X), max(Var)-deltay, "Median Regression", cex = 1.0, col = "blue", 
         lty = 1, bty = "n")
}