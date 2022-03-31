# LogPlot(depth=Depth_m, n_logs=4, 
#         log_list=list(Phie,logK,Vcl,Sw), 
#         log_name_list=list("Porosity", "logPermeability","Clay Volume","Water Saturation"), 
#         log_xlabel_list=list("Phie (dec)","logK (logmd)","Vcl (dec)","Sw (dec)"),
#         plot_mean=FALSE, plot_median=FALSE,
#         exist_log_catg=TRUE, log_catg=Facies, color_catg_list=list("green", "yellow"))

LogPlot<- function (depth, n_logs=1, log_list, log_name_list, log_xlabel_list,
                    plot_mean=FALSE, plot_median=FALSE, 
                    exist_log_catg=FALSE, log_catg=NULL, 
                    color_catg_list=NULL, fontproportion=1.0 ) 
{
  
#   if (save) {
#     png(plotname, bg = "white")
#   }  
#   else dev.new()
  par(mfrow = c(1, n_logs))
  for (i in 1:n_logs) {
    p=unlist(log_list[i])
    log_name=unlist(log_name_list[i])
    log_label=unlist(log_xlabel_list[i])
    s=summary(p)
    if (exist_log_catg) {
      plot(p, depth, ylim=rev(range(depth)), type = "l", main =log_name , 
           xlab =log_label , ylab = "Depth (m)", bty="o", cex.main=fontproportion, cex.lab=fontproportion,cex.axis=fontproportion)
      color_catg=unlist(color_catg_list)
      for (j in 1:length(depth)) {
        abline(h = depth[j], col = color_catg[log_catg[j]])
      }
      par(new=TRUE)
    }
    plot(p, depth, ylim=rev(range(depth)), type = "l", main =log_name , 
         xlab =log_label , ylab = "Depth (m)", bty="o", cex.main=fontproportion, cex.lab=fontproportion,cex.axis=fontproportion)
#    (Vp, Depth_m, ylim=rev(range(Depth_m)), type = "l", main = "P-velocity", 
#         xlab = "Vp (m/s)", ylab = "Depth (m)", bty="o")
#    revaxis(p, depth, xrev=FALSE, yrev=TRUE, type = "l", main =log_name , 
#            xlab =log_label , ylab = "Depth (m)", bty="o", xside=1)
    grid(col = "lightgray", lty = "dashed",
         lwd = par("lwd"), equilogs = TRUE)
    if (plot_mean) {
      abline(v = s[4], col = "Red")
    }  
    if (plot_median) {
      abline(v = s[3], col = "Blue")
    }
  }
  
  #legend((max(Vs) * 0.6), length(Vs), c("Mean", "Median"), 
  #       cex = 1.5, col = c("Red", "Blue"), lty = c(4,5), bty = "n")
  
#   if (save) {
#     dev.off()
#   }  

}
