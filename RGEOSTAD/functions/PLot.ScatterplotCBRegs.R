PLot.ScatterplotCBRegs<-function(Var1,Var2,
         RVP1,RegQ1,RVP2,RegQ2,RVP3,RegQ3,
         BREAKS, XLAB,YLAB, 
         Q1Lab, Q2Lab, Q3Lab,
         Xmax = ( t( summary( as.vector( Var1) ) )[1,6] ),
         Xmin = ( t( summary( as.vector( Var1) ) )[1,1] ),
         Ymax = ( t( summary( as.vector( Var2) ) )[1,6] ),
         Ymin = ( t( summary( as.vector( Var2) ) )[1,1] ) )
{
  
  
  # Expand the limits of the plot
  Xmax <- Xmax * (1.001)
  Xmin <- Xmin *(0.99)
  
  Ymax <- Ymax * (1.001)
  Ymin <- Ymin * (0.99)
  
  #Tranfer  the data into a matrix
  DatosN<-as.data.frame(cbind(Var1,Var2))
  colnames(DatosN)<-c("Var1","Var2")
  
  #Compute the bins for each variable
  difhis1<-Xmax-Xmin
  numclases<-BREAKS
  tamint1<-difhis1/numclases
  valorhis1<-0
  vectorhis1<-0
  for (i in 1:numclases)
  { 
    valorhis1[i]<-Xmin+i*tamint1
    vectorhis1<-c(Xmin, valorhis1)
  }
  
  #
  difhis2<-Ymax-Ymin
  numclases<-BREAKS
  tamint2<-difhis2/numclases
  valorhis2<-0
  vectorhis2<-0
  for (i in 1:numclases)
  {
    valorhis2[i]<-Ymin+i*tamint2
    vectorhis2<-c(Ymin, valorhis2) 
  }
  
  #
  cual1<-0
  cuales1<-0
  for (i in 1:length(DatosN$Var1))
  {
    if (DatosN$Var1[i]<min(vectorhis1) | DatosN$Var1[i]>max(vectorhis1))
    {
      cual1[i]<-i
      cuales1<-c(cual1)
    }
    if (DatosN$Var1[i]>=min(vectorhis1) & DatosN$Var1[i]<=max(vectorhis1))
    {
      cual1[i]<-0
      cuales1<-c(cual1)
    }  
  }
  
  #
  if (sum(cuales1)>0)
  { DatosN<-DatosN[-cuales1,]}
  
  cual2<-0
  cuales2<-0
  for (i in 1:length(DatosN$Var2))
  {
    if (DatosN$Var2[i]<min(vectorhis2) | DatosN$Var2[i]>max(vectorhis2))
    {
      cual2[i]<-i
      cuales2<-c(cual2)
    }
    if (DatosN$Var2[i]>=min(vectorhis2) & DatosN$Var2[i]<=max(vectorhis2))
    {
      cual2[i]<-0
      cuales2<-c(cual2)
    }  
  }
  
  #
  if (sum(cuales2)>0)
  { DatosN<-DatosN[-cuales2,]}
  
  #Compute Correlation coef.
  Corr<-cor(DatosN$Var2,DatosN$Var1, method="pearson")
  
  #Plotting parameters
  orden<-matrix(c(2,6,6,3,6,6,1,5,4,7,7,7),ncol=3,nrow=4,byrow=T) 
  div<-layout(orden, widths=c(6,0.7,1.5,2),heights=c(1.5,0.7,6,2), TRUE) 
  
  #Debug# print(length(DatosN$Var1))
  #Debug# print(length(DatosN$Var2))
  #Debug# print(length(RegQ1))
  #Debug# print(length(RegQ2))
  #Debug# print(length(RegQ3))
  
  
  #Scatter Color Palette 
  #   #Base Naranja: Aceptacion
  #   ColorPlot="#FFD173" # Scatter
  #   ColorVA1= "#FFC040" # Vs
  #   ColorVA2= "#A66F00" # Vp
  #   
  #   #Base Olivo: Relajante 
  #   ColorPlot="#E7C783" # Scatter
  #   ColorVA1= "#E7C783" # Vs
  #   ColorVA2= "#B8913D" # Vp
  #   
  #Base Verde 1: Relajante 
  ColorPlot="#FFE673" # Scatter
  ColorVA1= "#FFD200" # Vs
  ColorVA2= "#A68800" # Vp
  #  
  #   #Base Verde 2: Relajante 
  #   ColorPlot="#FFEE73" # Scatter
  #   ColorVA1= "#FFE100" # Vs
  #   ColorVA2= "#A69200" # Vp
  #   
  #   #Base Verde 2: Relajante 
  #   ColorPlot="#FFB773" # Scatter
  #   ColorVA1= "#FFB773" # Vs
  #   ColorVA2= "#FF7C00" # Vp
  
  
  
  #Scatterplot
  par(mar=c(5,5,0,0))
  plot(DatosN$Var1 ,DatosN$Var2, 
       xlim=c(min(vectorhis1),max(vectorhis1)), 
       ylim=c(min(vectorhis2),max(vectorhis2)), 
       xlab = XLAB, ylab=YLAB, pch= 21, col="black", 
       bg=ColorPlot, cex.lab = 1.2, cex=1.5, cex.axis = 1) 
  matplot(RVP1,RegQ1,type="l",col="red",lwd=2, add=TRUE)
  matplot(RVP2,RegQ2,type="l",col="green",lwd=2, add=TRUE)
  matplot(RVP3,RegQ3,type="l",col="blue",lwd=2, add=TRUE)
  
  legend(min(DatosN$Var1), max(DatosN$Var2), 
         c(Q1Lab, Q2Lab, Q3Lab), 
         col= c("Red","green","blue"), cex= 1.0, lty=1, lwd=2, bty = "n")
  #**
  
  #Histogram Var 1
  par(mar=c(0,5,1,0))
  histo1<-hist(DatosN$Var1, breaks= vectorhis1, plot= FALSE )  # Realiza un histograma de los datos, nclass es el numero de clases requeridas
  top <- max(histo1$counts)
  barplot(histo1$counts,  ylim=c(0, top+4), xlim=c(0,10), space=0, col= ColorVA1, ylab = "Frequency", cex.lab = 1, cex.axis=1, main = "")
  #**
  
  #Box plot Var 1
  par(mar=c(0,5,0,0))
  plot(0,0,type="n", xlim=c(min(vectorhis1),max(vectorhis1)), ylim=c(0,1.54), xaxt='n',yaxt='n', xlab="",ylab="")
  boxplot(DatosN$Var1,  range=1.5, ylim=c(min(vectorhis1),max(vectorhis1)), horizontal= TRUE, col= ColorVA1, pch= 22, axes= FALSE, add=TRUE ) # Realiza el grafico de caja
  #**
  
  #Histogram Var 2
  par(mar=c(5,0,0,1))
  histo2<-hist(DatosN$Var2, breaks= vectorhis2, plot= FALSE )  # Realiza un histograma de los datos, nclass es el numero de clases requeridas
  top <- max(histo2$counts)
  barplot(histo2$counts,  xlim=c(0, top+4),  space=0, col= ColorVA2, xlab = "Frequency", cex.lab = 1, cex.axis=1, main = "", horiz=TRUE)
  #**
  
  #Box plot Var 2
  par(mar=c(5,0,0,0))
  plot(0,0,type="n", ylim=c(min(vectorhis2),max(vectorhis2)), xlim=c(0,1.54), xaxt='n',yaxt='n', xlab="",ylab="")
  boxplot(DatosN$Var2,  range=1.5, ylim=c(min(vectorhis2),max(vectorhis2)), col= ColorVA2, pch= 22, axes= FALSE, add=TRUE ) # Realiza el grafico de caja
  #**
  
  #Plot Correlation coef.
  par(mar=c(0,0,0,0))
  plot(0,0,type="n",xlim = c(0, 60), ylim= c(0, 7), xaxt='n',yaxt='n', xlab="",ylab="")
  text(30,4, labels= "Pearson Coef= ", cex=1.3)
  text(40,3, labels= round(Corr,3),cex=1.3)
  
  # Compute statistics
  summaryVar1<-t(t(summary(Var1)))
  summaryVar2<-t(t(summary(Var2)))
  summaryRegQ1<-t(t(summary(RegQ1)))
  summaryRegQ2<-t(t(summary(RegQ2)))
  summaryRegQ3<-t(t(summary(RegQ3)))
  
  
  # Plot Statisttics 
  par(mar=c(0.5,1,0,1))
  plot(0,0,type="n",xlim = c(0, 60), ylim= c(0, 7), xaxt='n',yaxt='n', xlab="",ylab="")
  
  text(4,6,  labels= "             Min", cex=1.1)
  text(4,5,  labels= "1st Quartile", cex=1.1)
  text(4,4,  labels= "       Median", cex=1.1)
  text(4,3,  labels= "          Mean", cex=1.1)
  text(4,2,  labels= "3rd Quartile", cex=1.1)
  text(4,1,  labels= "            Max", cex=1.1)
  
  #Primary
  text(15,6.9, labels=XLAB,      cex=1)
  text(15,6, labels= sprintf( "%.5f",   summaryVar1[1,1] ), cex=1)
  text(15,5,   labels= sprintf( "%.5f", summaryVar1[2,1] ), cex=1)
  text(15,4,   labels= sprintf( "%.5f", summaryVar1[3,1] ), cex=1)
  text(15,3,   labels= sprintf( "%.5f", summaryVar1[4,1] ), cex=1)
  text(15,2,   labels= sprintf( "%.5f", summaryVar1[5,1] ), cex=1)
  text(15,1,   labels= sprintf( "%.5f", summaryVar1[6,1] ), cex=1)
  
  #Secondary
  text(24,6.9, labels= YLAB, cex=1)
  text(24,6,   labels=sprintf( "%.5f", summaryVar2[1,1] ), cex=1)
  text(24,5,   labels=sprintf( "%.5f", summaryVar2[2,1] ), cex=1)
  text(24,4,   labels=sprintf( "%.5f", summaryVar2[3,1] ), cex=1)
  text(24,3,   labels=sprintf( "%.5f", summaryVar2[4,1] ), cex=1)
  text(24,2,   labels=sprintf( "%.5f", summaryVar2[5,1] ), cex=1)
  text(24,1,   labels=sprintf( "%.5f", summaryVar2[6,1] ), cex=1)
  
  #REG Q1
  text(34,6.9, labels= Q1Lab, cex=1)
  text(34,6,   labels=sprintf( "%.5f", summaryRegQ1[1,1] ), cex=1)
  text(34,5,   labels=sprintf( "%.5f", summaryRegQ1[2,1] ), cex=1)
  text(34,4,   labels=sprintf( "%.5f", summaryRegQ1[3,1] ), cex=1)
  text(34,3,   labels=sprintf( "%.5f", summaryRegQ1[4,1] ), cex=1)
  text(34,2,   labels=sprintf( "%.5f", summaryRegQ1[5,1] ), cex=1)
  text(34,1,   labels=sprintf( "%.5f", summaryRegQ1[6,1] ), cex=1)
  
  #REG Q2
  text(44,6.9, labels= Q2Lab, cex=1)
  text(44,6,   labels=sprintf( "%.5f", summaryRegQ2[1,1] ), cex=1)
  text(44,5,   labels=sprintf( "%.5f", summaryRegQ2[2,1] ), cex=1)
  text(44,4,   labels=sprintf( "%.5f", summaryRegQ2[3,1] ), cex=1)
  text(44,3,   labels=sprintf( "%.5f", summaryRegQ2[4,1] ), cex=1)
  text(44,2,   labels=sprintf( "%.5f", summaryRegQ2[5,1] ), cex=1)
  text(44,1,   labels=sprintf( "%.5f", summaryRegQ2[6,1] ), cex=1)
  
  #REG Q3
  text(54,6.9, labels= Q3Lab, cex=1)
  text(54,6,   labels=sprintf( "%.5f", summaryRegQ3[1,1] ), cex=1)
  text(54,5,   labels=sprintf( "%.5f", summaryRegQ3[2,1] ), cex=1)
  text(54,4,   labels=sprintf( "%.5f", summaryRegQ3[3,1] ), cex=1)
  text(54,3,   labels=sprintf( "%.5f", summaryRegQ3[4,1] ), cex=1)
  text(54,2,   labels=sprintf( "%.5f", summaryRegQ3[5,1] ), cex=1)
  text(54,1,   labels=sprintf( "%.5f", summaryRegQ3[6,1] ), cex=1)
  
  
  box("outer", lty="solid", col="black")
  
}