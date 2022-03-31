#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#     NUEVAS METODOLOG?AS Y HERRAMIENTAS DE CARACTERIZACI?N EST?TICA Y DIN?MICA
#        CONSIDERANDO LAS PROPIEDADES FRACTALES DE LOS YACIMIENTOS PETROLEROS 
#                      Proyecto Fondos SENER-CONACYT  No. 143935 (Y-00114)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#         Obtiene la correlaci?n de Pearson de un conjunto de datos
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#                                        Autor: Javier M?ndez Venegas
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#                                          Fecha: Marzo de 2014
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
#                                   C?digo fuente de la aplicaci?n:


Correlacion<-function(datos,var_labels)
{
    
    Cperson<-cor(datos)
    
    print("Correlacion de Pearson")
    
    Correlaciones<-as.data.frame(rbind(Cperson))
    
    pairs(datos, cex.labels = 2, labels=var_labels)
    
    return(Correlaciones)
}