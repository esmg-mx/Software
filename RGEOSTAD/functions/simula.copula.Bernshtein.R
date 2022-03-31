simula.copula.Bernshtein <- function(tam.muestra){
#
# Simula observaciones bivariadas de la c’opula Bernshtein ajustada
# a la muestra original
#
# Entrada: tam.muestra
# <muestra> previamente cargada
# <matriz.copem> previamente calculada
#
# Salida: matriz de tam.muestra x 2 con las simulaciones
#
# Dependencias: utiliza el programa <cv.du.inv>
#
uu <- runif(tam.muestra)
tt <- runif(tam.muestra)
vv <- mapply(cv.du.inv,u=uu,a=tt)
return(cbind(uu,vv))
}
