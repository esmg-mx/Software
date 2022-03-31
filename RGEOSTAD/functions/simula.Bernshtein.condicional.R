simula.Bernshtein.condicional <- function(x,tam.muestra){
#
# Simula Y dado X=x, a partir de la distribuci’on conjunta de
# Bernshtein ajustada
#
# Entrada: x = valor de X en que se condiciona
# tam.muestra = deseado
# <muestra> original previamente cargada
# <matriz.copem> previamente calculada
#
# Salida: vector de simulaciones
#
# Dependencias: programas <cv.du.inv> , <Fn.Bernshtein> ,
#               <Fn.inv.Bernshtein>
#
uu <- runif(tam.muestra)
Fx <- Fn.Bernshtein(x)
v <- sapply(uu,cv.du.inv,u=Fx)
y <- sapply(v,Fn.inv.Bernshtein,valores.emp=muestra[,2])
return(y)
}
