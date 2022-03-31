transf.muestra.copula <- function(){
#
# Transformar la muestra original a pseudo-observaciones de la
# cópula subyacente
#
# Entrada: ninguna, pero previamente debe estar cargado el
# archivo <muestra> (bivariada)
#
# Salida: matriz de n x 2 (n=tam.muestra) con las
# pseudo-observaciones de la c’opula
#
# Dependencias: utiliza los programas <Fn.Bernshtein>, <Gn.Bernshtein>
#
u <- sapply(muestra[,1],Fn.Bernshtein)
v <- sapply(muestra[,2],Gn.Bernshtein)
return(cbind(u,v))
}

