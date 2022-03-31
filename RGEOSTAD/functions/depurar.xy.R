depurar.xy <- function(mat.xy){
#
# Input: matriz de dos columnas
#
# Output: matriz de dos columnas sin valores repetidos
#
# --------------------
#
# Ordenamos la matriz en la primera columna:
#
n <- dim(mat.xy)[1]
matriz.ord <- mat.xy
orden <- order(mat.xy[, 1])
for(i in 1:n) {
matriz.ord[i, ] <- mat.xy[orden[i], ]
}
#
# Depuramos de acuerdo a la primera columna
#
i <- 1
rango.bloque <- which(matriz.ord[,1]==matriz.ord[i,1])
matriz.dep <- matrix(c(matriz.ord[i,1],median(matriz.ord[
rango.bloque,2])),ncol=2,nrow=1)
i <- max(rango.bloque) + 1
while(i <= n){
rango.bloque <- which(matriz.ord[,1]==matriz.ord[i,1])
matriz.dep <- rbind(matriz.dep, c(matriz.ord[i,1],
median(matriz.ord[rango.bloque,2])) )
i <- max(rango.bloque) + 1
}
#
# Intercambiamos columnas 1 y 2
#
mat.xy <- matriz.dep[,2:1]
#
# Ordenamos la matriz en la primera columna:
#
n <- dim(mat.xy)[1]
matriz.ord <- mat.xy
orden <- order(mat.xy[, 1])
for(i in 1:n) {
matriz.ord[i, ] <- mat.xy[orden[i], ]
}
#
# Depuramos de acuerdo a la primera columna
#
i <- 1
rango.bloque <- which(matriz.ord[,1]==matriz.ord[i,1])
matriz.dep <- matrix(c(matriz.ord[i,1],median(matriz.ord[
rango.bloque,2])),ncol=2,nrow=1)
i <- max(rango.bloque) + 1
while(i <= n){
rango.bloque <- which(matriz.ord[,1]==matriz.ord[i,1])
matriz.dep <- rbind(matriz.dep,c(matriz.ord[i,1],
median(matriz.ord[rango.bloque,2])) )
i <- max(rango.bloque) + 1
}
#
# Intercambiamos columnas 1 y 2 nuevamente
#
mat.xy <- matriz.dep[,2:1]
#
# Ordenamos la matriz en la primera columna:
#
n <- dim(mat.xy)[1]
matriz.ord <- mat.xy
orden <- order(mat.xy[, 1])
for(i in 1:n) {
matriz.ord[i, ] <- mat.xy[orden[i], ]
}
#
return(matriz.ord)
}

