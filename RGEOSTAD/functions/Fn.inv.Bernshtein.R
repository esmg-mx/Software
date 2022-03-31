Fn.inv.Bernshtein <- function(u,valores.emp){
#
# Ajusta polinomio de Bernshtein a una cuasi-inversa
# de la funci’on de distribuci’on emp’irica
#
# Entrada: u = valor a evaluar, en [0,1]
#          valores.emp = valores observados (sin repetici’on)
#
# Salida: valor de Fn^(-1)(u)
#
x <- sort(valores.emp)
n <- length(x)
xm <- rep(0,n+1)
for (j in 2:n){
xm[j] <- (x[j-1] + x[j]) / 2
}
xm[1] <- x[1]
xm[n+1] <- x[n]
return(sum(xm*dbinom(0:n,n,u)))
}
