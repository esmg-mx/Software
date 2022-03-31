fn.Bernshtein <- function(x) 1/dFn.inv.Bernshtein(Fn.Bernshtein(x),
muestra[,1])

Gn.Bernshtein.aux <- function(u,xdada) Fn.inv.Bernshtein(u,
muestra[,2]) - xdada

Gn.Bernshtein <- function(x) uniroot(Gn.Bernshtein.aux,interval=
c(0,1),xdada=x,tol=tolerancia)$root

gn.Bernshtein <- function(x) 1/dFn.inv.Bernshtein(Gn.Bernshtein(x),
muestra[,2])

densidad.Bernshtein.emp <- function(x,y) dcopula.Bernshtein.emp(
Fn.Bernshtein(x),Gn.Bernshtein(y))*fn.Bernshtein(x)*gn.Bernshtein(y)

densidad.Bernshtein.emp.ydadox <- function(y,x) dcopula.Bernshtein.emp(
Fn.Bernshtein(x),Gn.Bernshtein(y))*gn.Bernshtein(y)

cv.du.aux <- function(v,ua.vec) cv.du(ua.vec[1],v) - ua.vec[2]

regresion.copulaB <- function(u,cuantil) uniroot(cv.du.aux,interval=
c(0,1),ua.vec=c(u,cuantil),tol=tolerancia)$root

regresion <- function(x,cuantil) Fn.inv.Bernshtein(regresion.copulaB(
Fn.Bernshtein(x),cuantil),muestra[,2])

cv.du.inv <- function(u,a) uniroot(cv.du.aux,interval=
c(0,1),ua.vec=c(u,a),tol=tolerancia)$root

