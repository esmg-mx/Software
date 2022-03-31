Fn.Bernshtein <- function(x) uniroot(Fn.Bernshtein.aux,interval=
c(0,1),xdada=x,tol=tolerancia)$root
