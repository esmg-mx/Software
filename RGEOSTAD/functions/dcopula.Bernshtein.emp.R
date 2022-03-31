dcopula.Bernshtein.emp <- function(u,v) ((dim(matriz.copem)[1]-1)^2)*
sum(matriz.copem*((dbinom(-1:(dim(matriz.copem)[1]-2),dim(matriz.copem)[
1]-2,u)-dbinom(0:(dim(matriz.copem)[1]-1),dim(matriz.copem)[1]-2,u)*c(-1,
rep(1,dim(matriz.copem)[1]-1)))%*%t(dbinom(-1:(dim(matriz.copem)[1]-2),
dim(matriz.copem)[1]-2,v) - dbinom(0:(dim(matriz.copem)[1]-1),
dim(matriz.copem)[1]-2,v)*c(-1,rep(1,dim(matriz.copem)[1]-1)))))
