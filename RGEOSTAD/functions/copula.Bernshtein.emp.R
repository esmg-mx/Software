copula.Bernshtein.emp <- function(u,v) sum(matriz.copem*
(dbinom(0:(dim(matriz.copem)[1]-1),dim(matriz.copem)[1]-1,u)%*%
t(dbinom(0:(dim(matriz.copem)[1]-1),dim(matriz.copem)[1]-1,v))))

