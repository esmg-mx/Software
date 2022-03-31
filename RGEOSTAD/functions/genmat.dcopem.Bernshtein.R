genmat.dcopem.Bernshtein <- function(u.vec,v.vec) {
#
# Genera matriz de valores de la densidad-copula Bernshtein construida
# a partir de la copula empirica
#
# Input: u.vec = valores de u
# v.vec = valores de u
# <matriz.copem> previamente calculada
#
# Output:lista con [[1]] vector de valores u
#                  [[2]] vector de valores v
#                  [[3]] matriz de valores de densidad-copula Bernstein empirica
#
# Observaciones: utiliza los programas <<dcopula.Bernshtein.emp>>
#
dcopula.B <- matrix(0,nrow=length(u.vec),ncol=length(v.vec))
for (i in 1:(length(u.vec))){
for (j in 1:(length(v.vec))){
dcopula.B[i,j] <- dcopula.Bernshtein.emp(u.vec[i],v.vec[j])
}
}
return(list(u=u.vec,v=v.vec,dcopemB=dcopula.B))
}

