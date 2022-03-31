genmat.copem.Bernshtein <- function(u.vec,v.vec) {
#
# Genera matriz de valores de la copula Bernshtein construida
# a partir de la copula empirica
#
# Input: u.vec = valores de u
#        v.vec = valores de v
#        <matriz.copem> previamente calculada
#
# Output: lista con [[1]] vector de valores u
#                   [[2]] vector de valores v
#                   [[3]] matriz de valores de copula Bernshtein empirica
#
# Observaciones: utiliza el programa <<copula.Bernshtein.emp>>
#
copula.B <- matrix(0,nrow=length(u.vec),ncol=length(v.vec))
for (i in 1:(length(u.vec))){
for (j in 1:(length(v.vec))){
copula.B[i,j] <- copula.Bernshtein.emp(u.vec[i],v.vec[j])
}
}
return(list(u=u.vec,v=v.vec,copemB=copula.B))
}
