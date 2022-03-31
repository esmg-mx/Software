genmat.densidad.Bernshtein <- function(x.vec,y.vec) {
#
# Genera matriz de valores de la densidad Bernshtein construida
# a partir de la copula empirica y marginales empiricas
#
# Input: x.vec = valores de x
#        y.vec = valores de y
#
# Output: lista con [[1]] vector de valores x
#                   [[2]] vector de valores y
#                   [[3]] matriz de valores de la densidad Bernshtein empirica
#
# Observaciones: utiliza el programa <<densidad.Bernshtein.emp>>,
# la copula empirica debe estar previamente generada en
# <matriz.copem>
#
densidad <- matrix(0,nrow=length(x.vec),ncol=length(y.vec))
for (i in 1:(length(x.vec))){
for (j in 1:(length(y.vec))){
densidad[i,j] <- densidad.Bernshtein.emp(x.vec[i],y.vec[j])
}
}
return(list(x=x.vec,y=y.vec,densidad=densidad))
}


