simula.Bernshtein <- function(tam.muestra){
#
# Simula observaciones a partir de la distribuci’on conjunta
# de Bernshtein
#
# Entrada: tam.muestra
# <muestra> previamente capturada
# <matriz.copem> previamete calculada
#
# Salida: matriz de tam.muestra x 2 con las simulaciones
#         matriz de tam.muestra x 2 con las simulaciones de la c’opula
#
# Dependencias: utiliza los programas <simula.copula.Bernshtein>
#
#               <Fn.inv.Bernshtein>
#
sim.copula <- simula.copula.Bernshtein(tam.muestra)
x <- sapply(sim.copula[,1],Fn.inv.Bernshtein,valores.emp=muestra[,1])
y <- sapply(sim.copula[,2],Fn.inv.Bernshtein,valores.emp=muestra[,2])
simulaciones <- cbind(x,y)
return(list(sim.xy=simulaciones,sim.copula=sim.copula))
}
