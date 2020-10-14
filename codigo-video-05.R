Hn <- function(n){
    sum(1/(1:n))
}
#H_4
1+1/2+1/3+1/4
Hn(4)

###########################
###########################

###########################
# Ejercicio (con fórmula)
choose(6,4)*(Hn(6)-1/(4))
# Ejercicio (sin fórmula)
choose(1,3)*Hn(1)+choose(2,3)*Hn(2)+choose(3,3)*Hn(3)+choose(4,3)*Hn(4)+choose(5,3)*Hn(5)
##
# Usando vectores
choose(1:5,3)
Hn(1:5) # Esto causará un error
c(Hn(1),Hn(2),Hn(3),Hn(4),Hn(5))
# De forma automática:
sapply(1:5,Hn)
# Solución a ejercicio:
sum(choose(1:5,3)*sapply(1:5,Hn))
#######################################################