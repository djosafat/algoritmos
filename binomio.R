choose(n,k) #función del paquete base de R para combinaciones.
choose(-3.7,8)
combinatoria <- function(n,k){prod(n:(n-k+1))/factorial(k)}
combinatoria(-3.7,8)
########################

####################################################
binomio <- function(a,b,x,sumandos=1000){
    alpha <- a/b
    aux <- 1*(abs(x)==floor(x))+1*(abs(x)==-floor(x))
    if(abs(alpha)<1 || aux==1){
        xx <- x
        if(abs(x)==-floor(x)){xx <- x; x <- -x}
        js <- 0:sumandos
        combinaciones <- choose(x,js)
        alphaj <- alpha^js
        bx <- b^x
        res <- sum(combinaciones*alphaj*bx)
        if(xx<0){res <- 1/res}
    }else{res <- 'no se cumple condición de convergencia'}
    res
}
a <- 3
b <- pi
x <- sqrt(2)
# (3+pi)^sqrt(2) =
binomio(a,b,x)
(3+pi)^sqrt(2) 
print(binomio(a,b,x),digits = 22)
print((3+pi)^sqrt(2),digits = 22)
#
print(binomio(a,b,x,sumandos = 100),digits = 22)
##############################
binomio(1,pi,5) # (1+pi)^(5)

binomio(1,1,5.5) # 2^(5.5)

binomio(0.5,1.5,5.5) # 2^(5.5)

binomio(1,2,5.5) # 3^(5.5)

binomio(1,pi,-5) # (1+pi)^(-5)

binomio(1,1,-5.5) # 2^(-5.5)

binomio(1,2,-5.5) # 3^(-5.5)

#################################################