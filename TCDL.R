# Script para el proyecto final de simulación

library(ggplot2)
############# Teorema de la convergencia dominada de Lebesgue

### f1

set.seed(18)
N <- 1000000
ns <- c(1, seq(from = 10, to = 1000, by = 10))
enes <- length(ns)

f1n <- function(n, x){
  return((n*x+x)/n)
}

f1 <- function(x){
  return(x)
}

# Monte Carlo crudo
exps <- rexp(N, rate = 2)

indicadora <- function(x)ifelse(x<=3 & x>= 1, 1, 0)

# Para la funcion limite
x <- f1(exps)*indicadora(exps)
# Sacamos el estimador de MC
(Int_f1C <- mean(x))


# Ahora lo hacemos para la sucesion
Int_f1nC <- rep(0, enes)

for (k in 1:enes){
  Int_f1nC[k] <- mean(f1n(ns[k], exps)*indicadora(exps))
}



# Variadas antiteticas
mi_rexp <- function(n) {
  u1 <- runif(n/2)
  u2 <- 1-u1
  u <- c(u1, u2)
  return( -0.5*log(-u+1) )
}

expsA <- mi_rexp(N)

# Para la funcion limite
x <- f1(expsA)*indicadora(expsA)
# Sacamos el estimador de MC
(Int_f1A <- mean(x))


# Ahora lo hacemos para la sucesion
Int_f1nA <- rep(0, enes)

for (k in 1:enes){
  Int_f1nA[k] <- mean(f1n(ns[k], expsA)*indicadora(expsA))
}

#(Int_f1nA[990:1001])




### f2
set.seed(18)

N <- 1000000
ns <- c(1, seq(from = 10, to = 10000, by = 10))


f2n <- function(n, x){
  return( (n*x^3-10*n*x^2 + 25*n*x - 10*x + 25)/(1+n*x) )
}

f2 <- function(x){
  return((x-5)^2)
}

# Monte Carlo crudo
gammas <- rgamma(N, 5, 1)

# Para la funcion limite

# Sacamos el estimador de MC
(Int_f2C <- mean(f2(gammas)))

# Ahora lo hacemos para la sucesion
Int_f2nC <- rep(0, enes)

for (k in 1:enes){
  Int_f2nC[k] <- mean(f2n(ns[k], gammas))
}

#(Int_f2nC[990:1001])







# f3, cuando no se pueden conmutar
set.seed(19)
f3 <- function(x){
  return(rep(0, length(x)))
}

f3n <- function(n, x){
  return(ifelse(x> 0 & x<=1/n, n, 0))
}

# Para la funcion limite
u <- runif(N)

Int_f3C <- mean(f3(u))

# Para la sucesion

Int_f3nC <- rep(0, enes)

for (k in 1:enes){
  Int_f3nC[k] <- mean(f3n(ns[k], u))
}


#(Int_f3nC[990:1001])


# Con variadas antiteticas

u1 <- runif(N)
u2 <- 1-u1
u <- c(u1, u2)

Int_f3A <- mean(f3(u))

# Para la sucesion

Int_f3nA <- rep(0, enes)

for (k in 1:enes){
  Int_f3nA[k] <- mean(f3n(ns[k], u))
}

#(Int_f3nA[990:1001])


datosGraficarCrudo <- data.frame(N = ns, f1Crudo = Int_f1nC, f2Crudo = Int_f2nC,
                            f3Crudo = Int_f3nC)
datosGraficarAnti <- data.frame(N = ns, f1Anti = Int_f1nA, f3Anti = Int_f3nA)

library(reshape2)
datosGraficarCrudo.m <- melt(datosGraficarCrudo, id.vars=c("N"))
datosGraficarAnti.m <- melt(datosGraficarAnti, id.vars=c("N"))


plotCrudo <- ggplot(datosGraficarCrudo.m, aes(x = N, y = value, color = variable)) + geom_point(size = 0.1) + 
  ggtitle("Convergencia integrales", "Monte Carlo crudo") + theme_light() + scale_colour_manual(values = c("#00197d", "#6d008e", "#007013")) +
  geom_hline(yintercept = Int_f1C, color = "#00197d", alpha = 0.3) + geom_hline(yintercept = Int_f2C, color = "#8801ff", alpha = 0.3) + 
  geom_hline(yintercept = Int_f3C, color = "#00ab1d", alpha = 0.3) + ylab("Valor de la integral") + xlab("Termino de la sucesión")
show(plotCrudo)



plotAnti <- ggplot(datosGraficarAnti.m, aes(x = N, y = value, color = variable)) + geom_point(size = 0.1) + 
  ggtitle("Convergencia integrales", "Tecnicas de reducción de varianza") + theme_light() + scale_colour_manual(values = c("#00197d", "#007013")) +
  geom_hline(yintercept = Int_f1A, color = "#00197d", alpha = 0.3) + 
  geom_hline(yintercept = Int_f3A, color = "#00ab1d",  alpha = 0.3) + ylab("Valor de la integral") + xlab("Termino de la sucesión")
show(plotAnti)





