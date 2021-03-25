library(openxlsx)
library(stringr)
library(magrittr)
library(tidyverse)
library(dplyr)
library(nortest) 
library(caret) 
library(magrittr)
library(kableExtra)
data_banco <- read.xlsx("BD/Data_Banco.xlsx")

data_sucursal <- read.xlsx("BD/Data_Banco.xlsx",
                           sheet = "Data_Sucursal")
data_cajero <- read.xlsx("BD/Data_Banco.xlsx",
                         sheet = "Data_Cajero")
data_banco <- data_banco %>%
  mutate( Monto= str_replace(Monto, pattern = ",", replacement = ".") ) %>%
  mutate(Sucursal= as.character(Sucursal),
         Cajero = as.character(Cajero),
         Satisfaccion = parse_factor(Satisfaccion,
                                     levels= c('Muy Malo', 'Malo', 'Regular', 'Bueno', 'Muy Bueno')),
         Monto= parse_number(Monto, locale = locale(decimal_mark = ".")))

data_sucursal <- data_sucursal %>%
  mutate(ID_Sucursal= as.character(ID_Sucursal))
data_cajero <- data_cajero %>%
  mutate(Cajero= as.character(Cajero))

data_banco <- data_banco %>%
  rename("ID_Sucursal"="Sucursal") %>%
  left_join(data_sucursal, by= c("ID_Sucursal")) %>%
  left_join(data_cajero, by= c("Cajero"))


#####intervalos bootstrap#######






######ejemplo1

caj4353<-data_banco[data_banco$Cajero=="4353",]
T1<-caj4353$Tiempo_Servicio_seg
plot(density(T1))
shapiro.test(T1)

mu1 <- mean(T1)
n <- length(T1)
alfa <- 0.05
namesI <- paste0(100*c(alfa/2, 1-alfa/2), "%")

B <- 1000
percentil <- numeric(B)
percentilt <- numeric(B)
percentilts <- numeric(B)

nsim <- 500
resultados <- array(dim = c(nsim, 2, 4))
dimnames(resultados) <- list(NULL, c("Cobertura", "Longitud"),
                             c("Normal", "Percentil", "Percentil-t", "Percentil-t simetrizado"))


# Bucle simulación
set.seed(1)
for (isim in 1:nsim) {
  # Aproximación clásica
  muestra <- sample(T1,n,replace = TRUE)
  media <- mean(muestra)
  desv <- sd(muestra)
  z <- qnorm(1 - alfa/2)
  ic_inf <- media - z*desv/sqrt(n)
  ic_sup <- media + z*desv/sqrt(n)
  I0 <- c(ic_inf, ic_sup)
  # names(I0) <- namesI
  resultados[isim, 1, 1] <- (I0[1] < mu1) && (mu1 < I0[2])
  resultados[isim, 2, 1] <- I0[2] - I0[1]
  
  # Remuestreo bootstrap
  for (k in 1:B) {
    remuestra <- sample(muestra, n, replace = TRUE)
    percentil[k] <- sqrt(n) * (mean(remuestra) - media)
    percentilt[k] <- percentil[k]/sd(remuestra)
    percentilts[k] <- abs(percentilt[k])
  }
  
  # Aproximación bootstrap percentil
  pto_crit <- quantile(percentil, c(alfa/2, 1 - alfa/2))
  # Construcción del IC
  ic_inf_boot <- media - pto_crit[2]/sqrt(n)
  ic_sup_boot <- media - pto_crit[1]/sqrt(n)
  I1 <- c(ic_inf_boot, ic_sup_boot)
  # names(I1) <- namesI
  resultados[isim, 1, 2] <- (I1[1] < mu1) && (mu1 < I1[2])
  resultados[isim, 2, 2] <- I1[2] - I1[1]
  
  # Aproximación bootstrap percentil-t
  pto_crit <- quantile(percentilt, c(alfa/2, 1 - alfa/2))
  # Construcción del IC
  ic_inf_boot <- media - pto_crit[2] * desv/sqrt(n)
  ic_sup_boot <- media - pto_crit[1] * desv/sqrt(n)
  I2 <- c(ic_inf_boot, ic_sup_boot)
  # names(I2) <- namesI
  resultados[isim, 1, 3] <- (I2[1] < mu1) && (mu1 < I2[2])
  resultados[isim, 2, 3] <- I2[2] - I2[1]
  
  # Aproximación bootstrap percentil-t simetrizado
  pto_crit <- quantile(percentilts, 1 - alfa)
  # Construcción del IC
  ic_inf_boot <- media - pto_crit * desv/sqrt(n)
  ic_sup_boot <- media + pto_crit * desv/sqrt(n)
  I3 <- c(ic_inf_boot, ic_sup_boot)
  # names(I3) <- namesI
  resultados[isim, 1, 4] <- (I3[1] < mu1) && (mu1 < I3[2])
  resultados[isim, 2, 4] <- I3[2] - I3[1]
}
apply(resultados, c(2, 3), mean)%>% kable() %>% kable_styling()







###ejemplo 2

##Consideremos una muestra de una distribución normal N(0,1)
##de tamaño 25.

y <- rnorm(25,0,1)
muestra2 <- y
n <- length(muestra2)
alfa <- 0.05
x_barra <- mean(muestra2)
cuasi_dt <- sd(muestra2)

# Remuestreo
set.seed(1)
B <- 1000
remuestra <- numeric(n)
estadistico_boot <- numeric(B)
for (k in 1:B) {
  remuestra <- sample(muestra2, n, replace = TRUE)
  x_barra_boot <- mean(remuestra)
  cuasi_dt_boot <- sd(remuestra)
  estadistico_boot[k] <- sqrt(n) * abs(x_barra_boot - x_barra)/cuasi_dt_boot
}

# Aproximación bootstrap del pto crítico
pto_crit <- quantile(estadistico_boot, 1 - alfa)

# Construcción del IC
ic_inf_boot <- x_barra - pto_crit * cuasi_dt/sqrt(n)
ic_sup_boot <- x_barra + pto_crit * cuasi_dt/sqrt(n)
IC_boot <- c(ic_inf_boot, ic_sup_boot)
names(IC_boot) <- paste0(100*c(alfa/2, 1-alfa/2), "%")
IC_boot
t.test(y)
t.ini <- proc.time()
rate <- 0.01
mu <- 1/rate
n <- 100

alfa <- 0.05
namesI <- paste0(100*c(alfa/2, 1-alfa/2), "%")

B <- 1000
percentil <- numeric(B)
percentilt <- numeric(B)
percentilts <- numeric(B)

nsim <- 500
resultados <- array(dim = c(nsim, 2, 4))
dimnames(resultados) <- list(NULL, c("Cobertura", "Longitud"),
                             c("Normal", "Percentil", "Percentil-t", "Percentil-t simetrizado"))
# Bucle simulación
set.seed(1)
for (isim in 1:nsim) {
  # Aproximación clásica
  muestra2 <- rexp(n, rate = 0.01)
  media <- mean(muestra2)
  desv <- sd(muestra2)
  z <- qnorm(1 - alfa/2)
  ic_inf <- media - z*desv/sqrt(n)
  ic_sup <- media + z*desv/sqrt(n)
  I0 <- c(ic_inf, ic_sup)
  # names(I0) <- namesI
  resultados[isim, 1, 1] <- (I0[1] < mu) && (mu < I0[2])
  resultados[isim, 2, 1] <- I0[2] - I0[1]
  
  # Remuestreo bootstrap
  for (k in 1:B) {
    remuestra <- sample(muestra2, n, replace = TRUE)
    percentil[k] <- sqrt(n) * (mean(remuestra) - media)
    percentilt[k] <- percentil[k]/sd(remuestra)
    percentilts[k] <- abs(percentilt[k])
  }
  
  # Aproximación bootstrap percentil
  pto_crit <- quantile(percentil, c(alfa/2, 1 - alfa/2))
  # Construcción del IC
  ic_inf_boot <- media - pto_crit[2]/sqrt(n)
  ic_sup_boot <- media - pto_crit[1]/sqrt(n)
  I1 <- c(ic_inf_boot, ic_sup_boot)
  # names(I1) <- namesI
  resultados[isim, 1, 2] <- (I1[1] < mu) && (mu < I1[2])
  resultados[isim, 2, 2] <- I1[2] - I1[1]
  
  # Aproximación bootstrap percentil-t
  pto_crit <- quantile(percentilt, c(alfa/2, 1 - alfa/2))
  # Construcción del IC
  ic_inf_boot <- media - pto_crit[2] * desv/sqrt(n)
  ic_sup_boot <- media - pto_crit[1] * desv/sqrt(n)
  I2 <- c(ic_inf_boot, ic_sup_boot)
  # names(I2) <- namesI
  resultados[isim, 1, 3] <- (I2[1] < mu) && (mu < I2[2])
  resultados[isim, 2, 3] <- I2[2] - I2[1]
  
  # Aproximación bootstrap percentil-t simetrizado
  pto_crit <- quantile(percentilts, 1 - alfa)
  # Construcción del IC
  ic_inf_boot <- media - pto_crit * desv/sqrt(n)
  ic_sup_boot <- media + pto_crit * desv/sqrt(n)
  I3 <- c(ic_inf_boot, ic_sup_boot)
  # names(I3) <- namesI
  resultados[isim, 1, 4] <- (I3[1] < mu) && (mu < I3[2])
  resultados[isim, 2, 4] <- I3[2] - I3[1]
}

t.fin <- proc.time() - t.ini
t.fin
apply(resultados, c(2, 3), mean)%>% kable() %>% kable_styling()







#####ejemplo 3


data <- data_banco %>% 
  filter(data_banco$Satisfaccion=="Bueno",
         data_banco$Sucursal=="Riocentro Sur",
         data_banco$Transaccion=="Cobro/Pago (Cta externa)")

z <- data$Tiempo_Servicio_seg
muestra3 <- z
n <- length(muestra3)
alfa <- 0.05
x_barra <- mean(muestra3)
cuasi_dt <- sd(muestra3)

# Remuestreo
set.seed(1)
B <- 1000
remuestra <- numeric(n)
estadistico_boot <- numeric(B)
for (k in 1:B) {
  remuestra <- sample(muestra3, n, replace = TRUE)
  x_barra_boot <- mean(remuestra)
  cuasi_dt_boot <- sd(remuestra)
  estadistico_boot[k] <- sqrt(n) * abs(x_barra_boot - x_barra)/cuasi_dt_boot
}

# Aproximación bootstrap del pto crítico
pto_crit <- quantile(estadistico_boot, 1 - alfa)

# Construcción del IC
ic_inf_boot <- x_barra - pto_crit * cuasi_dt/sqrt(n)
ic_sup_boot <- x_barra + pto_crit * cuasi_dt/sqrt(n)
IC_boot <- c(ic_inf_boot, ic_sup_boot)
names(IC_boot) <- paste0(100*c(alfa/2, 1-alfa/2), "%")
IC_boot
t.test(z)
t.ini <- proc.time()
rate <- 0.01
mu <- 1/rate
n <- 100

alfa <- 0.05
namesI <- paste0(100*c(alfa/2, 1-alfa/2), "%")

B <- 1000
percentil <- numeric(B)
percentilt <- numeric(B)
percentilts <- numeric(B)

nsim <- 500
resultados <- array(dim = c(nsim, 2, 4))
dimnames(resultados) <- list(NULL, c("Cobertura", "Longitud"),
                             c("Normal", "Percentil", "Percentil-t", "Percentil-t simetrizado"))
# Bucle simulación
set.seed(1)
for (isim in 1:nsim) {
  # Aproximación clásica
  muestra3 <- rexp(n, rate = 0.01)
  media <- mean(muestra3)
  desv <- sd(muestra3)
  z <- qnorm(1 - alfa/2)
  ic_inf <- media - z*desv/sqrt(n)
  ic_sup <- media + z*desv/sqrt(n)
  I0 <- c(ic_inf, ic_sup)
  # names(I0) <- namesI
  resultados[isim, 1, 1] <- (I0[1] < mu) && (mu < I0[2])
  resultados[isim, 2, 1] <- I0[2] - I0[1]
  
  # Remuestreo bootstrap
  for (k in 1:B) {
    remuestra <- sample(muestra3, n, replace = TRUE)
    percentil[k] <- sqrt(n) * (mean(remuestra) - media)
    percentilt[k] <- percentil[k]/sd(remuestra)
    percentilts[k] <- abs(percentilt[k])
  }
  
  # Aproximación bootstrap percentil
  pto_crit <- quantile(percentil, c(alfa/2, 1 - alfa/2))
  # Construcción del IC
  ic_inf_boot <- media - pto_crit[2]/sqrt(n)
  ic_sup_boot <- media - pto_crit[1]/sqrt(n)
  I1 <- c(ic_inf_boot, ic_sup_boot)
  # names(I1) <- namesI
  resultados[isim, 1, 2] <- (I1[1] < mu) && (mu < I1[2])
  resultados[isim, 2, 2] <- I1[2] - I1[1]
  
  # Aproximación bootstrap percentil-t
  pto_crit <- quantile(percentilt, c(alfa/2, 1 - alfa/2))
  # Construcción del IC
  ic_inf_boot <- media - pto_crit[2] * desv/sqrt(n)
  ic_sup_boot <- media - pto_crit[1] * desv/sqrt(n)
  I2 <- c(ic_inf_boot, ic_sup_boot)
  # names(I2) <- namesI
  resultados[isim, 1, 3] <- (I2[1] < mu) && (mu < I2[2])
  resultados[isim, 2, 3] <- I2[2] - I2[1]
  
  # Aproximación bootstrap percentil-t simetrizado
  pto_crit <- quantile(percentilts, 1 - alfa)
  # Construcción del IC
  ic_inf_boot <- media - pto_crit * desv/sqrt(n)
  ic_sup_boot <- media + pto_crit * desv/sqrt(n)
  I3 <- c(ic_inf_boot, ic_sup_boot)
  # names(I3) <- namesI
  resultados[isim, 1, 4] <- (I3[1] < mu) && (mu < I3[2])
  resultados[isim, 2, 4] <- I3[2] - I3[1]
}

t.fin <- proc.time() - t.ini
t.fin
apply(resultados, c(2, 3), mean)%>% kable() %>% kable_styling()








#############ejemplo3
data <- data_banco %>% 
  filter(data_banco$Satisfaccion=="Bueno",
         data_banco$Sucursal=="Riocentro Sur",
         data_banco$Transaccion=="Cobro/Pago (Cta externa)")

z <- data$Tiempo_Servicio_seg
muestra3 <- z
plot(density(z))
n <- length(muestra3)
alfa <- 0.05
x_barra <- mean(muestra3)
cuasi_dt <- sd(muestra3)

# Remuestreo
set.seed(1)
B <- 1000
remuestra <- numeric(n)
estadistico_boot <- numeric(B)
for (k in 1:B) {
  remuestra <- sample(muestra3, n, replace = TRUE)
  x_barra_boot <- mean(remuestra)
  cuasi_dt_boot <- sd(remuestra)
  estadistico_boot[k] <- sqrt(n) * abs(x_barra_boot - x_barra)/cuasi_dt_boot
}

# Aproximación bootstrap del pto crítico
pto_crit <- quantile(estadistico_boot, 1 - alfa)

# Construcción del IC
ic_inf_boot <- x_barra - pto_crit * cuasi_dt/sqrt(n)
ic_sup_boot <- x_barra + pto_crit * cuasi_dt/sqrt(n)
IC_boot <- c(ic_inf_boot, ic_sup_boot)
names(IC_boot) <- paste0(100*c(alfa/2, 1-alfa/2), "%")
IC_boot
t.test(z)
t.ini <- proc.time()
rate <- 0.01
mu <- 1/rate
n <- 100

alfa <- 0.05
namesI <- paste0(100*c(alfa/2, 1-alfa/2), "%")

B <- 1000
percentil <- numeric(B)
percentilt <- numeric(B)
percentilts <- numeric(B)

nsim <- 500
resultados <- array(dim = c(nsim, 2, 4))
dimnames(resultados) <- list(NULL, c("Cobertura", "Longitud"),
                             c("Normal", "Percentil", "Percentil-t", "Percentil-t simetrizado"))
# Bucle simulación
set.seed(1)
for (isim in 1:nsim) {
  # Aproximación clásica
  muestra3 <- rexp(n, rate = 0.01)
  media <- mean(muestra3)
  desv <- sd(muestra3)
  z <- qnorm(1 - alfa/2)
  ic_inf <- media - z*desv/sqrt(n)
  ic_sup <- media + z*desv/sqrt(n)
  I0 <- c(ic_inf, ic_sup)
  # names(I0) <- namesI
  resultados[isim, 1, 1] <- (I0[1] < mu) && (mu < I0[2])
  resultados[isim, 2, 1] <- I0[2] - I0[1]
  
  # Remuestreo bootstrap
  for (k in 1:B) {
    remuestra <- sample(muestra3, n, replace = TRUE)
    percentil[k] <- sqrt(n) * (mean(remuestra) - media)
    percentilt[k] <- percentil[k]/sd(remuestra)
    percentilts[k] <- abs(percentilt[k])
  }
  
  # Aproximación bootstrap percentil
  pto_crit <- quantile(percentil, c(alfa/2, 1 - alfa/2))
  # Construcción del IC
  ic_inf_boot <- media - pto_crit[2]/sqrt(n)
  ic_sup_boot <- media - pto_crit[1]/sqrt(n)
  I1 <- c(ic_inf_boot, ic_sup_boot)
  # names(I1) <- namesI
  resultados[isim, 1, 2] <- (I1[1] < mu) && (mu < I1[2])
  resultados[isim, 2, 2] <- I1[2] - I1[1]
  
  # Aproximación bootstrap percentil-t
  pto_crit <- quantile(percentilt, c(alfa/2, 1 - alfa/2))
  # Construcción del IC
  ic_inf_boot <- media - pto_crit[2] * desv/sqrt(n)
  ic_sup_boot <- media - pto_crit[1] * desv/sqrt(n)
  I2 <- c(ic_inf_boot, ic_sup_boot)
  # names(I2) <- namesI
  resultados[isim, 1, 3] <- (I2[1] < mu) && (mu < I2[2])
  resultados[isim, 2, 3] <- I2[2] - I2[1]
  
  # Aproximación bootstrap percentil-t simetrizado
  pto_crit <- quantile(percentilts, 1 - alfa)
  # Construcción del IC
  ic_inf_boot <- media - pto_crit * desv/sqrt(n)
  ic_sup_boot <- media + pto_crit * desv/sqrt(n)
  I3 <- c(ic_inf_boot, ic_sup_boot)
  # names(I3) <- namesI
  resultados[isim, 1, 4] <- (I3[1] < mu) && (mu < I3[2])
  resultados[isim, 2, 4] <- I3[2] - I3[1]
}

t.fin <- proc.time() - t.ini
t.fin
apply(resultados, c(2, 3), mean)%>% kable() %>% kable_styling()








#####ejemplo 4
base <- data_banco %>% dplyr::filter(Transaccion=="Cobrar cheque (Cta del Bco)" & Cajero=="2230")
x <- base$Tiempo_Servicio_seg
muestra1 <- x
plot(density(x))
n <- length(muestra1)
alfa <- 0.05
x_barra <- mean(muestra1)
cuasi_dt <- sd(muestra1)

# Remuestreo
set.seed(1)
B <- 1000
remuestra <- numeric(n)
estadistico_boot <- numeric(B)
for (k in 1:B) {
  remuestra <- sample(muestra1, n, replace = TRUE)
  x_barra_boot <- mean(remuestra)
  cuasi_dt_boot <- sd(remuestra)
  estadistico_boot[k] <- sqrt(n) * abs(x_barra_boot - x_barra)/cuasi_dt_boot
}

# Aproximación bootstrap del pto crítico
pto_crit <- quantile(estadistico_boot, 1 - alfa)

# Construcción del IC
ic_inf_boot <- x_barra - pto_crit * cuasi_dt/sqrt(n)
ic_sup_boot <- x_barra + pto_crit * cuasi_dt/sqrt(n)
IC_boot <- c(ic_inf_boot, ic_sup_boot)
names(IC_boot) <- paste0(100*c(alfa/2, 1-alfa/2), "%")
IC_boot
t.test(x)

t.ini <- proc.time()
rate <- 0.01
mu <- 1/rate
n <- 100

alfa <- 0.05
namesI <- paste0(100*c(alfa/2, 1-alfa/2), "%")

B <- 1000
percentil <- numeric(B)
percentilt <- numeric(B)
percentilts <- numeric(B)

nsim <- 500
resultados <- array(dim = c(nsim, 2, 4))
dimnames(resultados) <- list(NULL, c("Cobertura", "Longitud"),
                             c("Normal", "Percentil", "Percentil-t", "Percentil-t simetrizado"))
# Bucle simulación
set.seed(1)
for (isim in 1:nsim) {
  # Aproximación clásica
  muestra1 <- rexp(n, rate = 0.01)
  media <- mean(muestra1)
  desv <- sd(muestra1)
  z <- qnorm(1 - alfa/2)
  ic_inf <- media - z*desv/sqrt(n)
  ic_sup <- media + z*desv/sqrt(n)
  I0 <- c(ic_inf, ic_sup)
  # names(I0) <- namesI
  resultados[isim, 1, 1] <- (I0[1] < mu) && (mu < I0[2])
  resultados[isim, 2, 1] <- I0[2] - I0[1]
  
  # Remuestreo bootstrap
  for (k in 1:B) {
    remuestra <- sample(muestra1, n, replace = TRUE)
    percentil[k] <- sqrt(n) * (mean(remuestra) - media)
    percentilt[k] <- percentil[k]/sd(remuestra)
    percentilts[k] <- abs(percentilt[k])
  }
  
  # Aproximación bootstrap percentil
  pto_crit <- quantile(percentil, c(alfa/2, 1 - alfa/2))
  # Construcción del IC
  ic_inf_boot <- media - pto_crit[2]/sqrt(n)
  ic_sup_boot <- media - pto_crit[1]/sqrt(n)
  I1 <- c(ic_inf_boot, ic_sup_boot)
  # names(I1) <- namesI
  resultados[isim, 1, 2] <- (I1[1] < mu) && (mu < I1[2])
  resultados[isim, 2, 2] <- I1[2] - I1[1]
  
  # Aproximación bootstrap percentil-t
  pto_crit <- quantile(percentilt, c(alfa/2, 1 - alfa/2))
  # Construcción del IC
  ic_inf_boot <- media - pto_crit[2] * desv/sqrt(n)
  ic_sup_boot <- media - pto_crit[1] * desv/sqrt(n)
  I2 <- c(ic_inf_boot, ic_sup_boot)
  # names(I2) <- namesI
  resultados[isim, 1, 3] <- (I2[1] < mu) && (mu < I2[2])
  resultados[isim, 2, 3] <- I2[2] - I2[1]
  
  # Aproximación bootstrap percentil-t simetrizado
  pto_crit <- quantile(percentilts, 1 - alfa)
  # Construcción del IC
  ic_inf_boot <- media - pto_crit * desv/sqrt(n)
  ic_sup_boot <- media + pto_crit * desv/sqrt(n)
  I3 <- c(ic_inf_boot, ic_sup_boot)
  # names(I3) <- namesI
  resultados[isim, 1, 4] <- (I3[1] < mu) && (mu < I3[2])
  resultados[isim, 2, 4] <- I3[2] - I3[1]
}

t.fin <- proc.time() - t.ini
t.fin
apply(resultados, c(2, 3), mean)%>% kable() %>% kable_styling()