#librerias
library(readxl)
library(tidyverse)
library(dplyr)
library(effsize)
library(car)
library(openintro)
library(nortest)


#data
df <- read_excel("datos/datos_medicos.xlsx")


#transformacion
dfps <- df %>%
  filter(Presion == "PresionSistolica") |>
  pull(Valores)

dfpd <- df %>%
  filter(Presion == "PresionDiastolica") |>
  pull(Valores)

df$Presion <- factor(df$Presion)


df %>%
  group_by(Presion) %>%
  summarise(
    n = length(Valores),
    promedio = mean(Valores),
    desviacion_estandar = sd(Valores),
    mediana = median(Valores),
    ric = IQR(Valores),
    minimo = min(Valores),
    maximo = max(Valores)
  )


#verificacion de normalidad en la presion sistolica
ks.test(scale(dfps), "pnorm")

ks.test(
  dfps,
  "pnorm",
  mean = mean(dfps),
  sd = sd(dfps)
)


#verificacion de normalidad en la presion diastolica
ks.test(scale(dfpd), "pnorm")

ks.test(
  dfpd,
  "pnorm",
  mean = mean(dfpd),
  sd = sd(dfpd)
)


#Verificacion de igualdad de varianzas
var.test(dfpd, dfps)


#Comprobacion manual de la diferencia de varianzas
#Formula: F = s1^2 / s2^2

n1 <- length(dfpd)
n2 <- length(dfps)

s1_2 <- var(dfpd)
s2_2 <- var(dfps)

F_manual <- s1_2 / s2_2

gl1 <- n1 - 1
gl2 <- n2 - 1

alpha <- 0.05

IC_varianzas <- c(
  inferior = F_manual / qf(1 - alpha / 2, gl1, gl2),
  superior = F_manual / qf(alpha / 2, gl1, gl2)
)

F_manual
gl1
gl2
IC_varianzas


#Prueba ttest
t.test(
  x = dfps,
  y = dfpd,
  var.equal = TRUE,
  paired = FALSE
)


#Comprobacion manual de la diferencia de medias
#Formula para muestras independientes con varianzas iguales

n1 <- length(dfps)
n2 <- length(dfpd)

x1 <- mean(dfps)
x2 <- mean(dfpd)

s1_2 <- var(dfps)
s2_2 <- var(dfpd)

sp2 <- (
  (n1 - 1) * s1_2 +
    (n2 - 1) * s2_2
) / (n1 + n2 - 2)

t_manual <- (x1 - x2) /
  sqrt(sp2 * (1 / n1 + 1 / n2))

gl <- n1 + n2 - 2

IC_medias <- c(
  inferior = (x1 - x2) -
    qt(1 - alpha / 2, gl) *
    sqrt(sp2 * (1 / n1 + 1 / n2)),
  
  superior = (x1 - x2) +
    qt(1 - alpha / 2, gl) *
    sqrt(sp2 * (1 / n1 + 1 / n2))
)

x1
x2
x1 - x2
sp2
t_manual
gl
IC_medias


#tamaño del efecto
cohen.d(
  dfps,
  dfpd,
  paired = FALSE
)


#Comprobacion manual del tamaño del efecto
#Desviacion estandar combinada ponderada

sp <- sqrt(
  (
    (n1 - 1) * s1_2 +
      (n2 - 1) * s2_2
  ) / (n1 + n2 - 2)
)

d_cohen_manual <- abs(x1 - x2) / sp

sp
d_cohen_manual


#Comprobacion con el promedio cuadratico simple

sp_simple <- sqrt(
  (s1_2 + s2_2) / 2
)

d_cohen_simple <- abs(x1 - x2) / sp_simple

sp_simple
d_cohen_simple





#Diferencia de medias con varianzas distintas


#data 2---
dfe <- read_excel("datos/datos_economia.xlsx")


#transformacion 2--
dfg <- dfe |>
  filter(`Tipo de cuenta` == "Ganancia") |>
  pull(Precio)

dfi <- dfe |>
  filter(`Tipo de cuenta` == "Inversion") |>
  pull(Precio)


dfe %>%
  group_by(`Tipo de cuenta`) %>%
  summarise(
    n = length(Precio),
    promedio = mean(Precio),
    desviacion_estandar = sd(Precio),
    mediana = median(Precio),
    ric = IQR(Precio),
    minimo = min(Precio),
    maximo = max(Precio)
  )


#comprobar normalidad
ks.test(scale(dfg), "pnorm")
ks.test(scale(dfi), "pnorm")


#Pruebas de diferencias de varianzas
var.test(dfi, dfg)


#Comprobacion manual de la diferencia de varianzas
#Formula: F = s1^2 / s2^2

n1 <- length(dfi)
n2 <- length(dfg)

s1_2 <- var(dfi)
s2_2 <- var(dfg)

F_manual <- s1_2 / s2_2

gl1 <- n1 - 1
gl2 <- n2 - 1

IC_varianzas <- c(
  inferior = F_manual / qf(1 - alpha / 2, gl1, gl2),
  superior = F_manual / qf(alpha / 2, gl1, gl2)
)

F_manual
gl1
gl2
IC_varianzas


# Bartlett
bartlett.test(
  Precio ~ `Tipo de cuenta`,
  data = dfe
)


# Fligner-Killeen
fligner.test(
  Precio ~ `Tipo de cuenta`,
  data = dfe
)


# Levene
leveneTest(
  Precio ~ `Tipo de cuenta`,
  data = dfe
)


#ttest con Welch
t.test(
  x = dfg,
  y = dfi,
  var.equal = FALSE,
  paired = FALSE
)


#La formula t de la imagen no se aplica en esta parte
#porque esa formula supone varianzas iguales
#y esta prueba utiliza el procedimiento de Welch


#tamaño del efecto
cohen.d(
  dfi,
  dfg,
  paired = FALSE
)


#Comprobacion manual del tamaño del efecto

n1 <- length(dfi)
n2 <- length(dfg)

x1 <- mean(dfi)
x2 <- mean(dfg)

s1_2 <- var(dfi)
s2_2 <- var(dfg)

sp <- sqrt(
  (
    (n1 - 1) * s1_2 +
      (n2 - 1) * s2_2
  ) / (n1 + n2 - 2)
)

d_cohen_manual <- abs(x1 - x2) / sp

sp
d_cohen_manual


#Comprobacion con el promedio cuadratico simple

sp_simple <- sqrt(
  (s1_2 + s2_2) / 2
)

d_cohen_simple <- abs(x1 - x2) / sp_simple

sp_simple
d_cohen_simple





# 4.2 Comparacion de grupos pareados con estadistica parametrica
#data 3---

# Datos de tiempos antes y después de la intervención educativa
datos <- data.frame(
  estudiante = c(1:10),
  
  antes = c(
    12.9, 13.5, 12.8, 15.6, 17.2,
    19.2, 12.6, 15.3, 14.4, 11.3
  ),
  
  despues = c(
    12.7, 13.6, 12.0, 15.2, 16.8,
    20.0, 12.0, 15.9, 16.0, 11.1
  )
)


# Exploratorio de los datos
datos <- datos |>
  mutate(
    Diferencia = despues - antes
  )


datos |>
  summarise(
    media_antes = mean(antes),
    media_despues = mean(despues),
    
    desviacion_antes = sd(antes),
    desviacion_despues = sd(despues),
    
    media_diferencia = mean(Diferencia),
    desviacion_diferencia = sd(Diferencia)
  )


#supuesto de normalidad
shapiro.test(datos$Diferencia)


#Diferencia de medias
t.test(
  datos$despues,
  datos$antes,
  paired = TRUE
)


#La formula t de la imagen no se aplica en esta parte
#porque los datos son pareados y no muestras independientes


#prueba de efecto
cohen.d(
  datos$despues,
  datos$antes,
  paired = TRUE
)


#La formula de Cohen mostrada en la imagen corresponde
#a muestras independientes, no a datos pareados





#Comparacion de grupos independientes
#con estadistica no parametrica

data("births")

dfb <- births

#caracterizar el peso del bebe segun si la madre fuma o no
#transformaciones
dfbsum <- dfb %>%
  group_by(smoke) %>%
  summarise(
    n = length(weight),
    promedio = mean(weight),
    desviacion_estandar = sd(weight),
    mediana = median(weight),
    ric = IQR(weight),
    minimo = min(weight),
    maximo = max(weight)
  )


#Verificar supuestos de normalidad
df_dist <- dfb |>
  group_by(smoke) |>
  summarise(
    n = length(weight),
    
    est_ks = ks.test(
      scale(weight),
      "pnorm"
    )$statistic,
    
    p_ks = ks.test(
      scale(weight),
      "pnorm"
    )$p.value,
    
    est_sw = shapiro.test(
      weight
    )$statistic,
    
    p_sw = shapiro.test(
      weight
    )$p.value,
    
    est_l = lillie.test(
      weight
    )$statistic,
    
    p_l = lillie.test(
      weight
    )$p.value
  )


#Los datos no cuentan con una distribucion normal


#levene
leveneTest(
  weight ~ smoke,
  data = dfb,
  center = median
)


wilcox.test(
  weight ~ smoke,
  data = dfb,
)


#No hay diferencias significativas entre el peso del bebe
#Respecto a si la madre fuma o no


dataQ <- read.delim("C:/Users/Alejandro Cotes/Desktop/Verificacion_medias/datos/dataQ.tsv", header=TRUE)
head(dataQ)
dfag <- dataQ |> group_by(Experimento) |> 
  summarise(n=length(Q1),
            minimo=min(Q1),
            maximo=max(Q1),
            media=mean(Q1),
            mediana=median(Q1),
            desviacion_estandar=sd(Q1),
            RIC=IQR(Q1)
  )

df_d <- dataQ |>
  group_by(Experimento) |>
  summarise(
    n = length(Q1),
    
    est_ks = ks.test(scale(Q1),"pnorm")$statistic,
    
    p_ks = ks.test(scale(Q1),"pnorm")$p.value,
    
    est_sw = shapiro.test(Q1)$statistic,
    
    p_sw = shapiro.test(Q1)$p.value,
    
    est_l = lillie.test(Q1)$statistic,
    
    p_l = lillie.test(Q1)$p.value)

pretestq1 <- dataQ |> 
  select(Experimento,Q1) |> 
  filter(Experimento== "Pretest")

postestq1 <- dataQ |> 
  select(Experimento,Q1) |> 
  filter(Experimento== "Post-test")

wilcox.test(x= pretestq1$Q1,y= postestq1$Q1,paired=TRUE)
#La prueba de rango del signo de wilcoxon 
#con una confianza del 95% podemos decir que hay diferencias estadisticmaente
#significativas de la pregunta de percepcion q1 segun el experimento (v=0, pvalor <0.001)

#Determinar tamaño del efecto con pruebas no parametricas
dataQ |> 
  wilcox_effsize(Q1 ~ Experimento, paired=TRUE)
# El tamaño del efecto de la diferencia fue estadisticamente sifnificativa


#hacer ejercicio de diferencia de proporciones completo

# ANOVA de un factor ------------


df <- data.frame(Empleados = c(0.66, 0.63, 0.65, 0.69, 0.44, 0.63, 0.61, 0.42, 0.59, 0.46),
                 Agricultores = c(0.65, 0.60, 0.69, 0.73, 0.52, 0.85, 0.81, NA, NA, NA),
                 NoExpuestos = c(0.93, 0.99, 0.96, 0.74, 0.81, 0.93, 0.63, 0.68, 0.99, NA))

empleados <- na.omit(df$Empleados)
agricultores<- na.omit(df$Agricultores)
no_expuestos<- na.omit(df$NoExpuestos)

# Hallemos el tamaño de cada tratamiento
n1 <- length(empleados)
n2<- length(agricultores)
n3<- length(no_expuestos)
N <- n1+n2+n3
k<-ncol(df)

# Hallemos los promedios de los tratamientos
xbar_1=mean(empleados)
xbar_2=mean(agricultores)
xbar_3=mean(no_expuestos)
# Hallemos el promedio global
xbar_total <-  (sum(empleados)
                +sum(agricultores)
                +sum(no_expuestos))/N

# Hallemos el SST

sst= sum((empleados-xbar_total)**2)+
  sum((agricultores-xbar_total)**2)+
  sum((no_expuestos-xbar_total)**2)

#Hallemos el ssa
ssa=n1*(xbar_1-xbar_total)**2+
  n2*(xbar_2-xbar_total)**2+
  n3*(xbar_3-xbar_total)**2

#hallemos el sse
sse=sst-ssa
#MSE Y MSA
msa=ssa/(k-1)
mse=sse/(N-k)
# Estadistico F
f=msa/mse

# F tabulado
alpha=0.05
v1=k-1
v2=N-k
f_critico=qf(1-alpha,df1 = v1,df2 = v2)
# Al fcritico ser menor que el F se rechaza 
#la hipotesis nula y por tanto hay diferencia en por lo menos 1 grupo

#pvalor
pvalor=1-pf(f,v1,v2)
# el pvalor es <0.05 por lo que se rechaza la hipotesis 
#nula y hay por lo menos un grupo es diferente
