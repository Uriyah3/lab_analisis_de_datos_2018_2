#____________Universidad de Santiago de Chile__________________
#_______Departamento de Ingeniería en Informática______________
#__________________Análisis de Datos___________________________
#
# Laboratorio 3: Reglas de asociación
# Integrantes: Nicolás Mariangel | Juan Pablo Rojas
# Profesor: Max Chacón
# Ayudante: Ignacio Ibáñez Aliaga
#
# http://archive.ics.uci.edu/ml/datasets/Thyroid+Disease

library(readr)
library(ggplot2)
library("arulesViz")

# Lectura de los datos + asignarle nombres a las columnas de acuerdo a lo escrito en allhypo.names
allhypo <- read_csv("allhypo/allhypo.data", col_names = FALSE)
allhypoRownames <- c("age", "sex", "on thyroxine", "query on thyroxine", "on antithyroid medication", "sick", "pregnant", "thyroid surgery", "I131 treatment", "query hypothyroid", "query hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH measured", "TSH", "T3 measured", "T3", "TT4 measured", "TT4", "T4U measured", "T4U", "FTI measured", "FTI", "TBG measured", "TBG", "referral source", "results")
colnames(allhypo) <- allhypoRownames

# Limpieza de los resultados (no se usa el ".|numero" solo interesa la clase)
allhypo$results <- vapply(strsplit(allhypo$results,"\\."), `[`, 1, FUN.VALUE=character(1))

# Se eliminan las siguientes columnas porque no fue medida la TBG en la base de datos.
allhypo$TBG <- NULL
allhypo$`TBG measured` <- NULL

# Luego se procede a cambiar el caracter '?' por NA para poder ejecutar complete.cases()
allhypo[allhypo=="?"] <- NA
allhypo$age[allhypo$age >= 123] <- NA

# Se eliminan los datos que tienen NA (originalmente '?') por los motivos
# descritos en el informe
allhypo <- allhypo[complete.cases(allhypo), ]

# Luego como todas las columnas de si es que se mide o no algo son siempre 't'
# por haber eliminado los sujetos, no se usarán estas columnas en el análisis
allhypo$`T3 measured` <- NULL
allhypo$`T4U measured` <- NULL
allhypo$`TSH measured` <- NULL
allhypo$`TT4 measured` <- NULL
allhypo$`FTI measured` <- NULL

# Eliminar referral.source porque no entrega información relevante.
allhypo$`referral source` <- NULL

# Transformar todas las columnas numéricas a variables numéricas.
allhypo <- transform(allhypo, age = as.numeric(age))
allhypo <- transform(allhypo, TSH = as.numeric(TSH))
allhypo <- transform(allhypo, T3 = as.numeric(T3))
allhypo <- transform(allhypo, TT4 = as.numeric(TT4))
allhypo <- transform(allhypo, T4U = as.numeric(T4U))
allhypo <- transform(allhypo, FTI = as.numeric(FTI))

# Transformar todas las columnas booleanas a factores
allhypo <- transform(allhypo, sex = as.factor(sex))
allhypo <- transform(allhypo, on.thyroxine = as.factor(on.thyroxine))
allhypo <- transform(allhypo, query.on.thyroxine = as.factor(query.on.thyroxine))
allhypo <- transform(allhypo, on.antithyroid.medication = as.factor(on.antithyroid.medication))
allhypo <- transform(allhypo, sick = as.factor(sick))
allhypo <- transform(allhypo, pregnant = as.factor(pregnant))
allhypo <- transform(allhypo, thyroid.surgery = as.factor(thyroid.surgery))
allhypo <- transform(allhypo, I131.treatment = as.factor(I131.treatment))
allhypo <- transform(allhypo, query.hypothyroid = as.factor(query.hypothyroid))
allhypo <- transform(allhypo, query.hyperthyroid = as.factor(query.hyperthyroid))
allhypo <- transform(allhypo, lithium = as.factor(lithium))
allhypo <- transform(allhypo, goitre = as.factor(goitre))
allhypo <- transform(allhypo, tumor = as.factor(tumor))
allhypo <- transform(allhypo, hypopituitary = as.factor(hypopituitary))
allhypo <- transform(allhypo, psych = as.factor(psych))

# Para poder utilizar el paquete arulesViz y utilizar la función apriori para obtener
# las reglas de asociación, primero se deben transformar todos los datos a variables
# booleanas, por lo que se crean rangos para transformar las variables continuas.

# Definir limites para las edades
age.adult <- 18
age.old <- 65

# Definir valores minimos y maximos para los valores de las hormonas
TSH.min <- 0.4
T3.min <- 1.07
TT4.min <- 64.0
T4U.min <- 0.7
FTI.min <- 33.108

# Definicion de valores maximos
TSH.max <- 4.0
T3.max <- 3.37
TT4.max <- 154.0
T4U.max <- 1.8
FTI.max <- 135.191

# Crear vectores con valores 0 para iniciar con la transformación a valores booleanos (0 o 1)
allhypo$child <- integer(length(allhypo[[1]]))
allhypo$adult <- integer(length(allhypo[[1]]))
allhypo$old <- integer(length(allhypo[[1]]))
allhypo$TSH.low <- integer(length(allhypo[[1]]))
allhypo$T3.low <- integer(length(allhypo[[1]]))
allhypo$TT4.low <- integer(length(allhypo[[1]]))
allhypo$T4U.low <- integer(length(allhypo[[1]]))
allhypo$FTI.low <- integer(length(allhypo[[1]]))
allhypo$TSH.normal <- integer(length(allhypo[[1]]))
allhypo$T3.normal <- integer(length(allhypo[[1]]))
allhypo$TT4.normal <- integer(length(allhypo[[1]]))
allhypo$T4U.normal <- integer(length(allhypo[[1]]))
allhypo$FTI.normal <- integer(length(allhypo[[1]]))
allhypo$TSH.high <- integer(length(allhypo[[1]]))
allhypo$T3.high <- integer(length(allhypo[[1]]))
allhypo$TT4.high <- integer(length(allhypo[[1]]))
allhypo$T4U.high <- integer(length(allhypo[[1]]))
allhypo$FTI.high <- integer(length(allhypo[[1]]))

# Usar los rangos para transformar las variables continuas en "booleanas"
# Entregando valores 1 a los vectores recien creados cuando corresponda
for(i in 1:length(allhypo[[1]])){
  
  if(allhypo$age[i] < age.adult){
    allhypo$child[i] <- 1
  }else if(allhypo$age[i] >= age.adult & allhypo$age[i] < age.old){
    allhypo$adult[i] <- 1
  }else if(allhypo$age[i] >= age.old){
    allhypo$old[i] <- 1
  }
  
  if(allhypo$TSH[i] >= TSH.max){
    allhypo$TSH.high[i] <- 1
  }else if(allhypo$TSH[i] <= TSH.min){
    allhypo$TSH.low[i] <- 1
  } else {
    allhypo$TSH.normal[i] <- 1
  }
  if(allhypo$T3[i] >= T3.max){
    allhypo$T3.high[i] <- 1
  }else if(allhypo$T3[i] <= T3.min){
    allhypo$T3.low[i] <- 1
  } else {
    allhypo$T3.normal[i] <- 1
  }
  if(allhypo$TT4[i] >= TT4.max){
    allhypo$TT4.high[i] <- 1
  }else if(allhypo$TT4[i] <= TT4.min){
    allhypo$TT4.low[i] <- 1
  } else {
    allhypo$TT4.normal[i] <- 1
  }
  if(allhypo$T4U[i] >= T4U.max){
    allhypo$T4U.high[i] <- 1
  }else if(allhypo$T4U[i] <= T4U.min){
    allhypo$T4U.low[i] <- 1
  } else {
    allhypo$T4U.normal[i] <- 1
  }
  if(allhypo$FTI[i] >= FTI.max){
    allhypo$FTI.high[i] <- 1
  }else if(allhypo$FTI[i] <= FTI.min){
    allhypo$FTI.low[i] <- 1
  } else {
    allhypo$FTI.normal[i] <- 1
  }
}

allhypo$results <- ifelse(allhypo$results %in% c("primary hypothyroid", "secondary hypothyroid", "compensated hypothyroid"), 1, 0)

# Transformar a factores y eliminar las variables continuas
allhypo$age <- NULL
allhypo$TSH <- NULL
allhypo$T3 <- NULL
allhypo$TT4 <- NULL
allhypo$T4U <- NULL
allhypo$FTI <- NULL
allhypo <- transform(allhypo, child = as.factor(child))
allhypo <- transform(allhypo, adult = as.factor(adult))
allhypo <- transform(allhypo, old = as.factor(old))
allhypo <- transform(allhypo, TSH.low = as.factor(TSH.low))
allhypo <- transform(allhypo, T3.low = as.factor(T3.low))
allhypo <- transform(allhypo, TT4.low = as.factor(TT4.low))
allhypo <- transform(allhypo, T4U.low = as.factor(T4U.low))
allhypo <- transform(allhypo, FTI.low = as.factor(FTI.low))
allhypo <- transform(allhypo, TSH.normal = as.factor(TSH.normal))
allhypo <- transform(allhypo, T3.normal = as.factor(T3.normal))
allhypo <- transform(allhypo, TT4.normal = as.factor(TT4.normal))
allhypo <- transform(allhypo, T4U.normal = as.factor(T4U.normal))
allhypo <- transform(allhypo, FTI.normal = as.factor(FTI.normal))
allhypo <- transform(allhypo, TSH.high = as.factor(TSH.high))
allhypo <- transform(allhypo, T3.high = as.factor(T3.high))
allhypo <- transform(allhypo, TT4.high = as.factor(TT4.high))
allhypo <- transform(allhypo, T4U.high = as.factor(T4U.high))
allhypo <- transform(allhypo, FTI.high = as.factor(FTI.high))
allhypo$results <- as.factor(allhypo$results)
names(allhypo)[names(allhypo) == "results"] <- "hypothyroid"

# Obtener las reglas que sean de largo 2 minimo y largo 6 maximo, teniendo minimo soporte de 0.01
# y confianza minima de 0.5, se busca encontrar reglas que indiquen que atributos llevan a padecer
# de hipotiroides
#rules <- apriori(allhypo, parameter = list(minlen=2, support=0.01, confidence=0.5, maxlen=6), appearance = list(rhs=c("hypothyroid=1"), default="lhs"))

# Graficar las reglas
#plot(rules)

# Se analizan solo las reglas que tengan un largo máximo de 5 elementos.
rules <- apriori(allhypo, parameter = list(minlen=2, support=0.01, confidence=0.5, maxlen=5), appearance = list(rhs=c("hypothyroid=1"), default="lhs"))

# Revisar las mejores reglas segun lift
inspect(head(rules, n = 40, by ="lift"))

# Graficar las reglas
plot(rules)


