#____________Universidad de Santiago de Chile__________________
#_______Departamento de Ingeniería en Informática______________
#__________________Análisis de Datos___________________________
#
# Laboratorio 2: Análisis estadístico
# Integrantes: Nicolás Mariangel | Juan Pablo Rojas
# Profesor: Max Chacón
# Ayudante: Ignacio Ibáñez Aliaga
#
# http://archive.ics.uci.edu/ml/datasets/Thyroid+Disease

library(readr)
library(cluster)

# Lectura de los datos + asignarle nombres a las columnas de acuerdo a lo escrito en allhypo.names
allhypo <- read_csv("allhypo/allhypo.data", col_names = FALSE)
allhypoRownames <- c("age", "sex", "on thyroxine", "query on thyroxine", "on antithyroid medication", "sick", "pregnant", "thyroid surgery", "I131 treatment", "query hypothyroid", "query hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH measured", "TSH", "T3 measured", "T3", "TT4 measured", "TT4", "T4U measured", "T4U", "FTI measured", "FTI", "TBG measured", "TBG", "referral source", "results")
colnames(allhypo) <- allhypoRownames

# Mostrar la cantidad de sujetos con datos incompletos por variable
lostsex <- length(allhypo$sex[allhypo$sex == '?']) + length(allhypo$sex[allhypo$sex == 'NA'])
lostTSH <- length(allhypo$TSH[allhypo$TSH == '?']) + length(allhypo$TSH[allhypo$TSH == 'NA'])
lostT3 <- length(allhypo$T3[allhypo$T3 == '?']) + length(allhypo$T3[allhypo$T3 == 'NA'])
lostTT4 <- length(allhypo$TT4[allhypo$TT4 == '?']) + length(allhypo$TT4[allhypo$TT4 == 'NA'])
lostT4U <- length(allhypo$T4U[allhypo$T4U == '?']) + length(allhypo$T4U[allhypo$T4U == 'NA'])
lostFTI <- length(allhypo$FTI[allhypo$FTI == '?']) + length(allhypo$FTI[allhypo$FTI == 'NA'])
lostTBG <- length(allhypo$TBG[allhypo$TBG == '?']) + length(allhypo$TBG[allhypo$TBG == 'NA'])

# Calcular el porcentaje de perdida por cada variable con datos incompletos
percentageLostsex <- lostsex / 2800 * 100
percentageLostTSH <- lostTSH / 2800 * 100
percentageLostT3 <- lostT3 / 2800 * 100
percentageLostTT4 <- lostTT4 / 2800 * 100
percentageLostT4U <- lostT4U / 2800 * 100
percentageLostFTI <- lostFTI / 2800 * 100
percentageLostTBG <- lostTBG / 2800 * 100

# Mostrar en pantalla los datos de perdida
cat("El número de incidencias para la variable sex es", lostsex, "con un % de perdida de:", percentageLostsex, "\n")
cat("El número de incidencias para la variable TSH es", lostTSH, "con un % de perdida de:", percentageLostTSH, "\n")
cat("El número de incidencias para la variable T3 es", lostT3, "con un % de perdida de:", percentageLostT3, "\n")
cat("El número de incidencias para la variable TT4 es", lostTT4, "con un % de perdida de:", percentageLostTT4, "\n")
cat("El número de incidencias para la variable T4U es", lostT4U, "con un % de perdida de:", percentageLostT4U, "\n")
cat("El número de incidencias para la variable FTI es", lostFTI, "con un % de perdida de:", percentageLostFTI, "\n")
cat("El número de incidencias para la variable TBG es", lostTBG, "con un % de perdida de:", percentageLostTBG, "\n")

# Limpieza de los resultados (no se usa el ".|numero" solo interesa la clase)
allhypo$results <- vapply(strsplit(allhypo$results,"\\."), `[`, 1, FUN.VALUE=character(1))

# Se eliminan estas columnas porque no fue medida la TBG en la base de datos.
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

# Eliminar referral.source y results por las razones mencionandas en este informe
allhypo$referral.source <- NULL
allhypo$results <- NULL

# Transformar todas las columnas numéricas a variables numéricas.
allhypo <- transform(allhypo, age = as.numeric(age))
allhypo <- transform(allhypo, TSH = as.numeric(TSH))
allhypo <- transform(allhypo, T3 = as.numeric(T3))
allhypo <- transform(allhypo, TT4 = as.numeric(TT4))
allhypo <- transform(allhypo, T4U = as.numeric(T4U))
allhypo <- transform(allhypo, FTI = as.numeric(FTI))

# Transformar todas las columnas booleanas a factores
allhypo <- transform(allhypo)

# Eliminar los valores atípicos
sdRange <- 3
MaxTSH <- mean(allhypo$TSH) + sdRange * sd(allhypo$TSH)
MaxT3 <- mean(allhypo$T3) + sdRange * sd(allhypo$T3)
MaxTT4 <- mean(allhypo$TT4) + sdRange * sd(allhypo$TT4)
MaxT4U <- mean(allhypo$T4U) + sdRange * sd(allhypo$T4U)
MaxFTI <- mean(allhypo$FTI) + sdRange * sd(allhypo$FTI)
allhypo <- subset(allhypo , (age <= 120) & (TSH <= MaxTSH) & (T3 <= MaxT3) & (TT4 <= MaxTT4) & (T4U <= MaxT4U) & ( FTI <= MaxFTI) )

