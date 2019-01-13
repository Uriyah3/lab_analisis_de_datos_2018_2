#____________Universidad de Santiago de Chile__________________
#_______Departamento de Ingeniería en Informática______________
#__________________Análisis de Datos___________________________
#
# Laboratorio 4: Clasificador Bayesiano
# Integrantes: Nicolás Mariaágel | Juan Pablo Rojas
# Profesor: Max Chacón
# Ayudante: Ignacio Ibáñez Aliaga
#
# http://archive.ics.uci.edu/ml/datasets/Thyroid+Disease

# Naive Bayes Classifier
library("e1071")

library(readr)
library(cluster)
library(ggplot2)

preprocessing <- function(allhypo){
  # Limpieza de los resultados (no se usa el ".|numero" solo interesa la clase)
  allhypo$results <- vapply(strsplit(allhypo$results,"\\."), `[`, 1, FUN.VALUE=character(1))
  
  # Se eliminan estas columnas porque no fue medida la TBG en la base de datos.
  allhypo$TBG <- NULL
  allhypo$`TBG measured` <- NULL
  
  # Luego se procede a cambiar el caracter '?' por NA para poder ejecutar complete.cases()
  allhypo[allhypo=="?"] <- NA
  allhypo$age[allhypo$age >= 123] <- NA
  
  # Se eliminan los datos que tienen NA (originalmente '?') 
  allhypo <- allhypo[complete.cases(allhypo), ]
  
  # Luego como todas las columnas de si es que se mide o no algo son siempre 't'
  # por haber eliminado los sujetos, no se usarán estas columnas en el análisis
  allhypo$`T3 measured` <- NULL
  allhypo$`T4U measured` <- NULL
  allhypo$`TSH measured` <- NULL
  allhypo$`TT4 measured` <- NULL
  allhypo$`FTI measured` <- NULL
  
  # Eliminar referral.source debido a que no aporta información
  allhypo$`referral source` <- NULL
  
  # Transformar todas las columnas numéricas a variables numéricas.
  allhypo <- transform(allhypo, age = as.numeric(age))
  allhypo <- transform(allhypo, TSH = as.numeric(TSH))
  allhypo <- transform(allhypo, T3 = as.numeric(T3))
  allhypo <- transform(allhypo, TT4 = as.numeric(TT4))
  allhypo <- transform(allhypo, T4U = as.numeric(T4U))
  allhypo <- transform(allhypo, FTI = as.numeric(FTI))
  
  # Transformar todas las columnas categoricas a factores
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
  allhypo <- transform(allhypo, results = as.factor(results))
  
  # Eliminar los valores atípicos
  sdRange <- 3
  MaxTSH <- mean(allhypo$TSH) + sdRange * sd(allhypo$TSH)
  MaxT3 <- mean(allhypo$T3) + sdRange * sd(allhypo$T3)
  MaxTT4 <- mean(allhypo$TT4) + sdRange * sd(allhypo$TT4)
  MaxT4U <- mean(allhypo$T4U) + sdRange * sd(allhypo$T4U)
  MaxFTI <- mean(allhypo$FTI) + sdRange * sd(allhypo$FTI)
  allhypo <- subset(allhypo , (age <= 120) & (TSH <= MaxTSH) & (T3 <= MaxT3) & (TT4 <= MaxTT4) & (T4U <= MaxT4U) & ( FTI <= MaxFTI) )
  
  return(allhypo)
}

# Lectura de los datos de entrenamiento y de prueba + asignarle nombres a las columnas de acuerdo a lo escrito en allhypo.names
allhypo_training <- read_csv("allhypo/allhypo.data", col_names = FALSE)
allhypo_training_Rownames <- c("age", "sex", "on thyroxine", "query on thyroxine", "on antithyroid medication", "sick", "pregnant", "thyroid surgery", "I131 treatment", "query hypothyroid", "query hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH measured", "TSH", "T3 measured", "T3", "TT4 measured", "TT4", "T4U measured", "T4U", "FTI measured", "FTI", "TBG measured", "TBG", "referral source", "results")
colnames(allhypo_training) <- allhypo_training_Rownames

allhypo_test <- read_csv("allhypo/allhypo.test", col_names = FALSE)
allhypo_test_Rownames <- c("age", "sex", "on thyroxine", "query on thyroxine", "on antithyroid medication", "sick", "pregnant", "thyroid surgery", "I131 treatment", "query hypothyroid", "query hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH measured", "TSH", "T3 measured", "T3", "TT4 measured", "TT4", "T4U measured", "T4U", "FTI measured", "FTI", "TBG measured", "TBG", "referral source", "results")
colnames(allhypo_test) <- allhypo_test_Rownames

# Bases de datos de entrenamiento y prueba procesadas.
data_training <- preprocessing(allhypo_training)
data_test <- preprocessing(allhypo_test)

# Entrenaminto
model <- naiveBayes(results ~., data = data_training)

# Predecir los datos
results <- predict(object = model, newdata=data_test, type = "class")

# Matriz de confusion - Predecidos vs Entrenados
cm <- table(results,data_test$results)
#View(cm)

# Procentaje de aciertos (Precisión)
overall_accuracy <- sum(diag(cm)) / sum(cm)
p_compensated <- sum(cm[1,1]) / sum(cm[1,])
p_negative <- sum(cm[2,2]) / sum(cm[2,])
p_primary <- sum(cm[3,3]) / sum(cm[3,])

