#____________Universidad de Santiago de Chile__________________
#_______Departamento de Ingeniería en Informática______________
#__________________Análisis de Datos___________________________
#
# Laboratorio 1: Análisis estadístico
# Integrantes: Nicolás Mariangel | Juan Pablo Rojas
# Profesor: Max Chacón
# Ayudante: Ignacio Ibáñez Aliaga
#
# http://archive.ics.uci.edu/ml/datasets/Thyroid+Disease

library(readr)
library(ggplot2)
library(VIM)

# Definicion de funciones 

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Prints in console measures of central tendency and dispersion of
# a vector (mean, median, mode and standard deviation).
# Receives a column from a data frame as input (c(...)).
summaryEx <- function(dataColumn) {
  cat("Mean:", mean(dataColumn), "\n")
  cat("Median:", median(dataColumn), "\n")
  cat("Mode:", Mode(dataColumn), "\n")
  cat("Sd:", sd(dataColumn), "\n\n")
}

# Uses the function summaryEx() to print in console measures of
# all continous variables in allhypo dataset (from UCI 1987). 
fullSumaryEx <- function(data) {
  cat("Age summary: \n")
  summaryEx(data$age)
  cat("TSH summary: \n")
  summaryEx(data$TSH)
  cat("T3 summary: \n")
  summaryEx(data$T3)
  cat("TT4 summary: \n")
  summaryEx(data$TT4)
  cat("T4U summary: \n")
  summaryEx(data$T4U)
  cat("FTI summary: \n")
  summaryEx(data$FTI)
}

# Lectura de los datos + asignarle nombres a las columnas de acuerdo a lo escrito en allhypo.names
allhypo <- read_csv("allhypo/allhypo.data", col_names = FALSE)
allhypoRownames <- c("age", "sex", "on thyroxine", "query on thyroxine", "on antithyroid medication", "sick", "pregnant", "thyroid surgery", "I131 treatment", "query hypothyroid", "query hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH measured", "TSH", "T3 measured", "T3", "TT4 measured", "TT4", "T4U measured", "T4U", "FTI measured", "FTI", "TBG measured", "TBG", "referral source", "results")
colnames(allhypo) <- allhypoRownames

# Limpieza de los resultados (no se usa el ".|numero" solo interesa la clase)
allhypo$results <- vapply(strsplit(allhypo$results,"\\."), `[`, 1, FUN.VALUE=character(1))

# Se eliminan estas columnas porque no fue medida la TBG en estos casos.
allhypo$TBG <- NULL
allhypo$`TBG measured` <- NULL

# Luego se procede a cambiar el caracter '?' por NA para poder ejecutar
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

# Transformar todas las columnas numéricas a variables numéricas.
allhypo <- transform(allhypo, age = as.numeric(age))
allhypo <- transform(allhypo, TSH = as.numeric(TSH))
allhypo <- transform(allhypo, T3 = as.numeric(T3))
allhypo <- transform(allhypo, TT4 = as.numeric(TT4))
allhypo <- transform(allhypo, T4U = as.numeric(T4U))
allhypo <- transform(allhypo, FTI = as.numeric(FTI))

# Analisis de variables booleanas
classificationBars <- ggplot(allhypo, aes(results)) +
  geom_bar(fill = "#0063B5FF") + theme_minimal() +
  labs(title="Classification") +
  theme(plot.title = element_text(hjust = 0.5))
show(classificationBars)

sexBars <- ggplot(allhypo, aes(results, fill = sex)) +
  geom_bar(width=.8, position = "dodge") +
  theme_bw() + scale_fill_brewer(palette = "Paired") +
  ggtitle("Class vs sex") +
  theme(plot.title = element_text(hjust = 0.5))
show(sexBars)

pregnantBars <- ggplot(allhypo, aes(results, fill = pregnant)) +
  geom_bar(width=.8, position = "dodge") +
  theme_bw() + scale_fill_brewer(palette = "Paired") +
  ggtitle("Class vs pregnant") +
  theme(plot.title = element_text(hjust = 0.5))
show(pregnantBars)

# thyroid.surgeryBars <- ggplot(allhypo, aes(results, fill = thyroid.surgery)) +
#   geom_bar(width=.8, position = "dodge") +
#   theme_bw() + scale_fill_brewer(palette = "Paired") +
#   ggtitle("Class vs thyroid.surgery") +
#   theme(plot.title = element_text(hjust = 0.5))
# show(thyroid.surgeryBars)
# 
# lithiumBars <- ggplot(allhypo, aes(results, fill = lithium)) +
#   geom_bar(width=.8, position = "dodge") +
#   theme_bw() + scale_fill_brewer(palette = "Paired") +
#   ggtitle("Class vs lithium") +
#   theme(plot.title = element_text(hjust = 0.5))
# show(lithiumBars)
# 
# tumorBars <- ggplot(allhypo, aes(results, fill = tumor)) +
#   geom_bar(width=.8, position = "dodge") +
#   theme_bw() + scale_fill_brewer(palette = "Paired") +
#   ggtitle("Class vs tumor") +
#   theme(plot.title = element_text(hjust = 0.5))
# show(tumorBars)
# 
# goitreBars <- ggplot(allhypo, aes(results, fill = goitre)) +
#   geom_bar(width=.8, position = "dodge") +
#   theme_bw() + scale_fill_brewer(palette = "Paired") +
#   ggtitle("Class vs Goitre") +
#   theme(plot.title = element_text(hjust = 0.5))
# show(goitreBars)
# 
# psychBars <- ggplot(allhypo, aes(results, fill = psych)) +
#   geom_bar(width=.8, position = "dodge") +
#   theme_bw() + scale_fill_brewer(palette = "Paired") +
#   ggtitle("Class vs psych") +
#   theme(plot.title = element_text(hjust = 0.5))
# show(psychBars)

# Mostrar solo los resultados de hipotiroide dado que la variable
# booleana a analizar sea verdadera.
thyroid.surgeryTrueBars <- ggplot(allhypo[allhypo$thyroid.surgery=="t", ], aes(results)) +
  geom_bar(width=.5, position = "dodge", fill="#1F78B4FF") +
  theme_bw() + scale_fill_brewer(palette = "Paired") +
  ggtitle("thyroid.surgery results") +
  theme(plot.title = element_text(hjust = 0.5))
show(thyroid.surgeryTrueBars)

lithiumTrueBars <- ggplot(allhypo[allhypo$lithium=="t", ], aes(results)) +
  geom_bar(width=.5, position = "dodge", fill="#1F78B4FF") +
  theme_bw() + scale_fill_brewer(palette = "Paired") +
  ggtitle("Lithium results") +
  theme(plot.title = element_text(hjust = 0.5))
show(lithiumTrueBars)

tumorTrueBars <- ggplot(allhypo[allhypo$tumor=="t", ], aes(results)) +
  geom_bar(width=.5, position = "dodge", fill="#1F78B4FF") +
  theme_bw() + scale_fill_brewer(palette = "Paired") +
  ggtitle("tumor results") +
  theme(plot.title = element_text(hjust = 0.5))
show(tumorTrueBars)

goitreTrueBars <- ggplot(allhypo[allhypo$goitre=="t", ], aes(results)) +
  geom_bar(width=.5, position = "dodge", fill="#1F78B4FF") +
  theme_bw() + scale_fill_brewer(palette = "Paired") +
  ggtitle("Goitre results") +
  theme(plot.title = element_text(hjust = 0.5))
show(goitreTrueBars)

psychTrueBars <- ggplot(allhypo[allhypo$psych=="t", ], aes(results)) +
  geom_bar(width=.5, position = "dodge", fill="#1F78B4FF") +
  theme_bw() + scale_fill_brewer(palette = "Paired") +
  ggtitle("psych results") +
  theme(plot.title = element_text(hjust = 0.5))
show(psychTrueBars)

# Analisis de variables continuas

# Mostrar distribución de las variables continuas usando histogramas
# que muestran una línea punteada en la media de la distribución.
ageHistogram <- ggplot(allhypo, aes(x = age)) +
  geom_histogram(bins = 30, color = "black", fill = "#0052dabb") +
  geom_vline(aes(xintercept = mean(age), color="mean"), linetype = "F1", size = 0.9) + 
  geom_vline(aes(xintercept = Mode(age), color="mode"), linetype = "dashed", size = 0.75) +
  geom_vline(aes(xintercept = median(age), color="median"), linetype = "twodash", size = 0.75) +
  scale_colour_manual(name="Measures of central tendency",values=c(mean="black",mode="darkblue",median="brown")) +
  theme(legend.justification=c(0.02,1), legend.position=c(0.02,1)) +
  labs(title="Age distribution") +
  theme(plot.title = element_text(hjust = 0.5))
show(ageHistogram)

# Se grafica sin la cola de la derecha para poder ver mejor la distribución
TSHHistogram <- ggplot(allhypo[allhypo$TSH < 24, ], aes(x = TSH)) +
  geom_histogram(bins = 40, color = "black", fill = "#0052dabb") +
  geom_vline(aes(xintercept = mean(allhypo$TSH), color="mean"), linetype = "F1", size = 0.9) + 
  geom_vline(aes(xintercept = Mode(TSH), color="mode"), linetype = "dashed", size = 0.75) +
  geom_vline(aes(xintercept = median(TSH), color="median"), linetype = "twodash", size = 0.75) +
  scale_colour_manual(name="Measures of central tendency",values=c(mean="black",mode="darkblue",median="brown")) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  labs(title="TSH distribution") +
  theme(plot.title = element_text(hjust = 0.5))
show(TSHHistogram)

TSHFullHistogram <- ggplot(allhypo, aes(x = TSH)) +
  geom_histogram(bins = 300, color = "black", fill = "#0052dabb") +
  geom_vline(aes(xintercept = mean(allhypo$TSH), color="mean"), linetype = "F1", size = 0.9) + 
  geom_vline(aes(xintercept = Mode(TSH), color="mode"), linetype = "dashed", size = 0.75) +
  geom_vline(aes(xintercept = median(TSH), color="median"), linetype = "twodash", size = 0.75) +
  scale_colour_manual(name="Measures of central tendency",values=c(mean="black",mode="darkblue",median="brown")) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  labs(title="TSH distribution") +
  theme(plot.title = element_text(hjust = 0.5))
show(TSHFullHistogram)

T3Histogram <- ggplot(allhypo, aes(x = T3)) +
  geom_histogram(bins = 30, color = "black", fill = "#0052dabb") +
  geom_vline(aes(xintercept = mean(T3), color="mean"), linetype = "F1", size = 0.9) + 
  geom_vline(aes(xintercept = Mode(T3), color="mode"), linetype = "dashed", size = 0.75) +
  geom_vline(aes(xintercept = median(T3), color="median"), linetype = "twodash", size = 0.75) +
  scale_colour_manual(name="Measures of central tendency",values=c(mean="black",mode="darkblue",median="brown")) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  labs(title="T3 distribution") +
  theme(plot.title = element_text(hjust = 0.5))
show(T3Histogram)

TT4Histogram <- ggplot(allhypo, aes(x = TT4)) +
  geom_histogram(bins = 30, color = "black", fill = "#0052dabb") +
  geom_vline(aes(xintercept = mean(TT4), color="mean"), linetype = "F1", size = 0.9) + 
  geom_vline(aes(xintercept = Mode(TT4), color="mode"), linetype = "dashed", size = 0.75) +
  geom_vline(aes(xintercept = median(TT4), color="median"), linetype = "twodash", size = 0.75) +
  scale_colour_manual(name="Measures of central tendency",values=c(mean="black",mode="darkblue",median="brown")) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  labs(title="TT4 distribution") +
  theme(plot.title = element_text(hjust = 0.5))
show(TT4Histogram)

T4UHistogram <- ggplot(allhypo, aes(x = T4U)) +
  geom_histogram(bins = 30, color = "black", fill = "#0052dabb") +
  geom_vline(aes(xintercept = mean(T4U), color="mean"), linetype = "F1", size = 0.9) + 
  geom_vline(aes(xintercept = Mode(T4U), color="mode"), linetype = "dashed", size = 0.75) +
  geom_vline(aes(xintercept = median(T4U), color="median"), linetype = "twodash", size = 0.75) +
  scale_colour_manual(name="Measures of central tendency",values=c(mean="black",mode="darkblue",median="brown")) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  labs(title="T4U distribution") +
  theme(plot.title = element_text(hjust = 0.5))
show(T4UHistogram)

FTIHistogram <- ggplot(allhypo, aes(x = FTI)) +
  geom_histogram(bins = 30, color = "black", fill = "#0052dabb") +
  geom_vline(aes(xintercept = mean(FTI), color="mean"), linetype = "F1", size = 0.9) + 
  geom_vline(aes(xintercept = Mode(FTI), color="mode"), linetype = "dashed", size = 0.75) +
  geom_vline(aes(xintercept = median(FTI), color="median"), linetype = "twodash", size = 0.75) +
  scale_colour_manual(name="Measures of central tendency",values=c(mean="black",mode="darkblue",median="brown")) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  labs(title="FTI distribution") +
  theme(plot.title = element_text(hjust = 0.5))
show(FTIHistogram)

# Mostrar las medidas de tendencia central del dataset allhypo
fullSumaryEx(allhypo)

# Separate males from females (and pregnants from non-pregnants)
# to view a more controlled summary of all the data
males <- allhypo[allhypo$sex == "M", ]
females <- allhypo[allhypo$sex == "F", ]
pregante <- females[females$pregnant == "t", ]
females <- females[females$pregnant == "f", ]

# Show summary of separated allhypo data
cat("--------------------------------------------------\n")
cat("Full sumary for male patients: \n")
fullSumaryEx(males)

cat("--------------------------------------------------\n")
cat("Full sumary for female patients (not pregnant): \n")
fullSumaryEx(females)

cat("--------------------------------------------------\n")
cat("Full sumary for pregnant patients: \n")
fullSumaryEx(pregante)

# Correlación
corData <- data.frame(allhypo$age, allhypo$TSH, allhypo$T3, allhypo$TT4, allhypo$T4U, allhypo$FTI)
resCorrelate <- cor(corData, method = "pearson")
show(resCorrelate)
