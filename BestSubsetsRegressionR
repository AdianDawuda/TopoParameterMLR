############################
## Best Subset Regression ##
############################
#Benoetigte Pakete
library(leaps);library(Ecdat);library(car);library(lmtest)
#Arbeitsverzeichnis zuweisen
setwd("Pfad/zum/Arbeitsverzeichnis")
#Daten einlesen
daten <- read.csv("Eigenschaften.csv")
#Modell erstellen
model <- lm(GT ~ Altitude + TPI75 + SWI5 + Vdepth + CurvPla30 + CurvPro10, data = daten)
summary(model)
#Best Subsets berechnen
regfit<-regsubsets(GT ~ Altitude + TPI75 + SWI5 + Vdepth + CurvPla30 +
CurvPro10, daten)
regfitSummary<-summary(regfit)
#Mallows' Cp-Statistik visueller Vergleich
par(mfrow=c(1,2))
plot(regfitSummary$cp)
plot(regfit,scale = "Cp")
#Bayessches Informationskriterium visueller Vergleich
par(mfrow=c(1,2))
plot(regfitSummary$bic)
plot(regfit,scale = "bic")
#Adjustiertes R-Quadrat visueller Vergleich
par(mfrow=c(1,2))
plot(regfitSummary$adjr2)
plot(regfit,scale = "adjr2")
#Neues Modell ausgewaehlter unabhaengiger Variablen
model2 <- lm(GT ~ Altitude + SWI5 + CurvPla30, data = daten)
summary(model2)
#VIF-Werte
vif(model2)
#Varianz der Residuen visuell
par(mfrow=c(2,2))
plot(model2)
#Breusch-Pagan Test
bptest(model2)
