### Korrelationsanalyse der Fixationen der Kinder ###
rm(list=ls())

library(corrplot)
library("Matrix")
library("MASS")
library("effsize")

library(effects)
library(psych)
library(dplyr)
library("lme4")
library("buildmer")
library("lmerTest")

## Arbeitsverzeichnis einfügen
setwd("C:/Users/ra96nik/Desktop/Diss/Experiment/Programming/Eyetracking_Kinder")

## Einladen des Datensatzes 
agg_data <- read.table("aggregate.csv", sep = ";", header = TRUE)
summary(agg_data)

##Faktorisierung der Daten und Anpassung weiterer Datentypen
agg_data$Participant <- as.factor(agg_data$Participant)
agg_data$Gesamtmonate <- gsub(",", ".", agg_data$Gesamtmonate)
agg_data$Gesamtmonate <- as.numeric(agg_data$Gesamtmonate)
agg_data$LangStatus <- gsub(",", ".", agg_data$LangStatus)
agg_data$LangStatus <- as.numeric(agg_data$LangStatus)
agg_data$Trog <- gsub(",", ".", agg_data$Trog)
agg_data$Trog <- as.numeric(agg_data$Trog)
agg_data$TrogPunkte <- gsub(",", ".", agg_data$TrogPunkte)
agg_data$TrogPunkte <- as.numeric(agg_data$TrogPunkte)
agg_data$SES <- gsub(",", ".", agg_data$SES)
agg_data$SES <- as.numeric(agg_data$SES)
agg_data$Kindergarten <- gsub(",", ".", agg_data$Kindergarten)
agg_data$Kindergarten <- as.numeric(agg_data$Kindergarten)
agg_data$AoO <- gsub(",", ".", agg_data$AoO)
agg_data$AoO <- as.numeric(agg_data$AoO)
agg_data$PseudoFehler <- gsub(",", ".", agg_data$PseudoFehler)
agg_data$PseudoFehler <- as.numeric(agg_data$PseudoFehler)
agg_data$PseudoSilbe <- gsub(",", ".", agg_data$PseudoSilbe)
agg_data$PseudoSilbe <- as.numeric(agg_data$PseudoSilbe)

agg_data$ElogSentPre_ <- gsub(",", ".", agg_data$ElogSentPre_)
agg_data$ElogSentPre_ <- as.numeric(agg_data$ElogSentPre_)
agg_data$ElogBlickPre_ <- gsub(",", ".", agg_data$ElogBlickPre_)
agg_data$ElogBlickPre_  <- as.numeric(agg_data$ElogBlickPre_)
agg_data$ElogSentPost_ <- gsub(",", ".", agg_data$ElogSentPost_)
agg_data$ElogSentPost_ <- as.numeric(agg_data$ElogSentPost_)
agg_data$ElogBlickPost_ <- gsub(",", ".", agg_data$ElogBlickPost_)
agg_data$ElogBlickPost_ <- as.numeric(agg_data$ElogBlickPost_)
agg_data <-na.omit(agg_data)

summary(agg_data)

## Korrelationsanalyse der Variablen der Kinder
subset_cor <- subset(agg_data, select = c(Gesamtmonate, Trog, AoO, Kindergarten, PseudoFehler, SES, LangStatus))
subset_cor <-na.omit(subset_cor)
cor(subset_cor, method = "kendall", use = "pairwise.complete.obs")
cor(subset_cor, method = "spearman", use = "pairwise.complete.obs")

## Korrelationsanalyse der Variablen der Kinder plus Aggregierte Werte der Faktoren Satztyp und Blick für den ersten und zweiten Zeitraum
subset_cor_test <- subset(agg_data, select = c(Gesamtmonate, Trog, PseudoFehler, AoO, SES,LangStatus, ElogSentPre_, ElogBlickPre_, ElogSentPost_, ElogBlickPost_))
subset_cor_test <-na.omit(subset_cor_test)
## Überprüfung der Ergebenisse unterschiedlicher Korrelationsanalysen 
#cor(subset_cor_test, method = "kendall" ,use = "pairwise.complete.obs")
cor(subset_cor_test, method = "spearman", use = "pairwise.complete.obs")

## Korrelationen zwischen einzelnen Variablen
cor.test(subset_cor_test$ElogSentPre, subset_cor_test$PseudoFehler, method = "spearman" )
cor.test(subset_cor_test$ElogSentPre, subset_cor_test$Trog, method = "spearman" )

cor.test(subset_cor_test$ElogBlickPre, subset_cor_test$AoO, method = "pearson" ) # Kontrolle inwiefern sich die Korrelationswerte zwischen einer Pearson- und einer Spearman-Korrelationsberechnung unterscheiden 
cor.test(subset_cor_test$ElogBlickPre, subset_cor_test$AoO, method = "spearman" )

## Visualisierung der mittel bis stark korrelierenden Variablen: AoO und Blick im ersten Zeitraum
plot(subset_cor_test$ElogBlickPre, subset_cor_test$AoO)
lines(lowess(subset_cor_test$ElogBlickPre, subset_cor_test$AoO), col = "darkgrey")
plot(subset_cor_test$AoO, subset_cor_test$ElogBlickPre)
lines(lowess(subset_cor_test$AoO, subset_cor_test$ElogBlickPre), col = "darkgrey")

## Korrelationen und Visualisierung der mittel bis stark korrelierenden Variablen Mehrsprachigkeit und Satztyp im zweiten Zeitraum
cor.test(subset_cor_test$ElogSentPost, subset_cor_test$LangStatus, method = "spearman" )
plot(subset_cor_test$ElogSentPost, subset_cor_test$LangStatus)
lines(lowess(subset_cor_test$ElogSentPost, subset_cor_test$LangStatus), col = "darkgrey")

## Korrelationen und Visualisierung der mittel bis stark korrelierenden Variablen Alter und Blick im zweiten Zeitraum
cor.test(subset_cor_test$ElogBlickPost, subset_cor_test$Gesamtmonate, method = "spearman" )
plot(subset_cor_test$ElogBlickPost, subset_cor_test$Gesamtmonate)
lines(lowess(subset_cor_test$ElogBlickPost, subset_cor_test$Gesamtmonate), col = "darkgrey")

## Subsetting für Testung auf Normalverteilung für die Überprüfung der Unetrschiede
lmreg_agg <- subset(agg_data, select = c(Gesamtmonate, Trog, LangStatus, PseudoFehler, SES, ElogSentPre, ElogBlickPre, ElogSentPost, ElogBlickPost))

hist(lmreg_agg$Gesamtmonate)
shapiro.test(lmreg_agg$Gesamtmonate)
### -> keine Normalverteilung der Gesamtmonate
hist(lmreg_agg$Trog)
shapiro.test(lmreg_agg$Trog) 
### -> Normalverteilung von Trog (p-value = 5.939e-07)! bartlett.test(Trog, data = lmreg_agg)
hist(lmreg_agg$LangStatus)
shapiro.test(lmreg_agg$LangStatus)
### -> Normalverteilung von Mehrsprachigkeit (p-value = 1.233e-09)! bartlett.test(LangStatus, data = lmreg_agg)
hist(lmreg_agg$PseudoFehler)
shapiro.test(lmreg_agg$PseudoFehler)
### -> keine Normalverteilung der PseudoFehler
hist(lmreg_agg$SES)
shapiro.test(lmreg_agg$SES)
### -> Normalverteilung von Mehrsprachigkeit (p-value = 0.0001701)! bartlett.test(SES, data = lmreg_agg)

## Modellberechnung
SentPre.model <- lm(ElogSentPre ~ Gesamtmonate + Trog + LangStatus + PseudoFehler + SES, data =  lmreg_agg)
summary(SentPre.model)
plot(SentPre.model)
lines(lowess)
#cohen.d(ElogSentPre ~ Trog, data =lmreg_agg) 
#cohen.d(ElogSentPre ~ PseudoFehler, data =lmreg_agg) 


BlickPre.model <- lm(ElogBlickPre ~ Gesamtmonate + Trog + LangStatus + PseudoFehler + SES, data =  lmreg_agg)
summary(BlickPre.model)
plot(BlickPre.model)
#cohen.d(ElogBlickPre ~ AoO, data =lmreg_agg) 

SentPost.model <- lm(ElogSentPost  ~ Gesamtmonate + Trog + LangStatus + PseudoFehler + SES, data =  lmreg_agg)
summary(SentPost.model)
plot(SentPost.model)
#cohen.d(ElogSentPost ~ Mehrsprachigkeit, data =lmreg_agg) 

BlickPost.model <- lm(ElogBlickPost ~ Gesamtmonate * Trog * LangStatus * PseudoFehler * SES, data =  lmreg_agg)
summary(BlickPost.model)
plot(BlickPost.model)

cohen.d(ElogSentPost ~ LangStatus, data =lmreg_agg) 

## Korrelationsanalysen des Datensatzes der mehrsprachigen Kinder
subset_bili <- subset(agg_data, LangStatus == 1)
subset_bili_cor <- subset(subset_bili, select = c(Gesamtmonate, Trog, PseudoFehler, AoO, SES, ElogSentPre_, ElogBlickPre_, ElogSentPost_, ElogBlickPost_))
subset_bili_cor <-na.omit(subset_bili_cor)
cor(subset_bili_cor)
cor(subset_bili_cor, method = "kendall" ,use = "pairwise.complete.obs")
cor(subset_bili_cor, method = "spearman", use = "pairwise.complete.obs")

cor.test(subset_bili_cor$ElogSentPre_, subset_bili_cor$PseudoFehler, method = "spearman")
cor.test(subset_bili_cor$ElogBlickPre_, subset_bili_cor$AoO, method = "pearson" )
cor.test(subset_bili_cor$ElogBlickPre_, subset_bili_cor$Trog, method = "spearman")
cor.test(subset_bili_cor$ElogSentPost_, subset_bili_cor$Trog, method = "spearman")

## Korrelationen und Visualisierung der mittel bis stark korrelierenden Variable AoO und Blick im esten Zeitraum
cor.test(subset_bili_cor$ElogBlickPre, subset_bili_cor$AoO, method = "spearman" )
plot(subset_bili_cor$ElogBlickPre, subset_bili_cor$AoO)
lines(lowess(subset_bili_cor$ElogBlickPre, subset_bili_cor$AoO), col = "darkgrey")
