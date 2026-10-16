## deskriptive und inferenzstatistische Analyse der RTE-Daten ##
rm(list=ls())
set.seed(42)

## Arbeitsverzeichnis einf?gen
setwd("C:/Users/ra96nik/Desktop/Diss/Experiment/Programming")

library("Matrix")
library("lme4")
library("ggplot2")
library(tidyverse)
library("broom.mixed")
library("buildmer")
library("lmerTest")
library(hrbrthemes)
library(viridis)
library(dplyr)

library(nlme)
library(effects)
library(psych)

## Einlesen der Daten
data <- read.table("final_RT.txt", sep = "\t", header = TRUE)
summary(data)

## Zuordnung der Bedingungen
data$Condition <- ifelse(data$Gaze == "no" & data$SentenceMode == "declarative", "1",
                         ifelse(data$Gaze == "yes" & data$SentenceMode == "declarative", "3",
                                ifelse(data$Gaze == "no" & data$SentenceMode == "question", "2","4")))

## Faktorisierung der Daten
data$Gaze <- as.factor(data$Gaze)
data$SentenceMode <- as.factor(data$SentenceMode)
data$Condition <- as.factor(data$Condition)
data$Position <- data$Turn
data$Position <- as.factor(data$Position)
data$Turn <- NULL
data$NrSent <- data$NumberOSentences
data$NrSent <- as.factor(data$NrSent)
data$NumberOSentences <- NULL

## Umbenennung einzelner TN
data['Participant'][data['Participant'] == '103_1']="103"
data['Participant'][data['Participant'] == '103_2']="109"
data$Participant <- as.factor(data$Participant)

data <- na.omit(data)

##################
# Subsetting by Condition
###
subset = subset(data, TurnWindow != 0)


Inter_Blick = subset(data, Condition == "4")
Dekl_Blick = subset(data, Condition == "3")
Inter_NoBlick = subset(data, Condition == "2")
Dekl_NoBlick = subset(data, Condition == "1")

hist(Inter_Blick$RT_from_Offset,
     main = "Verteilung der Tastendrücke um das Turnende (Interrogativsatz + Blick)",
     ylab= "Häufigkeit der Tastendrücke",
     xlab = "Zeiträume (in 100 ms-Schritten)",
     col = c("#669966", "#669933"))

hist(Dekl_Blick$RT_from_Offset,
     main = "Verteilung der Tastendrücke um das Turnende (Deklarativsatz + Blick)",
     ylab= "Häufigkeit der Tastendrücke",
     xlab = "Zeiträume (in 100 ms-Schritten)",
     col = c("#669966", "#669933"))
hist(Inter_NoBlick$RT_from_Offset,
     main = "Verteilung der Tastendrücke um das Turnende (Interrogativsatz ohne Blick)",
     ylab= "Häufigkeit der Tastendrücke",
     xlab = "Zeiträume (in 100 ms-Schritten)",
     col = c("#669966", "#669933"))
hist(Dekl_NoBlick$RT_from_Offset,
     main = "Verteilung der Tastendrücke um das Turnende (Deklarativsatz ohne Blick)",
     ylab= "Häufigkeit der Tastendrücke",
     xlab = "Zeiträume (in 100 ms-Schritten)",
     col = c("#669966", "#669933"))


### 
#Data Exploration
###
hist(data$RT_from_Offset,
     main = "Verteilung der Tastendrücke um das Turnende",
     ylab= "Häufigkeit der Tastendrücke",
     xlab = "Zeiträume (in 100 ms-Schritten)",
     col = c("#669966", "#669933"))


boxplot(RT_from_Offset~Participant*Condition, data=Dekl_Blick)
boxplot(RT_from_Offset~Item_Nr*Condition, data=Dekl_Blick)

means <- tapply(data$RT_from_Offset, data$Position, mean)
points(means, pch=20, cex=1.5)
describe(RT_from_Offset~Position*Condition*NrSent, data=data)
###########################################
# Boxplot basic
data %>%
  ggplot( aes(x=Position, y=RT_from_Offset, fill=Condition)) +
  geom_boxplot() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  #theme_ipsum() +
  scale_fill_hue(labels = c("Deklarativ ohne Blick", "Deklarativ + Blick", "Interrogativ ohne Blick", "Interrogativ + Blick")) + 
  labs(fill = "Bedingung") + 
  theme(
    plot.title = element_text(size=12)
  ) +
  stat_summary(fun.y="mean", mapping = aes(group = Condition)) +
  ggtitle("Reaktionszeit je Position im Dialog") +
  ylab("Reaktionszeit (in ms)") +
  xlab("Positionen im Dialog")

# Violin basic
data %>%
  ggplot( aes(x=Position, y=RT_from_Offset, fill=Position)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  #theme_ipsum() +
  stat_summary(fun.y="mean") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

#############################################################
boxplot(RT_from_Offset~NrSent, data=data)
means <- tapply(data$RT_from_Offset, data$NrSent, mean)
points(means, pch=20, cex=1.5)
describe(RT_from_Offset~NrSent, data=data)

# Boxplot basic
data %>%
  ggplot( aes(x=NrSent, y=RT_from_Offset, fill="cadetblue")) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  #theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  stat_summary(fun.y="mean") +
  ggtitle("Reaktionszeit je Anzahl der Sätze") +
  ylab("Reaktionszeit (in ms)") +
  xlab("Anzahl der Sätze im Turn")

# Violin basic
data %>%
  ggplot( aes(x=NrSent, y=RT_from_Offset, fill="#69b3a2") ) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  #theme_ipsum() +
  stat_summary(fun.y="mean") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

boxplot(RT_from_Offset~Condition, data=data)
means <- tapply(data$RT_from_Offset, data$Condition, mean)
points(means, pch=20, cex=1.5)

mean(data[(data$RT_from_Offset & data$Condition),])
data <-  na.omit(data)


#####
# Deskriptive Analyse
#
#####

## Nummerische Berechnung
# Erstellung einzeln Untergruppen
subset_GazeYes = subset(data, Gaze == "yes")
subset_GazeNo = subset(data, Gaze == "no")
subset_SentModeDec = subset(data, SentenceMode == "declarative")
subset_SentModeQu = subset(data, SentenceMode == "question")

subset_GazeYesDec = subset(data, Gaze == "yes" & SentenceMode == "declarative")
subset_GazeYesQu = subset(data, Gaze == "yes" & SentenceMode == "question")

subset_GazeNoDec = subset(data, Gaze == "no" & SentenceMode == "declarative")
subset_GazeNoQu = subset(data, Gaze == "no" & SentenceMode == "question")

subset_SentModeDec = subset(data, SentenceMode == "declarative")
subset_SentModeQu = subset(data, SentenceMode == "question")

describe(subset_GazeYesDec$RT_from_Offset)
describe(subset_GazeYesQu$RT_from_Offset)
describe(subset_GazeNoDec$RT_from_Offset)
describe(subset_GazeNoQu$RT_from_Offset)


subset_Position1 = subset(data, Position == "1")

## Testung auf Normalverteilung

hist(subset_SentModeQu$RT_from_Offset)
shapiro.test(subset_SentModeQu$RT_from_Offset)
t.test(subset_GazeYesQu$RT_from_Offset, subset_GazeNoQu$RT_from_Offset)

hist(subset_SentModeDec$RT_from_Offset)
shapiro.test(subset_SentModeDec$RT_from_Offset)
# ist nicht normal verteilt --> wilcoxon
WT <- wilcox.test(subset_GazeYesDec$RT_from_Offset, subset_GazeNoDec$RT_from_Offset, exact = FALSE)
ZWT = qnorm(WT$p.value/2)
ZWT

## Visualisierung der TN-Daten ##
ggplot(aes(RT_from_Offset, Gaze), data = data) +
  geom_boxplot(aes(fill = Gaze)) + 
  theme_light() + 
  ylab("Effekt: Blick")+
  xlab("Zeit (in ms)") +
  labs(color =  "Blick")+
  scale_fill_discrete(name = "Blick", labels = c("Nein", "Ja")) +
  scale_y_discrete(labels = c("Nein", "Ja")) +
  facet_wrap(~ Participant) 

ggplot(aes(RT_from_Offset, SentenceMode), data = data) +
  geom_boxplot(aes(fill = SentenceMode)) + 
  theme_light() + 
  ylab("Effekt: Satzart")+
  xlab("Zeit (in ms)") +
  labs(color =  "Satzart")+
  scale_fill_discrete(name = "Satzart", labels = c("Deklarativ", "Interrogativ")) +
  scale_y_discrete(labels = c("Deklarativ", "Interrogativ")) +
  facet_wrap(~ Participant) 

### Performance der Participants über alle Bedingungen hinweg
aggregate(RT_from_Offset ~ Participant, data, mean)

boxplot(RT_from_Offset ~ Participant, data, 
        col = rainbow(length(unique(data$Participant)), alpha = 0.4), 
        horizontal = TRUE,
        xlab = "Zeit (in ms)",  # X-axis label
        ylab = "Teilnehmende")  # Y-axis label)^

### Reaktionszeiten pro Item über alle Bedingungen hinweg
aggregate(RT_from_Offset ~ Item_Nr, data, mean)

boxplot(RT_from_Offset ~ Item_Nr, data, 
        col = rainbow(length(unique(data$Item_Nr)), alpha = 0.4), 
        horizontal = TRUE,
        xlab = "Zeit (in ms)",  # X-axis label
        ylab = "Item")  # Y-axis label)

ggplot(aes(RT_from_Offset, Gaze), data = data) +
  geom_boxplot(aes(fill = Gaze)) + 
  theme_light() + 
  ylab("Effekt: Blick")+
  xlab("Zeit (in ms)") +
  labs(color =  "Blick")+
  scale_fill_discrete(name = "Blick", labels = c("Nein", "Ja")) +
  scale_y_discrete(labels = c("Nein", "Ja")) +
  facet_wrap(~ Item_Nr) 

ggplot(aes(RT_from_Offset, SentenceMode), data = data) +
  geom_boxplot(aes(fill = SentenceMode)) + 
  theme_light() + 
  ylab("Effekt: Satzart")+
  xlab("Zeit (in ms)") +
  labs(color =  "Satzart")+
  scale_fill_discrete(name = "Satzart", labels = c("Deklarativ", "Interrogativ")) +
  scale_y_discrete(labels = c("Deklarativ", "Interrogativ")) +
  facet_wrap(~ Item_Nr) 



## Boxplots der Daten für den Text

boxplot(RT_from_Offset ~ Gaze, data = data, col = '#669966',
        main = "Reaktionszeit mit und ohne Blick",
        ylab = "Reaktionszeit (in ms)",
        xlab = "Blickbewegung im Video vorhanden",
        names = c("Nein","Ja"))
boxplot(RT_from_Offset ~ SentenceMode, data = data, col = '#669933',
        main = "Reaktionszeit bei Aussage- oder Fragesatz",
        ylab = "Reaktionszeit (in ms)",
        xlab = "Satzart",
        names = c("Aussagesatz","Fragesatz"))

## Kombinierte Darstellung mit beiden Faktoren

boxplot(RT_from_Offset ~ Gaze*SentenceMode,
        col=c("#669966","#669933"),data,
        main = "Reaktionszeit in Abhängigkeit der Faktoren",
        xlab = "Satzart und Blickbewegung",
        ylab = "Reaktionszeit (in ms)",
        names = c("Deklarativ + \n kein Blick", "Deklarativ + \n Blick", "Interrogativ + \n kein Blick", "Interrogativ + \n Blick"))
points(mean(data$RT_from_Offset ~ data$Gaze), col = "red", pch = 19)


#####################################################
# Inferenzstatistik
#
#
####

## Erstellung von Summenkontrasten für die Modelle
contrasts(data$Gaze) <- contr.sum
contrasts(data$SentenceMode) <- contr.sum
contrasts(data$Position) <-contr.sum

RT_baseline.model <- lmer(RT_from_Offset ~ Gaze*SentenceMode  + (1 | Item_Nr) + (1 | Participant), 
                                 data = data, REML = FALSE)

# cleanly show important parts of model (see `summary()` for more)
tidy(RT_baseline.model, effects="fixed")

summary(RT_baseline.model)

#####
# Statistics with buildmer and lmertest
#####

full.model <- RT_from_Offset ~ Gaze*SentenceMode +
  (Gaze*SentenceMode | Participant) +
  (Gaze*SentenceMode | Item_Nr)

m_forwards <- buildmer(full.model, data=data, buildmerControl=buildmerControl(direction='order',
                                                                                   args=list(control=lmerControl(optimizer='bobyqa'))))
summary(m_forwards)

## Modellreduktion, das noch konvergiert (durch buildmer-order Berechnung):
max.model <- RT_from_Offset ~ 1 + SentenceMode + Gaze + SentenceMode:Gaze +  
  (1 + Gaze + SentenceMode | Participant) + (1 + SentenceMode |Item_Nr)

m_backwards <- buildmer(max.model, data=data, buildmerControl = buildmerControl(direction='backward',
                                                                                     args=list(control=lmerControl(optimizer='bobyqa'))))
summary(m_backwards)

## Berechnung der Effektstärke
cohen.d(RT_from_Offset~ Gaze, data =data) 
cohen.d(RT_from_Offset~ SentenceMode, data =data)
cohen.d(RT_from_Offset~ Gaze*SentenceMode, data =data) 


################################
### Inferenzstatistik über die Position der Turns im Dialog (--> Hinweise auf Lerneffekte)
contrasts(data$Position) <-contr.sum

RT_baseline_Position.model <- lmer(RT_from_Offset ~ Position  + (1 | Item_Nr) + (1 | Participant), 
                          data = data, REML = FALSE)

# cleanly show important parts of model (see `summary()` for more)
tidy(RT_baseline_Position.model, effects="fixed")

summary(RT_baseline_Position.model)

#####
# Statistics with buildmer and lmertest
#####

full_position.model <- RT_from_Offset ~ Position +
  (Position | Participant) +
  (Position | Item_Nr)


m_forwards_position <- buildmer(full_position.model, data=data, buildmerControl=buildmerControl(direction='order',
                                                                              args=list(control=lmerControl(optimizer='bobyqa'))))
summary(m_forwards_position)

## Maximales Modell, das noch konvergiert (durch buildmer-order Berechnung):

max_position.model <- RT_from_Offset ~ 1 + Position +  
  (1 + Position | Participant) + (1 + Position |Item_Nr)

m_backwards_position <- buildmer(max_position.model, data=data, buildmerControl = buildmerControl(direction='backward',
                                                                                args=list(control=lmerControl(optimizer='bobyqa'))))
summary(m_backwards_position)
