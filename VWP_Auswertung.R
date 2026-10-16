## Auswertung Eye-Tracking Studie - Data plotting ##
rm(list=ls())

## Arbeitsverzeichnis einfügen
setwd("C:/Users/ra96nik/Desktop/Diss/Experiment/Programming")

library("Matrix")
library("MASS")
library("lme4")
library("ggplot2")
library("eyetrackingR")
library("lattice")
library("dbplot")
library("mgcv")
library("effsize")

library("buildmer")
library("lmerTest")
library(nlme)
library(effects)
library(psych)
## Einlesen der Daten
data_adults <- read.table("cleaned_table.txt", sep = " ", header = TRUE)
summary(data_adults)

## Ausschluss einzelner TN
subset = subset(subset, Participant != "4002")
subset = subset(subset, Participant != "3005")
subset = subset(subset, Participant != "1008")

## Normierung einer "Target-Figur"
# Da Turn 1,3 und 5 jeweils die gegenteiligen Target-Aois haben, als Turn 2 und 4, werden an dieser Stelle die Blicke auf Figure_1 und Figure_2 bei den Turns 1,3 und 5 umgedreht. 
# Figure 1 ist also jetzt immer "Target" und Figure 2 immer "Competitor"
subset$Figure_2 <- ifelse((subset$Turn == "1" | subset$Turn == "3" | subset$Turn == "5") & (subset$Aoi == "Figure_2" | subset$Aoi == "Figure_0"), 0,
                          ifelse((subset$Turn == "2" | subset$Turn == "4") & (subset$Aoi == "Figure_1" | subset$Aoi == "Figure_0"), 0, 1))

subset$Figure_1 <- ifelse((subset$Turn == "1" | subset$Turn == "3" | subset$Turn == "5") & (subset$Aoi == "Figure_1" | subset$Aoi == "Figure_0"), 0,
                          ifelse((subset$Turn == "2" | subset$Turn == "4") & (subset$Aoi == "Figure_2" | subset$Aoi == "Figure_0"), 0, 1))

## 
# Faktorisierung von Daten
##

data_adults$Participant <- as.factor(data_adults$Participant)
data_adults$ItemNr <- as.factor(data_adults$ItemNr)
data_adults$Turn <- as.factor(data_adults$Turn)
data_adults$ItemTime <- as.numeric(data_adults$ItemTime)

data_adults$Condition <- as.factor(data_adults$Condition)
data_adults$Aoi <- as.factor(data_adults$Aoi)
data_adults$SentType <- as.factor(data_adults$SentType)
data_adults$Gaze <- as.factor(data_adults$Gaze)
data_adults$CorrectLooks <- as.factor(data_adults$CorrectLooks)

data_adults$Trackloss <- as.logical(data_adults$Trackloss)
data_adults$ACCURACY <- as.factor(data_adults$ACCURACY)
data_adults$RT <- as.numeric(data_adults$RT)
data_adults$CorrectClick <- as.factor(data_adults$CorrectClick)
data_adults$NoSent <- as.factor(data_adults$NoSent)
data_adults$TurnWindow <- as.factor(data_adults$TurnWindow)

summary(data_adults)

##
# Make eyetracking data
# Converting file into eye-tracking data --> muss konvertiert werden, um mit eyetrackingR bearbeitet zu werden.
##

TurnTime_adults <- make_eyetrackingr_data(data_adults, 
                                   participant_column = "Participant",
                                   trial_column = 'ItemNr',     # 
                                   time_column = "TurnTime",        # normalisierte Zeitdarstellung der Zeit -1400 bis +1000 msec um den Offset herum. 
                                   trackloss_column = "Trackloss",
                                   aoi_columns = c('Figure_1','Figure_2'),
                                   treat_non_aoi_looks_as_missing = TRUE)

summary(TurnTime_adults)


response_window_adults <- subset_by_window(TurnTime_adults, 
                                        window_start_time = -500, 
                                        window_end_time =800, 
                                        rezero = FALSE)

# clean data by trackloss --> hier wird ?berpr?ft, ob der Eye-Tracker die Augen verloren hat.
(trackloss <- trackloss_analysis(data = response_window_adults))

# remove trials with > 50% of trackloss
response_window_clean_adults <- clean_by_trackloss(data = response_window_adults, trial_prop_thresh = .5)

summary(response_window_clean_adults)

response_time_adults <- make_time_sequence_data(response_window_clean_adults, time_bin_size = 100,
                                             predictor_columns = c("Condition"),
                                             aois = "Figure_2",
                                             summarize_by = c("Participant", "ItemNr"))

summary(response_time_adults)

##############
# visualize time results:  by condition
##############

## get a subset without NAs 
subset_cond_full <-  na.omit(response_time_adults)

plot(response_time_adults, predictor_column = c("Condition")) + 
  theme_light() +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 14)
  ) +
  coord_cartesian(ylim = c(0,0.9)) + 
  geom_vline(xintercept=c(-332,0), linetype="dashed") +
  geom_vline(xintercept=c(200,0), linetype="dotdash") +
  ylab("Durchschnittliche Fixationen auf die nächste Figur")+
  xlab("Zeit (in ms)") +
  #labs(color =  "Bedingungen")+
  #guides(fill=FALSE)
  scale_fill_discrete(name="Bedingungen",
                    labels=c("Deklarativ", "Interrogativ", "Deklarativ + Blick", "Interrogativ + Blick"))
  
##### 
# Erster Zeitraum: zwischen -500 bis 200 Msec
# Time window from -500 bis 200 msec
#####

response_window_pre <- subset_by_window(TurnTime_adults, 
                                    window_start_time = -500, 
                                    window_end_time =200, 
                                    rezero = FALSE)

# clean data by trackloss --> hier wird ?berpr?ft, ob der Eye-Tracker die Augen verloren hat.
(trackloss <- trackloss_analysis(data = response_window_pre))

# remove trials with > 50% of trackloss
response_window_clean_pre <- clean_by_trackloss(data = response_window_pre, trial_prop_thresh = .5)

summary(response_window_clean_pre)


###
# Inferenzstatistik: Gaze + SentType based
###
time_window_data_pre <- make_time_window_data(response_window_clean_pre, 
                          aois = "Figure_2", 
                          predictor_columns = c("SentType", "Gaze"),
                          summarize_by = c("Participant", "ItemNr"))

subset_time_window_pre <-  na.omit(time_window_data_pre)

contrasts(subset_time_window_pre$Gaze) <- contr.sum
contrasts(subset_time_window_pre$SentType) <- contr.sum
#contrasts(subset_time_window_pre$Turn) <- contr.sum

aggregate(Elog ~ Gaze*SentType, subset_time_window_pre, mean)
aggregate(Elog, subset_time_window_pre, mean)

mean_elog <- aggregate(Elog ~ Gaze*SentType, subset_time_window_pre, mean)
boxplot(Elog ~ Gaze*SentType,
        col=c("#669966","#669933"),subset_time_window_pre,
        #main = "Reaktionszeit in Abhängigkeit der Faktoren",
        xlab = "Satzart * Blick",
        #ylab = "Reaktionszeit (in ms)",
        names = c("Deklarativ + \n kein Blick", "Deklarativ + \n Blick", "Interrogativ + \n kein Blick", "Interrogativ + \n Blick"))
points(mean_elog$Elog,col="red",pch=19)


### Baseline Modell mit Condition-Vergleich
subset_baseline_pre.model <- lmer(Elog ~ Gaze+SentType+Turn + (1 | ItemNr) + (1 | Participant), 
                              data = subset_time_window_pre)

# cleanly show important parts of model (see `summary()` for more)
#(est <- broom::tidy(subset_baseline.model, effects="fixed"))

summary(subset_baseline_pre.model)


#####
# Statistics with buildmer and lmertest
#####
full.model <- Elog ~ Gaze*SentType +
  (Gaze*SentType | Participant) +
  (Gaze*SentType | Turn) +
  (Gaze*SentType | ItemNr)


m_forwards_pre <- buildmer(full.model, data=subset_time_window_pre, buildmerControl=buildmerControl(direction='order',
                                                                    args=list(control=lmerControl(optimizer='bobyqa'))))
summary(m_forwards_pre)

## Maximales Modell, das noch konvergiert (durch buildmer-order Berechnung):

max.model <- Elog ~ 1 + SentType + Gaze + SentType:Gaze + 
  (1 + Gaze + SentType + Gaze:SentType | Participant) + 
  (1 + Gaze + SentType | ItemNr)


m_backwards_pre <- buildmer(max.model, data=subset_time_window_pre, buildmerControl = buildmerControl(direction='backward',
                                                                                     args=list(control=lmerControl(optimizer='bobyqa'))))
summary(m_backwards_pre)


cohen.d(Elog~ Gaze, data =subset_time_window_pre) 
cohen.d(Elog~ SentType, data =subset_time_window_pre)
cohen.d(Elog~ Gaze*SentType, data =subset_time_window_pre) 



## Zeitraum von 200 bis 750 msec
# second time window from 200 msec till 750 sec
response_window_post <- subset_by_window(TurnTime_adults, 
                                        window_start_time = 200, 
                                        window_end_time = 750, 
                                        rezero = FALSE)

# clean data by trackloss --> hier wird ?berpr?ft, ob der Eye-Tracker die Augen verloren hat.
(trackloss <- trackloss_analysis(data = response_window_post))

# remove trials with > 50% of trackloss
response_window_clean_post <- clean_by_trackloss(data = response_window_post, trial_prop_thresh = .5)

summary(response_window_clean_post)

###
# Inferenzstatistik: Gaze + SentType based
###

time_window_data_post <- make_time_window_data(response_window_clean_post, 
                                              aois = "Figure_2", 
                                              predictor_columns = c("SentType", "Gaze", "Turn"),
                                              summarize_by = c("Participant", "ItemNr"))

subset_time_window_post <-  na.omit(time_window_data_post)

plot(Elog ~ SentType, data = subset_time_window_post)
plot(Elog ~ Gaze, data = subset_time_window_post)
plot(Elog~Gaze*SentType, data = subset_time_window_post)

contrasts(subset_time_window_post$Gaze) <- contr.sum
contrasts(subset_time_window_post$SentType) <- contr.sum

mean_elog <- aggregate(Elog ~ Gaze*SentType, subset_time_window_post, mean)
boxplot(Elog ~ Gaze*SentType,
        col=c("#669966","#669933"),subset_time_window_post,
        #main = "Reaktionszeit in Abhängigkeit der Faktoren",
        xlab = "Satzart * Blick",
        #ylab = "Reaktionszeit (in ms)",
        names = c("Deklarativ + \n kein Blick", "Deklarativ + \n Blick", "Interrogativ + \n kein Blick", "Interrogativ + \n Blick"))
points(mean_elog$Elog,col="red",pch=19)

### Baseline Modell mit Condition-Vergleich
subset_baseline_post.model <- lmer(Elog ~ Gaze+SentType+Turn + (1 | ItemNr) + (1 | Participant), 
                              data = subset_time_window_post)

# cleanly show important parts of model (see `summary()` for more)
#(est <- broom::tidy(subset_baseline.model, effects="fixed"))

summary(subset_baseline_post.model)


#####
# Statistics with buildmer and lmertest
#####
full.model <- Elog ~ Gaze*SentType*Turn +
  (Gaze*SentType*Turn | Participant) +
  (Gaze*SentType*Turn | ItemNr)

m_forwards_post <- buildmer(full.model, data=subset_time_window_post, buildmerControl=buildmerControl(direction='order',
                                                                                                args=list(control=lmerControl(optimizer='bobyqa'))))
summary(m_forwards_post)

komisches_modell <- buildgam(full.model, data = subset_time_window_post)
summary(komisches_modell)

## Maximales Modell, das noch konvergiert (durch buildmer-order Berechnung):

max.model <- Elog ~ 1 + SentType + Gaze + Turn + Gaze:Turn + SentType:Gaze +  
  SentType:Turn + SentType:Gaze:Turn + (1 + Gaze | Participant) +  
  (1 + SentType + Gaze | ItemNr)


m_backwards_post <- buildmer(max.model, data=subset_time_window_post, buildmerControl = buildmerControl(direction='backward',
                                                                                                  args=list(control=lmerControl(optimizer='bobyqa'))))
summary(m_backwards_post)

## Effektstärke
cohen.d(Elog~ Gaze, data =subset_time_window_post) 
cohen.d(Elog~ SentType, data =subset_time_window_post)
cohen.d(Elog~ Gaze*SentType, data =subset_time_window_post) 
