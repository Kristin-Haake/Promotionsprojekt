#!/usr/bin/env python
# coding: utf-8

# # Databereinigung
# 
# Dieses Skript wurde zur Datenbereinigung der Eyetrackingdaten genutzt. 

# In[3]:


## Einladen der genutzten Pakete ##
import pandas as pd
from pandas import DataFrame
import json
import numpy as np
import os
from collections import defaultdict
import matplotlib.pyplot as plt
import statsmodels.api as sm
import statsmodels.formula.api as smf


# In[14]:


## Einladen der Metadaten ##
meta = pd.read_csv('MetaData.csv', delimiter=";", header=0)
meta = meta.drop(['Unnamed: 18', 'Unnamed: 20'], axis=1)

#data.columns = ["a", "b", "c", "etc."]


# In[15]:


## Erste Reinigungsstritte der Metadaten ##
meta= meta[meta.Turn != 6]
meta = meta.drop(['AudioFile','VideoFile', 'IntonationStart'], axis =1)

meta['Item-Nr.'] = meta['Dialogue'].astype(str) +'-'+meta['Turn'].astype(str)
meta=meta.dropna().reset_index(drop=True)
meta[['CrittUttStart', 'TurnStart','TurnEnd']] = meta[['CrittUttStart', 'TurnStart','TurnEnd']].apply(pd.to_numeric)


# In[20]:


meta.describe()


# In[33]:


## Überprüfung der Länger der Zielsätze 
meta['CUlength'] = meta['TurnEnd']-meta['CrittUttStart']
meta['CUlength'].max()
meta['CUlength'].min()
meta['CUlength'].mean()


# In[1]:


## Einladen der Eytetrackingdaten
results = pd.read_csv('full_results/Output/full_results.csv', delimiter="\t", header=0, dtype=None)


# In[17]:


## Übersicht über die Spalten und deren Datentypen
results.info()


# In[19]:


## Erste Reinigungsstritte der Eytetrackingdaten
results = results.dropna()
results= results[results.dialogue != 9]
results = results.drop(['itemtype',], axis =1)

results=results.dropna().reset_index(drop=True)


# In[20]:


## Umstellen der gemessenen Zeitpunkte
results["TIMESTAMP"] = [float(str(i).replace(",", ".")) for i in results["TIMESTAMP"]]
results['TIMESTAMP'] = pd.to_numeric(results['TIMESTAMP'])
results['Time'] = results['TIMESTAMP']-results['TRIAL_START_TIME']


# In[21]:


## Umbenennen der Spalten
results.rename(columns={'RECORDING_SESSION_LABEL': 'Participant', 'dialogue': 'Dialogue', 'list': 'List', 'endtime': 'Endtime', 'corr_answer': 'Corr_answer', 'question': 'Question'}, inplace=True)


# In[22]:


## Erster Ausschluss einzelner TN
results= results[results.Participant != '2006 (1)']
results= results[results.Participant != '1007 (1)']
results= results[results.Participant != '1008 (1)']
results= results[results.Participant != '3006 (1)'] 
results= results[results.Participant != '1002l']


# In[23]:


## Auswahl jeder vierten Aufnahme, sodass ein Fixationswert alle vier Millisekunden (250Hz) übrig bleibt 
results = results.iloc[::4, :]
results=results.dropna().reset_index(drop=True)


# ## Verbindung beider Tabellen 

# In[1]:


### Verbindung beider Tabelle (merging)
full_table = pd.merge(results, meta, on = ['Dialogue','List'])


# In[30]:


## Übersicht über die Spalten und deren Datentypen
full_table.info()


# In[31]:


## Veränderung der Datentypen
full_table['Time'] = full_table['Time'].astype(int)
full_table['TurnEnd'] = full_table['TurnEnd'].astype(int)


# In[36]:


## Zeitliche Beschränkung der Berücksichtigten Fixationen (für die )
final_table = full_table[(full_table['Time']>= full_table['TurnEnd']-500) & (full_table['Time']<= full_table['TurnEnd']+750)]
final_table=final_table.dropna().reset_index(drop=True)


# In[37]:


final_table.describe()


# In[38]:


## Übersicht über die Spalten und deren Datentypen
final_table.info()


# In[40]:


## Auswahl der Spalten für die fertige Tabelle
table_subset = final_table[["Participant", "TRIAL_INDEX", "ACCURACY", "Click", "RT", "TurnEnd","Corr_answer", "List", "Time", \
                            "Turn", "Condition", "Target","SentType", "NoSent", "Gaze", "LEFT_INTEREST_AREAS", \
                            "RIGHT_INTEREST_AREAS", "Item-Nr."]]
table_subset = table_subset.rename(columns={"Time": "ItemTime", "Item-Nr.": "ItemNr"})


# In[41]:


final_table.columns.values


# In[44]:


final_table.info()


# In[48]:


#table_subset["CorrectClick"] = np.where(table_subset['Click']==table_subset['Corr_answer'], 1,0)
table_subset.loc[table_subset['Click'] == table_subset['Corr_answer'], 'CorrectClick'] = 0
table_subset.loc[table_subset['Click'] != table_subset['Corr_answer'], 'CorrectClick'] = 1


# In[49]:


## Zuordnung zu AoI
table_subset['LEFT_INTEREST_AREAS'] = table_subset['LEFT_INTEREST_AREAS'].replace({"[ 2 ]": 2, "[ 1 ]": 1, '[]': 0})
table_subset['RIGHT_INTEREST_AREAS'] = table_subset['RIGHT_INTEREST_AREAS'].replace({"[ 2 ]": 2, "[ 1 ]": 1, '[]': 0})
table_subset['Aoi'] = table_subset['LEFT_INTEREST_AREAS']+table_subset['RIGHT_INTEREST_AREAS']


# In[50]:


## Zuordnung der AoI
table_subset.loc[table_subset['Aoi'] != table_subset['Target'], 'CorrectLooks'] = 0
table_subset.loc[table_subset['Aoi'] == table_subset['Target'], 'CorrectLooks'] = 1


# In[51]:


## Anpassen des Datentyps und Zurodnung zur Zielfigur
table_subset['Aoi'] = table_subset['Aoi'].astype(str)
table_subset['Aoi'] = 'Figure_'+table_subset['Aoi']


# In[52]:


## Anpassung der Zielfigur und der sprechenden Figur
table_subset.loc[table_subset['Aoi'] == 'Figure_1',  'Figure_1'] = 1
table_subset.loc[table_subset['Aoi'] != 'Figure_1',  'Figure_1'] = 0
table_subset.loc[table_subset['Aoi'] == 'Figure_2',  'Figure_2'] = 1
table_subset.loc[table_subset['Aoi'] != 'Figure_2',  'Figure_2'] = 0
table_subset['Figure_2'] = table_subset['Figure_2'].astype(int)
table_subset['Figure_1'] = table_subset['Figure_1'].astype(int)
table_subset['Figure_2'] = table_subset['Figure_2'].astype(str)
table_subset['Figure_1'] = table_subset['Figure_1'].astype(str)


# In[53]:


table_subset.loc[table_subset['Aoi'] == 'Figure_0', 'Trackloss'] = True
table_subset.loc[table_subset['Aoi'] == 'Figure_1', 'Trackloss'] = False
table_subset.loc[table_subset['Aoi'] == 'Figure_2', 'Trackloss'] = False


# In[54]:


table_subset['Trackloss'].unique()


# In[55]:


#table_subset['Trackloss'] = table_subset['Trackloss'].astype(int)
table_subset['CorrectLooks'] = table_subset['CorrectLooks'].astype(int)
table_subset['CorrectClick'] = table_subset['CorrectClick'].astype(int)


# In[56]:


table_subset = table_subset.drop(columns=['LEFT_INTEREST_AREAS', 'RIGHT_INTEREST_AREAS'])


# In[57]:


table_subset = table_subset.drop(columns=['Corr_answer', 'Click'])


# In[58]:


table_subset


# ### TurnTime berechnen. 
# *  jeder Turn beginnt bei 0 msec und dauert dann die Gesamtlänge des Turns. --> TurnTime
# * TurnTime nach Offset normalisiern
#     * kürzeste Critical Utterance 1401msec; da wir aber die Antizipation des Turnendes ansehen wollen, kürzen wir den Timestart auf -500ms -> TimeStart
#     * Maximales Zeitfenster bis +750 msec ab Offset

# In[59]:


table_subset['Turn'] = table_subset['Turn'].astype(str)


# In[60]:


## Normalisierung der Zeit mit dem Turnende als Nullpunkt
table_subset["TurnWindow"] = np.where(((table_subset['ItemTime'] >= (table_subset['TurnEnd']-1400)) & 
    (table_subset['ItemTime'] <= (table_subset['TurnEnd']+1000))), table_subset['ItemNr']+'-'+table_subset['Turn'], 0)
table_subset["TurnTime"] = np.where(table_subset["TurnWindow"] != 0, table_subset["ItemTime"]-table_subset['TurnEnd'], 0)


# In[62]:


table_subset['TurnTime']


# In[87]:


## Speicherung der bereinigten und für die Analyse vorbereiteten Tabelle
table_subset.to_csv(r'cleaned_table.txt', header=1, index=None, sep='\t')




