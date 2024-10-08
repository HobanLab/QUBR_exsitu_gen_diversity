library(tidyverse)
library(ggplot2)
library(dplyr)
library(tibble)
library(lubridate)
setwd("..")

Today <- today()
Monitor1Date <- dmy("13/02/2022")
Monitor2Date <- dmy("20/01/2023")
Monitor3Date <- dmy("13/12/2023")

#imports Daniel's databases (2023 and 2024 tabs)
outplanted_seedlings23 <- read_csv("./data/Datos de Siembra en Ranchos_Actualizado_05_2024.xlsx - Datos_Campaña_Seimbra_2023.csv")
outplanted_seedlings24 <- read_csv("./data/Datos de Siembra en Ranchos_Actualizado_05_2024.xlsx - Datos_Campaña_Seimbra_2024.csv")
summary(outplanted_seedlings23)
summary(outplanted_seedlings24)


#combines data from 2023 and 2024 tabs
seedlings_combined <- bind_rows(outplanted_seedlings23, outplanted_seedlings24)
summary(seedlings_combined)

#renames columns to simplified English, differentiates seed origin and seedling planted region
seedlings_clean <- seedlings_combined%>%
  rename(Name = 'Nombre')%>%
  rename(Town = 'Localidad')%>%
  rename(Ranch = 'Rancho, Lugar o Sitio')%>%
  rename(Monitor1 = '13/02/2022 - Monitoreo 1')%>%
  rename(Monitor2 = '20/01/2023 Monitoreo 2')%>%
  rename(Monitor3 = '13/12/2023\nMonitoreo 3')%>%
  rename(OriginReg = 'Procedencia semilla  (color)')%>%
  rename(PlantedReg = 'Región')%>%
  rename(DatePlanted = 'Fecha transplante')%>%
  rename(YearCollected = 'Año de colecta')%>%
  rename(Observations = 'Observaciones')%>% 
  rename(Contact = 'Contacto')%>%
  rename(PlantedIn = 'Sembrado en:')%>%
  rename(Watered = 'Lluvia solida')%>%
  rename(Monitor4 = 'Monitoreo 1 (__/__/__)')%>%
  rename(OriginLabelAsh = 'Procedencia Etiqueta Ash Abril 2024')%>%
  mutate(Monitor1=recode(Monitor1, 'Perdida' = 'Muerta'))%>%#reclass Perdida (poor) as Muerta (dead)
#calculate when a seedling died based on the last Monitoring date it was seen alive
  add_column(DateDied = NA)%>%
  mutate(DateDied = case_when(is.na(Monitor1) & is.na(Monitor2) & is.na(Monitor3) ~ DatePlanted,

                              Monitor1 == 'Nueva' & is.na(Monitor2) ~ DatePlanted,
                              Monitor1 == 'Nueva' & Monitor2 == 'Muerta' ~ DatePlanted,
                              
                              Monitor1 == 'Muerta' ~ DatePlanted,
                              
                              Monitor1 == 'Viva' & Monitor2 == 'Muerta' ~'13/02/2022',
                              
                              Monitor2 == 'Viva' & is.na(Monitor3) ~ '20/01/2023',
                              Monitor1 == 'Viva' & is.na(Monitor2) ~ '13/02/2022',
                              Monitor2 == 'Nueva' & Monitor3 == 'Muerta' ~ DatePlanted,
                              Monitor2 == 'Nueva' & is.na(Monitor3) ~ DatePlanted,
                              Monitor2 == 'Viva' & Monitor3 == 'Muerta' ~ '20/01/2023'))%>%
  add_column(Outcome = NA)%>%
  
#format date as DayMonthYear
  add_column(TimeAlive = NA)%>%
  add_column(RatioTimeAlive = NA)%>%
  add_column(PotentialTimeAlive = NA)%>%
  
  mutate(DateDied = dmy(DateDied))%>%
  mutate(DatePlanted = dmy(DatePlanted))%>%
  mutate(RatioTimeAlive = dmy(RatioTimeAlive))%>%
  mutate(PotentialTimeAlive = dmy(PotentialTimeAlive))%>%
  mutate(Outcome = case_when(Monitor1 == 'Muerta' | Monitor2 == 'Muerta' | Monitor3 == 'Muerta' ~ 'Dead',
                             
                             Monitor3 == 'Nueva' | Monitor3 == 'Viva' ~ 'Alive',
                             
                             Monitor2 == 'Nueva' & is.na(Monitor3) ~ 'Presumed Dead',
                             Monitor2 == 'Viva' & is.na(Monitor3) ~ 'Presumed Dead',
                             is.na(Monitor2) & is.na(Monitor3) ~ 'Presumed Dead',
                             is.na(Monitor1) & is.na(Monitor2) & is.na(Monitor3) ~ 'Presumed Dead'
                             ))%>%
  
#calculate TimeAlive as difference between DatePlanted and DateDied
  mutate(TimeAlive = DateDied - DatePlanted)%>%
  mutate(PotentialTimeAlive = Today - DatePlanted)%>% #days since it was first planted
  mutate(TimeAlive = case_when(Outcome == 'Alive' ~ (Today - DatePlanted),
                               Outcome == 'Dead' ~ (DateDied - DatePlanted),
                               Outcome == 'Presumed Dead' ~ (DateDied - DatePlanted)))%>%
  mutate(RatioTimeAlive = (as.numeric(TimeAlive)) / (as.numeric(PotentialTimeAlive)))%>%
#removes rows for individuals handed out at Festival 2023
    filter(!str_detect(Ranch, "Festival"))



#prioritize seedlings for visits while in Baja based on number of individuals at each ranch  
priority_sites <- seedlings_clean%>%
  filter(Outcome == 'Alive')%>%
  group_by(PlantedReg, Ranch, N, W)%>%
  summarise(n())
  
#export .csv to make Google Map of sites
#write.csv(priority_sites, "priority_sites.csv")


#increments of how old a seedling could be
df_age <- 
  data.frame("Days"=seq(0, 1200, 1), "DaysAlive" = NA)
  

#for loop  
for (i in df_age) {
  sum(i <= df_age$DaysAlive)
}




  
seedlings_clean%>%
  ggplot() +
  geom_histogram(aes(x = TimeAlive)) +
  theme_classic()
  

#TimeAlive per PlantedRegion
seedlings_clean%>%
  ggplot() +
  geom_boxplot(aes(x = PlantedReg, y = TimeAlive, fill = PlantedReg)) +
  theme_classic()

#seedling outcome by region of origin
seedlings_clean%>%
  ggplot() +
  geom_bar(aes(x = Monitor2, fill = Monitor2)) +
  facet_grid(~OriginReg) +
  theme_classic()

#how many of the seedlings are alive vs dead?
seedlings_alive <- seedlings_clean%>%
  filter(Outcome == 'Alive')
seedlings_dead <- seedlings_clean%>%
  filter(Outcome == 'Dead')

#total mortality of seedlings
seedlings_clean%>%
  ggplot(aes(x = Outcome, y =..count..,)) +
  geom_bar(aes(fill = Outcome)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.5) +
  theme_classic()

#mortality of seedlings by region planted
seedlings_clean%>%
  ggplot(aes(x = Outcome, y = ..count..,)) +
  geom_bar(aes(fill = PlantedReg)) +
  facet_grid(~PlantedReg) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.5) +
  theme_classic()

#mortality of seedlings by region of origin
seedlings_clean%>%
  ggplot(aes(x = Outcome, y = ..count..,)) +
  geom_bar(aes(fill = OriginReg)) +
  facet_grid(~OriginReg) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.5) +
  theme_classic()

#number of seedlings from each region of origin
seedlings_clean%>%
  ggplot(aes(x = OriginReg, y =..count..,)) +
  geom_bar(aes(fill = OriginReg)) +
  geom_text(aes(label = ..count..), stat = "count", vjust=-0.5) +
  theme_classic()

#CHI SQUARED TEST
# Create a data frame from the main data set
watered_data = data.frame(seedlings_clean$Watered, seedlings_clean$Outcome)
# Create a contingency table with the needed variables          
watered_data = table(seedlings_clean$Watered,seedlings_clean$Outcome)
print(watered_data)
print(chisq.test(watered_data))

OriginRegion_outcome = data.frame(seedlings_clean$Outcome, seedlings_clean$OriginReg)
OriginRegion_outcome = table(seedlings_clean$Outcome, seedlings_clean$OriginReg)
print(OriginRegion_outcome)
print(chisq.test(OriginRegion_outcome))

PlantedRegion_outcome = data.frame(seedlings_clean$Outcome, seedlings_clean$PlantedReg)
PlantedRegion_outcome = table(seedlings_clean$Outcome, seedlings_clean$PlantedReg)
print(PlantedRegion_outcome)
print(chisq.test(PlantedRegion_outcome))