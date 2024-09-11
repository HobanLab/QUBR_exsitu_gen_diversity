library(tidyverse)
library(ggplot2)
library(dplyr)
library(tibble)
library(lubridate)
setwd("..")

#assigns values for the dates monitoring took place
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
  mutate(DateDied = case_when((Monitor1 == 'Muerta') | (Monitor1 == 'Viva' & Monitor2 == 'Muerta') ~ DatePlanted,
                              (Monitor1 == 'Nueva' & Monitor2 == 'Muerta') ~ '13-02-2022',
                              (Monitor1 == 'Nueva' & Monitor3 == 'Muerta') | (Monitor1 == 'Viva' & Monitor3 == 'Muerta') | (Monitor2 == 'Nueva' & Monitor3 == 'Muerta') ~ '20-01-2023'))%>%
#format date as DayMonthYear
  add_column(TimeAlive = NA)%>%
  add_column(RatioTimeAlive = NA)%>%
  add_column(PotentialTimeAlive = NA)%>%
  
  mutate(DateDied = dmy(DateDied))%>%
  mutate(DatePlanted = dmy(DatePlanted))%>%
  mutate(RatioTimeAlive = dmy(RatioTimeAlive))%>%
  mutate(PotentialTimeAlive = dmy(PotentialTimeAlive))%>%
  
#calculate TimeAlive as difference between DatePlanted and DateDied
  mutate(TimeAlive = DateDied - DatePlanted)%>%
  mutate(PotentialTimeAlive = Today - DatePlanted)%>%#days since it was first planted
  mutate(RatioTimeAlive = TimeAlive / PotentialTimeAlive) #this line doesn't work because you can't divide difftimes, but I needed to convert those columns to  
  

summary(seedlings_clean)
?duration
?class


#separates seedlings into seed origin region
seedlings_E <- seedlings_clean23%>%
  filter(OriginReg == 'Golfo')
seedlings_W <- seedlings_clean23%>%
  filter(OriginReg =='Pacifico')
seedlings_N <- seedlings_clean23%>%
  filter(OriginReg == 'Norte')
#includes all seedlings that were dead at 1st OR 2nd monitoring
seedlings_dead <- seedlings_clean23%>% 
  filter(Monitor1 == "Muerta" | Monitor2 == "Muerta")
#includes all seedlings that were alive as of 2nd monitoring
seedlings_alive <- seedlings_clean23%>%
  filter(str_detect(Monitor2, "Viva"))
#survival of seedlings by town
#EAST 
seedlings_E%>% 
  ggplot() +
  geom_bar(aes(x = Monitor2, fill = Monitor2)) +
  facet_grid(~Town) +
  theme_classic()
#WEST
seedlings_W%>% 
  ggplot() +
  geom_bar(aes(x = Monitor2, fill = Monitor2)) +
  facet_grid(~Town) +
  theme_classic()
#NORTH
seedlings_N%>% 
  ggplot() +
  geom_bar(aes(x = Monitor2, fill = Monitor2)) +
  facet_grid(~Town) +
  theme_classic()
#seedling outcome by region
seedlings_clean23%>%
  ggplot() +
  geom_bar(aes(x = Monitor2, fill = Monitor2)) +
  facet_grid(~OriginReg) +
  theme_classic()

unique(seedlings_W$Monitor2)

#there are more town names in Daniel's database than we used previously         
seedlings_clean23 %>%
  group_by(Town) %>%
  summarise(count = n())

#what columns are only in one tab, add those to the other tab, rbind command to combine (2024 to bottom of)
?rbind
