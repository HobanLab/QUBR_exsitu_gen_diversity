library(tidyverse)
library(ggplot2)
library(dplyr)
library(tibble)
library(lubridate)
setwd("..")


#imports one of Daniel's databases
outplanted_seedlings <- read_csv("./data/Datos de Siembra en Ranchos_Actualizado_05_2024.xlsx - Datos_Campaña_Seimbra_2023.csv")

summary(outplanted_seedlings)


seedlings_clean <- outplanted_seedlings%>% 
#renames columns to simplified English, differentiates seed origin and seedling planted region
  rename(Name = 'Nombre')%>%
  rename(Town = 'Localidad')%>%
  rename(Ranch = 'Rancho, Lugar o Sitio')%>%
  rename(Monitor1 = '13/02/2022 - Monitoreo 1')%>%
  rename(Monitor2 = '20/01/2023 Monitoreo 2')%>%
  rename(Monitor3 = '13/12/2023\nMonitoreo 3')%>%
  rename(OriginReg = 'Procedencia semilla  (color)')%>% 
  rename(PlantedReg = 'Región')%>%
  rename(DatePlanted = 'Fecha transplante')%>%
  mutate(DatePlanted = dmy(DatePlanted)) %>%
  rename(YearCollected = 'Año de colecta')%>%
  mutate(Monitor1=recode(Monitor1, #reclass Perdida (poor) as Muerta (dead)
                         'Perdida' = 'Muerta'))%>%
  add_column(DateDied = NA)%>%
  
  #This mutate section isn't complete: it is working to update values in the DateDied column but the dates aren't correct and haven't covered all cases
  mutate(DateDied = case_when(Monitor1 == 'Muerta' ~ '01-01-2000', #should be DatePlanted
                              Monitor1 == 'Nueva' & Monitor2 == 'Muerta' ~ '13-02-2022', 
                              Monitor1 == 'Nueva' & Monitor3 == 'Muerta' ~ '20-01-2023',
                              Monitor2 == 'Nueva' & Monitor3 == 'Muerta' ~ '20-01-2023'))%>%
  mutate(DateDied = dmy(DateDied))%>%
  add_column(MonthsAlive = NA)%>%
  
summary(seedlings_clean)
unique(seedlings_clean$DatePlanted)
?duration

#s.durationstr() separates seedlings into seed origin region
seedlings_E <- seedlings_clean%>%
  filter(OriginReg == 'Golfo')
seedlings_W <- seedlings_clean%>%
  filter(OriginReg =='Pacifico')
seedlings_N <- seedlings_clean%>%
  filter(OriginReg == 'Norte')

#includes all seedlings that were dead at 1st OR 2nd monitoring
seedlings_dead <- seedlings_clean%>% 
  filter(Monitor1 == "Muerta" | Monitor2 == "Muerta")

#includes all seedlings that were alive as of 2nd monitoring
seedlings_alive <- seedlings_clean%>%
  filter(str_detect(Monitor2, "Viva"))

#there are more town names in Daniel's database than we used previously         
seedlings_clean %>%
  group_by(Town) %>%
  summarise(count = n())




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
seedlings_clean%>%
  ggplot() +
  geom_bar(aes(x = Monitor2, fill = Monitor2)) +
  facet_grid(~OriginReg) +
  theme_classic()

seedlings_clean%>%
  ggplot() +
  geom_bar(aes(x = Monitor2, fill = Monitor2)) +
  facet_grid(~OriginReg) +
  theme_classic()

seedlings_clean%>%
  ggplot() +
  geom_bar(aes(x = Monitor2, fill = Monitor2)) +
  facet_grid(~OriginReg) +
  theme_classic()

unique(seedlings_W$Monitor2)

#what columns are only in one tab, add those to the other tab, rbind command to combine (2024 to bottom of)