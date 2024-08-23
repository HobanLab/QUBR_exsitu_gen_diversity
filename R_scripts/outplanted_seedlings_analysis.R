library(tidyverse)
library(ggplot2)
library(dplyr)

#imports one of Daniel's databases
outplanted_seedlings <- read_csv("./data/Datos de Siembra en Ranchos_Actualizado_05_2024.xlsx - Datos_Campaña_Seimbra_2023.csv")

summary(outplanted_seedlings)

seedlings_clean <- outplanted_seedlings%>% 
#renames columns to simplified English, differentiates seed origin and seedling planted region
#should add Monitor3, but the column title has an enter in it that I can't remove
  rename(Name = 'Nombre')%>%
  rename(Site = 'Localidad')%>%
  rename(Ranch = 'Rancho, Lugar o Sitio')%>%
  rename(Monitor1 = '13/02/2022 - Monitoreo 1')%>%
  rename(Monitor2 = '20/01/2023 Monitoreo 2')%>%
  rename(OriginReg = 'Procedencia semilla  (color)')%>% 
  rename(PlantedReg = 'Región')

#separates seedlings into seed origin region
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

#there are more site names in Daniel's database than we used previously         
seedlings_clean %>%
  group_by(Site) %>%
  summarise(count = n())




#survival of seedlings by site
#EAST 
seedlings_E%>% 
  ggplot() +
  geom_bar(aes(x = Site, fill = Monitor2)) +
  theme_classic()
#WEST
seedlings_W%>% 
  ggplot() +
  geom_bar(aes(x = Site, fill = Monitor2)) +
  theme_classic()
#NORTH
seedlings_N%>% 
  ggplot() +
  geom_bar(aes(x = Site, fill = Monitor2)) +
  theme_classic()

#seedling outcome by region
seedlings_clean%>%
  ggplot() +
  geom_bar(aes(x = Monitor2, fill = OriginReg)) +
  theme_classic()
seedlings_clean%>%
  ggplot() +
  geom_bar(aes(x = OriginReg, fill = Monitor2)) +
  theme_classic()

seedlings_clean%>%
  ggplot() +
  geom_bar(aes(x = Monitor2, fill = OriginReg)) +
  theme_classic()


