library(tidyverse)
library(ggplot2)
library(dplyr)
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
  rename(YearCollected = 'Año de colecta')%>%
  add_column(months_alive = NA)%>% #number of months alive since transplant
  mutate(Monitor1=recode(Monitor1, #reclass Perdida (poor) as Muerta (dead)
                         'Perdida' = 'Muerta')) 

months_alive <- seedlings_clean%>%
  case_when(Monitor1 == "Muerta"~0,) #if dead at Monitor1, months_alive=0 (conservative estimate of time alive)
            
            
            #Monitor1 == "Viva" | Monitor2 == "Muerta" ~ x, #if alive at M1 and dead at M2, months_alive=x
            #Monitor1 == "Viva" | Monitor2 == "Viva" | Monitor3 == "Muerta" ~ y , #if alive at M2 and dead at M3, months_alive=y
            #Monitor1 == "Viva" | Monitor2 == "Viva" | Monitor3 == "Viva" ~ z ,) #if still alive, months_alive=z


#months alive post transplant- assign values based on if it was alive at M2, then M3...

summary(seedlings_clean)

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