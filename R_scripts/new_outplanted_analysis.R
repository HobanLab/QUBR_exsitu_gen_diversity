rm(list=ls())
library(tidyverse)
library(ggplot2)
library(dplyr)

setwd("C:/Users/DBarry/Desktop/GitHub/QUBR_exsitu_gen_diversity")

####IMPORT DATA####

#seedlings planted in 2023
outplanted_seedlings23 <- read_csv("./data/Datos de Siembra en Ranchos_Actualizado_05_2024.xlsx - Datos_Campaña_Seimbra_2023 04_23_2025.csv")
#seedlings planted in 2024
outplanted_seedlings24 <- read_csv("./data/Datos de Siembra en Ranchos_Actualizado_05_2024.xlsx - Datos_Campaña_Seimbra_2024 04_23_2025.csv")%>%
  mutate('Núm. Etiqueta' = as.character('Núm. Etiqueta')) #interpret MetalTagID as a character, not numeric, because some inds have an A & B
#ecological data 2024
outplanted_seedlings24.field <- read_csv("./data/QUBR Field Datasheets Nov 2024 - filled - OP Seedlings 03_27_25.csv")
#ecological data 2025
outplanted_seedlings25 <- read_csv("./data/QUBR Field Datasheets Nov 2025 - filled.csv")


####DATA CLEANING: ECO####

#cleaning 2024 ecological data
eco_monitoring_24 <- outplanted_seedlings24.field%>%
  dplyr::select('QUBR ID', 'Ranch', 'Region', 'Height', 'Canopy cover', 'Metal tag ID', 'Condition', 'Date', 'Notes/comments')%>%
  mutate(Monitor='Monitor4')%>%
  mutate(Ranch=recode(Ranch,
                      'San Dio' = 'Rancho San Dionisio',
                      'Santo Do' = 'Santo Domingo',
                      'La Palapa' = 'La Rueda (Palapa)',
                      'Parque de Santiago' = 'Parque Ecológico Santiago',
                      'Santa Gertrudis (orchard)' = 'Santa Gertudris (Huerta)',
                      'Santa Gertrudis' = 'Santa Gertudris',
                      'Palo Verdal' = 'Palo Verdad'))%>%
  mutate(Condition=recode(Condition,
                          'the best' = 'great'))%>%
  mutate(Condition_num=as.factor(recode(Condition,
                                        'dead' = '0',
                                        'poor' = '0.25',
                                        'fair' = '0.5',
                                        'good' = '0.75',
                                        'great' = '1')))%>%
  mutate(Condition_num_24=Condition_num)%>%
  mutate(Canopy=recode(`Canopy cover`,
                       'patial shade' = 'partial shade',
                       'mostly  sun' = 'mostly sun',
                       'total sun' = 'full sun'))%>%
  mutate(Canopy_num=recode(Canopy, 
                           'full shade' = '0', 
                           'mostly shade' = '0.25', 'partial sun' = '0.25',
                           'half shade' = '0.5', 'half sun' = '0.5', 
                           'partial shade' = '0.75', 'mostly sun' = '0.75', 
                           'full sun' = '1'))%>%
  mutate(Canopy_num = fct_relevel(Canopy_num, "0",
                                  "0.25",
                                  "0.5",
                                  "0.75",
                                  "1"))%>%
  mutate(Canopy_num_24=Canopy_num)%>%
  mutate(Height_cm=as.factor(recode(Height,
                                       'below ankle' = '0 - 7.5',
                                       'ankle' = '7.5 - 16.5',
                                       'above ankle' = '16.5 - 29.5',
                                       'mid shin' = '29.5 - 39.5',
                                       'below knee' = '39.5 - 47',
                                       'knee' = '47 - 57.5',
                                       'above knee' = '57.5 - 71.5',
                                       'above the knee' = '57.5 - 71.5',
                                       'below hip' = '71.5 - 85',
                                       'low hip' = '71.5 - 85',
                                       'hip' = '85 - 96.5',
                                       'mid hip' = '85 - 96.5',
                                       'above hip' = '96.5 - 109.5',
                                       'mid torso' = '109.5 - 123.5',
                                       'below shoulders' = '123.5 - 136',
                                       'shoulder' = '136 - 147',
                                       'above shoulder' = '147 - 158.5',
                                       'above shoulders' = '147 - 158.5',
                                       'Dana height' = '158.5 - 171.5',
                                       'taller than Dana' = '171.5+',
                                       'taller than Daniel' = '171.5+',
                                       '1.5 Daniels' = '171.5+',
                                       '2 Daniels' = '175.5+')))%>%
  mutate(Height_cm = fct_relevel(Height_cm, "0 - 7.5",
                                    "7.5 - 16.5",
                                    "16.5 - 29.5",
                                    "29.5 - 39.5",
                                    "39.5 - 47",
                                    "47 - 57.5",
                                    "57.5 - 71.5",
                                    "71.5 - 85",
                                    "85 - 96.5",
                                    "96.5 - 109.5",
                                    "109.5 - 123.5",
                                    "123.5 - 136",
                                    "136 - 147",
                                    "147 - 158.5",
                                    "158.5 - 171.5",
                                    "171.5+"))%>%
  mutate(Height_cm_24=Height_cm)%>%
  mutate(Year = '2024')%>%
  mutate(Notes_24=`Notes/comments`)


#cleaning 2025 ecological data
eco_monitoring_25 <- outplanted_seedlings25%>%
  dplyr::select('Ranch', 'Region', 'Height', 'Canopy cover', 'Metal tag ID', 'Condition', 'Date', 'Notes/comments')%>%
  mutate(Monitor='Monitor5')%>%
  mutate(Condition=recode(Condition,
                          'Great' = 'great',
                          'Good' = 'good',
                          'Fair' = 'fair',
                          'Fine' = 'fair',
                          'Poor' = 'poor',
                          'Dead' = 'dead'))%>%
  mutate(Condition_num=as.factor(recode(Condition,
                                        'great' = '1',
                                        'good' = '0.75',
                                        'fair' = '0.5',
                                        'poor' = '0.25',
                                        'dead' = '0')))%>%
  mutate(Condition_num_25=Condition_num)%>%
  mutate(Canopy_num=as.factor(recode(`Canopy cover`,
                                     'full shade' = '0',
                                     'mostly shade' = '0.25', 'partial sun' = '0.25',
                                     'half shade' = '0.5', 'half sun' = '0.5',
                                     'mostly sun' = '0.75', 'partial shade' = '0.75',
                                     'full sun' = '1')))%>%
  mutate(Canopy_num = fct_relevel(Canopy_num, "0",
                                  "0.25",
                                  "0.5",
                                  "0.75",
                                  "1"))%>%
  mutate(Canopy_num_25=Canopy_num)%>%
  mutate(Height_cm=as.factor(recode(Height,
                                    'below ankle' = '0 - 7.5',
                                    'above ankle' = '16.5 - 29.5',
                                    'below knee' = '39.5 - 47',
                                    'at knee' = '47 - 57.5',
                                    'above knee' = '57.5 - 71.5',
                                    'below hip' = '71.5 - 85',
                                    'low hip' = '71.5 - 85',
                                    'at hip' = '85 - 96.5',
                                    'mid hip' = '85 - 96.5',
                                    'at waist' = '85 - 96.5',
                                    'above hip' = '96.5 - 109.5',
                                    'below chest' = '109.5 - 123.5',
                                    'at chest' = '123.5 - 136',
                                    'below shoulder' = '123.5 - 136',
                                    'below head' = '136 - 147',
                                    'above shoulder' = '147 - 158.5',
                                    'above shoulders' = '147 - 158.5',
                                    'above neck (nose)' = '147 - 158.5',
                                    'approx 1 Dana' = '158.5 - 171.5',
                                    'taller than Dana' = '171.5+',
                                    'above head (2mts)' = '171.5+'
                                    )))%>%
  mutate(Height_cm = fct_relevel(Height_cm, "0 - 7.5",
                                # "7.5 - 16.5",
                                 "16.5 - 29.5",
                                # "29.5 - 39.5",
                                 "39.5 - 47",
                                 "47 - 57.5",
                                 "57.5 - 71.5",
                                 "71.5 - 85",
                                 "85 - 96.5",
                                 "96.5 - 109.5",
                                 "109.5 - 123.5",
                                 "123.5 - 136",
                                 "136 - 147",
                                 "147 - 158.5",
                                 "158.5 - 171.5",
                                 "171.5+"))%>%
  mutate(Height_cm_25=Height_cm)%>%
  mutate(Year = '2025')%>%
  mutate(Notes_25=`Notes/comments`)



####tanglegram####

#LONG: combining 2024 and 2025 ecological data so that each Metal tag ID has two rows, one for 2024 observations and one for 2025 observations

#removes individuals with incomplete data
eco_monitoring_24_long <- eco_monitoring_24%>%
  filter(!is.na(`Metal tag ID`))%>%
  filter(!is.na(Height))%>%
  filter(Height != 'N/A')%>%
  select('Metal tag ID', 'Height_cm', 'Condition_num', 'Canopy_num', 'Year', 'Notes/comments')

eco_monitoring_25_long <- eco_monitoring_25%>%
  filter(!is.na(`Metal tag ID`))%>%
  filter(!is.na(Height))%>%
  filter(Height != 'N/A')%>%
  filter(!is.na(Condition_num))%>%
  select('Metal tag ID', 'Height_cm', 'Condition_num', 'Canopy_num', 'Year', 'Notes/comments')

#combines 2024 and 2025 data for individuals that were observed in both years
eco_monitoring_long <- merge(eco_monitoring_24_long, eco_monitoring_25_long, all = TRUE)%>%
  group_by(`Metal tag ID`)%>%
  filter(n() == 2)

#WIDE: combining 2024 and 2025 data so that each Metal tag ID has one row and Height/Canopy/Condition have separate rows for 2024 and 2025



#removes individuals with incomplete data
eco_monitoring_24_wide <- eco_monitoring_24%>%
  filter(!is.na(`Metal tag ID`))%>%
  filter(!is.na(Height))%>%
  filter(Height != 'N/A')%>%
  select('Metal tag ID', 'Height_cm_24', 'Condition_num_24', 'Canopy_num_24', 'Notes_24')

eco_monitoring_25_wide <- eco_monitoring_25%>%
  filter(!is.na(`Metal tag ID`))%>%
  filter(!is.na(Height))%>%
  filter(Height != 'N/A')%>%
  filter(!is.na(Condition_num))%>%
  select('Metal tag ID', 'Height_cm_25', 'Condition_num_25', 'Canopy_num_25', 'Notes_25')

eco_monitoring_wide <- left_join(eco_monitoring_24_wide, eco_monitoring_25_wide, by='Metal tag ID')%>%
    mutate(Condition_num_24=as.numeric(levels(Condition_num_24))[Condition_num_24])%>%
  mutate(Condition_num_25=as.numeric(levels(Condition_num_25))[Condition_num_25])%>%
  mutate(slope=case_when(Condition_num_25 > Condition_num_24 ~ '1',
                         Condition_num_25 < Condition_num_24 ~ '-1',
                         Condition_num_25 == Condition_num_24 ~ '0'))


eco_monitoring_long_x <- eco_monitoring_wide%>%
  pivot_longer(cols = !`Metal tag ID`,
               names_to = ("Height_cm", "Canopy_num", "Condition_num"),
               names_sep = 
               values_to = "Condition_num_25")
  

?pivot_longer


#FIGURE: Height tanglegram
eco_monitoring_long%>%
  ggplot(. , aes(x = Year, y = Height_cm, color = as.factor(`Metal tag ID`))) +
  stat_summary(fun = mean, geom = "line", aes(group = `Metal tag ID`)) +
  ylab("Height bin (cm)") +
  theme_classic()

#FIGURE: Condition tanglegram
eco_monitoring_long%>%
  ggplot(. , aes(x = Year, y = Condition_num, color = `Metal tag ID`)) +
  stat_summary(fun = mean, geom = "line", aes(group = `Metal tag ID`)) +
  ylab("Condition") +
  theme_classic()




####DATA CLEANING: SURVIVORSHIP####

#combines all outplanted seedlings
outplanted_seedlings <- bind_rows(outplanted_seedlings23, outplanted_seedlings24)

#defines monitoring dates
Monitor1Date <- dmy("13/02/2022")
Monitor2Date <- dmy("20/01/2023")
Monitor3Date <- dmy("13/12/2023")
Monitor4Date <- dmy("23/11/2024")
Monitor5Date <- dmy("06/12/2025")

outplanted_seedlings_clean <-outplanted_seedlings%>%
#translates columns to English  
  rename(Name = 'Nombre', 
         Town = 'Localidad',
         Ranch = 'Rancho, Lugar o Sitio',
         Monitor1 = '13/02/2022 - Monitoreo 1',
         Monitor2 = '20/01/2023 Monitoreo 2',
         Monitor3 = '13/12/2023\nMonitoreo 3',
         OriginReg = 'Procedencia semilla  (color)',
         PlantedReg = 'Región',
         DatePlanted = 'Fecha transplante',
         YearCollected = 'Año de colecta',
         Observations = 'Observaciones',
         Contact = 'Contacto',
         PlantedIn = 'Sembrado en:',
         Watered = 'Lluvia solida',
         OriginLabelAsh = 'Procedencia Etiqueta Ash Abril 2024',
         MetalTagID = 'Núm. Etiqueta')%>%
#removes unneccesary columns  
  dplyr::select(-'Monitoreo 1 (__/__/__)', -'Contact', -'No. Code. Ind.')%>%
  filter(
    !str_detect(Ranch, "Arroyo:"))%>% #individuals from the Arroyo: El Palo Santo weren't observed in 2024 (they were fairly new)
#reclass Perdida (lost) as Muerta (dead)
  mutate(across(starts_with("Monitor"), ~ recode(.x, 'Perdida' = 'Muerta')))%>%
  mutate(DateDied_conservative = case_when((is.na(Monitor1) & is.na(Monitor2) & is.na(Monitor3)) | Monitor1 == 'Muerta' ~ dmy(DatePlanted)+1,
                                           (Monitor1 == 'Nueva' | Monitor1 == 'Viva') & (is.na(Monitor2) | Monitor2 == 'Muerta') ~ Monitor1Date+1,
                                           (Monitor2 == 'Nueva' | Monitor2 == 'Viva') & (is.na(Monitor3) | Monitor3 == 'Muerta') ~ Monitor2Date+1))%>%
  #calculate when a seedling died assuming that it was alive until the day before monitoring recorded it as Dead
  mutate(DateDied_liberal = case_when((is.na(Monitor1) & is.na(Monitor2) & is.na(Monitor3)) | Monitor1 == 'Muerta' ~ Monitor1Date-1,
                                      (Monitor1 == 'Nueva' | Monitor1 == 'Viva') & (is.na(Monitor2) | Monitor2 == 'Muerta') ~ Monitor2Date-1,
                                      (Monitor2 == 'Nueva' | Monitor2 == 'Viva') & (is.na(Monitor3) | Monitor3 == 'Muerta') ~ Monitor3Date-1))%>%
  #calculate DateDied_med assuming that it died halfway between the monitoring date it was Alive and the monitoring date it was Dead
  mutate(DateDied_med = case_when((is.na(Monitor1) & is.na(Monitor2) & is.na(Monitor3)) | Monitor1 == 'Muerta' ~ date(int_end(interval(dmy(DatePlanted), Monitor1Date)/2)),
                                  (Monitor1 == 'Nueva' | Monitor1 == 'Viva') & (is.na(Monitor2) | Monitor2 == 'Muerta') ~ date(int_end(interval(Monitor1Date, Monitor2Date)/2)),
                                  (Monitor2 == 'Nueva' | Monitor2 == 'Viva') & (is.na(Monitor3) | Monitor3 == 'Muerta') ~ date(int_end(interval(Monitor2Date, Monitor3Date)/2))))%>%
  #format date as DayMonthYear 
  mutate(DatePlanted = dmy(DatePlanted),
         DateDied_conservative = case_when(DateDied_conservative <= DatePlanted ~ DatePlanted, .default = DateDied_conservative))%>%
  #If TimeAlive_conservative is negative because DatePlanted occurs after DateDied_conservative, use DatePlanted, Otherwise default to using DateDied_conservative
  #Calculate whether an ind is alive base on the most recent positive observation
  mutate(Outcome = case_when(Monitor1 == 'Muerta' | Monitor2 == 'Muerta' | Monitor3 == 'Muerta' ~ 'Dead',
                             Monitor3 == 'Nueva' | Monitor3 == 'Viva' ~ 'Alive',
                             is.na(Monitor3) ~ 'Presumed Dead'),
         PotentialTimeAlive = (Monitor3Date+1) - DatePlanted, #days since it was first planted
         TimeAlive_conservative = case_when(Outcome == 'Alive' ~ ((Monitor3Date+1) - DatePlanted), #calculate TimeAlive_conservative as difference between DatePlanted and DateDied_conservative
                                            Outcome == 'Dead' ~ (DateDied_conservative - DatePlanted),
                                            Outcome == 'Presumed Dead' ~ (DateDied_conservative - DatePlanted)),
         TimeAlive_liberal = case_when(Outcome == 'Alive' ~ ((Monitor3Date+1) - DatePlanted), #calculate TimeAlive_conservative as difference between DatePlanted and DateDied_conservative
                                       Outcome == 'Dead' ~ (DateDied_liberal - DatePlanted),
                                       Outcome == 'Presumed Dead' ~ (DateDied_liberal - DatePlanted)),
         
         TimeAlive_med = case_when(Outcome == 'Alive' ~ ((Monitor3Date+1) - DatePlanted), #calculate TimeAlive_conservative as difference between DatePlanted and DateDied_conservative
                                   Outcome == 'Dead' ~ (DateDied_med - DatePlanted),
                                   Outcome == 'Presumed Dead' ~ (DateDied_med - DatePlanted)),
         
         RatioTimeAlive_conservative = (as.numeric(TimeAlive_conservative)) / (as.numeric(PotentialTimeAlive)),
         RatioTimeAlive_liberal = (as.numeric(TimeAlive_liberal)) / (as.numeric(PotentialTimeAlive)),
         RatioTimeAlive_med = (as.numeric(TimeAlive_med)) / (as.numeric(PotentialTimeAlive)))

#Decide priority sites to visit in Baja based on number of potentially living individuals at each ranch  
priority_sites <- outplanted_seedlings_clean%>%
  filter(Outcome == 'Alive')%>%
  group_by(PlantedReg, Ranch, N, W)%>%
  summarise(n())



eco_monitoring_25_clean%>%
  ggplot()+
  geom_bar(aes(x=Height_cm))+
  theme_classic()

eco_monitoring_24_clean%>%
  #filter(-is.na(Height_cm))%>%
  ggplot()+
  geom_bar(aes(x=Height_cm))+
  theme_classic()



#combining 2024 and 2025 ecological data
eco_monitoring_all <-bind_rows(eco_monitoring_24, eco_monitoring_25)%>%
  mutate(Ranch=recode(Ranch, 
                      'San Dio' = 'Rancho San Dionisio',
                      'Santo Do' = 'Santo Domingo',
                      'La Palapa' = 'La Rueda (Palapa)',
                      'Parque de Santiago' = 'Parque Ecológico Santiago',
                      'Santa Gertrudis (orchard)' = 'Santa Gertudris (Huerta)',
                      'Santa Gertrudis' = 'Santa Gertudris',
                      'Palo Verdal' = 'Palo Verdad'))%>%
  mutate(Outcome = case_when((Condition == 'great' |
                              Condition =='good' | 
                              Condition =='fair' | 
                              Condition == 'poor') ~ 'Alive',
                              .default = 'Dead'))%>%
#simplifying variables  
  mutate(Condition=recode(Condition,
                      'good' = 'Good',
                      'great' = 'Great',
                      'the best' = 'Great',
                      'fine' = 'Fair',
                      'Fine' = 'Fair',
                      'fair' = 'Fair',
                      'dead' = 'Dead',
                      'poor' = 'Poor'))%>%
  mutate(Condition_num=as.factor(recode(Condition,
                                        'Dead' = '0',
                                        'Poor' = '0.25',
                                        'Fair' = '0.5',
                                        'Good' = '0.75',
                                        'Great' = '1')))%>%
  mutate(Canopy=recode(`Canopy cover`, 'patial shade' = 'partial shade',
                             'mostly  sun' = 'mostly sun',
                             'total sun' = 'full sun'))%>%
  mutate(Canopy_num=recode(Canopy, 
                           'full shade' = '0', 
                           'mostly shade' = '0.25', 'partial sun' = '0.25',
                           'half shade' = '0.5', 'half sun' = '0.5', 
                           'partial shade' = '0.75', 'mostly sun' = '0.75', 
                           'full sun' = '1'))

  
####exploratory analysis after 2025 field work####

#FIGURE: compares conditions at San Dio between 2024 and 2025  
eco_monitoring_all%>%
  filter(Ranch == 'Rancho San Dionisio')%>%
    ggplot() +
    geom_bar(aes(x = Condition_num, fill = Monitor)) +
    facet_grid(~Monitor, labeller = labeller(Monitor = c("Monitor4" = "2024 (n=92)", "Monitor5" = "2025 (n=96)")))+
    scale_x_discrete(name = 'Condition',
                     labels = c("Dead", "Poor", "Fine", "Good", "Great"))+
    ggtitle('San Dio Conditions') +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#FIGURE: compares canopy cover at El Chinal and Rancho San Dio
#El Chinal data collected in 2025, Rancho San Dio collected in 2024
eco_monitoring_all%>%
  filter(Ranch == "El Chinal" | Ranch == "Rancho San Dionisio")%>%
  ggplot()+
  geom_bar(aes(x = Canopy_num, fill = Ranch))+
  facet_grid(~Ranch)+
  scale_x_discrete(name = 'Canopy Cover')+
  ylim(0, 35)+
  theme_classic()

#FIGURE: compares living seedling conditions at the two sites monitored in 2025
eco_monitoring_all%>%
  filter(Monitor == "Monitor5")%>%
  ggplot() +
  geom_bar(aes(x = Condition_num, fill = Ranch)) +
  facet_grid(~Ranch, labeller = labeller(Ranch = c(
    "Rancho San Dionisio" = "San Dio (n=96)",
    "El Chinal" = "El Chinal (n=83)"))) +
  scale_x_discrete(name = 'Condition',
                   labels = c("Dead", "Poor", "Fine", "Good", "Great"))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
