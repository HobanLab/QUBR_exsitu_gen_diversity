####SET UP####
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tibble)
library(lubridate)
library(foreign)
library(MASS)
library(Hmisc)
library(reshape2)
library(brant)
library(GGally)
library(rgl)
library(survival)
library(scales)

#creates a function
"%notin%"<-Negate("%in%")

setwd("C:/Users/DBarry/Desktop/GitHub/QUBR_exsitu_gen_diversity")
#setwd("../")

Monitor1Date <- dmy("13/02/2022")
Monitor2Date <- dmy("20/01/2023")
Monitor3Date <- dmy("13/12/2023")
Monitor4Date <- dmy("23/11/2024")
LastObservedDateM3 <- Monitor3Date + 1
LastObservedDateM4 <- Monitor4Date + 1

#imports databases (2023 & 2024 tabs, 2024 field work)
outplanted_seedlings23 <- read_csv("./data/Datos de Siembra en Ranchos_Actualizado_05_2024.xlsx - Datos_Campaña_Seimbra_2023 04_23_2025.csv")
outplanted_seedlings24 <- read_csv("./data/Datos de Siembra en Ranchos_Actualizado_05_2024.xlsx - Datos_Campaña_Seimbra_2024 04_23_2025.csv")%>%
  mutate('Núm. Etiqueta' = as.character('Núm. Etiqueta')) #interpret MetalTagID as a character, not numeric, because some inds have an A & B
outplanted_seedlings24.field <- read_csv("./data/QUBR Field Datasheets Nov 2024 - filled - OP Seedlings 03_27_25.csv")


####DATA CLEANING####
#combines data from Daniel's 2023 and 2024 tabs
seedlings_combined <- bind_rows(outplanted_seedlings23, outplanted_seedlings24)


#renames columns to simplified English, differentiates seed origin and seedling planted region
seedlings_clean <- seedlings_combined%>%
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
  dplyr::select(-'Monitoreo 1 (__/__/__)')%>% #remove the placeholder column in Daniel's 2024 datasheet
  filter(
    #!str_detect(Ranch, "Festival"), #removes rows for individuals handed out at Festival 2023
         !str_detect(Ranch, "Arroyo:"))%>% #removes individuals from the Arroyo: El Palo Santo for analysis bc we didn't observe them in 2024 (they were fairly new)
  mutate(across(starts_with("Monitor"), ~ recode(.x, 'Perdida' = 'Muerta')))%>%  #reclass Perdida (lost) as Muerta (dead)
#calculate when a seedling died based on when it was last positively observed
  mutate(DateDied = case_when((is.na(Monitor1) & is.na(Monitor2) & is.na(Monitor3)) | Monitor1 == 'Muerta' ~ dmy(DatePlanted),
                              (Monitor1 == 'Nueva' | Monitor1 == 'Viva') & (is.na(Monitor2) | Monitor2 == 'Muerta') ~ Monitor1Date,
                              (Monitor2 == 'Nueva' | Monitor2 == 'Viva') & (is.na(Monitor3) | Monitor3 == 'Muerta') ~ Monitor2Date))%>%
  
                                
#format date as DayMonthYear 
  mutate(DatePlanted = dmy(DatePlanted),
         DateDied = case_when(DateDied <= DatePlanted ~ DatePlanted, .default = DateDied))%>% #If TimeAlive is negative because DatePlanted occurs after DateDied, use DatePlanted, Otherwise default to using DateDied
  
#Calculate whether an ind is alive base on the most recent positive observation
  mutate(Outcome = case_when(Monitor1 == 'Muerta' | Monitor2 == 'Muerta' | Monitor3 == 'Muerta' ~ 'Dead',
                             Monitor3 == 'Nueva' | Monitor3 == 'Viva' ~ 'Alive',
                             is.na(Monitor3) ~ 'Presumed Dead'),
         PotentialTimeAlive = LastObservedDateM3 - DatePlanted, #days since it was first planted
         TimeAlive = case_when(Outcome == 'Alive' ~ (LastObservedDateM3 - DatePlanted), #calculate TimeAlive as difference between DatePlanted and DateDied
                                                   Outcome == 'Dead' ~ (DateDied - DatePlanted),
                                                   Outcome == 'Presumed Dead' ~ (DateDied - DatePlanted)),
         RatioTimeAlive = (as.numeric(TimeAlive)) / (as.numeric(PotentialTimeAlive)))

           
#Decide priority sites to visit in Baja based on number of potentially living individuals at each ranch  
priority_sites <- seedlings_clean%>%
  filter(Outcome == 'Alive')%>%
  group_by(PlantedReg, Ranch, N, W)%>%
  summarise(n())


#Takes data collected in the field in Nov 2024 and recodes some columns that have irregular (shade, height, etc)
outplanted_seedlings_nov24 <- outplanted_seedlings24.field%>%
#interprets dead individuals with a Height of N/A as a Height_cm of 0  
  mutate(Height_cm=recode(Height,'N/A' = '0'))%>%
  
#renames columns to match previous data
  mutate(Ranch=recode(Ranch, 
                      'San Dio' = 'Rancho San Dionisio',
                      'Santo Do' = 'Santo Domingo',
                      'La Palapa' = 'La Rueda (Palapa)',
                      'Parque de Santiago' = 'Parque Ecológico Santiago',
                      'Santa Gertrudis (orchard)' = 'Santa Gertudris (Huerta)',
                      'Santa Gertrudis' = 'Santa Gertudris',
                      'Palo Verdal' = 'Palo Verdad'))%>%
  
  #combines equivalent variables
  mutate(Monitor4=recode(Condition, 
                         'the best' = 'Viva',
                         'great' = 'Viva', 
                         'good' = 'Viva', 
                         'fair' = 'Viva', 
                         'poor' = 'Viva',
                         'dead' = 'Muerta'))%>%
  mutate(Condition=recode(Condition, 'the best' = 'great'))%>%
  mutate(Height=recode(Height, 'above the knee' = 'above knee',
                               'above shoulders' = 'above shoulder',
                               'taller than Daniel' = 'taller than Dana', 
                               '1.5 Daniels' = 'taller than Dana',
                               '2 Daniels' = 'taller than Dana',
                               'mid hip' = 'hip',
                               'low hip' = 'below hip'))%>%
  mutate(Canopy_cover=recode(`Canopy cover`, 'patial shade' = 'partial shade',
                                               'mostly  sun' = 'mostly sun',
                                               'total sun' = 'full sun'))%>%
  
  #assigns numeric values to continuous variables
  mutate(Condition_num=as.factor(recode(Condition, 
                              'dead' = '0',
                              'poor' = '0.25',
                              'fair' = '0.5',
                              'good' = '0.75',
                              'great' = '1')))%>%
  mutate(Canopy_num=recode(Canopy_cover, 
                           'full shade' = '0', 
                           'mostly shade' = '0.25', 'partial sun' = '0.25',
                           'half shade' = '0.5', 'half sun' = '0.5', 
                           'partial shade' = '0.75', 'mostly sun' = '0.75', 
                           'full sun' = '1'))%>%
  mutate(Height_cm=recode(Height,
                           'N/A' = '0',
                           'below ankle' = '0-7.5',
                           'ankle' = '7.5-16.5',
                           'above ankle' = '16.5-29.5',
                           'mid shin' = '29.5-39.5',
                           'below knee' = '39.5-47',
                           'knee' = '47-57.5',
                           'above knee' = '57.5-71.5',
                           'below hip'= '71.5-85',
                           'hip' = '85-96.5',
                           'above hip' = '96.5-109.5',
                           'mid torso' = '109.5-123.5',
                           'below shoulders' = '123.5-136',
                           'shoulder' = '136-147',
                           'above shoulder' = '147-158.5',
                           'Dana height' = '158.5-171.5',
                           'taller than Dana' = '>171.5'
                           ))%>%
  mutate(Height_lower=recode(Height,
                          'N/A' = '0',
                          'below ankle' = '0',
                          'ankle' = '7.5',
                          'above ankle' = '16.5',
                          'mid shin' = '29.5',
                          'below knee' = '39.5',
                          'knee' = '47',
                          'above knee' = '57.5',
                          'below hip'= '71.5',
                          'hip' = '85',
                          'above hip' = '96.5',
                          'mid torso' = '109.5',
                          'below shoulders' = '123.5',
                          'shoulder' = '136',
                          'above shoulder' = '147',
                          'Dana height' = '158.5',
                          'taller than Dana' = '171.5'))%>%
    
    mutate(Height_upper=recode(Height,
                            'N/A' = '0',
                            'below ankle' = '7.5',
                            'ankle' = '16.5',
                            'above ankle' = '29.5',
                            'mid shin' = '39.5',
                            'below knee' = '47',
                            'knee' = '57.5',
                            'above knee' = '71.5',
                            'below hip'= '85',
                            'hip' = '96.5',
                            'above hip' = '109.5',
                            'mid torso' = '123.5',
                            'below shoulders' = '136',
                            'shoulder' = '147',
                            'above shoulder' = '158.5',
                            'Dana height' = '171.5',
                            'taller than Dana' = '343'
                            ))%>%

#converts variables into numeric so they can do math
  mutate(Canopy_num=as.factor(Canopy_num))%>%
  mutate(Height_cm=as.factor(Height_cm))%>%
  mutate(MetalTagID = `Metal tag ID`)%>%
  mutate(MetalTagID = as.character(MetalTagID)) #Some of the tags have an A/B

#Adding relevant data from most recent monitoring (Nov 2024) to the pre-existing data
seedlings_clean_joined <- outplanted_seedlings_nov24%>%
  dplyr::select(Ranch, MetalTagID, Monitor4)%>% #these are the only columns we need to carry over to add the fourth monitoring date and then update survivorship curves
  left_join(seedlings_clean, ., by = 'MetalTagID')%>%

#Adding to previous Outcome in seedlings_clean
  mutate(Outcome = case_when(Monitor4 == 'Nueva' | Monitor4 == 'Viva' ~ 'Alive',
                            (Monitor1 == 'Muerta' | Monitor2 == 'Muerta' | Monitor3 == 'Muerta' | Monitor4 == 'Muerta') ~ 'Dead',
                             is.na(Monitor4) ~ 'Presumed Dead'))%>%
  
#Adding to previous DateDied in seedlings_clean:
  #DateDied is calculated based on last positive observation
  mutate(DateDied = case_when((is.na(Monitor1) & is.na(Monitor2) & is.na(Monitor3) & is.na(Monitor4)) | Monitor1 == 'Muerta' ~ DatePlanted,
                              (Monitor1 == 'Viva' | Monitor1 == 'Nueva') & (Monitor2 == 'Muerta' | is.na(Monitor2)) ~ Monitor1Date,
                              (Monitor2 == 'Viva' | Monitor2 == 'Nueva') & (Monitor3 == 'Muerta' | is.na(Monitor3)) ~ Monitor2Date,
                              (Monitor3 == 'Viva' | Monitor3 == 'Nueva') & (Monitor4 == 'Muerta' | is.na(Monitor4)) ~ Monitor3Date))%>%
  #If TimeAlive is negative because DatePlanted occurs after DateDied, use DatePlanted; otherwise default to DateDied
  mutate(DateDied = case_when(DateDied <= DatePlanted ~ DatePlanted, .default = DateDied))%>%
  mutate(PotentialTimeAlive = LastObservedDateM4 - DatePlanted)%>% #days since it was first planted
  #TimeAlive is calculated based on outcome and time betweeen DatePlanted &  DateDied
  mutate(TimeAlive = case_when(Outcome == 'Alive' ~ (LastObservedDateM4 - DatePlanted),
                               Outcome == 'Dead' ~ (DateDied - DatePlanted),
                               Outcome == 'Presumed Dead' ~ (DateDied - DatePlanted)))%>%
  mutate(RatioTimeAlive = (as.numeric(TimeAlive)) / (as.numeric(PotentialTimeAlive)))%>%
  mutate(LastObservedDateM4 = LastObservedDateM4)


#adds TimeAlive info to our outplanted individuals from Daniel's database
outplanted_seedlings_nov24 <- outplanted_seedlings_nov24%>%
  left_join(., dplyr::select(seedlings_clean_joined, c('MetalTagID', 'TimeAlive')), by = 'MetalTagID')%>%
  rename('Notes' = 'Notes/comments')%>% #below is to add TimeAlive to individuals missing TimeAlive
  mutate(DatePlanted = case_when(
    str_detect(Notes, 'festival') ~ '08/12/2023',
    str_detect(Ranch, 'San Dio') ~ '01/09/2023'))%>%#We don't know the exact date in September they were planted, so we are using Sept 1st
  mutate(TimeAlive = case_when(
    !is.na(DatePlanted) ~ LastObservedDateM4 - dmy(DatePlanted),
    is.na(DatePlanted) ~ TimeAlive))%>%
  mutate(TimeAliveNum = as.numeric(TimeAlive))%>%
  mutate(PotentialTimeAlive = LastObservedDateM4 - dmy(DatePlanted))


####FOR LOOP: SURVIVORSHIP CURVE####
#creating a df with increments of 1 day to represent how old a seedling could be
#not represented in Daniel's dataset, but that we found and can encorporate into survivorship curve
nov24_notindaniel <- outplanted_seedlings_nov24%>%
  filter(MetalTagID %notin% seedlings_clean_joined$MetalTagID)%>%
  filter(!is.na(TimeAlive))%>%
  dplyr::select(c(TimeAlive, PotentialTimeAlive))

input_for_df_age <- seedlings_clean_joined%>%
  dplyr::select(c(TimeAlive, PotentialTimeAlive))%>%
  rbind(nov24_notindaniel)

max_age <- as.numeric(max(input_for_df_age$TimeAlive, na.rm = TRUE))

df_age <- 
  data.frame("Days"=seq(0, max_age, 1), "TotalAlive" = NA) 

#for loop: how many individuals were still alive at any given duration?
#with Monitor4 added
for (i in 1:nrow(df_age)) {
  Day <- df_age$Days[i] #df_age$Days is a vector (one column in this df)
  Num_seedlings_alive <- sum(Day <= input_for_df_age$TimeAlive, na.rm = TRUE)
  #Day is a temporary object that holds the output of the day we are on in the iterative loop
  df_age$TotalAlive[i] <- paste0(Num_seedlings_alive)
  #fill one cell per iteration with the total number of seedlings alive by that day
}
#how many seedlings are alive after 1 year?
num_alive_1yr <- input_for_df_age%>%
  filter((TimeAlive>=365 & PotentialTimeAlive>=365) | (TimeAlive<365 & PotentialTimeAlive==TimeAlive))%>%
  nrow()

#how many seedlings are alive after 2 years?
num_alive_2yr <- input_for_df_age%>%
  filter((TimeAlive>=730 & PotentialTimeAlive>=730) | (TimeAlive<730 & PotentialTimeAlive==TimeAlive))%>%
  nrow()

#perc alive after 1 year
num_alive_1yr/nrow(input_for_df_age)

#perc of inds alive at 1yr that are also alive at 2yr
num_alive_2yr/num_alive_1yr

#plot survivorship curve
df_age_final <- df_age%>%
  mutate(TotalAlive = as.numeric(TotalAlive))%>%
  mutate(PercentAlive = TotalAlive/max(TotalAlive, na.rm = TRUE))%>%
  mutate(data_type = "all")
  
df_age_final %>%
  ggplot(aes(x = Days, y = PercentAlive)) +
  ggtitle("seedlings_clean_joined: all") +
  xlab('Days alive') +
  ylab('# of individuals') +
  ylim(0, 1) +
  geom_step() +
  theme_classic()


#representing y-axis as a ratio of days an individual lived / days it could have been alive for
#bc not all of the seedlings were planted on the same day, some have had the chance to grow longer than others

df_age_ratio <- #creates a df that counts from 0 to 1, and with blank columns for TotalValue & PercValue
  data.frame("Ratio" =seq(0, 1, .01), "TotalValue" = NA, "PercValue" = NA)

for (i in 1:nrow(df_age_ratio)) {#for loop fills in TotalValue column
  Ratio_Value <- df_age_ratio$Ratio[i]
  ratio_hold <- sum(Ratio_Value <= seedlings_clean_joined$RatioTimeAlive, na.rm = TRUE)
  df_age_ratio$TotalValue[i] <- as.numeric(ratio_hold)
}

df_age_ratio_perc <- df_age_ratio%>%
  mutate(TotalValue = as.numeric(TotalValue))%>%
  mutate(PercValue = TotalValue/max(TotalValue, na.rm = TRUE))


#I used this figure for my RaMP presentation
df_age_ratio %>% #curve shown with y = raw values
  ggplot()+
  geom_step(aes(x = Ratio, y = TotalValue)) +
  ylim(0, 2000) +
  xlab('Realized time alive / Potential time alive') +
  ylab('# of individuals') +
  ggtitle("Rate of death over time in outplanted seedlings:") +
  theme_classic()
df_age_ratio_perc %>% #curve shown with y = percentage
  ggplot() +
  geom_step(aes(x = Ratio, y = PercValue)) +
  ylim(0, 1) +
  xlab('Realized time alive / Potential time alive') +
  ylab('% of individuals') +
  ggtitle("seedlings_clean_joined: %") +
  theme_classic()

####CONVERTING FOR LOOP TO FUNCTION####

#Example of a function that prints "a b"
test_function <- function(sequence, fill_in){
  temp <- paste(sequence, fill_in)
  #return(temp)
  }
test_function("a", "b")

#these are the age classes I want to define
seedlings_clean_M1 <- seedlings_clean_joined%>% #planted before M1
  filter(DatePlanted < '2022-02-13')
seedlings_clean_M2 <- seedlings_clean_joined%>% #planted between M1 & M2
  filter(DatePlanted < '2023-01-20', DatePlanted > '2022-02-13')
seedlings_clean_M3 <- seedlings_clean_joined%>% #planted between M2 & M3
  filter(DatePlanted < '2023-12-13', DatePlanted > '2023-01-20')
#there have been seedlings planted between M3 and M4, but they have not been remonitored for M4 yet
seedlings_clean_M4 <- seedlings_clean_joined%>% #planted between M3 and M4
  filter(DatePlanted < '2024-11-23', DatePlanted > '2023-12-13')

#creates a blank loop that can be repeated for each age class(M1, M2, etc)
loop_function <- function(source, CustomSequence, fill_in){
  df <- data.frame("Ratio" = CustomSequence, fill_in = NA, "PercValue" = NA)
  names(df)[2] <- c(fill_in) #overwrites the column title
  for (i in 1:nrow(df)) {
    Ratio_Value <- df$Ratio[i]
    ratio_hold <- sum(Ratio_Value <= source$RatioTimeAlive, na.rm = TRUE)
    df[i,2] <-as.numeric(ratio_hold) #saves value to the i row in the 2nd column
  }
  return(df) #this is the part of the for loop we want back as results
}

#these are the age classes I want to repeat the for loop on
#also converts TotalValue into PercValue and fills an additional column
M1_age <- loop_function(seedlings_clean_M1, seq(0, 1, .01), "TotalValue")%>%
  mutate(TotalValue = as.numeric(TotalValue))%>%
  mutate(PercValue = TotalValue/max(TotalValue, na.rm = TRUE))
M2_age <- loop_function(seedlings_clean_M2, seq(0, 1, .01), "TotalValue")%>%
  mutate(TotalValue = as.numeric(TotalValue))%>%
  mutate(PercValue = TotalValue/max(TotalValue, na.rm = TRUE))
M3_age <- loop_function(seedlings_clean_M3, seq(0, 1, .01), "TotalValue")%>%
  mutate(TotalValue = as.numeric(TotalValue))%>%
  mutate(PercValue = TotalValue/max(TotalValue, na.rm = TRUE))
M4_age <- loop_function(seedlings_clean_M4, seq(0, 1, .01), "TotalValue")%>%
  mutate(TotalValue = as.numeric(TotalValue))%>%
  mutate(PercValue = TotalValue/max(TotalValue, na.rm = TRUE))

####VISUALIZING OUTLIERS####

#Scatter plot: Height & Condition
outplanted_seedlings_nov24%>%
  filter(!str_detect(Height_cm, "N/A"))%>%
  ggplot() +
  ggtitle("Height & Condition") +
  geom_point(aes(x = Height_cm, y = Condition_num)) +
#  xlim(0, 350) +
#  ylim(0, 1) +
  xlab("Height (cm)") +
  ylab("Condition") +
  theme_classic()

#Box plot: Height & Condition
outplanted_seedlings_nov24%>%
  mutate(Condition_num = as.factor(Condition_num))%>%
  filter(!str_detect(Height_cm, "N/A"))%>%
  ggplot() +
  ggtitle("Height & Condition") +
  geom_boxplot(aes(x = Condition_num, y = Height_cm), outlier.shape = NA) +
  geom_jitter(aes(x = Condition_num, y = Height_cm)) +
  xlab("Condition") +
  ylab("Height (cm)") +
  theme_classic()
  
#Box plot: Height & Ranch
outplanted_seedlings_nov24%>%
  mutate(Ranch=recode(Ranch, 'La Rueda (Palapa)' = 'La Rueda'))%>% #combines the two Ranches that are both at La Rueda
  filter(!str_detect(Height_cm, "N/A"))%>%
  ggplot() +
  ggtitle("Height & Ranch") +
  geom_boxplot(aes(x = Ranch, y = Height_cm, fill = Region)) +
  geom_jitter(aes(x = Ranch, y = Height_cm)) +
  facet_wrap(~Region, dir = "h") +
  #ylim(0, 350) +
  xlab("Ranch") +
  ylab("Height (cm)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))


####CHI SQUARED TEST####
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

####POST HOC TEST####
chisq.posthoc.test(OriginRegion_outcome)
chisq.posthoc.test(PlantedRegion_outcome)
chisq.posthoc.test(watered_data)


#export .csv to make Google Map of sites
#write.csv(priority_sites, "priority_sites.csv")

####INTERVAL REGRESSION####
# bivariate plots
outplanted_seedlings_nov24%>%
  mutate(Height_lower=as.numeric(Height_lower))%>%
  mutate(Height_upper=as.numeric(Height_upper))%>%
  dplyr::select(c(Height_upper, Height_lower, Condition_num, Canopy_num, Region))%>%
  ggpairs(., lower = list(combo = "box"), 
             upper = list(combo = "blank"))

outplanted_seedlings_nov24_aov <- outplanted_seedlings_nov24%>%
  filter(!is.na(TimeAliveNum))%>%
  mutate(Ranch=recode(Ranch, 'La Rueda (Palapa)' = 'La Rueda'))%>%
  mutate(Height_lower=as.numeric(Height_lower))%>%
  mutate(Height_upper=as.numeric(Height_upper))%>%
  
  mutate(Height_lower_standardized=Height_lower/TimeAliveNum)%>%
  mutate(Height_upper_standardized=Height_upper/TimeAliveNum)%>%
  
  mutate(Canopy_num = as.factor(Canopy_num))%>%
  mutate(Condition_num = as.factor(Condition_num))%>%
  
  mutate(Ranch = as.factor(Ranch))%>%
  mutate(Region = as.factor(Region))%>%
  
  dplyr::select(c(Height_upper_standardized, Height_lower_standardized, Canopy_num, Condition_num, Ranch, Region))

ggpairs(outplanted_seedlings_nov24_aov, lower = list(combo = "box"), upper = list(combo = "blank"))

summary(aov(data=outplanted_seedlings_nov24_aov, Height_upper_standardized ~ Canopy_num + Ranch + Region))
summary(aov(data=outplanted_seedlings_nov24_aov, Height_lower_standardized ~ Canopy_num + Ranch + Region))

####ORDINAL REGRESSION####
#data exploration pre analysis
outplanted_seedlings_nov24%>%
  filter(!is.na(Canopy_num))%>%
  mutate(Ranch=recode(Ranch, 'La Rueda (Palapa)' = 'La Rueda'))%>%
ggplot(., aes(x = Condition_num, y = as.numeric(Canopy_num))) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(~Ranch, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#start of ordinal regression
# fit ordered logit model and store results 'r'
r <- polr(Condition_num ~ Canopy_num + Ranch, data = outplanted_seedlings_nov24, Hess=TRUE)
#When we use both Ranch & Region, we get this warning: design appears to be rank-deficient, so dropping some coefs
#Presumably due to high colinearity between Region & Ranch

## view a summary of the model
summary(r)

## store table
(ctable <- coef(summary(r)))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

(ci <- confint(r)) # default method gives profiled CIs

confint.default(r) # CIs assuming normality

## odds ratios: how much more likely is this thing than anything else?
exp(coef(r))

## OR and CI
exp(cbind(OR = coef(r), ci))

#checking assumption that relationship between each pair of outcome groups (condition bins) is the same
brant(r)
#we tried using the graphical test from https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/ and got Infinity in the initial table
#Instead, we used the brant test above, but note that we also get a warning that the test results might be invalid

sf <- function(y) {
  c('Y>=0' = qlogis(mean(y >= 0)),
    'Y>=0.25' = qlogis(mean(y >= 0.25)),
    'Y>=0.5' = qlogis(mean(y >= 0.5)),
    'Y>=0.75' = qlogis(mean(y >= 0.75)),
    'Y>=1' = qlogis(mean(y >= 1)))
}
(s <- with(outplanted_seedlings_nov24, summary(as.numeric(Condition_num) ~ Region + Canopy_num, fun=sf)))


####CPC POSTER FIGURES####

#SURVIVORSHIP CURVE (marked at 1 yr and 2 yrs)
df_age_final%>%
  ggplot() +
  geom_step(aes(x = Days, y = TotalAlive)) +
  ylab('# of live individuals') +
  scale_x_continuous(name = 'time since transplanting', 
                     breaks = c(0, 182.5, 365, 547.5, 730, 912.5), #adds tick marks at 6 month intervals
                     labels = c('0', '0.5yr', '1yr', '1.5yrs','2yrs', '2.5yrs')) +
  geom_vline(xintercept = 365, linetype="dashed", color='red') +  #marks 1 year
  geom_vline(xintercept = (365*2), linetype="dashed", color='red') + #marks 2 years
  ggtitle("# of seedlings alive over time") +
  theme_classic()

#How many seedlings that are still alive are less than 1 year old?
seedlings_clean_joined%>%
  filter(Outcome == 'Alive')%>%
  filter(TimeAlive < (365))%>%
  nrow()
#How many seedlings that are still alive are less than 2 years old?
seedlings_clean_joined%>%
  filter(Outcome == 'Alive')%>%
  filter(TimeAlive < (365*2))%>%
  nrow()

show_ages <- seedlings_clean_joined%>%
  filter(Outcome == 'Alive')
#youngest living seedlings are 653 days, oldest are 1005
#there are 9 inds at 653



#RANCH VS CONDITION
#calculating sample size for each Ranch
Ranch_summary_condition <- outplanted_seedlings_nov24_aov %>%
  group_by(Ranch) %>%
  summarise(samp = n())
Ranch_summary_condition

outplanted_seedlings_nov24_aov%>%
  ggplot() +
  ggtitle("Ranch vs Condition") +
  #geom_bar(aes(x = Condition_num, y=..prop.., fill = Region), stat = 'prop') +
  geom_bar(aes(x = Condition_num, fill = Region)) +
  #facet_wrap(~Ranch) + #without sample size
  facet_wrap(~Ranch,
             labeller = labeller(Ranch = c(
    "El Ancón" = "El Ancón \n (n=9)",
    "La Rueda" = "La Rueda \n (n=4)",
    "La Semilla" = "La Semilla \n (n=3)",
    "Palo Verdad" = "Palo Verdad \n (n=6)",
    "Parque Ecológico Santiago" = "Parque Ecológico Santiago \n (n=17)", 
    "Rancho San Dionisio" = "Rancho San Dionisio \n (n=92)",
    "Santa Gertudris" = "Santa Gertudris \n (n=5)",
    "Santa Gertudris (Huerta)" = "Santa Gertudris (Huerta) \n (n=12)", 
    "Santo Domingo" = "Santo Domingo \n (n=2)"))) + #with sample size
  scale_fill_manual(values = c("N" = "#558b2f", 
                               "E" = "#e65100", 
                               "W" = "#0288d1")) + #uses standard colors for regions
  xlab("Condition") +
  ylab("# of individuals") +
  #ylab("proportion of individuals") +
  scale_x_discrete(labels = c("0" = "Dead", #replaces numeric labels with condition classes
                              "0.25" = "Poor", 
                              "0.5" = "Fine", 
                              "0.75" = "Good", 
                              "1" = "Great")) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) #adjusts x axis labels for readability


#CONDITION FREQ, FACET BY CANOPY
#calculates sample size for each Canopy class
Canopy_summary <- outplanted_seedlings_nov24_aov %>% 
  group_by(Canopy_num) %>% 
  summarise(samp = n())
#also calculate sample size for each Condition class but I don't think that we need this
Condition_summary <- outplanted_seedlings_nov24_aov %>%
  group_by(Condition_num) %>%
  summarise(samp = n())
#displays samples sizes
Canopy_summary
Condition_summary

outplanted_seedlings_nov24_aov%>%
  filter(!is.na(Canopy_num))%>% #removes any individuals missing Canopy data
  mutate(Canopy_num = as.factor(as.character(Canopy_num)))%>%
  ggplot() +
  ggtitle('Condition of seedlings by canopy cover') +
  xlab('Seedling Condition') +
  ylab('Proportion of individuals \n within canopy class') +
  geom_bar(aes(x=Condition_num, y=..prop.., fill=Condition_num), stat = 'prop') + #stat = 'prop' creates the proportion of individuals rather than the # of individuals
  scale_fill_manual(values=c("#dddddd", 
                             "#aaaaaa", 
                             "#777777", 
                             "#555555", 
                             "#333333"), 
                    labels=c('dead (n=6)', 
                             'poor (n=38)', 
                             'fine (n=50)', 
                             'good (n=39)', 
                             'great (n=17)'),
                    (name = "Condition"))+ #adds shading to Condition classes and creates legend
  facet_grid(~Canopy_num, labeller = labeller(Canopy_num = c(
    "0" = "full shade \n (n=9)", 
    "0.25" = "partial sun \n (n=34)", 
    "0.5" = "half sun \n (n=36)", 
    "0.75" = "mostly sun \n (n=46)", 
    "1" = "full sun \n (n=22)"))) + #labels the facets with the names of Canopy classes and their sample sizes
  scale_x_discrete(labels = c("0" = "Dead", 
                              "0.25" = "Poor", 
                              "0.5" = "Fine", 
                              "0.75" = "Good", 
                              "1" = "Great")) + #replaces the numeric labels on x axis with Condition class
theme_classic() +
theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

####EXPLORATORY FIGURES####
#Height (upper) vs Height (lower)
outplanted_seedlings_nov24_aov%>%
  ggplot() +
  ggtitle("Height (upper) vs Height (lower)") +
  geom_point(aes(x = Height_upper_standardized, y = Height_lower_standardized)) +
  xlim(0,1) +
  ylim(0,1) +
  theme_classic()

#Height (lower) vs Height (upper)
outplanted_seedlings_nov24_aov%>%
  ggplot() +
  ggtitle("Height (lower) vs Height (upper)") +
  geom_point(aes(x = Height_lower_standardized, y = Height_upper_standardized)) +
  xlim(0,1) +
  ylim(0,1) +
  theme_classic()

outplanted_seedlings_nov24_aov%>%
  ggplot() +
  geom_histogram(aes(x = Height_upper_standardized))+
  theme_classic()

#Canopy vs Height (upper)
outplanted_seedlings_nov24_aov%>%
  filter(!is.na(Canopy_num))%>%
  mutate(Canopy_num = as.factor(as.character(Canopy_num)))%>%
  ggplot() +
  ggtitle("Canopy vs Height (upper)") +
  geom_boxplot(aes(x = Canopy_num, y = Height_upper_standardized)) +
  geom_jitter(aes(x = Canopy_num, y = Height_upper_standardized)) +
  theme_classic()

#Canopy vs Height (lower)
outplanted_seedlings_nov24_aov%>%
  filter(!is.na(Canopy_num))%>%
  mutate(Canopy_num = as.factor(as.character(Canopy_num)))%>%
  ggplot() +
  ggtitle("Canopy vs Height (lower)") +
  geom_boxplot(aes(x = Canopy_num, y = Height_lower_standardized)) +
  geom_jitter(aes(x = Canopy_num, y = Height_lower_standardized)) +
  theme_classic()

#Ranch vs Height (upper)
outplanted_seedlings_nov24_aov%>%
  ggplot() +
  ggtitle("Ranch vs Height (upper)") +
  ylim(0,1) +
  geom_boxplot(aes(x = Ranch, y = Height_upper_standardized)) +
  geom_jitter(aes(x = Ranch, y = Height_upper_standardized)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Ranch vs Height (lower)
outplanted_seedlings_nov24_aov%>%
  ggplot() +
  ggtitle("Ranch vs Height (lower)") +
  ylim(0,1) +
  geom_boxplot(aes(x = Ranch, y = Height_lower_standardized)) +
  geom_jitter(aes(x = Ranch, y = Height_lower_standardized)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Condition vs Height (upper)
outplanted_seedlings_nov24_aov%>%
  ggplot() +
  ggtitle("Condition vs Height (upper)") +
  geom_boxplot(aes(x = Condition_num, y = Height_upper_standardized)) +
  geom_jitter(aes(x = Condition_num, y = Height_upper_standardized)) +
  theme_classic()

#Condition vs Height (lower)
outplanted_seedlings_nov24_aov%>%
  ggplot() +
  ggtitle("Condition vs Height (lower)") +
  geom_boxplot(aes(x = Condition_num, y = Height_lower_standardized)) +
  geom_jitter(aes(x = Condition_num, y = Height_lower_standardized)) +
  theme_classic()

#Condition vs Canopy
outplanted_seedlings_nov24_aov%>%
  filter(!is.na(Canopy_num))%>%
  mutate(Canopy_num = as.factor(as.character(Canopy_num)))%>%
  ggplot() +
  ggtitle("Condition vs Canopy") +
  geom_boxplot(aes(x = Condition_num, y = Canopy_num)) +
  theme_classic()

#Canopy vs Ranch
outplanted_seedlings_nov24_aov%>%
  filter(!is.na(Canopy_num))%>%
  mutate(Canopy_num = as.factor(as.character(Canopy_num)))%>%
  ggplot() +
  ggtitle("Canopy vs Ranch") +
  geom_boxplot(aes(x = Ranch, y = Canopy_num)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1, vjust = 1))

####OTHER GRAPHS####
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
####SURVIVORSHIP BY AGE CLASS####
#displays the survivorship curve for each age class (M1, M2, M3 & M4)
#y = raw numbers
M1_age %>% #planted between 06/22/21 and 02/13/22: 236 days
  ggplot() +
  ggtitle("M1") +
  geom_step(aes(x = Ratio, y = TotalValue)) +
  xlim(0, 1) +
  ylim(0, 510) +
  geom_vline(xintercept = 100/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #100 days
  geom_text(label="100 days", x=(100/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=500) +
  geom_vline(xintercept = 200/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #200 days
  geom_text(label="200 days", x=(200/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=450) +
  geom_vline(xintercept = 300/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #300 days
  geom_text(label="300 days", x=(300/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=500) +
  geom_vline(xintercept = 400/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #400 days
  geom_text(label="400 days", x=(400/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=450) +
  geom_vline(xintercept = 500/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #500 days
  geom_text(label="500 days", x=(500/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=500) +
  geom_vline(xintercept = 600/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #600 days
  geom_text(label="600 days", x=(600/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=450) +
  geom_vline(xintercept = 700/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #700 days
  geom_text(label="700 days", x=(700/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=500) +
  geom_vline(xintercept = 800/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #800 days
  geom_text(label="800 days", x=(800/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=450) +
  geom_vline(xintercept = 900/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #900 days
  geom_text(label="900 days", x=(900/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=500) +
  geom_vline(xintercept = 1000/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #1000 days
  geom_text(label="1000 days", x=(1000/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=450) +
  geom_vline(xintercept = 1100/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #1100 days
  geom_text(label="1100 days", x=(1100/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=500) +
  geom_vline(xintercept = 1200/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #1200 days
  geom_text(label="1200 days", x=(1200/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=450) +
  theme_classic()

#y = percent
M1_age %>%
  ggplot() +
  ggtitle("M1") +
  geom_step(aes(x = Ratio, y = PercValue)) +
  ylim(0, 1) +
  geom_vline(xintercept = 100/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #100 days
  geom_text(label="100 days", x=(100/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=1) +
  geom_vline(xintercept = 200/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #200 days
  geom_text(label="200 days", x=(200/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=0.8) +
  geom_vline(xintercept = 300/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #300 days
  geom_text(label="300 days", x=(300/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=1) +
  geom_vline(xintercept = 400/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #400 days
  geom_text(label="400 days", x=(400/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=0.8) +
  geom_vline(xintercept = 500/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #500 days
  geom_text(label="500 days", x=(500/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=1) +
  geom_vline(xintercept = 600/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #600 days
  geom_text(label="600 days", x=(600/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=0.8) +
  geom_vline(xintercept = 700/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #700 days
  geom_text(label="700 days", x=(700/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=1) +
  geom_vline(xintercept = 800/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #800 days
  geom_text(label="800 days", x=(800/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=0.8) +
  geom_vline(xintercept = 900/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #900 days
  geom_text(label="900 days", x=(900/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=1) +
  geom_vline(xintercept = 1000/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #1000 days
  geom_text(label="1000 days", x=(1000/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=0.8) +
  geom_vline(xintercept = 1100/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #1100 days
  geom_text(label="1100 days", x=(1100/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=1) +
  geom_vline(xintercept = 1200/as.numeric(max(seedlings_clean_M1$TimeAlive)), linetype="dashed") + #1200 days
  geom_text(label="1200 days", x=(1200/as.numeric(max(seedlings_clean_M1$TimeAlive))), y=0.8) +
  theme_classic()

####TROUBLESHOOTING####
#find the MetalTagIDs that were recorded twice
seedlings_clean_joined%>%
  group_by(MetalTagID)%>%
  summarize(n=n())%>%
  filter(n>1)
seedlings_clean%>%
  group_by(MetalTagID)%>%
  summarize(n=n())%>%
  filter(n>1)
#shows all of the information on the two metal tags that have duplicates
temp <- seedlings_clean_joined%>%
  filter(MetalTagID %in% c('68', '318'))


#identifies MetalTagIDs from seedlings_clean_joined that are not in seedlings_clean
#seedlings_clean_joined%>%
#  filter(!MetalTagID %in% seedlings_clean$MetalTagID)

####WATERED/UNWATERED####
seedlings_clean_watered <- seedlings_clean%>%
  filter(Watered == "Si")

df_age_watered <-
  data.frame("Days"=seq(0, 1220, 1), "TotalAlive" = NA)

for (i in 1:nrow(df_age)) {
  Day <- df_age_watered$Days[i]
  Num_seedlings_alive <- sum(Day <= seedlings_clean_watered$TimeAlive, na.rm = TRUE)
  df_age_watered$TotalAlive[i] <- paste0(Num_seedlings_alive)
}
df_age_watered_final <- df_age_watered%>%
  mutate(TotalAlive = as.numeric(TotalAlive))%>%
  mutate(PercentAlive = TotalAlive/max(TotalAlive, na.rm = TRUE))%>%
  mutate(data_type = "watered")

df_age_watered_final %>%
  ggplot(aes(x = Days, y = PercentAlive)) +
  ggtitle("Watered") +
  geom_step() +
  theme_classic()


#UNWATERED
seedlings_clean_unwatered <- seedlings_clean%>%
  filter(Watered == 'No')

df_age_unwatered <-
  data.frame("Days"=seq(0, 1220, 1), "TotalAlive" = NA)

for (i in 1:nrow(df_age)) {
  Day <- df_age_unwatered$Days[i]
  Num_seedlings_alive <- sum(Day <= seedlings_clean_unwatered$TimeAlive, na.rm = TRUE)
  df_age_unwatered$TotalAlive[i] <- paste0(Num_seedlings_alive)
}

df_age_unwatered_final <- df_age_unwatered%>%
  mutate(TotalAlive = as.numeric(TotalAlive))%>%
  mutate(PercentAlive = TotalAlive/max(TotalAlive, na.rm = TRUE)) %>%
  mutate(data_type = "unwatered")

df_age_unwatered_final %>%
  ggplot(aes(x = Days, y = PercentAlive)) +
  ggtitle("Unwatered") +
  geom_step() +
  theme_classic()

#combining layers for all individuals, watered, and unwatered onto one axis
df_age_for_plotting <- rbind(df_age_final, df_age_watered_final, df_age_unwatered_final)

#displaying as total values of each data_type (watered/unwatered/all)
df_age_for_plotting %>%
  ggplot(aes(x = Days, y = TotalAlive, color = data_type)) +
  geom_step() +
  theme_classic()

#displaying as percentages of each data_type (watered/unwatered/all)
df_age_for_plotting %>%
  ggplot(aes(x = Days, y = PercentAlive, color = data_type)) +
  geom_step() +
  theme_classic()