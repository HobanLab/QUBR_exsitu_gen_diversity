####SET UP####
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tibble)
library(lubridate)
library(chisq.posthoc.test)
setwd("C:/Users/DBarry/Desktop/GitHub/QUBR_exsitu_gen_diversity")

Monitor1Date <- dmy("13/02/2022")
Monitor2Date <- dmy("20/01/2023")
Monitor3Date <- dmy("13/12/2023")
Monitor4Date <- dmy("23/11/2024")
LastObservedDateM3 <- Monitor3Date + 1
LastObservedDateM4 <- Monitor4Date + 1


#imports databases (2023 & 2024 tabs, 2024 field work)
outplanted_seedlings23 <- read_csv("./data/Datos de Siembra en Ranchos_Actualizado_05_2024.xlsx - Datos_Campaña_Seimbra_2023 03_07_25.csv")
outplanted_seedlings24 <- read_csv("./data/Datos de Siembra en Ranchos_Actualizado_05_2024.xlsx - Datos_Campaña_Seimbra_2024 03_07_25.csv")%>%
  mutate('Núm. Etiqueta' = as.character('Núm. Etiqueta')) #interpret MetalTagID as a character, not numeric, because some inds have an A & B
outplanted_seedlings24.field <- read_csv("./data/QUBR Field Datasheets Nov 2024 - filled - OP Seedlings 03_27_25.csv")


####DATA CLEANING####
#combines data from Daniel's 2023 and 2024 tabs
seedlings_combined <- bind_rows(outplanted_seedlings23, outplanted_seedlings24)

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
  select(-'Monitoreo 1 (__/__/__)')%>% #remove the placeholder column in Daniel's 2024 datasheet for the monitoring we did in Nov 2024
  rename(OriginLabelAsh = 'Procedencia Etiqueta Ash Abril 2024')%>%
  rename(`Metal tag ID` = 'Núm. Etiqueta')%>%
  mutate(Monitor1=recode(Monitor1, 'Perdida' = 'Muerta'))%>% #reclass Perdida (lost) as Muerta (dead)
  add_column(DateDied = NA)%>%
  filter(!str_detect(Ranch, "Arroyo:"))%>% #removes individuals from the Arroyo: El Palo Santo for analysis bc we didn't observe them in 2024 (they were fairly new)

#calculate when a seedling died based on when it was last positively observed
  mutate(DateDied = case_when((is.na(Monitor1) & is.na(Monitor2) & is.na(Monitor3)) | Monitor1 == 'Muerta' ~ dmy(DatePlanted),
                              (Monitor1 == 'Nueva' | Monitor1 == 'Viva') & (is.na(Monitor2) | Monitor2 == 'Muerta') ~ Monitor1Date,
                              (Monitor2 == 'Nueva' | Monitor2 == 'Viva') & (is.na(Monitor3) | Monitor3 == 'Muerta') ~ Monitor2Date))%>%
  
                                
#format date as DayMonthYear
  add_column(Outcome = NA)%>%
  add_column(TimeAlive = NA)%>%
  add_column(RatioTimeAlive = NA)%>%
  add_column(PotentialTimeAlive = NA)%>%
  mutate(DatePlanted = dmy(DatePlanted))%>%
  
#If TimeAlive is negative because DatePlanted occurs after DateDied, use DatePlanted
#Otherwise default to using DateDied
  mutate(DateDied = case_when(DateDied <= DatePlanted ~ DatePlanted, .default = DateDied))%>%
  mutate(RatioTimeAlive = dmy(RatioTimeAlive))%>%
  mutate(PotentialTimeAlive = dmy(PotentialTimeAlive))%>%
  
#Calculate whether an ind is alive base on the most recent positive observation
  mutate(Outcome = case_when(Monitor1 == 'Muerta' | Monitor2 == 'Muerta' | Monitor3 == 'Muerta' ~ 'Dead',
                             Monitor3 == 'Nueva' | Monitor3 == 'Viva' ~ 'Alive',
                             is.na(Monitor1) | is.na(Monitor2) | is.na(Monitor3) ~ 'Presumed Dead'
                             ))%>%
  
#calculate TimeAlive as difference between DatePlanted and DateDied
  mutate(PotentialTimeAlive = LastObservedDateM3 - DatePlanted)%>% #days since it was first planted
  mutate(TimeAlive = case_when(Outcome == 'Alive' ~ (LastObservedDateM3 - DatePlanted),
                               Outcome == 'Dead' ~ (DateDied - DatePlanted),
                               Outcome == 'Presumed Dead' ~ (DateDied - DatePlanted)))%>%
  mutate(RatioTimeAlive = (as.numeric(TimeAlive)) / (as.numeric(PotentialTimeAlive)))%>%
#removes rows for individuals handed out at Festival 2023
  filter(!str_detect(Ranch, "Festival"))%>%
  mutate(LastObservedDateM3 = LastObservedDateM3)

#Decide priority sites to visit in Baja based on number of potentially living individuals at each ranch  
priority_sites <- seedlings_clean%>%
  filter(Outcome == 'Alive')%>%
  group_by(PlantedReg, Ranch, N, W)%>%
  summarise(n())

#adds new columns
outplanted_seedlings_nov24 <- outplanted_seedlings24.field%>%
  add_column(Monitor4 = NA)%>%
  add_column(Canopy_num = NA)%>%
  add_column(Condition_num = NA)%>%
  add_column(Height_cm = NA)%>%
  
#interprets dead individuals with a Height of N/A as a Height_cm of 0  
  mutate(Height_cm=recode(Height,'N/A' = '0'))%>%
  
#renames columns to match previous data
  mutate(Ranch=recode(Ranch, 'San Dio' = 'Rancho San Dionisio'))%>%
  mutate(Ranch=recode(Ranch, 'Santo Do' = 'Santo Domingo'))%>%
  mutate(Ranch=recode(Ranch, 'La Palapa' = 'La Rueda (Palapa)'))%>%
  mutate(Ranch=recode(Ranch, 'Parque de Santiago' = 'Parque Ecológico Santiago'))%>%
  mutate(Ranch=recode(Ranch, 'Santa Gertrudis (orchard)' = 'Santa Gertudris (Huerta)'))%>%
  mutate(Ranch=recode(Ranch, 'Santa Gertrudis' = 'Santa Gertudris'))%>%
  mutate(Ranch=recode(Ranch, 'Palo Verdal' = 'Palo Verdad'))%>%
  
  #combines equivalent variables
  mutate(Monitor4=recode(Condition, 'the best' = 'Viva','great' = 'Viva', 'good' = 'Viva', 'fair' = 'Viva', 'poor' = 'Viva', 
                                    'dead' = 'Muerta'))%>%
  mutate(Condition=recode(Condition, 'the best' = 'great'))%>%
  mutate(Height=recode(Height, 'above the knee' = 'above knee',
                               'above shoulders' = 'above shoulder',
                               'taller than Daniel' = 'taller than Dana',
                               'mid hip' = 'hip',
                               'low hip' = 'below hip'))%>%
  mutate(`Canopy cover`=recode(`Canopy cover`, 'patial shade' = 'partial shade',
                                               'mostly  sun' = 'mostly sun',
                                               'total sun' = 'full sun'))%>%
  
  #assigns numeric values to continuous variables
  mutate(Condition_num=recode(Condition, 
                              'dead' = '0',
                              'poor' = '0.25',
                              'fair' = '0.5',
                              'good' = '0.75',
                              'great' = '1'))%>%
  mutate(Canopy_num=recode(`Canopy cover`, 
                           'full shade' = '0', 
                           'mostly shade' = '0.25', 'partial sun' = '0.25',
                           'half shade' = '0.5', 'half sun' = '0.5', 
                           'partial shade' = '0.75', 'mostly sun' = '0.75', 
                           'full sun' = '1'))%>%
  mutate(Height_cm=recode(Height,
                           'N/A' = '0',
                           'below ankle' = '5',
                           'ankle' = '10',
                           'above ankle' = '23',
                           'mid shin' = '36',
                           'below knee' = '43',
                           'knee' = '51',
                           'above knee' = '64',
                           'below hip'= '79',
                           'hip' = '91',
                           'above hip' = '102',
                           'mid torso' = '117',
                           'below shoulders' = '130',
                           'shoulder' = '142',
                           'above shoulder' = '152',
                           'Dana height' = '165',
                           'taller than Dana' = '191',
                           '1.5 Daniels' = '258',
                           '2 Daniels' = '344'))%>%
  
  
#converts variables into numeric so they can do math
  mutate(Date = mdy(Date))%>%
  mutate(Condition_num=as.numeric(Condition_num))%>%
  mutate(Canopy_num=as.numeric(Canopy_num))%>%
  mutate(Height_cm=as.numeric(Height_cm))%>%
  mutate(`Metal tag ID` = as.character(`Metal tag ID`)) #Some of the tags have an A/B
  

#Adding relevant data from most recent monitoring (Nov 2024) to the pre-existing data
seedlings_clean_joined <- outplanted_seedlings_nov24%>%
  select(Ranch, `Metal tag ID`, Monitor4)%>% #these are the only parts we care about
  left_join(seedlings_clean, ., by = 'Metal tag ID')%>%
#Check that the ranch names match in both source databases
  #I can remove this section now
  #mutate(QualityControl = case_when(Ranch.x == Ranch.y~'y', 
  #                                  Ranch.x != Ranch.y~'n'))%>%
#Adding to previous Outcome in seedlings_clean: 
  mutate(Outcome = case_when((Monitor1 == 'Muerta' | Monitor2 == 'Muerta' | Monitor3 == 'Muerta' | Monitor4 == 'Muerta') ~ 'Dead',
                             Monitor4 == 'Nueva' | Monitor4 == 'Viva' ~ 'Alive',
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


seedlings_clean_joined%>%
  filter(!`Metal tag ID` %in% seedlings_clean$`Metal tag ID`)

#find the Metal Tag IDs that were recorded twice
seedlings_clean_joined%>%
  group_by(`Metal tag ID`)%>%
  summarize(n=n())%>%
  filter(n>1)
seedlings_clean%>%
  group_by(`Metal tag ID`)%>%
  summarize(n=n())%>%
  filter(n>1)
#shows all of the information on the two metal tags that have duplicates
temp <- seedlings_clean_joined%>%
  filter(`Metal tag ID` %in% c('68', '318'))

####FOR LOOP: SURVIVORSHIP CURVE####
#creating a df with increments of 1 day to represent how old a seedling could be
max_age <- as.numeric(max(seedlings_clean_joined$TimeAlive, na.rm = TRUE))

df_age <- 
  data.frame("Days"=seq(0, max_age, 1), "TotalAlive" = NA) 

#for loop: how many individuals were still alive at any given duration?
#with Monitor4 added
for (i in 1:nrow(df_age)) {
  Day <- df_age$Days[i] #df_age$Days is a vector (one column in this df)
  Num_seedlings_alive <- sum(Day <= seedlings_clean_joined$TimeAlive, na.rm = TRUE)
  #Day is a temporary object that holds the output of the day we are on in the iterative loop
  df_age$TotalAlive[i] <- paste0(Num_seedlings_alive)
  #fill one cell per iteration with the total number of seedlings alive by that day
}
#how many seedlings are alive after 1 year?
num_alive_1yr <- seedlings_clean_joined%>%
  filter((TimeAlive>=365 & PotentialTimeAlive>=365) | (TimeAlive<365 & RatioTimeAlive==1))%>%
  nrow()

#how many seedlings are alive after 2 years?
num_alive_2yr <- seedlings_clean_joined%>%
  filter((TimeAlive>=730 & PotentialTimeAlive>=730) | (TimeAlive<730 & RatioTimeAlive==1))%>%
  nrow()

#perc alive after 1 year
num_alive_1yr/nrow(seedlings_clean_joined)

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
  ggtitle("seedlings_clean_joined: ratio") +
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

####VISUALIZING OUTLIERS

#Scatter plot: Height & Condition
outplanted_seedlings_nov24%>%
  filter(!str_detect(`Height_cm`, "N/A"))%>%
  ggplot() +
  ggtitle("Height & Condition") +
  geom_point(aes(x = Height_cm, y = Condition_num)) +
  xlim(0, 350) +
  ylim(0, 1) +
  xlab("Height (cm)") +
  ylab("Condition") +
  theme_classic()

#Box plot: Height & Condition
outplanted_seedlings_nov24%>%
  mutate(`Condition_num` = as.factor(`Condition_num`))%>%
  filter(!str_detect(`Height_cm`, "N/A"))%>%
  ggplot() +
  ggtitle("Height & Condition") +
  geom_boxplot(aes(x = Condition_num, y = Height_cm)) +
  geom_jitter(aes(x = Condition_num, y = Height_cm)) +
  xlab("Condition") +
  ylab("Height (cm)") +
  theme_classic()
  

#Box plot: Height & Ranch
outplanted_seedlings_nov24%>%
  mutate(Ranch=recode(Ranch, 'La Rueda (Palapa)' = 'La Rueda'))%>% #combines the two Ranches that are both at La Rueda
  filter(!str_detect(`Height_cm`, "N/A"))%>%
  ggplot() +
  ggtitle("Height & Ranch") +
  geom_boxplot(aes(x = Ranch, y = Height_cm, fill = Region)) +
  geom_jitter(aes(x = Ranch, y = Height_cm)) +
  facet_wrap(~Region, dir = "h") +
  ylim(0, 350) +
  xlab("Ranch") +
  ylab("Height (cm)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))


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

M2_age %>% #planted between 02/13/22 and 01/20/23: 341 days
  ggplot() +
  ggtitle("M2") +
  geom_step(aes(x = Ratio, y = TotalValue)) +
  ylim(0, 1159) +
  geom_vline(xintercept = 100/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #100 days
  geom_text(label="100 days", x=(100/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1150) +
  geom_vline(xintercept = 200/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #200 days
  geom_text(label="200 days", x=(200/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1050) +
  geom_vline(xintercept = 300/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #300 days
  geom_text(label="300 days", x=(300/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1150) +
  geom_vline(xintercept = 400/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #400 days
  geom_text(label="400 days", x=(400/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1050) +
  geom_vline(xintercept = 500/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #500 days
  geom_text(label="500 days", x=(500/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1150) +
  geom_vline(xintercept = 600/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #600 days
  geom_text(label="600 days", x=(600/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1050) +
  geom_vline(xintercept = 700/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #700 days
  geom_text(label="700 days", x=(700/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1150) +
  geom_vline(xintercept = 800/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #800 days
  geom_text(label="800 days", x=(800/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1050) +
  geom_vline(xintercept = 900/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #900 days
  geom_text(label="900 days", x=(900/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1150) +
  geom_vline(xintercept = 1000/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #1000 days
  geom_text(label="1000 days", x=(1000/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1050) +
  theme_classic()

M3_age %>% #planted between 01/20/23 and 12/13/23: 327 days
  ggplot() +
  ggtitle("M3") +
  geom_step(aes(x = Ratio, y = TotalValue)) +
  xlim(0, 1) +
  ylim(0, 40) +
  geom_vline(xintercept = 100/as.numeric(max(seedlings_clean_M3$TimeAlive)), linetype="dashed") + #100 days
  geom_text(label="100 days", x=(100/as.numeric(max(seedlings_clean_M3$TimeAlive))), y=40) +
  geom_vline(xintercept = 200/as.numeric(max(seedlings_clean_M3$TimeAlive)), linetype="dashed") + #200 days
  geom_text(label="200 days", x=(200/as.numeric(max(seedlings_clean_M3$TimeAlive))), y=35) +
  geom_vline(xintercept = 300/as.numeric(max(seedlings_clean_M3$TimeAlive)), linetype="dashed") + #300 days
  geom_text(label="300 days", x=(300/as.numeric(max(seedlings_clean_M3$TimeAlive))), y=40) +
  geom_vline(xintercept = 400/as.numeric(max(seedlings_clean_M3$TimeAlive)), linetype="dashed") + #400 days
  geom_text(label="400 days", x=(400/as.numeric(max(seedlings_clean_M3$TimeAlive))), y=35) +
  geom_vline(xintercept = 500/as.numeric(max(seedlings_clean_M3$TimeAlive)), linetype="dashed") + #500 days
  geom_text(label="500 days", x=(500/as.numeric(max(seedlings_clean_M3$TimeAlive))), y=40) +
  geom_vline(xintercept = 600/as.numeric(max(seedlings_clean_M3$TimeAlive)), linetype="dashed") + #600 days
  geom_text(label="600 days", x=(600/as.numeric(max(seedlings_clean_M3$TimeAlive))), y=35) +
  theme_classic()

M4_age %>% #planted between 12/13/23 and 11/23/24 (346 days)
  ggplot() +
  ggtitle("M4") +
  geom_step(aes(x = Ratio, y = TotalValue)) +
  xlim(0, 1) +
  ylim(0, 1000) +
  geom_vline(xintercept = 100/as.numeric(max(seedlings_clean_M4$TimeAlive)), linetype="dashed") + #100 days
  geom_text(label="100 days", x=(100/as.numeric(max(seedlings_clean_M4$TimeAlive))), y=40) +
  # geom_vline(xintercept = 200/as.numeric(max(seedlings_clean_M4$TimeAlive)), linetype="dashed") + #200 days
  # geom_text(label="200 days", x=(200/as.numeric(max(seedlings_clean_M4$TimeAlive))), y=35) +
  # geom_vline(xintercept = 300/as.numeric(max(seedlings_clean_M4$TimeAlive)), linetype="dashed") + #300 days
  # geom_text(label="300 days", x=(300/as.numeric(max(seedlings_clean_M4$TimeAlive))), y=40) +
  # geom_vline(xintercept = 400/as.numeric(max(seedlings_clean_M4$TimeAlive)), linetype="dashed") + #400 days
  # geom_text(label="400 days", x=(400/as.numeric(max(seedlings_clean_M4$TimeAlive))), y=35) +
  # geom_vline(xintercept = 500/as.numeric(max(seedlings_clean_M4$TimeAlive)), linetype="dashed") + #500 days
  # geom_text(label="500 days", x=(500/as.numeric(max(seedlings_clean_M4$TimeAlive))), y=40) +
  # geom_vline(xintercept = 600/as.numeric(max(seedlings_clean_M4$TimeAlive)), linetype="dashed") + #600 days
  # geom_text(label="600 days", x=(600/as.numeric(max(seedlings_clean_M4$TimeAlive))), y=35) +
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

M2_age %>%
  ggplot() +
  ggtitle("M2") +
  geom_step(aes(x = Ratio, y = PercValue)) +
  ylim(0, 1) +
  geom_vline(xintercept = 100/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #100 days
  geom_text(label="100 days", x=(100/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1) +
  geom_vline(xintercept = 200/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #200 days
  geom_text(label="200 days", x=(200/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=0.8) +
  geom_vline(xintercept = 300/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #300 days
  geom_text(label="300 days", x=(300/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1) +
  geom_vline(xintercept = 400/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #400 days
  geom_text(label="400 days", x=(400/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=0.8) +
  geom_vline(xintercept = 500/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #500 days
  geom_text(label="500 days", x=(500/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1) +
  geom_vline(xintercept = 600/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #600 days
  geom_text(label="600 days", x=(600/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=0.8) +
  geom_vline(xintercept = 700/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #700 days
  geom_text(label="700 days", x=(700/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1) +
  geom_vline(xintercept = 800/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #800 days
  geom_text(label="800 days", x=(800/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=0.8) +
  geom_vline(xintercept = 900/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #900 days
  geom_text(label="900 days", x=(900/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=1) +
  geom_vline(xintercept = 1000/as.numeric(max(seedlings_clean_M2$TimeAlive)), linetype="dashed") + #1000 days
  geom_text(label="1000 days", x=(1000/as.numeric(max(seedlings_clean_M2$TimeAlive))), y=0.8) +
  theme_classic()

M3_age %>%
  ggplot() +
  ggtitle("M3") +
  geom_step(aes(x = Ratio, y = PercValue)) +
  ylim(0, 1) +
  geom_vline(xintercept = 100/as.numeric(max(seedlings_clean_M3$TimeAlive)), linetype="dashed") + #100 days
  geom_text(label="100 days", x=(100/as.numeric(max(seedlings_clean_M3$TimeAlive))), y=1) +
  geom_vline(xintercept = 200/as.numeric(max(seedlings_clean_M3$TimeAlive)), linetype="dashed") + #200 days
  geom_text(label="200 days", x=(200/as.numeric(max(seedlings_clean_M3$TimeAlive))), y=0.9) +
  geom_vline(xintercept = 300/as.numeric(max(seedlings_clean_M3$TimeAlive)), linetype="dashed") + #300 days
  geom_text(label="300 days", x=(300/as.numeric(max(seedlings_clean_M3$TimeAlive))), y=1) +
  geom_vline(xintercept = 400/as.numeric(max(seedlings_clean_M3$TimeAlive)), linetype="dashed") + #400 days
  geom_text(label="400 days", x=(400/as.numeric(max(seedlings_clean_M3$TimeAlive))), y=0.9) +
  geom_vline(xintercept = 500/as.numeric(max(seedlings_clean_M3$TimeAlive)), linetype="dashed") + #500 days
  geom_text(label="500 days", x=(500/as.numeric(max(seedlings_clean_M3$TimeAlive))), y=1) +
  geom_vline(xintercept = 600/as.numeric(max(seedlings_clean_M3$TimeAlive)), linetype="dashed") + #600 days
  geom_text(label="600 days", x=(600/as.numeric(max(seedlings_clean_M3$TimeAlive))), y=0.9) +
  theme_classic()

M4_age %>%
  ggplot() +
  ggtitle("M4") +
  geom_step(aes(x = Ratio, y = PercValue)) +
  ylim(0, 1) +
  geom_vline(xintercept = 100/as.numeric(max(seedlings_clean_M4$TimeAlive)), linetype="dashed") + #100 days
  geom_text(label="100 days", x=(100/as.numeric(max(seedlings_clean_M4$TimeAlive))), y=1) +
  geom_vline(xintercept = 200/as.numeric(max(seedlings_clean_M4$TimeAlive)), linetype="dashed") + #200 days
  geom_text(label="200 days", x=(200/as.numeric(max(seedlings_clean_M4$TimeAlive))), y=0.9) +
  geom_vline(xintercept = 300/as.numeric(max(seedlings_clean_M4$TimeAlive)), linetype="dashed") + #300 days
  geom_text(label="300 days", x=(300/as.numeric(max(seedlings_clean_M4$TimeAlive))), y=1) +
  geom_vline(xintercept = 400/as.numeric(max(seedlings_clean_M4$TimeAlive)), linetype="dashed") + #400 days
  geom_text(label="400 days", x=(400/as.numeric(max(seedlings_clean_M4$TimeAlive))), y=0.9) +
  geom_vline(xintercept = 500/as.numeric(max(seedlings_clean_M4$TimeAlive)), linetype="dashed") + #500 days
  geom_text(label="500 days", x=(500/as.numeric(max(seedlings_clean_M4$TimeAlive))), y=1) +
  geom_vline(xintercept = 600/as.numeric(max(seedlings_clean_M4$TimeAlive)), linetype="dashed") + #600 days
  geom_text(label="600 days", x=(600/as.numeric(max(seedlings_clean_M4$TimeAlive))), y=0.9) +
  theme_classic()

    
####WATERED####
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



####UNWATERED####
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
