####SET UP####
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tibble)
library(lubridate)
library(chisq.posthoc.test)
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

####DATA CLEANING####
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

#How many different lengths of time have individuals been alive for?

unique(seedlings_clean$PotentialTimeAlive)
#prioritize seedlings for visits while in Baja based on number of individuals at each ranch  
priority_sites <- seedlings_clean%>%
  filter(Outcome == 'Alive')%>%
  group_by(PlantedReg, Ranch, N, W)%>%
  summarise(n())
  



####SURVIVORSHIP CURVE####
#creating a df with increments of 1 day to represent how old a seedling could be
df_age <- 
  data.frame("Days"=seq(0, 1220, 1), "TotalAlive" = NA) 

#for loop: how many individuals were still alive at any given duration?
#ALL
for (i in 1:nrow(df_age)) {
  Day <- df_age$Days[i] #df_age$Days is a vector (one column in this df)
  Num_seedlings_alive <- sum(Day <= seedlings_clean$TimeAlive, na.rm = TRUE)
  #Day is a temporary object that holds the output of the day we are on in the iterative loop
  df_age$TotalAlive[i] <- paste0(Num_seedlings_alive)
  #fill one cell per iteration with the total number of seedlings alive by that day
  }

#plot survivorship curve
df_age_final <- df_age%>%
  mutate(TotalAlive = as.numeric(TotalAlive))%>%
  mutate(PercentAlive = TotalAlive/max(TotalAlive, na.rm = TRUE))%>%
  mutate(data_type = "all")
  
df_age_final %>%
  ggplot(aes(x = Days, y = PercentAlive)) +
  ggtitle("All") +
  geom_step() +
  theme_classic()


#representing y-axis as a ratio of days an individual lived / days it could have been alive for
#bc not all of the seedlings were planted on the same day, some have had the chance to grow longer than others
df_age_ratio <- 
  data.frame("Ratio" =seq(0, 1, .01), "TotalValue" = NA)

for (i in 1:nrow(df_age_ratio)) {
  Ratio_Value <- df_age_ratio$Ratio[i]
  ratio_hold <- sum(Ratio_Value <= seedlings_clean$RatioTimeAlive, na.rm = TRUE)
  df_age_ratio$TotalValue[i] <- as.numeric(ratio_hold)
}

df_age_ratio %>%
  ggplot() +
  geom_step(aes(x = Ratio, y = TotalValue)) +
  theme_classic()

#representing the same information, but broken up into groups by planting date

#filtering seedlings: individuals planted before M1
seedlings_clean_M1 <- seedlings_clean%>%
  filter(DatePlanted < '2022-02-13')

M1_age <-
  data.frame("Ratio" =seq(0, 1, .01), "TotalValue" = NA)

for (i in 1:nrow(M1_age)) {
  Ratio_Value <- M1_age$Ratio[i]
  ratio_hold <- sum(Ratio_Value <= seedlings_clean_M1$RatioTimeAlive, na.rm = TRUE)
  M1_age$TotalValue[i] <- as.numeric(ratio_hold)
}

M1_age %>%
  ggplot() +
  ggtitle("M1") +
  geom_step(aes(x = Ratio, y = TotalValue)) +
  ylim(0, 510) +
  theme_classic()
 

####CONVERTING FOR LOOP TO FUNCTION####

#Example of a function that prints "a b"
test_function <- function(sequence, fill_in){
  temp <- paste(sequence, fill_in)
  #return(temp)
  }
test_function("a", "b")
test_function("c", "d")


#creates a blank loop that can be repeated for each age class(M1, M2, etc)
loop_function <- function(source, CustomSequence, fill_in){
  df <- data.frame("Ratio" = CustomSequence, fill_in = NA)
  names(df)[2] <- c(fill_in) #overwrites the column title
  for (i in 1:nrow(df)) {
    Ratio_Value <- df$Ratio[i]
    ratio_hold <- sum(Ratio_Value <= source$RatioTimeAlive, na.rm = TRUE)
    df[i,2] <-as.numeric(ratio_hold) #saves value to the i row in the 2nd column
  }
  return(df) #this is the part of the for loop we want back as results
}

M2_age <- loop_function(seedlings_clean_M2, seq(0, 1, .01), "TotalValue")
M3_age <- loop_function(seedlings_clean_M3, seq(0, 1, .01), "TotalValue")
M3.1_age <- loop_function(seedlings_clean_M3.1, seq(0, 1, .01), "TotalValue")



#Without a function: Repeat filtering seedlings: individuals planted between M1 & M2
seedlings_clean_M2 <- seedlings_clean%>%
  filter(DatePlanted < '2023-01-20', DatePlanted > '2022-02-13')

M2_age <-
  data.frame("Ratio" =seq(0, 1, .01), "TotalValue" = NA)
  
for (i in 1:nrow(M2_age)) {
  Ratio_Value <- M2_age$Ratio[i]
  ratio_hold <- sum(Ratio_Value <= seedlings_clean_M2$RatioTimeAlive, na.rm = TRUE)
  M2_age$TotalValue[i] <- as.numeric(ratio_hold)
  }

M2_age %>%
  ggplot() +
  ggtitle("M2") +
  geom_step(aes(x = Ratio, y = TotalValue)) +
  ylim(0, 1159) +
  theme_classic()

#Repeat filtering seedlings: individuals planted between M2 & M3

seedlings_clean_M3 <- seedlings_clean%>%
  filter(DatePlanted < '2023-12-13', DatePlanted > '2023-01-20')

M3_age <-
  data.frame("Ratio" =seq(0, 1, .01), "TotalValue" = NA)

for (i in 1:nrow(M3_age)) {
  Ratio_Value <- M3_age$Ratio[i]
  ratio_hold <- sum(Ratio_Value <= seedlings_clean_M3$RatioTimeAlive, na.rm = TRUE)
  M3_age$TotalValue[i] <- as.numeric(ratio_hold)
}

M3_age %>%
  ggplot() +
  ggtitle("M3") +
  geom_step(aes(x = Ratio, y = TotalValue)) +
  ylim(0, 40) +
  theme_classic()


#Same as M3, plus the 10 individuals planted after M3

seedlings_clean_M3.1 <- seedlings_clean%>%
  filter(DatePlanted > '2023-01-20')

M3.1_age <-
  data.frame("Ratio" =seq(0, 1, .01), "TotalValue" = NA)

for (i in 1:nrow(M3.1_age)) {
  Ratio_Value <- M3.1_age$Ratio[i]
  ratio_hold <- sum(Ratio_Value <= seedlings_clean_M3.1$RatioTimeAlive, na.rm = TRUE)
  M3.1_age$TotalValue[i] <- as.numeric(ratio_hold)
}

M3.1_age %>%
  ggplot() +
  ggtitle("M3+") +
  geom_step(aes(x = Ratio, y = TotalValue)) +
  ylim(0, 40) +
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