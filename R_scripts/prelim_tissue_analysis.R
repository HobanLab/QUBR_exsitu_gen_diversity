rm(list=ls())
setwd("/Users/Ashley/Desktop/Hoban/Quercus_brandegeei/RaMP_2024-25")

library(tidyverse)
library(ggplot2)

field_data <- read_csv("./data/04_2024_field_datasheets_full.csv", na = c("N/A",""))

field_data_clean <- field_data %>%
  rename(QUBR_ID =`QUBR ID`) %>%
  rename(Recorder_name =`Name of recorder`) %>%
  mutate(Site = case_when(str_detect(Site, "[?]$") ==T ~ NA, 
                          str_detect(Site, "[?]$") == F ~ Site)) %>%
  mutate(Region = case_when(str_detect(Region, "[?]") ==T ~ NA, 
                          str_detect(Region, "[?]") == F ~ Region))

  



field_data_clean %>%
  ggplot() +
  geom_bar(aes(x = Region, fill = Region)) +
  theme_classic() 

field_data_clean %>%
  ggplot() +
  geom_bar(aes(x = Site, fill = Site)) +
  theme_classic() +
  facet_grid(~Region)


str_detect(c("?", "!", "."), "[?]")
          
unique(field_data_clean$Site)
