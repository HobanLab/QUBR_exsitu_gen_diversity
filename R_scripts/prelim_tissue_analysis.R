rm(list=ls())
setwd("..")

library(tidyverse)
library(ggplot2)

field_data <- read_csv("./data/04_2024_field_datasheets_full.csv", na = c("N/A",""))

#cleaning raw field data so we can graph it
field_data_clean <- field_data %>%
  rename(QUBR_ID =`QUBR ID`) %>% #renaming QUBR_ID column to remove space
  rename(Recorder_name =`Name of recorder`) %>% #renaming recorder_name column to remove space
  mutate(Site = case_when(str_detect(Site, "[?]$") ==T ~ NA, #overriding column called Site, anywhere with a ? becomes NA
                          str_detect(Site, "[?]$") == F ~ Site)) %>%
  mutate(Region = case_when(str_detect(Region, "[?]") ==T ~ NA, #overriding column called Region, anywhere with a ? becomes NA
                          str_detect(Region, "[?]") == F ~ Region))

  


#creating bar graph
field_data_clean %>%
  ggplot() + #creates empty plot
  geom_bar(aes(x = Region, fill = Region)) + #plots Regions on the x axis and gives each their own color
  theme_classic() #removes gray background
#
field_data_clean %>%
  ggplot() + #creates empty plot
  geom_bar(aes(x = Site, fill = Site)) +
  theme_classic() +
  facet_grid(~Region) #nests sites within region

#load tissue transfer data
#explore tissue transfer data (what kinds of columns, NAs, left_join function to join by QUBR_ID column (same column name in both datasets))
