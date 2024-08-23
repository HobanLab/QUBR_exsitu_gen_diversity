rm(list=ls())
setwd("..")

library(tidyverse)
library(ggplot2)
library(dplyr)

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

field_data_clean %>%
  ggplot() + #creates empty plot
  geom_bar(aes(x = Site, fill = Site)) +
  theme_classic() +
  facet_grid(~Region) #nests sites within region



#load tissue transfer data
tissue_data <- read_csv("./data/Transfer QUBR Tissue to Freezer 2024 April.csv")

#explore tissue transfer data (what kinds of columns, NAs, left_join function to join by QUBR_ID column)
#(same column name in both data sets)
tissue_data_clean <- tissue_data%>%
  rename(Color = `Leaf Tissue Color (Green, Brown, Black)`)%>% #rename leaf color column
  rename(Condition = `Leaf Tissue Condition (Moldy, Necrotic, or Good)`)%>% #rename leaf condition column
  rename(Weight = `Weight (g)`) %>% #rename weight column to remove units
  rename(QUBR_ID = `Accession ID`)%>%
  filter(str_detect(Color, "reen")) %>% #selects only colors named Green and green
  filter(str_detect(Condition, "Good"))%>%
  filter(Weight>=0.04) #removes leaf samples that are too small to do extraction with
  #  

tissue_data_clean%>%
  ggplot() + #creates empty plot
  geom_bar(aes(x = Color, fill = Color)) +
  theme_classic() #removes gray background

summary(tissue_field_data) #shows character vs numeric value details

tissue_field_data <- tissue_data_clean%>% #joins field data and tissue transfer data by QUBR_ID
  left_join(., field_data_clean, join_by(QUBR_ID==QUBR_ID))

tissue_field_data%>% #bar chart of number of tissue samples per region
  ggplot() + 
  geom_bar(aes(x = Region, fill = Region)) +
  theme_classic()

tissue_field_data%>% #box plot of tissue weights per region
  ggplot() +
  geom_boxplot(aes(x = Region, y = Weight, fill = Region)) +
  theme_classic()

tissue_west <- tissue_field_data%>% #we will extract from all of these samples from the West
  filter(Region == 'W')
tissue_east <- tissue_field_data%>%
  filter(Region == 'E')
tissue_north <- tissue_field_data%>%
  filter(Region == 'N')
                        
tissue_field_data %>%
  group_by(Region) %>%
  summarise(count = n())

tissue_west%>%
  ggplot() +
  geom_col(aes(x = QUBR_ID, y = Weight))

