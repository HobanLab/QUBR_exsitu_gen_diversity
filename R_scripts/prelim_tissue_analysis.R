rm(list=ls())
setwd("..")

library(tidyverse)
library(ggplot2)
library(dplyr)

`%notin%` <- Negate(`%in%`)

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
  filter(str_detect(Condition, "Good"))

tissue_field_data <- tissue_data_clean%>% #joins field data and tissue transfer data by QUBR_ID
  left_join(., field_data_clean, join_by(QUBR_ID==QUBR_ID))


tissue_field_for_extraction <- tissue_field_data %>%
  filter(Weight>=0.04) #removes leaf samples that are too small to do extraction with


tissue_field_for_extraction%>%
  ggplot() + #creates empty plot
  geom_bar(aes(x = Color, fill = Color)) +
  theme_classic() #removes gray background




summary(tissue_field_for_extraction) #shows character vs numeric value details

tissue_field_for_extraction%>% #bar chart of number of tissue samples per region
  ggplot() + 
  geom_bar(aes(x = Region, fill = Region)) +
  theme_classic()

tissue_field_for_extraction%>% #box plot of tissue weights per region
  ggplot() +
  geom_boxplot(aes(x = Region, y = Weight, fill = Region)) +
  theme_classic()

#how many inds from each region weigh enough to do extractions
num_inds_by_region <- tissue_field_for_extraction %>%
  group_by(Region) %>%
  summarise(count = n())
#v few from the W --> we are hoping to do 600 extractions which would mean 200 from each region and we have less than that from the W --> will do all of those and then divide the remaining number of inds by 2 to figure out how many inds to sample from E and N

num_west <- num_inds_by_region$count[num_inds_by_region$Region=="W"][1]
num_na <- num_inds_by_region$count[is.na(num_inds_by_region$Region)]

num_to_sample <- floor((600-num_west-num_na)/2)

#Separate tissue with high enough weight to do actual extractions into different regions
tissue_west_and_NAs <- tissue_field_for_extraction%>% #we will extract from all of these samples from the West because there are so few
  filter(Region == 'W' | is.na(Region))

#look at the weights of the viable W inds to see how close to problematic they are
tissue_west_and_NAs%>%
  ggplot() +
  geom_col(aes(x = QUBR_ID, y = Weight)) +
  geom_hline(yintercept = .04, color = "red") + #add red line at minimum viable weight for extraction
  theme_classic()
#5 inds with literally no buffer room but mostly this seems fine

tissue_east <- tissue_field_for_extraction%>%
  filter(Region == 'E')

tissue_north <- tissue_field_for_extraction%>%
  filter(Region == 'N')
                        

#randomly sample the correct number of inds from N and E
set.seed(2024)

north_inds_to_sample <- sample(tissue_north$QUBR_ID, size = num_to_sample, replace = F)

east_inds_to_sample <- sample(tissue_east$QUBR_ID, size = num_to_sample, replace = F)

#create dataset with only 600 chosen inds for extractions
tissue_field_600_inds <- tissue_field_for_extraction %>%
  filter(QUBR_ID %in% tissue_west_and_NAs$QUBR_ID | QUBR_ID %in% north_inds_to_sample | QUBR_ID %in% east_inds_to_sample) 

#take the top 100 heaviest weights, these will be the first 100 extractions 
top_100_for_extractions <- tissue_field_600_inds %>%
  top_n(., 100, wt = Weight) 

#maybe unsurprisingly, nearly all of these are from the North
top_100_for_extractions %>%
  ggplot() +
  geom_bar(aes(x = Region, fill = Region)) +
  theme_classic()

# Bind the remaining inds on to this list so we can keep these 100 as the first extractions but then proceed semi randomly
top_100_for_extractions %>%
  rbind(filter(tissue_field_600_inds, QUBR_ID %notin% top_100_for_extractions$QUBR_ID)) %>%
  write_csv(., "./data/600_inds_for_extractions.csv")


# Dana making graphs to show Daniel 9/16/24
summary(tissue_field_data)

tissue_field_data%>%
  ggplot() +
  geom_boxplot(aes(x = Region, y = Weight, fill = Region)) +
  theme_classic()

tissue_field_data%>%
  ggplot() +
  geom_bar(aes(x = Region, fill = Region)) +
  theme_classic()
