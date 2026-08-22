####Loading packages + setting wd####
rm(list=ls())
library(tidyverse)
library(adegenet)
library(poppr)
library(sf)
library(RColorBrewer)
library(hierfstat)
library(ggrepel)
library(magrittr)
library(PopGenReport)


#set to wherever you want your outputs from these analyses to go
setwd("~/Documents/GitHub/QUBR_exsitu_gen_diversity/Genetic Analysis")

# Create a function that is the opposite of %in%
`%notin%` <- Negate(`%in%`)

####Loading in the raw scores from geneious####

path_to_code_outputs = "~/Documents/GitHub/QUBR_exsitu_gen_diversity/Genetic Analysis/data/inputs"

# the location on my computer where outputs are aggregated 
path_to_geneious_outputs = "~/Documents/GitHub/QUBR_exsitu_gen_diversity/Genetic Analysis/data/inputs/Genetic_inputs"

# defines naming scheme for sample names 
name_pattern_regex <- "SH-Q\\d{4}" #currently: match SH-Q followed by 4 digits

#dealing with the 3 plates that have 2 files for MP1
path_to_weird_outputs = "~/Documents/GitHub/QUBR_exsitu_gen_diversity/Genetic Analysis/data/inputs/Genetic_inputs/RaMP_inputs/Weird_RaMP_inputs"
weird_file_list <- list.files(path=path_to_weird_outputs, full.names= T, pattern = "*.csv")

DP_list_RaMP <- c("DP04", "DP06", "DP07") #these are the DPs with multiple files per plate in MP1
MP_list_RaMP <- c("MP1", "MP4") #these are the only multiplexes my RaMP work used and therefore the only one I care about in other data


#creates csv files
lapply(DP_list_RaMP, function(DP){
  DP_file_list <- weird_file_list[which(str_detect(weird_file_list, DP))]
  DP_data <- lapply(DP_file_list, function(DP_file){
    read_csv(DP_file, na = "") %>%
      filter(str_detect(Name, "BLANK", negate = T)) %>% #filters out the blanks
      mutate(Name = paste0(str_extract(Name, name_pattern_regex),"_", str_extract(Name,  "DP\\d{2}")))
  }) %>%
    reduce(left_join, by = "Name") %>%
    select(Name | matches("03_284") | ends_with("y")) %>% #we removed this primer because it was always unscorable
    select(!(contains("03_284") & ends_with("y"))) %>%
    rename_with(~str_remove(., '.x|.y'), .cols = !Name) 
  write_csv(DP_data, paste0(path_to_geneious_outputs, "/RaMP_inputs/RaMP_MP1_", DP, ".csv", sep = ""))
})

# makes a list of file names for applying functions over
file_list <- list.files(path=paste0(path_to_geneious_outputs), full.names= T, pattern = "*.csv", recursive = TRUE) 
file_list <- file_list[which(file_list%notin%weird_file_list)] #excluding weird files which have 2 files for the same DP and MP


# reads in csv data
#edits data so that name column is correct and consistent across input csvs and then
#join all csvs together with left_join 
all_data <- reduce(lapply(MP_list_RaMP, function(MP){
  MP_file_list <- file_list[which(str_detect(file_list, MP))]
  MP_data <- bind_rows(lapply(MP_file_list, function(file){
    tmp <- read_csv(file, na = "") %>%
      mutate(across(everything(), as.character))# makes everything a character rather than a numeric
  }))
  
  MP_data_cleaned <- MP_data %>%
    filter(str_detect(Name, "BLANK", negate = T)) %>% # filters out the blanks
    mutate(DP_num = str_extract(Name, "DP\\d{2}")) %>% # makes a column that lists the DP associated with the sample --> this will enable me to search up the origin of dups with  mismatches
    arrange(DP_num)%>% #ensures that dups are always read after origs  
    mutate(Name = str_extract(Name, name_pattern_regex)) %>% # gets rid of cell/plate junk at the end of the name columns in each df so the Name column now contains names that will be consistent across input csvs
    
    # start of code to rename duplicate occurrences of sample names
    group_by(Name) %>%
    mutate(
      occurrence_num = row_number(), # obtains occurrence counts of each unique name
      Name = ifelse(occurrence_num == 2, paste0(Name,"_B"), Name)
    ) %>% # changes the 2nd occurrence of any name to "Name_B"
    ungroup() %>%
    select(-occurrence_num)%>% # gets rid of occurrence number col
  # end of code to rename duplicate occurrences of sample names
    mutate(across(everything(), as.character)) # makes everything a character rather than a numeric
  
  return(MP_data_cleaned) # returns the edited data from each csv 
}), left_join, by = c("Name", "DP_num")) %>% # joins edited data frames of input csvs via the "Name" column, note: if there are multiple occurrences of the same column names (aka re-run primers), the first will be named "Name.x" and the second will be "Name.y"
  select(Name, DP_num, order( as.numeric(gsub("[0-9$]", "", names(.))))) %>% # order columns numerically by any numbers at the end of the column name (so the loci are in numeric order)
  arrange(Name)

####Cleaning the data####


#THIS IS NOT RELEVANT UNTIL TRIPLOIDS ARE SCORED
#Code below removes the 3rd allele possibility from locus 08_529 so that locus matches the rest of the data (from all other loci)
#all_data <- all_data %>%
  # mutate(`QUVA 08_529 - 3` =
  #          case_when(`QUVA 08_529 - 3` != `QUVA 08_529 - 2` & !is.na(`QUVA 08_529 - 3`) ~ "Too many alleles",
  #                    .default = NA)) %>% #overwrite existing scores in the 3rd 08_529 col with "too many alleles" if the 3rd allele was different than the 2nd AND wasn't an NA
  # mutate(across(starts_with("QUVA 08_529"),
  #               ~ case_when(`QUVA 08_529 - 3` == "Too many alleles" ~ "Too many alleles",
  #                           .default = .))) %>% #overwrite existing scores in all 08_529 cols with "too many alleles" if the 3rd allele was previously overwritten with "too many alleles"
  #select(-`QUVA 08_529 - 3`) %>% # drop the now unnecessary 3rd 08_529 column
  #select(-c(`QUVA 08_528 - 1`, `QUVA 08_528 - 2`))


####Exploring polyploidy####  

## Exploring the proportion of polyploid inds *per locus*

#make table of proportion of individuals at each locus that have too many alleles
prop_inds_polyploid <- all_data %>%
  select(-c(Name, DP_num, ends_with("- 2"))) %>% #keep only the -1 calumn of each locus (because we don't need redundant data at each locus)
  summarise(across(everything(), ~ sum(. == "Too many alleles", na.rm = TRUE)/nrow(all_data))) %>% #summarizes the number of times in each column (locus) there are "too many alleles" and divides that by the number of rows to get a proportion of 
  
  #rename all locus columns so they are more legible (only have relevant info)
  rename_with(~str_remove(., ' - 1')) %>% 
  rename_with(~str_remove(., 'QUVA ')) %>%
  rename_with(~str_remove(., '\\(ALEXA\\)')) %>%
  
  pivot_longer(cols = everything(), names_to = "locus", values_to = "prop_polyploid") #pivot the table so that the locus names are now a single column and the values in those columns are the proportion of inds that were polyploid at that locus


#plot proportion of individuals that are polyploid at each locus
ggplot() +
  geom_point(data = prop_inds_polyploid, aes(x = locus, y = prop_polyploid)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) + #turn the axis label so locus names are legible 
  theme_minimal()


## Exploring the proportion of polyploid loci *per individual*

#Add a column to the data that has a count of the number of polyploid loci per ind
all_data_cleaned <- all_data %>% 
  select(-c(`QUVA 08_528 - 1`, `QUVA 08_528 - 2`))%>% # this locus was unscorable (08_528 - 1 & 08_528), so we are removing it
  rowwise() %>% #make it so the following functions are performed on rows rather than cols
  mutate(num_loci_polyploid = sum(str_detect(c_across(-c(Name, DP_num)), "Too many alleles"))/2) #get a count of the number of cells across all cols w/ too many alleles per row (ind) (except  the Name and DP_num cols)

# Get the number of loci in the data
num_loci <- ((ncol(all_data_cleaned) - 3) / 2) # We subtracted the num_loci_polyploid column, the name column, and the DP_num column

# Make a historgram of the number of polyploid loci present in individuals with at least 1 polyploid locus
all_data_cleaned %>%
  filter(num_loci_polyploid != 0) %>% #filter out inds which are not polyploid
  ggplot() +
  geom_histogram(aes(x = num_loci_polyploid), binwidth = 1) + #set the binwidth to 1 so that you get counts at each possible number of loci
  scale_x_continuous(limits = c(0, num_loci), breaks = seq(0, num_loci, by = 1)) + #set max x value to be the number of loci in the data 
  theme_minimal()


####Cleaning data for futher analyses####

# Write the (mostly) cleaned data to the working directory 
write_csv(all_data_cleaned, paste0(path_to_code_outputs, "/all_genos_aggregated.csv"))

## Final cleaning steps for further analyses include:
#Renaming the loci to have cleaner names
#Removing individuals w/ loci w/ no peaks
#Removing individuals w/ loci w 3 alleles
#Turning all loci data into numerics
#WILL ALSO NEED TO REMOVE UNBINNED DATA (str detect "Unbinned peaks in locus")

# Make a temporary dataframe for further cleaning
data_tmp <- all_data_cleaned %>%
  ungroup()%>% #get rid of rowwise operator
  #remove the extraneous info in loci names
  rename_with(~str_remove(., 'QUVA ')) %>%
  rename_with(~str_remove(., '\\(ALEXA\\)')) %>%
  #make a new col w/ TRUE if any of the values in the loci cols have the value "No peak" or "Unbinned"
  mutate(need_reamp = if_any(
    .cols = -c(Name, DP_num, num_loci_polyploid), 
    .fns = ~ str_detect(., "No peak")))%>%
  mutate(need_recheck = if_any(
    .cols = -c(Name, DP_num, num_loci_polyploid), 
    .fns = ~ str_detect(., "nbinned")))
#WRITE CSV FOR ASH

# Make a df w/ only the info about inds which are putative polyploids
putative_polyploids <- data_tmp %>%
  filter(num_loci_polyploid > 1) %>%
  select(Name, DP_num, num_loci_polyploid)

#Write the polyploid data to the working directory
#write_csv(putative_polyploids, paste0(path_to_code_outputs, "/putative_polyploid_list.csv"))

#make a df about inds which are iffy polyploids
iffy_putative_polyploids <- data_tmp %>%
  filter(num_loci_polyploid == 1) %>%
  select(Name, DP_num, num_loci_polyploid)

#NOT IMPORTANT BUT STILL RUN IT
# Make a df of the inds that need to be re-amplified and a list of which of their loci need reamp
need_reamp_list <- data_tmp %>%
  filter(need_reamp ==T) %>% #keep only the inds which need reamp
  select(c(ends_with(" - 1"), Name)) %>% #keep only the info from first of the 2 alleles per locus 
  rename_with(~str_remove(., ' - 1')) %>% #clean up locus names
  rowwise() %>% #make it so the following functions are performed on rows rather than columns (used w/ c_across to look across all columns in a row for data aggregation)
  mutate(reamp_loci = paste(names(.)[str_detect(c_across(-c(Name)), "No peak")], collapse = ", ")) %>% #make a new col with the names of all the cols (loci) that have "No peak" values (aka need to be reamplified) 
  ungroup() %>% #stop performing operations rowwise
  select(Name, reamp_loci) #keep only the individual name and which loci need to be reamped 

#Write the reamp list data to working dir 
#write_csv(need_reamp_list, paste0(path_to_code_outputs, "/reamplification_list.csv"))

need_recheck_list <- data_tmp %>%
  filter(need_recheck ==T) %>% #keep only the inds which need reamp
  select(c(ends_with(" - 1"), Name)) %>% #keep only the info from first of the 2 alleles per locus 
  rename_with(~str_remove(., ' - 1')) %>% #clean up locus names
  rowwise() %>% #make it so the following functions are performed on rows rather than columns (used w/ c_across to look across all columns in a row for data aggregation)
  mutate(recheck_loci = paste(names(.)[str_detect(c_across(-c(Name)), "nbinned")], collapse = ", ")) %>% #make a new col with the names of all the cols (loci) that have "No peak" values (aka need to be reamplified) 
  ungroup() %>% #stop performing operations rowwise
  select(Name, recheck_loci) #keep only the individual name and which loci need to be rechecked 
# Write the recheck list data to working dir 
#write_csv(need_recheck_list, paste0(path_to_code_outputs, "/recheck_list.csv"))
#MAKE CSV FOR ASH


# Make a df for further analyses w/ the polyploids and inds that need to be reamped removed
final_clean_data <- data_tmp %>%
  filter(Name %notin% c(putative_polyploids$Name, need_reamp_list$Name, need_recheck_list$Name, iffy_putative_polyploids$Name)) %>%
  select(-c(need_reamp, num_loci_polyploid)) %>% #get rid of the now extraneous cols 
  #add filter out string detect
  mutate(across(-c(Name, DP_num), as.numeric)) #make the genetic data numeric since there should be no more cells w/ text rather than scores

# Check to see the underlying cause of any NAs in any of the columns in the clean data
check_NA <- data_tmp %>%
  filter(Name %in% filter(final_clean_data, if_any(everything(), is.na))$Name)
#all of the inds that get flagged are places where the dup (_B) is in an earlier DP than the original


####Dealing w/ duplicate data####  

## Checking that dups match each other at every allele

# Make a df w/ only the dups (2nd of inds w/ repeat names that end with '_B')
dups <- data_tmp %>%
  filter(str_detect(Name, "_B")) %>%
  arrange(Name) %>% #arrange so the SHQ IDs are in order so that my dups can be easily compared between each other 
  mutate(real_ID = str_remove(Name, "_B")) #make a column that has the SHQ ID (without the _B modifier) of each ind so they can be compared w/ the originals easily
  #filter(real_ID != "SH-Q3357") #THIS IS JUST FOR EXAMPLE
  

# Make a df w/ only the original scoring of dups (1st of inds w/ repeat names)
origs <- data_tmp %>%
  filter(Name %in% c(dups$real_ID)) %>% #keep only the inds w/ the SHQ IDs that match the real IDs of the dups
  arrange(Name) %>% #arrange so the SHQ IDs are in order so that my dups can be easily compared between each other 
  mutate(real_ID = Name) #make a matching real ID column so that the dups and origs df are identical

# Make a df w/ FALSE in any cell that isn't identical between the duplicate runs of a given individual 
dup_mismatches <- as.tibble(dups == origs) %>%
  select(-c(Name, real_ID, DP_num)) %>% #get rid of cols which are uninformative about genetic data
  cbind(origs$real_ID, .) #add a col w/ the SHQ ID of the inds

dups_origs_combo <- rbind(dups, origs) # a table to make searching up differences in individuals easier

# Write the df w/ mismatch info to the dir w/ the raw geneious data so any mismatches can be evaluated 
write_csv(dup_mismatches, paste0(path_to_code_outputs, "/dup_mismatches.csv"))


#if everything is correct these should all be 0
has_nas <- dup_mismatches%>% 
  filter(if_any(-c(`origs$real_ID`, num_loci_polyploid, need_reamp), ~ is.na(.)))%>%
  select(`origs$real_ID`)

full_na_list <- dups_origs_combo%>%
  filter(real_ID%in%has_nas$`origs$real_ID`)#exploring source of NAs in dup_mismatches

has_nas_fulldata <- data_tmp%>%
  filter(if_any(-c(Name, DP_num, num_loci_polyploid, need_reamp), ~ is.na(.)))%>%
  select(Name) #checking if there are any other NAs (ans: there aren't)

## Removing all duplicate inds from my data then re-adding them so they only occur 1 time

# Make a df w/ only 1 copy of each duplicated ind 
dups_to_keep <- dups_origs_combo %>%
  filter(if_any(-c(Name, real_ID, DP_num),~ !is.na(.))) %>% #get rid of inds that are na in any col
  filter(num_loci_polyploid<1)%>%
  filter(need_reamp==FALSE)%>%
  #filter(across(-c(Name, real_ID, DP_num),~ !is.na(.))) %>% #get rid of inds that are na in any col
  distinct(real_ID, .keep_all = T) %>% #keep only a single row of any unique SHQ ID (will ditch the 2nd occurrence of anything with a dup)
  select(-c(Name)) %>% #get rid of the Name column
  rename(Name = real_ID)%>% #make the SHQ ID the name for these inds (so nothing should have a _B modifier even if it was originally the 2nd copy of an SHQ) --> I will still be able to tell which of the 2 possible inds these are by maintaining DP_num (should I need to look into the sequences for any reason)
  select(-c(num_loci_polyploid, need_reamp))

# Make a df w/ only the single selected ind for all dups (de-duplicated data)
final_clean_dedup_data <- final_clean_data %>%
  filter(Name %notin% dups_origs_combo$Name) %>% #remove all dups (the org and the dup)
  rbind(., dups_to_keep) #add back in only the ind in the dups_to_keep data

#how many mismatches occurred in the dup_mismatches?
#did we score the orig and the dup differently?
false_dup_mismatches <-
  dup_mismatches%>%
  select(1:(last_col() - 2))%>% #removes polyploid & reamp columns
  filter_at(vars(2:23), any_vars(.=='FALSE'))#checks all columns except for SH-Q ID for 'FALSE'
#ans: 12 individuals (SH-Q3303, SH-Q3447, SH-Q3490, SH-Q3548, SH-Q4039, SH-Q4072, SH-Q4075, SH-Q4091, SH-Q4164, SH-Q4490, SH-Q4515, SH-Q4983) 
#ans: across 4 loci(07_187 - 1,	07_187 - 2, 02_829 - 1,	02_829 - 2,	03_101 - 1,	03_101 - 2, 02_754 - 1,	02_754 - 2)


####Preping a df that connects all info I have about every ind together####

## First, I need to load my other data that will enable me to link the SHQ IDs in the genetic data to all other info about that individual

# Load in the SHQ database
TCB_QUBR_IDs <- read_csv("~/Documents/GitHub/QUBR_exsitu_gen_diversity/Genetic Analysis/data/inputs/QUBR_database - Sheet1.csv") %>%
  filter(str_detect(`Species - specific epithet`, "randegeei")) %>% #keep only the QUBR data
  select(c(`Extraction Tube #`, `Tissue_ID`, `TCB_ID`, `Tissue Weight`, `Nanodrop conc (ng/uL)`, `Notes`)) #keep only the info that I actually care about (mostly SHQ ID and TCB ID but also other cols that could explain issues w/ amp)


# Load in the processed data with tree coords + DBHs
RaMP_adults <- read.csv("~/Documents/GitHub/QUBR_exsitu_gen_diversity/Genetic Analysis/data/inputs/QUBR Field Datasheets Nov 2024 - filled - Adults.csv")%>%
  rename(QUBR_ID = QUBR.ID)%>%
  mutate(QUBR_ID = paste0("QUBR_", QUBR_ID)) %>% #adding QUBR to the QUBR IDs so they match the IDs in the TCB database
  left_join(., TCB_QUBR_IDs, by = join_by(QUBR_ID == Tissue_ID)) %>% #merge w/ SHQ database by the QUBR IDs
  rename(SHQ_ID = `Extraction Tube #`)%>%
  filter(Locality %notin% c('LS', 'LT', 'SB'))
exploring_RaMP <- RaMP_adults%>%filter(SHQ_ID%notin%final_clean_dedup_data$Name) #looks at individuals which are missing from our data (they were used for testing)

seedlings_2022 <- read.csv("~/Documents/GitHub/QUBR_exsitu_gen_diversity/Genetic Analysis/data/inputs/04_2024_field_datasheets_full.xlsx - 2022 seedlings.csv")%>%
  rename(QUBR_ID = QUBR.ID)%>%
  left_join(., TCB_QUBR_IDs, by = join_by(QUBR_ID == Tissue_ID)) %>% #merge w/ SHQ database by the QUBR IDs
  rename(SHQ_ID = `Extraction Tube #`)
exploring_2022 <- seedlings_2022%>% #these maybe were filtered out previously, even though they have SHQ_IDs? unsure based on what
  filter(SHQ_ID%notin%final_clean_dedup_data$Name)%>%
  filter(!is.na(SHQ_ID))
#why did some adults not get extracted?

#are these SHQ_IDs in the polyploid or need reamp list?
reamp_exploration_overlap <- intersect(exploring_2022$SHQ_ID, need_reamp_list$Name)
print(reamp_exploration_overlap) #4 inds overlap (17 total need reamp)
polyploid_exploration_overlap <- intersect(exploring_2022$SHQ_ID, putative_polyploids$Name)
print(polyploid_exploration_overlap) #10 inds overlap (60 total putative polyploids)
#this is the answer to 14 of the 52 questionable inds

#no overlap between individuals in (exploring_2022 & reamp) and (exploring_2022 & polyploid)
iffy_polyploid_exploration_overlap <- intersect(exploring_2022$SHQ_ID, iffy_putative_polyploids$Name)
print(iffy_polyploid_exploration_overlap) #10 inds overlap (60 total putative polyploids)
#this explains 32 of the 52 questionable inds
#that leaves 6 inds unexplained- are these from MSAT testing?


outplanted <- read.csv("~/Documents/GitHub/QUBR_exsitu_gen_diversity/Genetic Analysis/data/inputs/QUBR Field Datasheets Nov 2024 - filled - OP Seedlings.csv", na.strings = "N/A")%>%
  rename(QUBR_ID = QUBR.ID)%>%
  filter(!is.na(QUBR_ID))%>%
  mutate(QUBR_ID = paste0("QUBR_", QUBR_ID)) %>% #adding QUBR to the QUBR IDs so they match the IDs in the TCB database
  left_join(., TCB_QUBR_IDs, by = join_by(QUBR_ID == Tissue_ID)) %>% #merge w/ SHQ database by the QUBR IDs
  rename(SHQ_ID = `Extraction Tube #`)

Ash_adults <- read_csv("~/Documents/GitHub/QUBR_exsitu_gen_diversity/Genetic Analysis/data/inputs/all_data_merged.csv") %>%
  mutate(QUBR_ID = paste0("QUBR_", QUBR_ID)) %>% #adding QUBR to the QUBR IDs so they match the IDs in the TCB database
  left_join(., TCB_QUBR_IDs, by = join_by(QUBR_ID == Tissue_ID)) %>% #merge w/ SHQ database by the QUBR IDs
  rename(SHQ_ID = `Extraction Tube #`)


# Make a db that will serve to decode the names (Sh-Q IDs) in the genetic data
name_decoder_RaMP <- final_clean_dedup_data %>%
  select(Name) %>% #keep only the names from the genetic data --> this way the decoder will have the right number of IDs in the right order
  left_join(., RaMP_adults, by = join_by(Name == SHQ_ID))%>% #join w/ the adult data (which was merged w/ the SH-Q data) via the SHQ IDs
  filter(Name%in%RaMP_adults$SHQ_ID)%>%
  arrange(Name)

name_decoder_Ash <- final_clean_dedup_data %>%
  select(Name) %>% #keep only the names from the genetic data --> this way the decoder will have the right number of IDs in the right order
  left_join(., Ash_adults, by = join_by(Name == SHQ_ID))%>% #join w/ the adult data (which was merged w/ the SH-Q data) via the SHQ IDs
  filter(Name%in%Ash_adults$SHQ_ID)%>%
  arrange(Name)

#merging all of the adults into one df
Ash_adults_merging <- Ash_adults%>%
  select(c(Metal_ID, QUBR_ID, SHQ_ID, TCB_ID, locality))
RaMP_adults_merging <- RaMP_adults%>%
  select(c(`Blue.tag.ID..`, QUBR_ID, SHQ_ID, TCB_ID, Locality))%>%
  rename(Metal_ID = `Blue.tag.ID..`)%>%
  rename(locality = Locality)
adults_all <- Ash_adults_merging%>%
  rbind(RaMP_adults_merging)

name_decoder_adults <- final_clean_dedup_data %>%
  select(Name)%>%
  left_join(., adults_all, by = join_by(Name == SHQ_ID))%>%
  filter(Name%in%adults_all$SHQ_ID)%>%
  arrange(Name)

name_decoder_outplanted <- final_clean_dedup_data %>%
  select(Name) %>% #keep only the names from the genetic data --> this way the decoder will have the right number of IDs in the right order
  left_join(., outplanted, by = join_by(Name == SHQ_ID))%>% #join w/ the adult data (which was merged w/ the SH-Q data) via the SHQ IDs
  filter(Name%in%outplanted$SHQ_ID)%>%
  arrange(Name)
  
name_decoder_2022 <- final_clean_dedup_data %>%
  select(Name) %>% #keep only the names from the genetic data --> this way the decoder will have the right number of IDs in the right order
  left_join(., seedlings_2022, by = join_by(Name == SHQ_ID))%>% #join w/ the adult data (which was merged w/ the SH-Q data) via the SHQ IDs
  filter(Name%in%seedlings_2022$SHQ_ID)%>%
  arrange(Name)
  
# Check for duplicate TCB IDs (that aren't dup SHQ IDs)
#if correct, all should have 0
name_decoder_adults %>%
  group_by(TCB_ID) %>%
  summarize(n = n()) %>%
  filter(n > 1)
name_decoder_outplanted %>%
  group_by(TCB_ID) %>%
  summarize(n = n()) %>%
  filter(n > 1)
name_decoder_2022 %>%
  group_by(TCB_ID) %>%
  summarize(n = n()) %>%
  filter(n > 1)


# Make a df of the genetic data alone (without any of the problematic SHQs above)
locus_data_adults <- final_clean_dedup_data %>%
  #filter(Name %notin% Nas_in_TCBID_adults$Name) %>%
  arrange(Name) %>% #arrange data so it's in SHQ order
  filter(Name %in% name_decoder_adults$Name)%>%
  select(-c(Name, DP_num, need_recheck)) %>% #get rid of any cols that aren't locus data
  rename_with(~str_remove(., ' - 1'))%>% #get rid of the -1 at the end of the first allele of a locus
  rename_with(~str_trim(.))

locus_data_outplanted <- final_clean_dedup_data %>%
  #filter(Name %notin% Nas_in_TCBID_outplanted$Name) %>%
  arrange(Name) %>% #arrange data so it's in SHQ order
  filter(Name %in% name_decoder_outplanted$Name)%>%
  select(-c(Name, DP_num, need_recheck)) %>% #get rid of any cols that aren't locus data
  rename_with(~str_remove(., ' - 1'))%>% #get rid of the -1 at the end of the first allele of a locus
  rename_with(~str_trim(.))

locus_data_2022 <- final_clean_dedup_data %>%
  #filter(Name %notin% Nas_in_TCBID_2022$Name) %>%
  arrange(Name) %>% #arrange data so it's in SHQ order
  filter(Name %in% name_decoder_2022$Name)%>%
  select(-c(Name, DP_num, need_recheck)) %>% #get rid of any cols that aren't locus data
  rename_with(~str_remove(., ' - 1'))%>% #get rid of the -1 at the end of the first allele of a locus
  rename_with(~str_trim(.))



####Turning data into genind object####

#Adults
i <- seq.int(1L, ncol(locus_data_adults), by = 2L) #make a vector of values that will correlate to the column number of each new locus
geno_data_adults <- as.data.frame(mapply(paste, locus_data_adults[i], locus_data_adults[i + 1], sep = "_")) #make a df where the values of each column were the values in the ith and ith + 1 column of my locus data, separated by a _, for all values of i 
genind_data_adults <- df2genind(geno_data_adults, sep = "_", ind.names = name_decoder_adults$QUBR_ID) #turn the geno_data into a genind with the designated seperator of an _ and the names coming from the name_decoder df

strata(genind_data_adults) <- data.frame(locality = name_decoder_adults$locality) #assign the locality to a strata of the genind 
setPop(genind_data_adults) <- ~locality #turn the locality strata into pop info
range(genind_data_adults@loc.n.all) #get the range of number of alleles per locus (min and max)

#OP
i <- seq.int(1L, ncol(locus_data_outplanted), by = 2L) #make a vector of values that will correlate to the column number of each new locus
geno_data_outplanted <- as.data.frame(mapply(paste, locus_data_outplanted[i], locus_data_outplanted[i + 1], sep = "_")) #make a df where the values of each column were the values in the ith and ith + 1 column of my locus data, separated by a _, for all values of i 
genind_data_outplanted <- df2genind(geno_data_outplanted, sep = "_", ind.names = name_decoder_outplanted$QUBR_ID) #turn the geno_data into a genind with the designated seperator of an _ and the names coming from the name_decoder df

strata(genind_data_outplanted) <- data.frame(locality = name_decoder_outplanted$Ranch) #assign the locality to a strata of the genind 
setPop(genind_data_outplanted) <- ~locality #turn the locality strata into pop info
range(genind_data_outplanted@loc.n.all) #get the range of number of alleles per locus 

#2022
i <- seq.int(1L, ncol(locus_data_2022), by = 2L) #make a vector of values that will correlate to the column number of each new locus
geno_data_2022 <- as.data.frame(mapply(paste, locus_data_2022[i], locus_data_2022[i + 1], sep = "_")) #make a df where the values of each column were the values in the ith and ith + 1 column of my locus data, separated by a _, for all values of i 
genind_data_2022 <- df2genind(geno_data_2022, sep = "_", ind.names = name_decoder_2022$QUBR_ID) #turn the geno_data into a genind with the designated seperator of an _ and the names coming from the name_decoder df

#strata(genind_data_2022) <- data.frame(locality = name_decoder_2022$locality) #assign the locality to a strata of the genind 
#setPop(genind_data_2022) <- ~locality #turn the locality strata into pop info
range(genind_data_2022@loc.n.all) #get the range of number of alleles per locus 

####Assigning MLL clones w/ genetic distance####
#Only for adults

## First set the repeat lengths for my loci
loci_order <- names(genind_data_adults@loc.n.all)

# Reading the csv w/ the actual repeat length info in from my working directory 
replen_info <- read_csv("~/Documents/GitHub/QUBR_exsitu_gen_diversity/Genetic Analysis/data/inputs/primer_replen_info.csv") %>%
  mutate(`Primer Name` = str_remove(`Primer Name`, 'QUVA ')) %>%
  filter(MP %in% c(1,4)) %>%
  mutate(`Primer Name` = factor(`Primer Name`, levels = loci_order)) %>%
  arrange(`Primer Name`)

# Setting the repeat length info to reflect the real repeat lengths 
replen_real <- replen_info$`Repeat length (in real scores)` 

test_replen(genind_data_adults, replen_real) #if any of these come up as false, there might be some incorrectly labelled bins in geneious that need to be edited (but it shouldn't affect future analyses very much)

replen_real <- fix_replen(genind_data_adults, replen_real, e = 1e-05, fix_some = TRUE)

### Actually assigning the clones (MLLs) in the adult data
unique(genind_data_adults@pop) #determine which pop is which

## LM
LM <- popsub(genind_data_adults,1)

# Make a genetic distance matrix for each population
gen_dists_LM <- as.matrix(bruvo.dist(popsub(genind_data_adults,1), replen = replen_real))

# Determine what genetic distance threshold is best for the given population
thresholds <- mlg.filter(LM, distance = "bruvo.dist", stats = "THRESHOLDS", replen = replen_real, algorithm = "nearest_neighbor", threshold = 1)
cutoff <- cutoff_predictor(thresholds)

LM_genclone <- as.genclone(LM)

# Use the pre-determined genetic distance threshold to assign MLLs (unique clones) to the given adult population data
mlg.filter(LM_genclone, distance = "bruvo.dist", replen = replen_real) <- cutoff
LM_genclone # look at the summary for the given population 

#length(unique(LM@mlg)) # how many unique MLLs are there in this pop
#diversity_stats(mlg.table(LM)) # what is the evenness in the spread of genotypes in this pop

## LC
LC <- popsub(genind_data_adults,2)
gen_dists_LC <- as.matrix(bruvo.dist(LC, replen =  replen_real))

LC_genclone <- as.genclone(LC)

thresholds <- mlg.filter(LC, distance = "bruvo.dist", stats = "THRESHOLDS", replen = replen_real, algorithm = "nearest_neighbor", threshold = 1)
cutoff <- cutoff_predictor(thresholds)

mlg.filter(LC_genclone, distance = "bruvo.dist", replen = replen_real) <- cutoff
LC_genclone

# length(unique(LC@mlg))
# diversity_stats(mlg.table(LC))

## SD
SD <- popsub(genind_data_adults,3)
gen_dists_SD <- as.matrix(bruvo.dist(SD, replen =  replen_real))

SD_genclone <- as.genclone(SD)

thresholds <- mlg.filter(SD, distance = "bruvo.dist", stats = "THRESHOLDS", replen = replen_real, algorithm = "nearest_neighbor", threshold = 1)
cutoff <- cutoff_predictor(thresholds)

mlg.filter(SD_genclone, distance = "bruvo.dist", replen = replen_real) <- cutoff
SD_genclone

# length(unique(SD@mlg))
# diversity_stats(mlg.table(SD))


## SDo
SDo <- popsub(genind_data_adults,4)
gen_dists_SDo <- as.matrix(bruvo.dist(SDo, replen =  replen_real))

SDo_genclone <- as.genclone(SDo)

thresholds <- mlg.filter(SDo, distance = "bruvo.dist", stats = "THRESHOLDS", replen = replen_real, algorithm = "nearest_neighbor", threshold = 1)
cutoff <- cutoff_predictor(thresholds)

mlg.filter(SDo_genclone, distance = "bruvo.dist", replen = replen_real) <- cutoff
SDo_genclone

# length(unique(SDo@mlg))
# diversity_stats(mlg.table(SDo))

## LB
LB <- popsub(genind_data_adults,5)
gen_dists_LB <- as.matrix(bruvo.dist(LB, replen =  replen_real))

LB_genclone <- as.genclone(LB)

thresholds <- mlg.filter(LB, distance = "bruvo.dist", stats = "THRESHOLDS", replen = replen_real, algorithm = "nearest_neighbor", threshold = 1)
cutoff <- cutoff_predictor(thresholds)

mlg.filter(LB_genclone, distance = "bruvo.dist", replen = replen_real) <- cutoff
LB_genclone

# length(unique(LB@mlg))
# diversity_stats(mlg.table(LB))

## EC
EC <- popsub(genind_data_adults,6)
gen_dists_EC <- as.matrix(bruvo.dist(EC, replen =  replen_real))

EC_genclone <- as.genclone(EC)

thresholds <- mlg.filter(EC, distance = "bruvo.dist", stats = "THRESHOLDS", replen = replen_real, algorithm = "nearest_neighbor", threshold = 1)
cutoff <- cutoff_predictor(thresholds)

mlg.filter(EC_genclone, distance = "bruvo.dist", replen = replen_real) <- cutoff
EC_genclone

# length(unique(EC@mlg))
# diversity_stats(mlg.table(EC))

####Perform clone correction in the adult data####
#First, recombine the now seperated pop level data by simply assigning the MLL info to the original genind

## LM
clone_info <- mll(LM)

LM_decoder <- name_decoder_adults %>%
  filter(QUBR_ID %in% indNames(LM)) %>% #keep only the QUBR IDs of the inds at the given pop in the decoder
  cbind(clone_info) #add a column with the mll assignment info at that pop

## LC
clone_info <- mll(LC)

LC_decoder <- name_decoder_adults %>%
  filter(QUBR_ID %in% indNames(LC)) %>% #keep only the QUBR IDs of the inds at the given pop in the decoder
  cbind(clone_info) #add a column with the mll assignment info at that pop

## SD
clone_info <- mll(SD)

SD_decoder <- name_decoder_adults %>%
  filter(QUBR_ID %in% indNames(SD)) %>% #keep only the QUBR IDs of the inds at the given pop in the decoder
  cbind(clone_info) #add a column with the mll assignment info at that pop

## SDo
clone_info <- mll(SDo)

SDo_decoder <- name_decoder_adults %>%
  filter(QUBR_ID %in% indNames(SDo)) %>% #keep only the QUBR IDs of the inds at the given pop in the decoder
  cbind(clone_info) #add a column with the mll assignment info at that pop

## LB
clone_info <- mll(LB)

LB_decoder <- name_decoder_adults %>%
  filter(QUBR_ID %in% indNames(LB)) %>% #keep only the QUBR IDs of the inds at the given pop in the decoder
  cbind(clone_info) #add a column with the mll assignment info at that pop

## EC
clone_info <- mll(EC)

EC_decoder <- name_decoder_adults %>%
  filter(QUBR_ID %in% indNames(EC)) %>% #keep only the QUBR IDs of the inds at the given pop in the decoder
  cbind(clone_info) #add a column with the mll assignment info at that pop

# Finally, rbind all the individual population data back together  
name_decoder_MLLs <- rbind(LM_decoder, LC_decoder, SD_decoder, SDo_decoder, LB_decoder, EC_decoder)


## Turn the genind data into a single large genclone so it can be corrected
MLL_genclone_data <- as.genclone(genind_data_adults, mlg = name_decoder_MLLs$clone_info)


# Correct all of the adult data (remove duplicate observations of the same MLL) such that all genetic diversity statistics will be performed only on the unique MLLS
MLL_corrected_data <- clonecorrect(MLL_genclone_data)


####MLL F statistics ####

# Convert genind to hierfstat format
MLL_corr_data_hierfstat <- genind2hierfstat(MLL_corrected_data)


# Calculate basic stats (includes Fis per locus and population)

#uncorrected data
MLL_basic_stats <- basic.stats(genind2hierfstat(MLL_genclone_data))

#corrected data
MLL_corr_basic_stats <- basic.stats(MLL_corr_data_hierfstat)

# Get overall mean Fis
MLL_mean_fis <- colMeans(MLL_basic_stats$Fis, na.rm = TRUE)
MLL_mean_fis


# Get overall mean Fst
MLL_overall_fst <- MLL_basic_stats$overall["Fstp"] #"unbiased" or standardized Fst designed to behave better when genetic diversity (heterozygosity) is high, which often occurs with microsatellites or when comparing few populations
MLL_overall_fst


## Number of alleles (Na) at each pop

#turn into genpop data so we can summarize easily
MLL_genpop_corr_data <- genind2genpop(MLL_corrected_data)

#totals without respect to pop
locus_table(genind_data_2022, lev = "allele")
locus_table(genind_data_outplanted, lev = "allele")
locus_table(MLL_corrected_data, lev = "allele")


#get totals at each pop
MLL_summary <- summary(MLL_genpop_corr_data)
totals <- MLL_summary$pop.n.all

#get totals of each locus at each pop
LM_corr <- clonecorrect(LM)
LC_corr <- clonecorrect(LC)
SD_corr <- clonecorrect(SD)
SDo_corr <- clonecorrect(SDo)
LB_corr <- clonecorrect(LB)
EC_corr <- clonecorrect(EC)

LM_Na <- data.frame(LM = nAll(LM_corr))
LC_Na <- data.frame(LC = nAll(LC_corr))
SD_Na <- data.frame(SD = nAll(SD_corr))
SDo_Na <- data.frame(SDo = nAll(SDo_corr))
LB_Na <- data.frame(LB = nAll(LB_corr))
EC_Na <- data.frame(EC = nAll(EC_corr))

Na_by_pop <- cbind(LM_Na, LC_Na, SD_Na, SDo_Na, LB_Na, EC_Na) %>%
  rbind(total = totals)

#comparing dataset types
Outplanted_Na <- data.frame(Outplanted = nAll(genind_data_outplanted))
#error when running below:
#changed MLL_genind to genind_data_adults
Adults_Na <- data.frame(Adults = nAll(genind_data_adults))
Progeny2022_Na <- data.frame(Progeny = nAll(genind_data_2022))

Na_by_type <- cbind(Adults_Na, Progeny2022_Na, Outplanted_Na)

colSums(Na_by_type)

locus_table(LM_corr)
locus_table(LC_corr)
locus_table(SD_corr)
locus_table(SDo_corr)
locus_table(LB_corr)
locus_table(EC_corr)


## Rareified allele richness (Ar)
# standardizes by sample size

pop(genind_data_2022) <- rep("2022", nInd(genind_data_2022))
pop(genind_data_outplanted) <- rep("OP", nInd(genind_data_outplanted))
pop(genind_data_adults) <- rep("Adult", nInd(genind_data_adults))

all_data_genind <- repool(genind_data_adults, genind_data_2022, genind_data_outplanted)

#Ar by pop and locus
Ar_adults <- allelic.richness(MLL_corr_data_hierfstat)$Ar 
colSums(Ar_adults)/num_loci

hierfstat_all <- genind2hierfstat(all_data_genind)
Ar_all <- allelic.richness(hierfstat_all)$Ar 
colSums(Ar_all)/num_loci


## Private alleles
#Private alleles
private_alleles <- tibble(private_alleles(MLL_corrected_data, form = alleles ~ ., level = "population", report = "data.frame", count.alleles = TRUE)) %>%
  rename(locus = allele) %>%
  mutate(allele = str_extract(locus,  "([^.]+)$")) %>% #extracts everything after the last "."
  mutate(locus = as.factor(str_extract(locus,  "[^\\s.]+")))  %>% #extracts everything after the before "." excluding spaces
  filter(count > 0) #keep only the info about the allles/loci that are private

private_alleles_by_locus <- private_alleles %>%
  group_by(locus, population) %>%
  summarize(num_private_alleles = n(), num_inds = sum(count), mean_num_inds = mean(count))

private_alleles %>%
  group_by(population) %>%
  summarize(num_private_alleles = n(), num_inds = sum(count), mean_num_inds = mean(count))

#for all_data
private_alleles <- tibble(private_alleles(all_data_genind, form = alleles ~ ., level = "population", report = "data.frame", count.alleles = TRUE)) %>%
  rename(locus = allele) %>%
  mutate(allele = str_extract(locus,  "([^.]+)$")) %>% #extracts everything after the last "."
  mutate(locus = as.factor(str_extract(locus,  "[^\\s.]+")))  %>% #extracts everything after the before "." excluding spaces
  filter(count > 0) #keep only the info about the allles/loci that are private

private_alleles_by_locus <- private_alleles %>%
  group_by(locus, population) %>%
  summarize(num_private_alleles = n(), num_inds = sum(count), mean_num_inds = mean(count))

private_alleles %>%
  group_by(population) %>%
  summarize(num_private_alleles = n(), num_inds = sum(count), mean_num_inds = mean(count))

####Identifying alleles####

alleles_2022 <- alleles(genind_data_2022)
alleles_OP <- alleles(genind_data_outplanted)
alleles_adults <- alleles(genind_data_adults)

adults_and_2022 <- repool(genind_data_adults, genind_data_2022)
adults_and_OP <- repool(genind_data_adults, genind_data_outplanted)

private_alleles_adults_2022 <- tibble(private_alleles(adults_and_2022, form = alleles ~ ., level = "population", report = "data.frame", count.alleles = TRUE)) %>%
  rename(locus = allele) %>%
  mutate(allele = str_extract(locus,  "([^.]+)$")) %>% #extracts everything after the last "."
  mutate(locus = as.factor(str_extract(locus,  "[^\\s.]+")))  %>% #extracts everything after the before "." excluding spaces
  filter(count > 0)%>% #keep only the info about the allles/loci that are private
  filter(population=="Adult")%>%
  mutate(locus.allele=paste0(locus, ".", allele))

allele_freq_adults_2022 <- as.tibble(makefreq(MLL_genpop_corr_data), rownames="pop")%>%
  select(c(pop, private_alleles_adults_2022$locus.allele))

pop_count_adults_2022 <- allele_freq_adults_2022%>%
  mutate(across(-c(pop), ~ .x>0))%>%
  mutate(total=rowSums(across(-c(pop))))

private_alleles_adults_OP <- tibble(private_alleles(adults_and_OP, form = alleles ~ ., level = "population", report = "data.frame", count.alleles = TRUE)) %>%
  rename(locus = allele) %>%
  mutate(allele = str_extract(locus,  "([^.]+)$")) %>% #extracts everything after the last "."
  mutate(locus = as.factor(str_extract(locus,  "[^\\s.]+")))  %>% #extracts everything after the before "." excluding spaces
  filter(count > 0)%>% #keep only the info about the allles/loci that are private
  filter(population=="Adult")%>%
  mutate(locus.allele=paste0(locus, ".", allele))

allele_freq_adults_OP <- as.tibble(makefreq(MLL_genpop_corr_data), rownames="pop")%>%
  select(c(pop, private_alleles_adults_OP$locus.allele))

pop_count_adults_OP <- allele_freq_adults_OP%>%
  mutate(across(-c(pop), ~ .x>0))%>%
  mutate(total=rowSums(across(-c(pop))))%>%
  cbind()

absent_only_from_OP <- private_alleles_adults_OP$locus.allele[which(private_alleles_adults_OP$locus.allele %notin% private_alleles_adults_2022$locus.allele)]

allele_freq_only_OP <- as.tibble(makefreq(MLL_genpop_corr_data), rownames="pop")%>%
  select(c(pop, absent_only_from_OP))

only_OP <- allele_freq_only_OP%>%
  mutate(across(-c(pop), ~ .x>0))%>%
  mutate(total=rowSums(across(-c(pop))))%>%
  cbind()





####Figures####
#standardizes table format for all
bar_chart_2022 <- pop_count_adults_2022%>%
  select(c(pop, total))%>%
  rename("2022" = total)

swap_Na_by_pop <- Na_by_pop%>%
  slice(12)%>%
  pivot_longer(c("LM", "LC", "SD", "SDo", "LB", "EC"))

bar_chart_all <- only_OP%>%
  select(c(pop, total))%>%
  rename("OP" = total)%>%
  mutate("2022" = bar_chart_2022$`2022`)%>%
  mutate("Adult" = swap_Na_by_pop$value)%>%
  mutate("Sum_Adult" = Adult - (OP + `2022`))%>%
  select(-"Adult")%>%
  pivot_longer(c("2022", "OP", "Sum_Adult"))%>%
  mutate(region = case_when(pop == "LM" | pop == "LC" ~ "North", 
                            pop == "SD" | pop == "EC" ~ "East",
                            pop == "SDo" | pop == "LB" ~ "West"))
  

bar_chart_all$name <- (ordered(bar_chart_all$name, levels=c("OP", "2022", "Sum_Adult")))
bar_chart_all$pop <- (ordered(bar_chart_all$pop, levels=c("EC", "SD", "LC", "LM", "LB", "SDo")))

bar_chart_all%>%
  ggplot+
  geom_bar(aes(x=pop, y=value, fill=name), stat="identity", position="stack")+
  xlab("population") +
  ylab(" # of alleles") +
  scale_x_discrete(labels = c("EC" = "EC \n (n=128)", "SD" = "SD \n (n=122)", "LC" = "LC \n (n=138)", "LM" = "LM \n (n=127)", "LB" = "LB \n (n=108)", "SDo" = "SDo \n (n=68)"))+
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by=20))+
  scale_fill_manual(name = "Stages", 
                    values=c('#a994b0', '#8fb8b5', '#f2e56e'),
                    labels = c("Lost after outplanting", "Lost before outplanting", "Present at all stages")) +
  
  # geom_text(label="7 (5.5%)", x="LM", y=124, size=3) +
  theme_classic()+
  facet_grid(~region, scales = "free_x")


#Are any putative polyploids from 2022 or outplanted?
#selects all ID names
gen_TCB <- TCB_QUBR_IDs%>%
  select("Tissue_ID", `Extraction Tube #`)%>%
  rename(Name = `Extraction Tube #`)

gen_putative_polyploids <- putative_polyploids%>%
  left_join(gen_TCB, by = "Name")

 

# Source - https://stackoverflow.com/a/6645506
# Posted by Ramnath, modified by community. See post 'Timeline' for change history
# Retrieved 2026-07-10, License - CC BY-SA 3.0

#ggplot(Data, aes(x = Year, y = Frequency, fill = Category, label = Frequency)) +
  #geom_bar(stat = "identity") +
  #geom_text(size = 3, position = position_stack(vjust = 0.5))



####Performing AMOVA on MLLs####
#turn the corrected genclone data into a genind again
MLL_genind <- genclone2genind(MLL_corrected_data)

#assign region info to all pops
region_info <- as.data.frame(MLL_genind@strata) %>%
  mutate(region = case_when(locality == "LM" | locality == "LC" ~ "North", 
                            locality == "SD" ~ "East"))

#assign region info to a strata in the genetic data
strata(MLL_genind) <- region_info

#run both types of amovas and compare results
#ade
MLL_amova_ade <- poppr.amova(MLL_genind, hier = ~region/locality, within = T)
MLL_amova_ade

amova.test <- randtest(MLL_amova_ade) # Test for significance
plot(amova.test)

amova.test

#pegas
MLL_amova_pegas <- poppr.amova(MLL_genind, hier = ~region/locality, within = T, method = "pegas")
MLL_amova_pegas



####Making a DAPC####
MLL_genind <- genclone2genind(MLL_corrected_data)

region_info <- as.data.frame(MLL_genind@strata) %>%
  mutate(region = case_when(locality == "LM" | locality == "LC" ~ "North", 
                            locality == "SD" | locality == "EC" ~ "East",
                            locality == "SDo" | locality == "LB" ~ "West"))

strata(MLL_genind) <- region_info

#this figure will take a long time to run
#200 PCs, 6 clusters
grps <- find.clusters(MLL_genind, max.n.clust=20, n.iter = 250000, n.start = 25) #keep all PCs then use number of pops based on the lowest BIC value (here 300 and then 3)

table(pop(MLL_genind), grps$grp) #mostly alligned but there are some inds which aren't especially between LM and LC

DAPC_grouping <- as.data.frame(grps$grp) # we can left_join this to the original data to figure out if pops match assigned ones

all_info <- rbind(LM_updated_clones_for_mapping, LC_updated_clones_for_mapping, SD_updated_clones_for_mapping) %>%
  left_join()

dapc1 <- dapc(MLL_genind, grps$grp)
#will need to figure out where we stop gaining new info (asymptote in the returned graph)
#result = ind.coord and grp.coord slots will have the coordinates of each individual in PC space

scatter(dapc1)

scatter(dapc1,1,1, bg="white",
        scree.da=FALSE, legend=TRUE, solid=.4) #looking at just the first discriminant function

compoplot(dapc1,
          txt.leg=paste("Cluster", 1:3), lab="",
          ncol=1, xlab="individuals", col=funky(6)) # used to make structure like figure


####PCoA####
gen_dists_MLLs <- as.matrix(bruvo.dist(MLL_genind, replen =  replen_real))


# Perform PCoA and extract eigenvalues
pcoa_results <- cmdscale(gen_dists_MLLs, eig = TRUE, k = 2)

# Calculate the variance explained for PC1 and PC2
variance_explained <- pcoa_results$eig / sum(pcoa_results$eig) * 100
pc1_var <- round(variance_explained[1], 1)
pc2_var <- round(variance_explained[2], 1)


# Extract PCoA coordinates
pcoa_data <- as.data.frame(pcoa_results$points)
colnames(pcoa_data) <- c("PC1", "PC2")

# Add population or grouping information (replace 'your_groups' with your actual metadata)
pcoa_data$Group <- MLL_genind@pop 
pcoa_data$Region <- MLL_genind@strata$region 


ggplot(pcoa_data, aes(x = PC1, y = PC2, color = Region)) +
  geom_point(size = 4) +
  theme_minimal() +
  labs(
    title = "PCoA of Microsatellite Data",
    x = paste0("PC1 (", pc1_var, "%)"),
    y = paste0("PC2 (", pc2_var, "%)")
  ) +
  stat_ellipse(aes(fill = Region), geom = "polygon", alpha = 0.2, show.legend = FALSE)
