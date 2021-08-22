## ADHD SST Meta-analysis screening ##

###################################

# load libraries
install.packages("revtools")
install.packages("stringr")
install.packages("tidyverse")
library(dplyr)
library(revtools)
library(stringr)

# set working directory to where your RIS files are located
setwd("/home/mervyns/screen")

# read all RIS files into a single dataframe
file_names <- list.files(pattern = ".ris")
all <- read_bibliography(file_names)
nrow(all) # no. of rows
# write to csv
write.csv(all, file = "all_articles.csv")
save.image("NEW_ADHDscreen.Rdata")

##########

# DEDUPLICATION ---

# NOTE: Due to the extensive processing time involved, Deduplication is done separately using job scripts submitted to a SLURM scheduler
# Jobs scripts are labelled dup.sh and dup2.sh for phase 1 and phase 2 respectively

# Phase 1
# based on doi
duplicates <- find_duplicates(all)
# extract unique references
doi_duplicates_removed <- extract_unique_references(all, duplicates)
nrow(doi_duplicates_removed)
nrow(all)-nrow(doi_duplicates_removed) # how many duplicates were removed
# write to csv
write.csv(doi_duplicates_removed, file = "doi_duplicates_removed.csv")
save.image("NEW_ADHDscreen.Rdata")

# Phase 2
# based on title (fuzzy matching)
duplicates2 <- find_duplicates(doi_duplicates_removed, match_variable = "title")
# extract unique references
title_doi_duplicates_removed <- extract_unique_references(doi_duplicates_removed, duplicates2)
nrow(title_doi_duplicates_removed)
nrow(doi_duplicates_removed)-nrow(title_doi_duplicates_removed) # how many duplicates were removed from Phase 1
nrow(all)-nrow(title_doi_duplicates_removed) # how many duplicates were removed overall
# write to csv
write.csv(title_doi_duplicates_removed, file = "title_doi_duplicates_removed.csv")


deduplicated_data <- title_doi_duplicates_removed # save a dataframe for the deduplicated data (save to environment)
data <- deduplicated_data #set 'data' as the dataframe to filter
data$unique_value <-  1:nrow(data) #create a unique value to help with the filtering process

save.image("Dedup.Rdata")
save.image("NEW_ADHDscreen.Rdata")

##########

# FILTERING ----
  #Note that filtering is done in two steps:
    #We first remove papers based on the absence of information
    #We then remove papers based on the presence of information
    #While we create dataframes to estimate the number of papers removed at each step, we do NOT remove them from the dataframe until the very end
      #this controls for the fact that that filters will overlap, and will allow us to assess the overlap if need be
    #After filtering is completed, we apply all the filters to the main dataframe (see section below)

# Filtering based on absence ----------------------------------------------

# 1. Remove papers where there is an absence of information ---------------

does_not_contain_author <- subset(data, is.na(data$author)) #no author
does_not_contain_journal <- subset(data, is.na(data$journal)) #no journal

data_excluding_absence_of_features <- subset(data, !data$unique_value %in% c(does_not_contain_author$unique_value,
                                                                             does_not_contain_journal$unique_value))

n_removed_due_to_absence_of_features <- nrow(data) - nrow(data_excluding_absence_of_features)

#identify the unique ids of the rows removed
unique_values_for_rows_removed_for_absence_of_features <- data$unique_value[!data$unique_value %in% data_excluding_absence_of_features$unique_value]

#temp <- data$title[!data$unique_value %in% data_excluding_absence_of_features$unique_value] #identify the unique ids of the rows removed


# 2. Remove papers where there is no ADHD in abstract -----------

does_not_contain_adhd_abs <- data[!str_detect(data$abstract, regex("attention deficit disorder with hyperactivity|attention deficit hyperactivity disorder|attention predominately hyperactive|impulsive|impusivity|impulsiveness|hyperactive|hyperactivity|attention deficit|attention deficit disorder|diagnosed with hyperactivity|attentional disorder|inattention|inattentiveness|hyperkinesis|hyperkinetic| hyperkinetic disorder|hyperkinetic disorders|ADHD|ADD|AD-HD|AD/HD|ADHD-I|ADHD/I|ADHD-IA|ADHD/IA|ADHD-HI|ADHD/HI|ADHD-H|ADHD/H|ADHD-C|ADHD/C|ADDH|impulsive people", ignore_case = TRUE, negate = FALSE)),]

data_excluding_absence_of_adhd_abs <- subset(data, !data$unique_value %in% c(does_not_contain_adhd_abs$unique_value))

n_removed_due_to_no_adhd_abs <- nrow(data) - nrow(data_excluding_absence_of_adhd_abs)

#identify the unique ids of the rows removed
unique_values_for_rows_removed_for_no_adhd_abs <- data$unique_value[!data$unique_value %in% data_excluding_absence_of_adhd_abs$unique_value]


# 3. Remove papers where there is no SST in abstract -----------

#does_not_contain_sst_abs <- data[!str_detect(data$abstract, regex("stop signal task|stop-signal task|stop task|stop-task|stop signal|stop-signal|stop paradigm|stop-paradigm|SST|inhibtion task|inhibit task|inhibit attention task|inhibit attentional task|response inhibition|response control|inhibtion control|inhibit control|behavioural inhibition|behavioral inhibition|motor inhibition|action restraint|action cancellation|executive function|executive functioning|executive control|cognitive control|motor control|inhibit*", ignore_case = TRUE, negate = FALSE)),]

new_does_not_contain_sst_abs <- data[!str_detect(data$abstract, regex("stop signal task|stop-signal task|stop task|stop-task|stop signal|stop-signal|stop paradigm|stop-paradigm|SST|inhibtion task|inhibit task|inhibit attention task|inhibit attentional task|response inhibition|response control|inhibtion control|inhibit control|behavioural inhibition|behavioral inhibition|motor inhibition|action restraint|action cancellation|executive function|executive functioning|executive control|cognitive control|motor control|inhibit*|stop*", ignore_case = TRUE, negate = FALSE)),]


data_excluding_absence_of_sst_abs <- subset(data, !data$unique_value %in% c(new_does_not_contain_sst_abs$unique_value))

n_removed_due_to_no_sst_abs <- nrow(data) - nrow(data_excluding_absence_of_sst_abs)

#identify the unique ids of the rows removed
unique_values_for_rows_removed_for_no_sst_abs <- data$unique_value[!data$unique_value %in% data_excluding_absence_of_sst_abs$unique_value]



# Filtering based on presence ---------------------------------------------

# 4. Remove papers where the type is wrong ----

is_abst <- data[data$type=="ABST",] #is abstract
is_book <- data[data$type=="BOOK",] #is book
is_case <- data[data$type=="CASE",] #is case study
is_chap <- data[data$type=="CHAP",] # is book chapter
is_conf <- data[data$type=="CONF",] #is conference
is_gen <- data[data$type=="GEN",] # is gen
is_rprt <- data[data$type=="RPRT",] # is report
is_thesis <- data[data$type=="THES",] #is thesis

#calculate how many papers were removed
data_excluding_presence_of_wrong_type <- subset(data, !data$unique_value %in% c(is_abst$unique_value,
                                                                                is_book$unique_value,
                                                                                is_case$unique_value,
                                                                                is_chap$unique_value,
                                                                                is_conf$unique_value,
                                                                                is_gen$unique_value,
                                                                                is_rprt$unique_value,
                                                                                is_thesis$unique_value))

n_removed_due_to_wrong_type <- nrow(data) - nrow(data_excluding_presence_of_wrong_type)

#identify the unique ids of the rows removed
unique_values_for_rows_removed_for_presence_of_wrong_type <- data$unique_value[!data$unique_value %in% data_excluding_presence_of_wrong_type$unique_value] #identify the unique ids of the rows removed


# 5. Remove papers that are either reviews, meta-analyses or conference papers ----

contain_review_or_meta_title <- data[str_detect(data$title, regex("review|meta-analysis", ignore_case = TRUE, negate = FALSE)),]


contain_review_or_meta_abs <- data[str_detect(data$abstract, regex("we review|this meta-analysis|our meta-analysis", ignore_case = TRUE, negate = FALSE)),]


contain_conference_abs <- data[str_detect(data$abstract, regex("conference", ignore_case = TRUE, negate = FALSE)),]

data_excluding_presence_of_reviews_metas_or_conf_papers <- subset(data, !data$unique_value %in% c(contain_review_or_meta_title$unique_value,
                                                                                                  contain_review_or_meta_abs$unique_value,
                                                                                                  contain_conference_abs$unique_value))

n_removed_due_to_excluding_reviews_metas_or_conf_papers <- nrow(data) - nrow(data_excluding_presence_of_reviews_metas_or_conf_papers)

unique_values_for_rows_removed_for_metas_or_conf_papers <- data$unique_value[!data$unique_value %in% data_excluding_presence_of_reviews_metas_or_conf_papers$unique_value] #identify the unique ids of the rows removed


# 6. Remove papers that are animal studies ----

contain_animal_title <- data[str_detect(data$title, regex("rodent|rodents|rats|mouse|mice|animal|animals|monkey|monkeys|squirrel|squirrels|primate|primates|sheep", ignore_case = TRUE, negate = FALSE)),]

contain_animal_abs <- data[str_detect(data$abstract, regex("rodent|rodents|rats|mouse|mice|animal|animals|monkey|monkeys|squirrel|squirrels|primate|primates|sheep", ignore_case = TRUE, negate = FALSE)),]


data_excluding_presence_of_animal_studies <- subset(data, !data$unique_value %in% c(contain_animal_title$unique_value,
                                                                                    contain_animal_abs$unique_value))

n_removed_due_to_being_animal_study <- nrow(data) - nrow(data_excluding_presence_of_animal_studies)

unique_values_for_rows_removed_for_being_animal_study <- data$unique_value[!data$unique_value %in% data_excluding_presence_of_animal_studies$unique_value] #identify the unique ids of the rows removed

save.image("NEW_ADHDscreen.Rdata")


#Apply all the filters to the main dataframe - leaving the remaining dataframe to be manually sorted ----
filtered_data <- data[!data$unique_value %in% c(unique_values_for_rows_removed_for_absence_of_features, #1.
                                                unique_values_for_rows_removed_for_no_adhd_abs, #2.
                                                unique_values_for_rows_removed_for_no_sst_abs, #3.
                                                unique_values_for_rows_removed_for_presence_of_wrong_type, #4.
                                                unique_values_for_rows_removed_for_metas_or_conf_papers, #5.
                                                unique_values_for_rows_removed_for_being_animal_study),] #6.

nrow(data) - nrow(filtered_data) #numnber of papers removed due to filtering


# Save filtered dataframe
write.csv(filtered_data, file = "V3_REGEX_for_manual_sorting.csv")

save.image("V3_RGEX_ADHDscreen.Rdata")

# Export filtered dataframe to RIS format

write_bibliography(filtered_data, filename = "Phase1_filtered.ris", format = "ris")

### END ###
