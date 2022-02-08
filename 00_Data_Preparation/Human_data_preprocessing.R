################################################################################
# Protocol: Data Preparation: Getting Data Ready For People & Machines 
# Human Readable 
# CRISP-DM: Data Preparation  
# Date: 2021.01.24 
################################################################################

# Data Preparation -----

# Libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(stringr)
library(forcats)

# Load Data
path_train <- "00_Data/telco_train.xlsx"
path_data_definition <- "00_Data/telco_data_definitions (1).xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
definition_raw_tbl <- read_excel(path_data_definition, sheet = 1, col_names = FALSE)


# Tidying The Data 
train_raw_tbl %>% glimpse()

# fill(): Replaces missing values (NAs) with the closest entry (previous if. direction =
# "down" or next if .direction = "up")
definition_tbl <- definition_raw_tbl %>% 
    setNames(c("column_1", "column_2")) %>% 
    fill(column_1, .direction = "down") %>% 
    filter(!is.na(column_2)) %>% 
    separate(column_2, into = c("key", "value"), sep = " '", remove = T) %>% 
    rename(column_name = column_1) %>% 
    mutate(key = as.numeric(key)) %>% 
    mutate(value = value %>% str_replace("'", "")) 
    

definition_list <- definition_tbl %>% 
    split(.$column_name) %>% 
    map(~select(., -column_name)) %>% 
    map(~mutate(., value = as_factor(value)))

definition_list[[1]]


for (i in seq_along(definition_list)) {
    list_name <- names(definition_list)[i]
    colnames(definition_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
}
definition_list


# ProTips: Use lists to collect objects that need to be iterated over. use purrr functions to iterate 

list(HR_Data = train_raw_tbl) %>% 
    append(definition_list, after = 1) %>% 
    reduce(left_join) %>% 
    select(-one_of(names(definition_list))) %>% glimpse()

data_merged_tbl <- list(HR_Data = train_raw_tbl) %>% 
    append(definition_list, after = 1) %>% 
    reduce(left_join) %>% 
    select(-one_of(names(definition_list))) %>% 
    set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>% 
    select(sort(names(.)))

data_merged_tbl %>% glimpse()

data_merged_tbl %>% 
    select_if(is.character) %>% 
    glimpse()

# Converting Character to Factor 
data_merged_tbl %>% 
    distinct(BusinessTravel)


data_processed_tbl <- data_merged_tbl %>% 
    mutate_if(is.character, as.factor) %>% 
    select_if(is.factor) %>% 
    mutate(
        BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", "Travel_Rarely", "Travel_Frequently"),
        MaritalStatus  = MaritalStatus  %>% fct_relevel("Single",  "Married", "Divorced")
    )

data_processed_tbl %>% 
    select_if(is.factor) %>% 
    map(levels)


# Processing Piplines 
definition_raw_tbl -> definition_tbl 
train_raw_tbl -> data
process_hr_data_readable <- function(data, definition_tbl){
    
    definition_list <- definition_tbl %>% 
        setNames(c("column_1", "column_2")) %>% 
        fill(column_1, .direction = "down") %>% 
        filter(!is.na(column_2)) %>% 
        separate(column_2, into = c("key", "value"), sep = " '", remove = T) %>% 
        rename(column_name = column_1) %>% 
        mutate(key = as.numeric(key)) %>% 
        mutate(value = value %>% str_replace("'", "")) %>% 
        
        split(.$column_name) %>% 
        map(~select(., -column_name)) %>% 
        map(~mutate(., value = as_factor(value)))
    
    for (i in seq_along(definition_list)) {
        list_name <- names(definition_list)[i]
        colnames(definition_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
    }
    
    data_merged_tbl <- list(HR_Data = data) %>% 
        append(definition_list, after = 1) %>% 
        reduce(left_join) %>% 
        select(-one_of(names(definition_list))) %>% 
        set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>% 
        select(sort(names(.))) %>% 
        mutate_if(is.character, as.factor) %>% 
        select_if(is.factor) %>% 
        mutate(
            BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", "Travel_Rarely", "Travel_Frequently"),
            MaritalStatus  = MaritalStatus  %>% fct_relevel("Single",  "Married", "Divorced")
        )
    return(data_merged_tbl)
}

train_raw_tbl %>% process_hr_data_readable(definition_tbl = definition_raw_tbl) %>% 
    glimpse()


source("00_Scripts/data_processing_pipeline.R")

train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definition_raw_tbl)


# Eduction 
# Before 
train_raw_tbl %>% 
    ggplot(aes(Education)) +
    geom_bar()

# After 
train_readable_tbl %>% 
    ggplot(aes(Education)) +
    geom_bar()


# Business Travel  
# Before 
# Has label identified but not in the correct order 
# Order: Non -> Rarely -> Frequently 
# low -> medium -> High 
train_raw_tbl %>% 
    ggplot(aes(BusinessTravel)) +
    geom_bar()



# After 
train_readable_tbl %>% 
    ggplot(aes(BusinessTravel)) +
    geom_bar()



