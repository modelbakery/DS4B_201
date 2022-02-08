##############################################################################
# Data Preparation For Machine With Recipes 
##############################################################################
# Data Preparation ---
# Machine Readable ---

# Setip ---

# Libraries 
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)


# Load Data
path_train <- "00_Data/telco_train.xlsx"
path_test <- "00_Data/telco_test (1).xlsx"
path_data_definition <- "00_Data/telco_data_definitions (1).xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
test_raw_tbl  <- read_excel(path_test, sheet = 1)
definition_raw_tbl <- read_excel(path_data_definition, sheet = 1)


# Processing Piplines 
source("03_Data_Preparation/data_preparation_part_1_human_readable.R")
train_readable_tbl  <- process_hr_data_readable(train_raw_tbl, definition_raw_tbl)
test_readable_tbl   <- process_hr_data_readable(test_raw_tbl, definition_raw_tbl)

# Plot Faceted Histogram -> pre-planning feature engineering 
source("00_Scripts/plot_hist_facet.R")
train_readable_tbl %>% 
    select(Attrition, everything()) %>% 
    plot_hist_facet(bins = 10, ncol = 5, fct_rev = F)


# Data Preprocessing With Recipes ----

# Plan: Correlation Analysis 
# 1. Zero Varaince Feature 
# 2. Transformation 
# 3. Center/Scales -> before dummying 
# 4. Dummy Varaiables

skewed_feature_names <- train_readable_tbl %>% 
    select_if(is.numeric) %>% 
    map_df(skewness) %>% 
    gather(key = factor, value = skewness) %>% 
    arrange(desc(skewness)) %>% 
    filter(skewness >= 0.8) %>% 
    filter(!factor %in% c("JobLevel", "StockOptionLevel")) %>% 
    pull(factor) %>% 
    as.character()

train_readable_tbl %>% 
    select(skewed_feature_names) %>% 
    plot_hist_facet()

factor_names <- c("JobLevel", "StockOptionLevel")

# --Set up - recipe template 
recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
    # 1. Zero Varaince Feature 
    step_zv(all_predictors()) %>% 
    # 2. Transformation 
    step_YeoJohnson(skewed_feature_names) %>% 
    # 3. Convert selected feature to as.factor 
    step_mutate_at(factor_names, fn = as.factor) %>% 
    # 4. Centers
    step_center(all_numeric()) %>% 
    # 5. Scales
    step_scale(all_numeric()) %>% 
    # 6. Dummy variables -> all factor variables 
    # -- expands to d(n-1) 
    # -- If a factorhas 3 levels, the feature is expanded into 2 columns 
    #    (1 less than total number of levels)
    step_dummy(all_nominal())
    
prepared_recipe <- recipe_obj %>% prep()
# For step 4 
prepared_recipe$steps[[4]]


# Final Recipe ---- 
recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
    step_zv(all_predictors()) %>% 
    step_YeoJohnson(skewed_feature_names) %>% 
    step_mutate_at(factor_names, fn = as.factor) %>% 
    step_center(all_numeric()) %>% 
    step_scale(all_numeric()) %>% 
    step_dummy(all_nominal()) %>% 
    prep()

# Process Recipes on Train/Test dataset 
train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

train_tbl %>% glimpse()

recipe_obj %>% 
    tidy()








