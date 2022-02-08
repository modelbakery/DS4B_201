################################################################################
# Protocol: Data Understanding By Data Type & Feature-Target Interactions 
# CRISP-DM: Data Understanding  
# Date: 2021.01.24 
################################################################################

# Data Understanding -----

# Libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)

# Load Data
path_train <- "00_Data/telco_train.xlsx"
path_data_definition <- "00_Data/telco_data_definitions (1).xlsx"

train_raw_tbl <- read_excel(path_train, sheet = 1)
definition_raw_tbl <- read_excel(path_data_definition, sheet = 1, col_names = FALSE)


train_tbl <- train_raw_tbl %>% 
    janitor::clean_names()

# Pro Tips: Breakdown data collection activities in to strategic areas !!

# [1] Descriptive Features
# [2] Employment Features
# [3] Compensation Feature
# [4] Survey Results
# [5] Performance Data 
# [6] Work-live balance 
# [7] Training & Education 
# [8] Time-based Features 
glimpse(train_raw_tbl)


# Exploratory Data Analysis (EDA) ----


# Step 1: Data Summarisation 
# Pro tips: Separating your data by data type (e.g. numeric vs categorical) is a 
# great way to investigate properties of the data 
# ProTips: if number of unique categorical features is large, consider creating an "other" category 
skim(train_tbl)


# character data type 

train_tbl %>% 
    select_if(is.character) %>% 
    glimpse()

train_tbl %>% 
    select_if(is.character) %>% 
    map(unique)

# counts $ proportions: 
# map() + table()  
# map() + table() + prop.table()

# Anonymous Tidy function
# Can make anonymous functions that are not pre-defined (hence anonymous). The begin with the tilde (~) and take the dot(.) as the argument 
train_tbl %>% 
    select_if(is.character) %>% 
    map(~table(.) %>% prop.table())


# Inspect Numeric Levels
# map() + unique() + length()
train_tbl %>% 
    select_if(is.numeric) %>% 
    map(~unique(.))

train_tbl %>% 
    select_if(is.numeric) %>% 
    map(~unique(.) %>% length())


# map_df(): works exactly like map() except attempts to convert the list output to a dataframe
# Varaibles with only one level are non-essential variables (also called zero-variance features).
# These features are not useful to when modeling

# Numeric variables that are lower in levels are likely to be discrete, and numeric variables that are
# higher in levels are likely to be continuous 


train_tbl %>% 
    select_if(is.numeric) %>% 
    map_df(~unique(.) %>% length()) %>% 
    gather(key = varaiable, value = no_unique_value) %>% 
    arrange(no_unique_value) %>% 
    # Likely to be discrete
    filter(no_unique_value <= 10)


train_tbl %>% 
    select_if(is.numeric) %>% 
    map_df(~unique(.) %>% length()) %>% 
    gather(key = varaiable, value = no_unique_value) %>% 
    arrange(no_unique_value) %>% 
    # likely to be continuous 
    filter(no_unique_value > 10)


# GGally - extend to ggplot2 

# Step 2: Data Visualisation ----
# diagnoally - shows the population with in that feature 
train_tbl %>% 
    select(attrition, age, gender, marital_status, num_companies_worked, over18, distance_from_home) %>% 
    ggpairs()

train_tbl %>% 
    select(attrition, age, gender, marital_status, num_companies_worked, over18, distance_from_home) %>% 
    ggpairs(aes(colour = attrition), lower = "blank", lengend = 1,
            diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
    theme(legend.position = "bottom")



plot_ggpairs <- function(data, colour = NULL, density_alpha = 0.5){
    
    colour_expr <- enquo(colour)
    
    if (rlang::quo_is_null(colour_expr)) {
        # rlang::quo_is_null(): returns TRUE if the quosure contains a NULL value
        g <- data %>% 
            ggpairs(lower = "blank")
        
    } else {
        
        colour_name <- quo_name(colour_expr)
        
        g <- data %>% 
            ggpairs(mapping = aes_string(colour = colour_name), 
                    lower = "blank", lengend = 1,
                    diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
            theme(legend.position = "bottom")
    }
    
    return(g)
    
}

train_tbl %>% 
    select(attrition, age, gender, marital_status, num_companies_worked, over18, distance_from_home) %>% 
    plot_ggpairs(colour = attrition)

# Explore Features by Category

#   1. Descriptive features: age, gender, marital status 
train_tbl %>%
    select(attrition, age, gender, marital_status, num_companies_worked, over18, distance_from_home) %>%
    plot_ggpairs(attrition)

#   2. Employment features: department, job role, job level
train_tbl %>%
    select(attrition, contains("employee"), contains("department"), contains("job")) %>%
    plot_ggpairs(attrition) 

#   3. Compensation features: HourlyRate, MonthlyIncome, StockOptionLevel 
train_tbl %>%
    select(attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
    plot_ggpairs(attrition)

#   4. Survey Results: Satisfaction level, WorkLifeBalance 
train_tbl %>%
    select(attrition, contains("satisfaction"), contains("life")) %>%
    plot_ggpairs(attrition)

#   5. Performance Data: Job Involvment, Performance Rating
train_tbl %>%
    select(attrition, contains("performance"), contains("involvement")) %>%
    plot_ggpairs(attrition)

#   6. Work-Life Features 
train_tbl %>%
    select(attrition, contains("over_time"), contains("travel")) %>%
    plot_ggpairs(attrition)

#   7. Training and Education 
train_tbl %>%
    select(attrition, contains("training"), contains("education")) %>%
    plot_ggpairs(attrition)

#   8. Time-Based Features: Years at company, years in current role
train_tbl %>%
    select(attrition, contains("years")) %>%
    plot_ggpairs(attrition)


    







