library(fs)

make_project_dir <- function() {
    dir_names <- c(
        "00_Data",
        "00_Scripts",
        "00_Business_Understanding",
        "00_Data_Understanding",
        "00_Data_Preparation",
        "00_Modeling",
        "00_Evaluation",
        "00_Deployment")
    
    dir_create(dir_names)
    
    dir_ls()
}

# Use 
dir_delete() # to delete any unnesccary directories 

# Our first script file 
getwd("/Users/seunghyunsung/Desktop/University_business_science/DS4B_201/HR_201")

