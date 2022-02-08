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

