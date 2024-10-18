#load library
library(tidyverse)

# import the dataset chronic_kidney_disease_full.arff.txt
df3 <- read_delim(file = "./data/chronic_kidney_disease_full.arff.txt",
                  delim = ",",
                  col_names = FALSE,           
                  na = "?",                    
                  skip = 145)                  


#Assign column names 
col_names <- c("age", "Blood Pressure", "Specific Gravity", "Albumin", "Sugar", "Red Blood Cells",
               "Pus Cell", "Pus Cell clumps", "Bacteria", "Blood Glucose Random", "Blood Urea", "Serum Creatinine",
               "Sodium", "Potassium", "Hemoglobin","Packed  Cell Volume", "White Blood Cell Count cells/cumm", 
               "Red Blood Cell Count millions/cmm", "Hypertension", "Diabetes Mellitus", "Coronary Artery Disease",
               "Appetite","Pedal Edema", "Anemia", "Class")

colnames(df3) <- col_names

#correct way to import chronic_kidney_disease_full.arff.txt
main_data <- read_delim(file = "./data/chronic_kidney_disease_full.arff.txt",
                        col_names = F,
                        delim = ",",
                        na =c("?", "NA"),
                        skip=145)
#renaming columns
colnames(main_data) <- c("age", "Blood Pressure", "Specific Gravity", "Albumin", "Sugar", "Red Blood Cells",
                         "Pus Cell", "Pus Cell clumps", "Bacteria", "Blood Glucose Random", "Blood Urea", "Serum Creatinine",
                         "Sodium", "Potassium", "Hemoglobin","Packed  Cell Volume", "White Blood Cell Count cells/cumm", 
                         "Red Blood Cell Count millions/cmm", "Hypertension", "Diabetes Mellitus", "Coronary Artery Disease",
                         "Appetite","Pedal Edema", "Anemia", "Class")

#creating a copy of the main dataset
main_data <- df3

#clean variable names
library(janitor)
clean_names(main_data)
main_data <- main_data%>% clean_names()
colnames(main_data)
colnames(df3)

#selecting variables
main_data %>% select(age, blood_pressure)
df1 %>% select(age, `Blood Pressure`)

#display unique values in categorical data
cat_col <- names(main_data)[sapply(main_data, function(x) is.character(x))]

for (col in cat_col) {
  cat(paste(col, "has" , unique(main_data[col]), "values\n"))
  
}

num <- 1:5
for (x in num) {
  print(x)
#correct values in the diabetes mellitus column
  main_data <- main_data %>% mutate(
    new_diabete_mellitus = case_when(
      diabetes_mellitus ==" yes" ~ "yes",
      diabetes_mellitus =="\tno" ~ "no",
      diabetes_mellitus =="\tyes" ~ "yes",
      diabetes_mellitus == "" ~ NA,
      .default= diabetes_mellitus
      
  ))
x <- select(main_data, diabetes_mellitus, new_diabete_mellitus)
filter(x, diabetes_mellitus %in% c(" yes","\tno","\tyes", ""))
main_data %>% select(diabetes_mellitus, new_diabete_mellitus) %>% 
  filter(diabetes_mellitus %in% c(" yes","\tno","\tyes", ""))

#correcting values in white_blood_cell_count
main_data <- main_data %>% mutate(
  new_white_blood_cell_count = case_when(
    white_blood_cell_count_cells_cumm == "\t6200" ~ "6200",
    white_blood_cell_count_cells_cumm == "\t8400" ~ "8400",
    white_blood_cell_count_cells_cumm == "\t?" ~ "NA",
    .default = white_blood_cell_count_cells_cumm
  ))

#selecting and filtering white_blood_cell count and new_white_blood_cell_count
main_data %>% select(white_blood_cell_count_cells_cumm, new_white_blood_cell_count) %>% 
  filter(white_blood_cell_count_cells_cumm %in% c("\t6200", "\t8400", "\t?"))

#correcting values in class
main_data <- main_data %>% mutate(
  new_class = case_when(
    class == "ckd\t" ~ "ckd",
    class == "ckd," ~ "ckd",
    class == "no,notckd" ~ "notckd",
    .default = class
  ))

#selecting and filtering class
main_data %>% select(class, new_class) %>% 
  filter(class %in% c("ckd\t", "ckd,", "no,notckd"))

#selecting and filtering white_blood_cell count and new_white_blood_cell_count
main_data %>% select(white_blood_cell_count, new_white_blood_cell_count) %>% 
  filter(white_blood_cell_count %in% c("\t6200", "\t8400", "\t?"))

#correcting values in red_blood_cell_count
main_data <- main_data %>% mutate(
  red_blood_cell_count_millions_cmm = case_when(
    red_blood_cell_count_millions_cmm == "\t?" ~ "NA",
    .default = red_blood_cell_count_millions_cmm
  ))

#selecting and filtering red_blood_cell count 
main_data %>% select(red_blood_cell_count_millions_cmm, new_white_blood_cell_count) %>% 
  filter(red_blood_cell_count_millions_cmm %in% c("\t6200", "\t8400", "\t?"))
