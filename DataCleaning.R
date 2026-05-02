library(tidyverse)
library(readr)

# Define the 14 column names based on the dataset documentation
col_names <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", 
               "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")

# Read the data, explicitly telling R to treat "?" and "-9.0" as NA (missing values)
heart_data <- read_csv("processed.cleveland.data", 
                       col_names = col_names, 
                       na = c("?", "-9.0"))
clean_data <- heart_data %>%
  mutate(
    # Convert Sex (1 = male; 0 = female)
    sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
    
    # Convert Chest Pain Type (cp)
    cp = factor(cp, levels = c(1, 2, 3, 4), 
                labels = c("Typical Angina", "Atypical Angina", "Non-anginal Pain", "Asymptomatic")),
    
    # Convert Fasting Blood Sugar (fbs) (> 120 mg/dl: 1 = true; 0 = false)
    fbs = factor(fbs, levels = c(0, 1), labels = c("False", "True")),
    
    # Convert Resting Electrocardiographic Results (restecg)
    restecg = factor(restecg, levels = c(0, 1, 2), 
                     labels = c("Normal", "ST-T Abnormality", "LV Hypertrophy")),
    
    # Convert Exercise Induced Angina (exang) (1 = yes; 0 = no)
    exang = factor(exang, levels = c(0, 1), labels = c("No", "Yes")),
    
    # Convert the slope of the peak exercise ST segment (slope)
    slope = factor(slope, levels = c(1, 2, 3), 
                   labels = c("Upsloping", "Flat", "Downsloping")),
    
    # Convert Thal (3 = normal; 6 = fixed defect; 7 = reversable defect)
    thal = factor(thal, levels = c(3, 6, 7), 
                  labels = c("Normal", "Fixed Defect", "Reversable Defect"))
  )
clean_data <- clean_data %>%
  mutate(
    # Create a new binary target variable
    heart_disease = factor(ifelse(num > 0, "Presence", "Absence"), 
                           levels = c("Absence", "Presence"))
  )
# Drop rows with any missing values
final_clean_data <- clean_data %>%
  drop_na()

# Save the cleaned dataset to be used in your app.R
write_csv(final_clean_data, "cleaned_heart_data.csv")

