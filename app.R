# ============================================================
# DASC3240 Final Project: Heart Disease Risk Explorer
# Group Members: [Your Name], [Member 2], [Member 3], [Member 4]
# Dataset: UCI Heart Disease (Cleveland) - License: CC BY 4.0
# ============================================================

# Load required libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# ============================================================
# DATA LOADING & CLEANING
# ============================================================

# Define column names based on the data dictionary (heart-disease.names)
col_names <- c(
  "age",       # Age in years
  "sex",       # 1 = male, 0 = female
  "cp",        # Chest pain type (1-4)
  "trestbps",  # Resting blood pressure (mm Hg)
  "chol",      # Serum cholesterol (mg/dl)
  "fbs",       # Fasting blood sugar > 120 mg/dl (1 = true, 0 = false)
  "restecg",   # Resting ECG results (0-2)
  "thalach",   # Maximum heart rate achieved
  "exang",     # Exercise induced angina (1 = yes, 0 = no)
  "oldpeak",   # ST depression induced by exercise relative to rest
  "slope",     # Slope of peak exercise ST segment (1-3)
  "ca",        # Number of major vessels colored by fluoroscopy (0-3)
  "thal",      # Thalassemia (3 = normal, 6 = fixed defect, 7 = reversible defect)
  "num"        # Diagnosis of heart disease (0 = no, 1-4 = yes)
)

# Read the data (note: the file uses "?" for missing values)
heart_raw <- read.csv("processed.cleveland.data", 
                      header = FALSE, 
                      col.names = col_names,
                      na.strings = "?")  # Convert "?" to NA

# Data cleaning steps
heart_clean <- heart_raw %>%
  # Convert categorical variables to factors with meaningful labels
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
    cp = factor(cp, levels = 1:4, 
                labels = c("Typical Angina", "Atypical Angina", 
                           "Non-anginal Pain", "Asymptomatic")),
    fbs = factor(fbs, levels = c(0, 1), labels = c("≤ 120 mg/dl", "> 120 mg/dl")),
    restecg = factor(restecg, levels = 0:2,
                     labels = c("Normal", "ST-T Abnormality", "LV Hypertrophy")),
    exang = factor(exang, levels = c(0, 1), labels = c("No", "Yes")),
    slope = factor(slope, levels = 1:3,
                   labels = c("Upsloping", "Flat", "Downsloping")),
    thal = factor(thal, levels = c(3, 6, 7),
                  labels = c("Normal", "Fixed Defect", "Reversible Defect")),
    # Create binary target for easier interpretation
    heart_disease = factor(ifelse(num > 0, "Disease", "No Disease"))
  ) %>%
  # Remove rows with missing values in ca and thal (documented in UCI notes)
  na.omit()

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  titlePanel("Heart Disease Risk Explorer"),
  h4("Exploring the UCI Heart Disease Dataset (Cleveland)"),
  
  tabsetPanel(
    # Tab 1: Introduction
    tabPanel("Introduction",
             h3("About This App"),
             p("This app explores the UCI Heart Disease dataset to answer:"),
             strong("Which patient characteristics are most strongly associated with a diagnosis of heart disease?"),
             h4("Dataset Information"),
             p("- Source: UCI Machine Learning Repository (CC BY 4.0)"),
             p("- 303 patients from Cleveland Clinic"),
             p("- 13 clinical variables + heart disease diagnosis"),
             p("- Missing values in 'ca' and 'thal' were removed (documented in data dictionary)")
    ),
    
    # Tab 2: Data Exploration
    tabPanel("Data Exploration",
             sidebarLayout(
               sidebarPanel(
                 h4("Explore Variables"),
                 selectInput("xvar", "X-axis Variable:",
                             choices = c("age", "trestbps", "chol", "thalach", "oldpeak")),
                 selectInput("color_by", "Color by:",
                             choices = c("heart_disease", "sex", "cp")),
                 sliderInput("age_range", "Age Range:",
                             min = 29, max = 77, value = c(29, 77))
               ),
               mainPanel(
                 plotlyOutput("explore_plot"),
                 p("Hover over points to see details. Zoom by dragging a rectangle.")
               )
             )
    ),
    
    # Tab 3: Risk Factor Analysis
    tabPanel("Risk Factor Analysis",
             sidebarLayout(
               sidebarPanel(
                 h4("Build a Patient Profile"),
                 selectInput("patient_sex", "Sex:",
                             choices = c("Female", "Male")),
                 selectInput("patient_cp", "Chest Pain Type:",
                             choices = c("Typical Angina", "Atypical Angina", 
                                         "Non-anginal Pain", "Asymptomatic")),
                 sliderInput("patient_age", "Age:",
                             min = 29, max = 77, value = 55),
                 sliderInput("patient_thalach", "Max Heart Rate:",
                             min = 71, max = 202, value = 150)
               ),
               mainPanel(
                 plotlyOutput("risk_plot"),
                 p("This plot shows where your patient profile falls compared to diagnosed patients.")
               )
             )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output) {
  
  # Reactive: Filter data based on age range
  filtered_data <- reactive({
    heart_clean %>%
      filter(age >= input$age_range[1], age <= input$age_range[2])
  })
  
  # Exploration plot
  output$explore_plot <- renderPlotly({
    p <- ggplot(filtered_data(), 
                aes_string(x = input$xvar, y = "thalach", 
                           color = input$color_by)) +
      geom_point(size = 2, alpha = 0.7) +
      labs(x = input$xvar,
           y = "Max Heart Rate (thalach)",
           color = input$color_by,
           title = paste("Max Heart Rate vs", input$xvar)) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
  })
  
  # Risk factor analysis plot
  output$risk_plot <- renderPlotly({
    # Filter data matching user's patient profile
    similar_patients <- heart_clean %>%
      filter(sex == input$patient_sex,
             cp == input$patient_cp)
    
    p <- ggplot(heart_clean, aes(x = age, y = thalach, color = heart_disease)) +
      geom_point(alpha = 0.4) +
      geom_point(data = similar_patients, 
                 aes(x = age, y = thalach), 
                 color = "black", size = 3, shape = 1) +
      geom_vline(xintercept = input$patient_age, linetype = "dashed", color = "red") +
      geom_hline(yintercept = input$patient_thalach, linetype = "dashed", color = "red") +
      labs(x = "Age",
           y = "Max Heart Rate (thalach)",
           color = "Diagnosis",
           title = "Patient Profile vs. Diagnosed Cases") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("age", "thalach", "heart_disease"))
  })
}

# ============================================================
# RUN THE APP
# ============================================================
shinyApp(ui = ui, server = server)