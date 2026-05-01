# ============================================================
# DASC3240 Final Project: Heart Disease Risk Explorer
# Dataset: UCI Heart Disease (Cleveland) - License: CC BY 4.0
# ============================================================

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# ============================================================
# DATA LOADING & CLEANING
# ============================================================
col_names <- c(
  "age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
  "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num"
)

heart_raw <- read.csv("processed.cleveland.data",
                      header = FALSE,
                      col.names = col_names,
                      na.strings = "?")

heart_clean <- heart_raw %>%
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
    cp = factor(cp, levels = 1:4,
                labels = c("Typical Angina", "Atypical Angina",
                           "Non-anginal Pain", "Asymptomatic")),
    fbs = factor(fbs, levels = c(0, 1),
                 labels = c("<= 120 mg/dl", "> 120 mg/dl")),
    restecg = factor(restecg, levels = 0:2,
                     labels = c("Normal", "ST-T Abnormality", "LV Hypertrophy")),
    exang = factor(exang, levels = c(0, 1), labels = c("No", "Yes")),
    slope = factor(slope, levels = 1:3,
                   labels = c("Upsloping", "Flat", "Downsloping")),
    thal = factor(thal, levels = c(3, 6, 7),
                  labels = c("Normal", "Fixed Defect", "Reversible Defect")),
    heart_disease = factor(ifelse(num > 0, "Disease", "No Disease"),
                           levels = c("No Disease", "Disease"))
  ) %>%
  na.omit()

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  titlePanel("Heart Disease Risk Explorer"),
  h4("UCI Cleveland Dataset — CC BY 4.0"),
  hr(),
  
  tabsetPanel(
    
    # ========== TAB 1: Introduction (Member 1 — 3 min) ==========
    tabPanel("Introduction",
             h3("Exploring Heart Disease Risk Factors"),
             br(),
             p(strong("Question:"), "Which patient characteristics are most strongly associated with a diagnosis of heart disease?"),
             br(),
             h4("The Dataset"),
             p("- 303 patients from Cleveland Clinic Foundation"),
             p("- 13 clinical variables (age, blood pressure, cholesterol, etc.)"),
             p("- Published by Detrano et al. (1989), American Journal of Cardiology"),
             p("- License: CC BY 4.0 (free to share and adapt with attribution)"),
             br(),
             h4("Data Cleaning"),
             p("Missing values in 'ca' and 'thal' were removed. Categorical variables converted to factors with descriptive labels. Target variable converted to binary: Disease vs No Disease.")
    ),
    
    # ========== TAB 2: Data Exploration (Member 2 — 4 min) ==========
    tabPanel("Data Exploration",
             sidebarLayout(
               sidebarPanel(
                 h4("Explore the Data"),
                 selectInput("xvar", "X-axis Variable:",
                             choices = c("Age" = "age",
                                         "Blood Pressure" = "trestbps",
                                         "Cholesterol" = "chol",
                                         "Max Heart Rate" = "thalach",
                                         "ST Depression" = "oldpeak")),
                 selectInput("color_by", "Color by:",
                             choices = c("Heart Disease" = "heart_disease",
                                         "Sex" = "sex",
                                         "Chest Pain" = "cp")),
                 sliderInput("age_range", "Filter by Age:",
                             min = 29, max = 77, value = c(29, 77))
               ),
               mainPanel(
                 h4("Interactive Scatter Plot"),
                 p("Hover for details. Use the controls to explore relationships."),
                 plotlyOutput("explore_plot"),
                 br(),
                 p(strong("What to notice:"), " Look at how patients with heart disease (red) differ from healthy patients (blue). Which variables show the clearest separation?")
               )
             )
    ),
    
    # ========== TAB 3: Risk Factor Analysis (Member 3 — 4 min) ==========
    tabPanel("Risk Factor Analysis",
             sidebarLayout(
               sidebarPanel(
                 h4("Build a Patient Profile"),
                 selectInput("p_sex", "Sex:",
                             choices = c("Female", "Male")),
                 selectInput("p_cp", "Chest Pain Type:",
                             choices = c("Typical Angina", "Atypical Angina",
                                         "Non-anginal Pain", "Asymptomatic")),
                 selectInput("p_exang", "Exercise Angina:",
                             choices = c("No", "Yes")),
                 sliderInput("p_age", "Age:",
                             min = 29, max = 77, value = 55),
                 sliderInput("p_thalach", "Max Heart Rate:",
                             min = 71, max = 202, value = 150)
               ),
               mainPanel(
                 h4("Patient Profile vs. Diagnosed Cases"),
                 p("Red dashed lines show your patient. Hollow circles show similar patients."),
                 plotlyOutput("risk_plot"),
                 br(),
                 h4("Variable Comparison: Disease vs No Disease"),
                 plotlyOutput("compare_plot")
               )
             )
    ),
    
    # ========== TAB 4: Key Findings (Member 4 — 3 min) ==========
    tabPanel("Key Findings",
             h3("What We Discovered"),
             br(),
             h4("1. Chest Pain Type Matters"),
             p("Patients with asymptomatic chest pain are most likely to have heart disease. This is dangerous because they don't feel symptoms."),
             br(),
             h4("2. Max Heart Rate Tells a Story"),
             p("Heart disease patients achieve lower maximum heart rates during exercise — a sign of reduced cardiac function."),
             br(),
             h4("3. ST Depression is a Key Indicator"),
             p("Higher ST segment depression during exercise is consistently associated with heart disease diagnosis."),
             br(),
             h4("Conclusion"),
             p("Non-invasive exercise testing can effectively identify high-risk patients. Chest pain assessment combined with heart rate and ST segment analysis provides valuable diagnostic information."),
             br(),
             h4("Limitations"),
             p("Data from 1989, 303 patients, single clinic. Findings may not generalize.")
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output) {
  
  # TAB 2: Filtered data + scatter plot
  filtered_data <- reactive({
    heart_clean %>%
      filter(age >= input$age_range[1], age <= input$age_range[2])
  })
  
  output$explore_plot <- renderPlotly({
    p <- ggplot(filtered_data(),
                aes(x = .data[[input$xvar]], y = thalach,
                    color = .data[[input$color_by]])) +
      geom_point(size = 2, alpha = 0.7) +
      stat_smooth(method = "lm", se = FALSE) +
      scale_color_manual(values = c("No Disease" = "#2E86AB",
                                    "Disease" = "#A23B72",
                                    "Female" = "#FFB6C1",
                                    "Male" = "#4682B4")) +
      theme_minimal() +
      labs(x = input$xvar, y = "Max Heart Rate (bpm)")
    
    ggplotly(p)
  })
  
  # TAB 3: Risk plot
  output$risk_plot <- renderPlotly({
    similar <- heart_clean %>%
      filter(sex == input$p_sex,
             cp == input$p_cp,
             exang == input$p_exang)
    
    p <- ggplot() +
      geom_point(data = heart_clean,
                 aes(x = age, y = thalach, color = heart_disease),
                 alpha = 0.4) +
      geom_point(data = similar,
                 aes(x = age, y = thalach),
                 color = "black", size = 3, shape = 1) +
      geom_vline(xintercept = input$p_age,
                 linetype = "dashed", color = "red", size = 1) +
      geom_hline(yintercept = input$p_thalach,
                 linetype = "dashed", color = "red", size = 1) +
      scale_color_manual(values = c("No Disease" = "#2E86AB",
                                    "Disease" = "#A23B72")) +
      theme_minimal() +
      labs(x = "Age (years)", y = "Max Heart Rate (bpm)")
    
    ggplotly(p)
  })
  
  # TAB 3: Comparison plot
  output$compare_plot <- renderPlotly({
    comp <- heart_clean %>%
      select(heart_disease, age, trestbps, chol, thalach, oldpeak) %>%
      pivot_longer(-heart_disease, names_to = "variable", values_to = "value")
    
    p <- ggplot(comp, aes(x = heart_disease, y = value, fill = heart_disease)) +
      geom_boxplot(alpha = 0.7) +
      facet_wrap(~ variable, scales = "free_y") +
      scale_fill_manual(values = c("No Disease" = "#2E86AB",
                                   "Disease" = "#A23B72")) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
}

# ============================================================
# RUN APP
# ============================================================
shinyApp(ui = ui, server = server)