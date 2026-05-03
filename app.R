library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)


col_names <- c(
  "age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
  "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num"
)

# Load data and treating ? as missing value
heart_raw <- read.csv("processed.cleveland.data",
                      header = FALSE,
                      col.names = col_names,
                      na.strings = "?")

# Clean data, convert to factors, create binary target and remove NA
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


# UI
ui <- fluidPage(
  titlePanel("Heart Disease Risk Explorer"),
  h4("UCI Cleveland Dataset — CC BY 4.0"),
  hr(),
  
  tabsetPanel(
    
    # Tab 1
    tabPanel("Introduction",
             h3("Exploring Heart Disease Risk Factors"),
             br(),
             p(strong("Question:"), "Which patient characteristics are most strongly associated with a diagnosis of heart disease?"),
             br(),
             h4("The Dataset"),
             p("- 303 patients from Cleveland Clinic Foundation"),
             p("- 13 clinical variables"),
             p("- Published by Detrano et al. (1989), American Journal of Cardiology"),
             p("- License: CC BY 4.0"),
             br(),
             h4("Data Cleaning"),
             p("Missing values removed. Categorical variables converted to factors. Target variable converted to binary: Disease vs No Disease.")
    ),
    
    # Tab 2
    tabPanel("Data Exploration",
             sidebarLayout(
               sidebarPanel(
                 h4("Explore the Data"),
                 selectInput("yvar", "Y-axis Variable:",
                             choices = c("Max Heart Rate" = "thalach",
                                         "Blood Pressure" = "trestbps",
                                         "Cholesterol" = "chol",
                                         "ST Depression" = "oldpeak")),
                 selectInput("color_by", "Color by:",
                             choices = c("Heart Disease" = "heart_disease",
                                         "Sex" = "sex")),
                 sliderInput("age_range", "Filter by Age:",
                             min = 29, max = 77, value = c(29, 77))
               ),
               mainPanel(
                 h4("Age vs Selected Variable"),
                 plotlyOutput("explore_plot"),
                 br(),
                 h4("Distribution of All Variables by Diagnosis"),
                 plotlyOutput("box_plot")
               )
             )
    ),
    
    # Tab 3
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
                 plotlyOutput("risk_plot")
               )
             )
    ),
    
    # Tab 4
    tabPanel("Key Findings",
             h3("What We Discovered"),
             br(),
             h4("1. Chest Pain Type"),
             p("Asymptomatic patients are most likely to have heart disease."),
             br(),
             h4("2. Max Heart Rate"),
             p("Disease patients have lower max heart rates (139 vs 158 bpm)."),
             br(),
             h4("3. ST Depression"),
             p("Higher ST depression is associated with disease (1.6 vs 0.8 mm)."),
             br(),
             h4("4. Exercise Angina"),
             p("Exercise-induced angina is a strong predictor of heart disease."),
             br(),
             h4("Conclusion"),
             p("Non-invasive exercise testing can effectively identify high-risk patients."),
             br(),
             h4("Limitations"),
             p("- Temporal Relevance: data collected in 1989"),
             p("- Sample Size Constraints: 303 patients"),
             p("- Geographic & Demographic Scope: single clinic")
    )
  )
)

# SERVER
server <- function(input, output) {
  
  # Updates when age slider changes
  filtered_data <- reactive({
    heart_clean %>%
      filter(age >= input$age_range[1], age <= input$age_range[2])
  })
  
  # Scatter plot to see Age vs choosable Y variable
  output$explore_plot <- renderPlotly({
    p <- ggplot(filtered_data(),
                aes(x = age, y = .data[[input$yvar]],
                    color = .data[[input$color_by]])) +
      geom_point(size = 2, alpha = 0.7) +
      stat_smooth(method = "lm", se = FALSE) +
      scale_color_manual(values = c("No Disease" = "#2E86AB",
                                    "Disease" = "#A23B72",
                                    "Female" = "#FFB6C1",
                                    "Male" = "#4682B4")) +
      theme_minimal() +
      labs(x = "Age (years)", y = input$yvar)
    
    ggplotly(p)
  })
  
  # Boxplots to compare all variables by disease status
  output$box_plot <- renderPlotly({
    comp <- heart_clean %>%
      select(heart_disease, age, trestbps, chol, thalach, oldpeak) %>%
      pivot_longer(-heart_disease, names_to = "variable", values_to = "value") %>%
      mutate(variable = factor(variable,
                               levels = c("age", "trestbps", "chol", "thalach", "oldpeak"),
                               labels = c("Age (years)", "Blood Pressure (mm Hg)", "Cholesterol (mg/dl)",
                                          "Max Heart Rate (bpm)", "ST Depression (mm)")))
    
    p <- ggplot(comp, aes(x = heart_disease, y = value, fill = heart_disease)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.2, size = 0.5) +
      facet_wrap(~ variable, scales = "free_y", nrow = 1) +
      scale_fill_manual(values = c("No Disease" = "#2E86AB",
                                   "Disease" = "#A23B72")) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        strip.text = element_text(size = 10, face = "bold")
      ) +
      labs(x = "", y = "")
    
    ggplotly(p) %>%
      layout(margin = list(b = 80))
  })
  
  # Risk profile, compare user's patient to other patients
  output$risk_plot <- renderPlotly({
    similar <- heart_clean %>%
      filter(sex == input$p_sex,
             cp == input$p_cp,
             exang == input$p_exang)
    
    p <- ggplot() +
      # All patients in background
      geom_point(data = heart_clean,
                 aes(x = age, y = thalach, color = heart_disease),
                 alpha = 0.4) +
      # Similar patients highlighted as black circles
      geom_point(data = similar,
                 aes(x = age, y = thalach),
                 color = "black", size = 3, shape = 1) +
      # Reference lines for user's patient
      geom_vline(xintercept = input$p_age,
                 linetype = "dashed", color = "#D64550", size = 1) +
      geom_hline(yintercept = input$p_thalach,
                 linetype = "dashed", color = "#D64550", size = 1) +
      scale_color_manual(values = c("No Disease" = "#2E86AB",
                                    "Disease" = "#A23B72")) +
      theme_minimal() +
      labs(x = "Age (years)", y = "Max Heart Rate (bpm)")
    
    ggplotly(p, height = 500)
  })
}

shinyApp(ui = ui, server = server)
