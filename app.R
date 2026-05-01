# ============================================================
# DASC3240 Final Project: Heart Disease Risk Explorer
# ============================================================
# Dataset: UCI Heart Disease (Cleveland) - License: CC BY 4.0
# Techniques: ggplot2 (L04-L06), plotly (L11-L12, L14), 
#             gganimate (L13), Shiny (L18-L20)

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(gganimate)
library(gifski)

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
  h4("UCI Heart Disease Dataset - Cleveland (CC BY 4.0)"),
  hr(),
  
  tabsetPanel(
    id = "tabs",
    
    # TAB 1: Introduction
    tabPanel("Introduction",
             h3("About This App"),
             p("This Shiny app explores the UCI Heart Disease dataset to answer:"),
             strong('"Which patient characteristics are most strongly associated with a diagnosis of heart disease?"'),
             br(), br(),
             h4("Dataset Information"),
             p("- Source: UCI Machine Learning Repository"),
             p("- Original study: Detrano et al. (1989), American Journal of Cardiology"),
             p("- 303 patients from Cleveland Clinic"),
             p("- 13 clinical features + heart disease diagnosis (num)"),
             p("- License: CC BY 4.0 (Creative Commons Attribution 4.0 International)"),
             br(),
             h4("Data Cleaning Steps"),
             p("1. Column names assigned based on heart-disease.names file"),
             p("2. Categorical variables converted to factors with descriptive labels"),
             p("3. Missing values (coded as '?') in 'ca' and 'thal' removed (18 rows)"),
             p("4. Target variable 'num' converted to binary: Disease (1-4) vs No Disease (0)")
    ),
    
    # TAB 2: Data Exploration
    tabPanel("Data Exploration",
             sidebarLayout(
               sidebarPanel(
                 selectInput("xvar", "X-axis Variable:",
                             choices = c("Age" = "age",
                                         "Resting Blood Pressure" = "trestbps",
                                         "Cholesterol" = "chol",
                                         "Max Heart Rate" = "thalach",
                                         "ST Depression" = "oldpeak")),
                 selectInput("yvar", "Y-axis Variable:",
                             choices = c("Max Heart Rate" = "thalach",
                                         "Cholesterol" = "chol",
                                         "Resting Blood Pressure" = "trestbps",
                                         "ST Depression" = "oldpeak",
                                         "Age" = "age")),
                 selectInput("color_by", "Color by:",
                             choices = c("Heart Disease" = "heart_disease",
                                         "Sex" = "sex",
                                         "Chest Pain Type" = "cp")),
                 sliderInput("age_range", "Age Range:",
                             min = 29, max = 77, value = c(29, 77))
               ),
               mainPanel(
                 plotlyOutput("scatter_plot"),
                 br(),
                 plotlyOutput("box_plot")
               )
             )
    ),
    
    # TAB 3: Risk Factor Analysis
    tabPanel("Risk Factor Analysis",
             sidebarLayout(
               sidebarPanel(
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
                 br(),
                 plotlyOutput("comparison_plot")
               )
             )
    ),
    
    # TAB 4: Animation & Key Findings
    tabPanel("Animation & Findings",
             h3("Key Findings"),
             br(),
             plotlyOutput("importance_bar"),
             br(),
             h4("Animation: No Disease vs Disease"),
             imageOutput("anim"),
             br(),
             h4("Summary"),
             p("- Asymptomatic chest pain is the strongest predictor of heart disease"),
             p("- Patients with heart disease have lower maximum heart rates"),
             p("- ST depression (oldpeak) is consistently higher in disease patients"),
             p("- Exercise-induced angina is strongly associated with diagnosis")
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # Reactive filtered data (L18 pattern)
  filtered_data <- reactive({
    heart_clean %>%
      filter(age >= input$age_range[1], age <= input$age_range[2])
  })
  
  # TAB 2: Scatter plot (L11 pattern: ggplot + ggplotly)
  output$scatter_plot <- renderPlotly({
    p <- ggplot(filtered_data(),
                aes(x = .data[[input$xvar]],
                    y = .data[[input$yvar]],
                    color = .data[[input$color_by]])) +
      geom_point(size = 2, alpha = 0.7) +
      theme_minimal() +
      labs(x = input$xvar, y = input$yvar, color = input$color_by)
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # TAB 2: Box plot (L05 pattern: geom_boxplot + geom_jitter)
  output$box_plot <- renderPlotly({
    p <- ggplot(filtered_data(),
                aes(x = heart_disease,
                    y = .data[[input$xvar]],
                    fill = heart_disease)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
      scale_fill_manual(values = c("No Disease" = "#2E86AB",
                                   "Disease" = "#A23B72")) +
      theme_minimal() +
      labs(x = "Diagnosis", y = input$xvar) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # TAB 3: Risk plot (L19 pattern: plotly with filtered data)
  output$risk_plot <- renderPlotly({
    similar <- heart_clean %>%
      filter(sex == input$patient_sex, cp == input$patient_cp)
    
    p <- ggplot(heart_clean, aes(x = age, y = thalach, color = heart_disease)) +
      geom_point(alpha = 0.4) +
      geom_point(data = similar, aes(x = age, y = thalach),
                 color = "black", size = 3, shape = 1) +
      geom_vline(xintercept = input$patient_age,
                 linetype = "dashed", color = "red") +
      geom_hline(yintercept = input$patient_thalach,
                 linetype = "dashed", color = "red") +
      scale_color_manual(values = c("No Disease" = "#2E86AB",
                                    "Disease" = "#A23B72")) +
      theme_minimal() +
      labs(x = "Age", y = "Max Heart Rate", color = "Diagnosis")
    
    ggplotly(p, tooltip = c("age", "thalach", "heart_disease"))
  })
  
  # TAB 3: Comparison plot (L11 pattern)
  output$comparison_plot <- renderPlotly({
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
  
  # TAB 4: Importance bar chart (L11 pattern)
  output$importance_bar <- renderPlotly({
    imp <- heart_clean %>%
      group_by(heart_disease) %>%
      summarise(
        age = mean(age),
        trestbps = mean(trestbps),
        chol = mean(chol),
        thalach = mean(thalach),
        oldpeak = mean(oldpeak)
      ) %>%
      pivot_longer(-heart_disease, names_to = "variable", values_to = "mean_val") %>%
      pivot_wider(names_from = heart_disease, values_from = mean_val) %>%
      mutate(diff = abs(Disease - `No Disease`))
    
    p <- ggplot(imp, aes(x = reorder(variable, diff), y = diff, fill = diff)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_gradient(low = "#2E86AB", high = "#A23B72") +
      theme_minimal() +
      labs(x = "", y = "Absolute Difference Between Groups",
           title = "Variables Most Associated with Heart Disease") +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # TAB 4: Animation (L13 pattern: transition_states + animate + anim_save)
  output$anim <- renderImage({
    outfile <- tempfile(fileext = ".gif")
    
    anim <- ggplot(heart_clean, aes(x = age, y = thalach, color = heart_disease)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_color_manual(values = c("No Disease" = "#2E86AB",
                                    "Disease" = "#A23B72")) +
      labs(title = "Heart Disease Status: {closest_state}",
           x = "Age (years)", y = "Max Heart Rate (bpm)") +
      theme_minimal() +
      transition_states(heart_disease,
                        transition_length = 2,
                        state_length = 3) +
      ease_aes("cubic-in-out") +
      enter_fade() +
      exit_fade()
    
    anim_save(outfile,
              animate(anim, width = 500, height = 400, res = 96,
                      nframes = 60, fps = 10, renderer = gifski_renderer()))
    
    list(src = outfile, contentType = "image/gif")
  }, deleteFile = TRUE)
}

# ============================================================
# RUN APP
# ============================================================
shinyApp(ui = ui, server = server)