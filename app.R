#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(htmltools)
library(dplyr)
library(shinydashboard)

emp <-read.csv("D:/aditya/TRINITY/Data Visualization/WA_Fn-UseC_-HR-Employee-Attrition.csv")

n_rows <- as.numeric(nrow(emp))
all_Jobroles <- sort(unique(emp$JobRole))

colnames(emp)[1] = "Age"
emp$Attrition = as.integer(as.factor(emp$Attrition)) - 1
emp$BusinessTravel = as.integer(as.factor(emp$BusinessTravel))
emp$Department = as.integer(as.factor(emp$Department))
emp$Gender = as.integer(as.factor(emp$Gender))
emp$JobRole = as.factor(emp$JobRole)
emp$MaritalStatus = as.integer(as.factor(emp$MaritalStatus))
emp$OverTime = as.integer(as.factor(emp$OverTime))
emp$EducationField = as.integer(as.factor(emp$EducationField))
emp$StandardHours <- NULL
emp$PerformanceRating <- NULL
emp$Over18 <- NULL
emp$EmployeeCount <- NULL
emp$JobLevel <- NULL
emp$DailyRate <- NULL
emp$HourlyRate <- NULL
emp$DailyRate <- NULL
emp$MonthlyRate <- NULL
emp$PercentSalaryHike <- NULL
# Define UI for application that plots features of movies
ui <- dashboardPage(
  dashboardHeader(title = "Analysis of Employee Attrition"),
  dashboardSidebar(HTML(paste("Enter a value between 1 and", n_rows)),
                   
                   selectInput(inputId = "JobRole",
                               label = "Select Job role:",
                               choices = all_Jobroles,
                               selected = "Research Scientist",
                               selectize = TRUE,
                               multiple =FALSE
                   ),
                   
                  
                   
                   # Select variable for y-axis
                   selectInput(
                     inputId = "y",
                     label = "Y-axis:",
                     choices = c(
                       "Age" = "Age",
                       "Attrition" = "Attrition",
                       "Gende" = "Gender",
                       "Job Satisfaction" = "JobSatisfaction",
                       "Monthly Income" = "MonthlyIncome",
                       "Distance From Home" = "DistanceFromHome"
                     ),
                     selected = "Age"
                   ),
                   # Select variable for x-axis
                   selectInput(
                     inputId = "x",
                     label = "X-axis:",
                     choices = c(
                       "Age" = "Age",
                       "Attrition" = "Attrition",
                       "Gende" = "Gender",
                       "Job Satisfaction" = "JobSatisfaction",
                       "Monthly Income" = "MonthlyIncome",
                       "Distance From Home" = "DistanceFromHome"
                     ),
                     selected = "MonthlyIncome"
                   ),
                   selectInput(
                     inputId = "z",
                     label = "Color by:",
                     choices = c(
                       "Age" = "Age",
                       "Attrition" = "Attrition",
                       "Gender" = "Gender",
                       "Job Satisfaction" = "JobSatisfaction",
                       "Monthly Income" = "MonthlyIncome",
                       "Distance From Home" = "DistanceFromHome"
                     ),
                     selected = "Attrition"
                   ),
                   # Set alpha level for transparency plots
                   sliderInput(
                     inputId = "alpha",
                     label = "Alpha:",
                     min = 0,
                     max = 1,
                     value = 0.5
                   )),
  dashboardBody(  tabBox(
    title = "Scatterplot ",
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabset1", height = "450px",
  
    tabPanel("Tab1", "First tab content",  plotOutput(outputId = "scatterplot")),
    tabPanel("Tab2", "Tab content 2",plotOutput(outputId = "densityplot", height = 350)),
    tabPanel("Tab3", "First tab content", DT::dataTableOutput(outputId = "Attritiontable", height = 350) ), width = 12)
  )
  
    
    
               )

  


# Define server function required to create the scatterplot
server <- function(input, output) {
  
  dataInput <- reactive({
    filter(emp,emp$JobRole==input$JobRole)
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = emp, aes_string(
      x = input$x,
      y = input$y,
      color = input$z
    )) +
      geom_point(alpha = input$alpha)
  })
  
  output$densityplot <- renderPlot({
    ggplot(data = emp, aes_string(
      x = input$x
    )) +
      geom_density()
  })
  
  # Create data table
  output$Attritiontable <- DT::renderDataTable({
    dataInput()
  })
  
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
