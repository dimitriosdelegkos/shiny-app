library(dplyr)
library(randomForest)
library(shiny)
library(shinythemes)
library(shinyjs)

model <- readRDS("./rfc.rds")
scaler <- readRDS("./scaler.rds")

dataset = read.csv("./data/hr_train.csv")


# Define the UI
ui = fluidPage(theme = shinytheme("spacelab"),
  h1(strong("Employee Churn Prediction")),
  h5("The",strong("goal"), "of this project is to predict if an employee will leave a company, based on specific features."),
  img(src="./churn.jpg"),
  h2('Employee Dataset'),
  h5('The data includes employee informations, such as the salary, the department, the duration of being at the company, etc.'),
  div(),
  h5("Let's see the first 5 rows of the dataset:"),
  tableOutput('head.dataset'),
  h2('Model'),
  div(),
  h5('Here you can select the characteristics of the employee'),
           fluidRow(
             column(6, sliderInput("satisfaction_level", "Select a satisfaction level", 0.00, 1.00, 0.2, ticks = FALSE),
                    sliderInput("evaluation_level", "Select an evaluation level", 0.00, 1.00, 0.72, ticks = FALSE),
                    sliderInput("projects", "Select the number of projects", 0, 7, 6, ticks = FALSE),
                    sliderInput("monthly_hours", "Select the monthly hours", 80, 240, 224, ticks = FALSE),
                    sliderInput("working_years", "Select the years of working", 0, 10, 4, ticks = FALSE)),
             column(6, radioButtons("accident","Had the employee an accident?",choices = c("No","Yes")),
                    radioButtons("promotion","Did the employee get a promotion?",choices = c("No","Yes")),
                    radioButtons("salary","What is the salary?",choices = c("Low","Medium","High"),selected = "High"),
                    selectInput(
                      "department", "Select the department",
                      c("Technical", "IT", "Support", "Sales","Product Management","Marketing","Management","HR","Accounting","R&D")
                    ),
                    actionButton("submit", "Submit", class = "btn-primary"))
           )
    )


# Define the server code
server <- function(input, output, session) {
  #display the head
  output$head.dataset = renderTable(head(dataset))
  
  fieldsAll = c("satisfaction_level","evaluation_level","projects","monthly_hours","working_years","accident","promotion","salary","department")
  
  # get the form data
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- t(data)
  })
  
  ls = rep(0,18)
  
  # Applying scaling and getting the prediction for a new employee
  test_prediction = function(ls) {
    col_to_be_scaled = c('number_project','average_montly_hours', 'time_spend_company', 'salary')
    df_pred <- data.frame(matrix(nrow = 0, ncol = 18))
    colnames(df_pred) <- c('satisfaction_level', 'last_evaluation', 'number_project',
                           'average_montly_hours', 'time_spend_company', 'Work_accident',
                           'promotion','department.accounting','department.hr','department.IT','department.management','department.marketing',
                           'department.product_mng','department.RandD','department.sales','department.support','department.technical','salary')
    df_pred[1, ] <- ls
    df_pred[1, col_to_be_scaled] <- scale(df_pred[1, col_to_be_scaled], attr(scaler, "scaled:center"), attr(scaler, "scaled:scale"))
    test_pred <- predict(model, df_pred)
    return(test_pred[[1]])
  }
  
  # action to take when submit button is pressed
  observeEvent(input$submit, {
    sat_lev = as.double(formData()[1])
    ls[1] = sat_lev
    last_eval = as.double(formData()[2])
    ls[2] = last_eval
    proj = as.numeric(formData()[3])
    ls[3] = proj
    hours = as.numeric(formData()[4])
    ls[4] = hours
    time_spend = as.numeric(formData()[5])
    ls[5] = time_spend
    accident = ifelse(formData()[6] == 'No',0,1)
    ls[6] = accident
    promotion = ifelse(formData()[7] == 'No',0,1)
    ls[7] = promotion
    salary = case_when(formData()[8] == 'Low' ~ 0,formData()[8] == 'Medium' ~ 1,formData()[8] == 'High' ~ 2)
    ls[18] = salary 
    
    if (formData()[9] == 'Accounting') {
      ls[8] = 1
    } else if (formData()[9] == 'HR') {
      ls[9] = 1
      }else if (formData()[9] == 'IT') {
      ls[10] = 1
    } else if (formData()[9] == 'Management') {
        ls[11] = 1
    } else if (formData()[9] == 'Marketing') {
        ls[12] = 1
    } else if (formData()[9] == 'Management') {
        ls[13] = 1
    } else if (formData()[9] == 'R&D ') {
        ls[14] = 1
    } else if (formData()[9] == 'Sales') {
        ls[15] = 1
    } else if (formData()[9] == 'Support') {
        ls[16] = 1
    } else if (formData()[9] == 'Technical') {
        ls[17] = 1
    } else {
        ls[9] = 0
      }
    
    prediction = test_prediction(ls)
    
    showModal(modalDialog("The probability of leaving the company is:",trunc(prediction *100^2)/10^2," %",size = 'l'))
  })
}


# Return a Shiny app object
shinyApp(ui = ui, server = server)
