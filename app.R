# Install and load the required packages
if (!require("shiny")) install.packages("shiny")
if (!require("dplyr")) install.packages("dplyr")

library(shiny)
library(dplyr)

# Data initialization
patients <- data.frame(
  PatientID = numeric(0),
  Name = character(0),
  Age = numeric(0)
)

appointments <- data.frame(
  AppointmentID = numeric(0),
  PatientID = numeric(0),
  Date = character(0),
  Status = character(0)
)

# Define the UI
ui <- fluidPage(
  titlePanel("Health Care Management System", windowTitle = "Health Care Management"),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Patient Name"),
      numericInput("age", "Patient Age", NULL, min = 1),
      dateInput("date", "Appointment Date"),
      actionButton("register", "Register Patient", class = "btn-primary"),
      actionButton("schedule", "Schedule Appointment", class = "btn-success"),
      textInput("search", "Search Patient"),
      actionButton("searchButton", "Search", class = "btn-info"),
      actionButton("cancel", "Cancel Appointment", class = "btn-danger"),
      textOutput("searchResult")
    ),
    mainPanel(
      h3("Patient Information"),
      tableOutput("patientTable"),
      h3("Appointment Information"),
      tableOutput("appointmentTable")
    )
  ),
  tags$style(
    HTML(".mainPanel { max-width: 800px; margin: 0 auto; }"),
    HTML(".btn-primary { background-color: #007BFF; color: #FFFFFF; border-color: #007BFF; }"),
    HTML(".btn-success { background-color: #28A745; color: #FFFFFF; border-color: #28A745; }"),
    HTML(".btn-info { background-color: #17A2B8; color: #FFFFFF; border-color: #17A2B8; }"),
    HTML(".btn-danger { background-color: #DC3545; color: #FFFFFF; border-color: #DC3545; }")
  )
)

# Define the server
server <- function(input, output, session) {
  observeEvent(input$register, {
    if (!is.null(input$name) && !is.na(input$name) && nchar(trimws(input$name)) > 0 && !is.null(input$age) && !is.na(input$age) && !is.null(input$date)) {
      patient_id <- nrow(patients) + 1
      patients <<- rbind(patients, data.frame(PatientID = patient_id, Name = input$name, Age = input$age))
    } else {
      cat("Please enter patient name, age, and appointment date.\n")
    }
  })
  
  observeEvent(input$schedule, {
    if (!is.null(input$name) && !is.na(input$name) && nchar(trimws(input$name)) > 0 && !is.null(input$age) && !is.na(input$age) && !is.null(input$date)) {
      patient_id <- ifelse(nrow(patients) == 0, 1, nrow(patients))
      appointments <<- rbind(appointments, data.frame(AppointmentID = nrow(appointments) + 1, PatientID = patient_id, Date = as.character(input$date), Status = "Scheduled"))
    } else {
      cat("Please enter patient name, age, and appointment date.\n")
    }
  })
  
  observeEvent(input$searchButton, {
    if (!is.null(input$search)) {
      result <- patients[grepl(tolower(input$search), tolower(patients$Name)), ]
      if (nrow(result) > 0) {
        output$searchResult <- renderText({
          paste("Patient found: ID =", result$PatientID, ", Name =", result$Name, ", Age =", result$Age)
        })
      } else {
        output$searchResult <- renderText("Patient not found.")
      }
    } else {
      output$searchResult <- renderText("Please enter a search term.")
    }
  })
  
  observeEvent(input$cancel, {
    if (!is.null(input$search)) {
      patient_result <- patients[grepl(tolower(input$search), tolower(patients$Name)), ]
      if (nrow(patient_result) > 0) {
        patient_id_to_cancel <- patient_result$PatientID
        result <- appointments[appointments$PatientID %in% patient_id_to_cancel, ]
        if (nrow(result) > 0) {
          appointments <<- appointments[!(appointments$PatientID %in% patient_id_to_cancel), ]
          output$searchResult <- renderText("Appointment canceled successfully.")
        } else {
          output$searchResult <- renderText("Appointment not found.")
        }
      } else {
        output$searchResult <- renderText("Patient not found.")
      }
    } else {
      output$searchResult <- renderText("Please enter a search term.")
    }
  })
  
  
  
  
  output$patientTable <- renderTable({
    patients
  })
  
  output$appointmentTable <- renderTable({
    appointments %>%
      left_join(patients, by = "PatientID") %>%
      mutate(Date = format(as.Date(Date), "%Y-%m-%d")) %>%
      select(AppointmentID, Name, Age, Date, Status)
  })
}

# Run the application
shinyApp(ui, server)
