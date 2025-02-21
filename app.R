library(shiny)
library(dplyr)
library(tidyr)
library(readxl)

# Define required column names
required_columns <- c("task", "member", "start_month", "total_months", "eti_percentage")

# Function to Validate and Process Data
validate_and_process_data <- function(data) {
  missing_cols <- setdiff(required_columns, colnames(data))

  if (length(missing_cols) > 0) {
    stop("Please make sure that the input file is a .xlsx, and that the data table follows the format described in the Input instructions tab. Thank you!")
  }

  # Expand data to a month-level resolution
  expanded_data <- data %>%
    rowwise() %>%
    mutate(month_seq = list(seq(start_month, start_month + total_months - 1))) %>%
    unnest(month_seq) %>%
    select(task, member, month_seq, eti_percentage)

  # Count the number of tasks per member per month
  task_counts_per_month <- expanded_data %>%
    group_by(member, month_seq) %>%
    summarise(task_count = n(), .groups = "drop")

  # Adjust ETI per task per month based on task count
  adjusted_data <- expanded_data %>%
    left_join(task_counts_per_month, by = c("member", "month_seq")) %>%
    mutate(adjusted_eti = (eti_percentage / 100) / task_count)  # Distribute ETI evenly

  # Sum the adjusted ETI per person per task
  task_eti_summary <- adjusted_data %>%
    group_by(task, member) %>%
    summarise(person_month = sum(adjusted_eti, na.rm = TRUE), .groups = "drop")

  # Sum total ETI per task
  total_eti_per_task <- task_eti_summary %>%
    group_by(task) %>%
    summarise(total_eti = sum(person_month, na.rm = TRUE), .groups = "drop")

  return(list(per_person = task_eti_summary, per_task = total_eti_per_task))
}

# Define UI with tab panels
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
      }
      .wrapper {
        min-height: 100vh;
        display: flex;
        flex-direction: column;
      }
      .content {
        flex: 1;
      }
      .footer {
        text-align: center;
        padding: 10px;
        font-size: 14px;
        background-color: #f8f9fa;
        width: 100%;
      }
    "))
  ),
  div(class = "wrapper",
      div(class = "content",
          titlePanel("FTE Calculator for Research Tasks"),
          tabsetPanel(
            tabPanel("Input Instructions",
                     br(),
                     fluidRow(
                       column(6,
                              img(src = "timeline_and_expected_table_example.png", width = "100%")
                       )
                     ),
                     br(),
                     h4("Column Descriptions"),
                     tags$ul(
                       tags$li(strong("task:"), " Task name."),
                       tags$li(strong("member:"), " Team member name."),
                       tags$li(strong("start_month:"), " Starting month of the task - numeric value (e.g., 15 for the 15th month of the project)."),
                       tags$li(strong("total_months:"), " Total task duration, in months."),
                       tags$li(strong("eti_percentage:"), " The FTE percentage dedicated by the team member (as a whole number, e.g., 50 for 50%).")
                     )
            ),
            
            tabPanel("FTE Calculation",
                     br(),
                     fileInput("file", "Upload Excel File", accept = c(".xlsx")),
                     selectInput("sheet", "Select Sheet", choices = NULL, selected = NULL),
                     actionButton("process", "Calculate FTEs"),
                     
                     br(),
                     
                     conditionalPanel(
                       condition = "output.show_error_message == true",
                       div(style = "color: red; font-weight: bold; margin-top: 10px;",
                           textOutput("error_message"))
                     ),
                     
                     br(),
                     h4("Results | Person*Month "),
                     tableOutput("eti_table"),
                     
                     br(),
                     h4("Results | Total FTEs Per Task"),
                     tableOutput("total_eti_table")
            )
          )
      ),
      tags$footer(class = "footer",
                  tags$p(
                    "Author: Isabel Duarte | ",
                    tags$a(href = "https://github.com/patterninstitute/FTE_calculator", "View on GitHub")
                  )
      )
  )
)


# Define Server
server <- function(input, output, session) {

  error_message <- reactiveVal(NULL)  # Stores the error message

  # Reactive: Load uploaded file and get its sheet names
  task_data <- reactive({
    req(input$file)

    # Validate file format
    if (!grepl("\\.xlsx$", input$file$name)) {
      error_message("Please make sure that the input file is a .xlsx, and that the data table follows the format described in the Input instructions tab. Thank you!")
      return(NULL)
    }

    error_message(NULL)  # Reset error if file is correct
    readxl::excel_sheets(input$file$datapath)
  })

  # Update sheet selection when file is uploaded
  observeEvent(task_data(), {
    if (!is.null(task_data())) {
      updateSelectInput(session, "sheet", choices = task_data(), selected = task_data()[1])
    }
  })

  # Reactive: Read data from selected sheet and validate it
  task_data_df <- eventReactive(input$process, {
    req(input$file, input$sheet)

    tryCatch({
      data <- readxl::read_xlsx(input$file$datapath, sheet = input$sheet, col_names = TRUE)
      validate_and_process_data(data)  # Ensure correct format
      error_message(NULL)  # Clear error if data is correct
      return(data)
    }, error = function(e) {
      error_message("Please make sure that the input file is a .xlsx, and that the data table follows the format described in the Input instructions tab. Thank you!")
      return(NULL)
    })
  })

  # Show/hide error message dynamically
  output$error_message <- renderText({
    error_message()
  })

  output$show_error_message <- reactive({
    !is.null(error_message())
  })

  outputOptions(output, "show_error_message", suspendWhenHidden = FALSE)  # Required for `conditionalPanel()`

  # Calculate ETIs and display the results
  output$eti_table <- renderTable({
    req(task_data_df())
    validate_and_process_data(task_data_df())$per_person
  })

  output$total_eti_table <- renderTable({
    req(task_data_df())
    validate_and_process_data(task_data_df())$per_task
  })
}

# Run the application
shinyApp(ui = ui, server = server)
