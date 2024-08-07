library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(utf8)

ui <- fluidPage(
  # Application title
  titlePanel("Comparison of chronic disease prescriptions in major 
             medical centers in Taiwan"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12,
               actionButton("toggle_sidebar", "Toggle Sidebar")
        )
      ),
      conditionalPanel(
        condition = "input.toggle_sidebar == 1",
        checkboxGroupInput("disease_filter", "Filter Doctors by Disease:",
                           choices = c("hypertension", "high_blood_sugar",
                                       "Hyperlipidemia", "metabolic_syndrome",
                                       "chronic_kidney_disease"),
                           selected = c("hypertension", "high_blood_sugar")
        ),
        helpText("Switch to the doctor list to filter the doctor's specialty"),
        
        sliderInput("nrows", "Enter the number of rows to display:",
                    min = 1,
                    max = 100,
                    value = 10),
        selectInput("table_select", "Select table:",
                    choices = c("hospital", "doctor", "prescript", 
                                "bed_occupancy", "Chronic_diseases", 
                                "Treatment_Guidelines"),
                    selected = "hospital")
      )
    ),
    
    mainPanel(
      fluidRow(
        column(12,
               dataTableOutput("tbl"),
               plotOutput("plot")
        )
      ),
      img(src = "schema.jpg", height = 680, width = 1200)
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$tbl <- renderDataTable({
    # Establish a connection to the SQLite database
    sqlite_conn <- dbConnect(RSQLite::SQLite(), "hospital.db")
    
    # Create SQL query to retrieve data from the selected table with JOIN
    if (input$table_select == "doctor") {
      # Filter doctors based on selected diseases
      selected_diseases <- input$disease_filter
      if (length(selected_diseases) > 0) {
        # Construct the IN clause for Disease_name
        selected_diseases_str <- paste0("'", selected_diseases, "'",
                                        collapse = ", ")
        query <- paste0("SELECT d.doctor_id, d.hospital_name, d.doctor_name,
        d.Disease_name, h.hospital_id
                         FROM doctor d
                         LEFT JOIN hospital h ON d.hospital_name = h.hospital_name
                         WHERE d.Disease_name IN (", selected_diseases_str, ")
                         LIMIT ", input$nrows)
      } else {
        query <- paste0("SELECT d.doctor_id, d.hospital_name, d.doctor_name, 
        d.Disease_name, h.hospital_id
                         FROM doctor d
                         LEFT JOIN hospital h ON d.hospital_name = h.hospital_name
                         LIMIT ", input$nrows)
      }
    } else if (input$table_select == "prescript") {
      query <- paste0("SELECT p.*, h.hospital_name
                       FROM prescript p
                       LEFT JOIN hospital h ON p.hospital_id = h.hospital_id
                       LIMIT ", input$nrows)
    } else if (input$table_select == "bed_occupancy") {
      query <- paste0("SELECT b.*, h.hospital_name
                       FROM bed_occupancy b
                       LEFT JOIN hospital h ON b.hospital_id = h.hospital_id
                       LIMIT ", input$nrows)
    } else if (input$table_select == "Chronic_diseases") {
      query <- paste0("SELECT * FROM Chronic_diseases LIMIT ", input$nrows)
    } else if (input$table_select == "Treatment_Guidelines") {
      query <- paste0("SELECT * FROM Treatment_Guidelines LIMIT ", input$nrows)
    } else {
      query <- paste0("SELECT * FROM ", input$table_select, " LIMIT ", input$nrows)
    }
    
    # Execute SQL query to retrieve data
    data <- dbGetQuery(sqlite_conn, query)
    
    # Close the database connection
    dbDisconnect(sqlite_conn)
    
    # Return the data to be displayed in the data table
    data
  })
  
  
  # Render plot
  output$plot <- renderPlot({
    # Establish a connection to the SQLite database
    sqlite_conn <- dbConnect(RSQLite::SQLite(), "hospital.db")
    
    # Create SQL query to calculate prescript count by hospital_id and hospital_name
    prescript_count_query <- "SELECT p.hospital_id, h.hospital_name,
    SUM(p.Number_cases_pharmacies_andhospital_prescript_chronic) AS prescript_count
                            FROM prescript p
                            LEFT JOIN hospital h ON p.hospital_id = h.hospital_id
                            GROUP BY p.hospital_id, h.hospital_name
                            ORDER BY prescript_count DESC
                            LIMIT 10"
    
    # Execute SQL query to retrieve prescript count data
    prescript_count_data <- dbGetQuery(sqlite_conn, prescript_count_query)
    
    # Close the database connection
    dbDisconnect(sqlite_conn)
    
    # Plot the prescript count data with adjusted x and y axis
    ggplot(prescript_count_data, aes(x = reorder(hospital_name, prescript_count),
                                     y = prescript_count)) +
      geom_bar(stat = "identity", fill = 'orange', color = 'cyan') +
      labs(title = "Top 10 Hospitals by Prescript Count", x = "Hospital Name",
           y = "Prescript Count") +
      scale_x_discrete(labels = function(x) utf8::utf8_encode(x)) +  
      # Encode labels in UTF-8 for traditional Chinese characters
      scale_y_continuous(labels = scales::comma) +  # Adjust y axis labels format
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x axis labels
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
