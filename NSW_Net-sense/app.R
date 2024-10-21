library(shiny)
library(ggplot2)
library(readxl)
library(knitr)
library(openxlsx)

# Set a predefined password
password <- "NSW2024"  # Change "your_password" to the password you want

# Load the Excel sheet with data
data <- read.xlsx("NSW_ShinyApp_2023.09.13.xlsx", sheet = "AllData")

# Define UI
ui <- fluidPage(
  # Create conditional UI: If password is incorrect, show the password page
  uiOutput("ui_main")
)

# Define server logic
server <- function(input, output, session) {
  
  # Password verification logic
  output$ui_main <- renderUI({
    if (is.null(input$submit) || input$password_input != password) {
      fluidPage(
        titlePanel("Password Protected App"),
        passwordInput("password_input", "Enter the password:", ""),
        actionButton("submit", "Submit"),
        conditionalPanel(
          condition = "input.password_input != password && input.submit > 0",
          p("Incorrect password. Please try again.", style = "color: red;")
        )
      )
    } else {
      # Show the app UI once the password is correct
      fluidPage(
        tags$style(HTML("
          body {
            background-color: #add8e6;
          }
          .shiny-table {
            background-color: white;
          }
        ")),
        titlePanel(tags$strong("Net-sense, a NSW PVA tool", style = "color: black;")),
        p("By inputting the number of leatherbacks bycaught in NSW shark meshing nets within a year, this app will generate an output plot that shows the population's fate after ten generations if that level of annual bycatch persisted.", style = "font-weight: bold;"),
        helpText("To use this tool, ", style = "color: black;"),
        helpText(
          "1. Decide whether the sex of the bycaught leatherbacks is known. Then, use the drop down box for ",
          tags$strong("Is the sex of bycaught leatherbacks known?"), " to select Yes or No. ",
          HTML("<span style='display: inline-block; width: 20px;'></span>"), 
          "Note: this assumes that all leatherbacks caught are dead on retrieval or suffer post-release mortality; i.e. worst-case scenario",
          style = "color: black;"
        ),
        # Rest of your existing UI
        sidebarLayout(
          sidebarPanel(
            selectInput("sex_known", "Is the sex of bycaught leatherbacks known?", choices = c("Yes", "No")),
            conditionalPanel(
              condition = "input.sex_known == 'Yes'",
              sliderInput("female_input", "Female", value = 0, min = 0, max = 25),
              sliderInput("male_input", "Male", value = 0, min = 0, max = 25)
            ),
            conditionalPanel(
              condition = "input.sex_known == 'No'",
              radioButtons("sex_ratio", "Sex Ratio of bycaught leatherbacks (% female)",
                           choices = c("75 (population default)", "0", "50", "100"), selected = "75 (population default)"),
              sliderInput("unknown_input", "Unknown", value = 0, min = 0, max = 25)
            ),
            selectInput("population", "Population of origin of bycaught leatherbacks:", choices = c("West Pacific", "Solomon Islands")),
            selectInput("result_type", "Show results as:", choices = c("Probability of long-term survival", "Population Size")),
            actionButton("generate_plot", "Generate Plot")
          ),
          mainPanel(
            h3("Survival Plot", style = "font-size: 16px;", style = "color: black;"),
            plotOutput("plot"),
            conditionalPanel(
              condition = "input.result_type == 'Probability of long-term survival'",
              h3(paste("Fate: Probability of population survival to ___ generations"), style = "font-size: 16px;", style = "color: black;")
            ),
            conditionalPanel(
              condition = "input.result_type == 'Population Size'",
              h3(paste("Fate: Number of individuals after ___ generations"), style = "font-size: 16px;", style = "color: black;")
            ),
            tableOutput("summary_table")
          )
        )
      )
    }
  })
  
  # Original server logic for plot generation and table rendering
  observe({
    if (!is.null(input$sex_known)) {
      if ((input$sex_known == "Yes" && (input$female_input > 25 || input$male_input > 25)) ||
          (input$sex_known == "No" && input$unknown_input > 25)) {
        showModal(modalDialog(
          title = "Error",
          "This value is outside the bounds of this predictive tool, based on expected leatherback bycatch in NSW shark meshing nets from previous fisheries reports"
        ))
      }
    }
  })
  
  filtered_data <- eventReactive(input$generate_plot, {
    tmp_data <- data
    tmp_data <- tmp_data[tmp_data$population == input$population, ]
    if (input$sex_known == "Yes") {
      tmp_data <- tmp_data[tmp_data$female == input$female_input & tmp_data$male == input$male_input, ]
    } else {
      sex_ratio <- as.numeric(input$sex_ratio)
      sex_ratio[is.na(sex_ratio)] <- 0
      tmp_data <- tmp_data[tmp_data$ratio == sex_ratio & tmp_data$total == input$unknown_input, ]
    }
    if (input$result_type == "Probability of long-term survival") {
      tmp_data <- tmp_data[tmp_data$output == "psurv", ]
    } else {
      tmp_data <- tmp_data[tmp_data$output == "popsize", ]
    }
    print(head(tmp_data))
    tmp_data
  })
  
  output$plot <- renderPlot({
    if (nrow(filtered_data()) == 0) return(NULL)
    
    df <- filtered_data()
    x_values <- as.numeric(colnames(df)[7:215])
    y_values <- as.numeric(df[1, 7:215])
    std_deviation <- as.numeric(df[1, 216])
    
    if (input$result_type == "Probability of long-term survival") {
      x_label <- "Simulated Year"
      y_label <- "Probability of Survival"
    } else {
      x_label <- "Simulated Year"
      y_label <- "Population Size"
    }
    
    p <- ggplot(data = data.frame(x = x_values, y = y_values), aes(x = x, y = y)) +
      geom_line() +
      labs(title = " ", x = x_label, y = y_label) +
      ylim(0, max(y_values))
    
    p <- p +
      geom_errorbar(aes(ymin = y - std_deviation, ymax = y + std_deviation), color = "dark grey") +
      labs(y = "Standard Deviation")
    
    p
  })
  
  output$summary_table <- renderTable({
    if (nrow(filtered_data()) == 0) return(NULL)
    df <- filtered_data()
    
    if (input$result_type == "Probability of long-term survival") {
      decimal_places <- 3
      title <- paste("Probability of population survival to", df[1, 215], "generations")
    } else {
      decimal_places <- 0
      title <- paste("Number of individuals after", df[1, 215], "generations")
    }
    
    summary_df <- data.frame("1" = ifelse(decimal_places > 0, format(df[1, 26], nsmall = decimal_places), as.integer(df[1, 26])),
                             "3" = ifelse(decimal_places > 0, format(df[1, 68], nsmall = decimal_places), as.integer(df[1, 68])),
                             "5" = ifelse(decimal_places > 0, format(df[1, 109], nsmall = decimal_places), as.integer(df[1, 109])),
                             "10" = ifelse(decimal_places > 0, format(df[1, 215], nsmall = decimal_places), as.integer(df[1, 215])))
    colnames(summary_df) <- c("1", "3", "5", "10")
    rownames(summary_df) <- title
    
    summary_df
  })
}

# Run the app
shinyApp(ui, server)
