# data$std_dev <- apply(data[, -(1:5)], 1, sd)
# library(openxlsx)

# write.xlsx(data, "NSW_Shiny_StdDev.xlsx")

# Now the Shiny app
library(shiny)
library(ggplot2)
library(readxl)
library(knitr)
library(openxlsx)

# Load the Excel sheet with data from GitHub
data_url <- "https://github.com/annaor24/NSW_PVA/raw/main/NSW_Shiny_StdDev.xlsx"
data <- read.xlsx(data_url, sheet = 1)

# Define UI
ui <- fluidPage(
  tags$style(HTML(
    "
    body {
      background-color: #f2f2f2; /* Change to your desired background color */
    }
    "
  )),
  titlePanel("NSW Shark Meshing net mortalities: impact on leatherback population survival"),
  sidebarLayout(
    sidebarPanel(
      selectInput("population", "Population harvested from:", choices = c("West Pacific", "Solomon Islands")),
      sliderInput("leatherbacks", "Leatherback mortalities:", min = 0, max = 13, value = 1),
      selectInput("female", "Sex", choices = c("All Female", "Male and female, __chosen value__ of each")),
      actionButton("generate_plot", "Generate Plot")
    ),
    mainPanel(
      h3("Survival Plot", style = "font-size: 16px;"),
      plotOutput("plot"),
      h3("Survival Probability by Year", style = "font-size: 16px;"),
      tableOutput("summary_table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- eventReactive(input$generate_plot, {
    filter_data(input$population, input$leatherbacks, input$female)
  })
  
  output$plot <- renderPlot({
    if (is.null(filtered_data())) {
      return(NULL)
    }
    
    df <- filtered_data()
    
    x_values <- as.numeric(colnames(df)[3:71])
    y_values <- as.numeric(df[1, 3:71])
    std_dev_values <- as.numeric(df[1, "std_dev"])
    
    df_plot <- data.frame(x = x_values, y = y_values, std_dev = std_dev_values)
    
    first_below_0.5 <- min(x_values[y_values < 0.5], na.rm = TRUE)
    first_below_0.5_column <- colnames(df)[which(x_values == first_below_0.5) + 2]
    
    ggplot(data = df_plot, aes(x = x, y = y)) +
      geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#f2f2f2") +
      geom_line() +
      geom_errorbar(aes(ymin = y - std_dev, ymax = y + std_dev), width = 0.2) +  # Adding error bars
      scale_x_continuous(name = "Simulated Year", limits = c(2017, 2085), breaks = seq(2017, 2085, by = 5)) +
      scale_y_continuous(name = "Probability of Survival", limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      labs(title = "Leatherbacks Survival Plot") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  
  output$summary_table <- renderTable({
    if (is.null(filtered_data())) {
      return(NULL)
    }
    
    df <- filtered_data()
    
    summary_df <- data.frame("2025" = df[1,12],"2050" = df[1, 37], "2075" = df[1, 62], "2085" = df[1,72])
    colnames(summary_df) <- c("2025", "2050", "2075", "2085")
    rownames(summary_df) <- "Probability of Population Survival"
    
    summary_df
  })
  
  filter_data <- function(population, leatherbacks, female) {
    filtered_data <- data[data$population == population &
                            data$leatherbacks == leatherbacks &
                            data$female == ifelse(female == "Yes", "female", "mixed"), ]
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    return(filtered_data)
  }
}

# Run the app
shinyApp(ui, server)

