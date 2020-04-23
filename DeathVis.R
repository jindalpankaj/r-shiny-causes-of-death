library(shiny)
library(data.table)

data <- read.csv(file = "data/death_data.csv")
data <- as.data.table(data)
data <- setorder(data, location, year)
country_names <- sort(unique(data$location))
death_causes <- as.character(sort(unique(data$cause)))

cause_details <- read.table(file = "data/cause_details.txt", 
                            header = FALSE, sep = "\n", stringsAsFactors = FALSE)
cause_details <- as.data.table(setorder(cause_details, V1))
cause_details[, c("cause", "detail") := tstrsplit(V1, " include: ", fixed=TRUE)]

# Define UI for app that draws the line plot ----
ui <- fluidPage(
  # App title ----
  titlePanel("Causes of worldwide deaths in the last 30 years."),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(width = 6,
      # Input: Dropdown for selecting country ----
      selectInput(inputId = "inputCountry", 
                  label = "Select Country:", 
                  choices = country_names,
                  selected = "United States", multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      
      selectInput(inputId = "inputCause", 
                  label = "Select Cause of death:", 
                  choices = death_causes,
                  selected = NULL, multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      
      textOutput("selected_cause_details")
      
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(width = 6,
      # Output: Line Plot ----
      plotOutput(outputId = "distPlot")
    )
  ),
  
  hr(),
  
    fluidRow(
    column(12, helpText("This is a non-commercial project created for 
                        the purpose of self-learning. Code is available at 
                        https://github.com/jindalpankaj/r-shiny-causes-of-death. 
                        Last updated 22 April 2020."), 
           align = "center")
  ),
  
  fluidRow(
    column(12, helpText("All data is sourced from the Global Disease Burden 
             project, available to download from http://ghdx.healthdata.org/gbd-results-tool."),
           align = "center")
  )
  
)

# Define server logic required to draw the line plot ----

server <- function(input, output) {

  yr <- reactive(sort(unique(data[location == input$inputCountry, year])))
  y_tot <- reactive(data[location == input$inputCountry & cause == input$inputCause, sum(val), year]$V1)
  y_male <- reactive(data[location == input$inputCountry & cause == input$inputCause & sex == "Male", val, ])
  y_fem <- reactive(data[location == input$inputCountry & cause == input$inputCause & sex == "Female", val, ])  
  
  output$selected_cause_details <- renderText({
   switch(input$inputCause,
          "Cardiovascular diseases" = cause_details[1]$V1,
          "Chronic respiratory diseases" = cause_details[2]$V1,
          "Diabetes and kidney diseases" = cause_details[3]$V1,
          "Digestive diseases" = cause_details[4]$V1,
          "Enteric infections" = cause_details[5]$V1,
          "Mental disorders" = cause_details[6]$V1,
          "Musculoskeletal disorders" = cause_details[7]$V1,
          "Neglected tropical diseases and malaria"	 =  cause_details[8]$V1,
            "Neoplasms" =  cause_details[9]$V1,
            "Neurological disorders" =  cause_details[10]$V1,
            "Nutritional deficiencies" =  cause_details[11]$V1,
            "Other infectious diseases" = cause_details[12]$V1,
            "Other non-communicable diseases" =  cause_details[13]$V1,
            "Self-harm and interpersonal violence"	 =  cause_details[14]$V1,
            "Sense organ diseases"	 = cause_details[15]$V1,
            "Skin and subcutaneous diseases"	 =  cause_details[16]$V1,
            "Substance use disorders"	 =  cause_details[17]$V1,
            "Unintentional injuries"	 =  cause_details[18]$V1,
          input$inputCause)
  }
  )
    
  output$distPlot <- renderPlot({
    plot(x = yr(),
         y = y_tot(),
         type = "l", col = "black", lwd = 2,
         xlab = "Year", ylab = "Number of deaths",
         main = paste0("Death in ", input$inputCountry, " due to ", input$inputCause),
         ylim = c(min(y_male(),y_fem()),max(y_tot()))
    )

    lines(x = yr(),
          y = y_fem(),
          type = "l",  col = "darkred", lwd = 2)
    
    lines(x = yr(),
          y = y_male(),
          type = "l", col = "darkblue", lwd = 2)
    
    legend("topleft", legend=c("Total", "Female", "Male"),
           col=c("black", "darkred", "darkblue"), lty = 1, lwd = 2, cex=0.8)
    
    grid()
    })
}

# running the app
shinyApp(ui = ui, server = server)
