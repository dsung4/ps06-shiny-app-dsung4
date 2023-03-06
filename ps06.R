library(shiny)
library(tidyverse)
library(rsconnect)

demographic <- read_delim("Count Us In/Demographics-Table 1.csv")
causes <- read_delim("Count Us In/Cause-Table 1.csv")
total <- read_delim("Count Us In/Total-Table 1.csv")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("About", titlePanel("Homelessness Statistics"),
             p("This app uses data collected on homelessness in ", 
               em("Seattle, WA"), "from", strong("1998-2020.")),
             br(),
             p("Below is a random sample of the homeless population in King 
               County along with its overall population."),
             tableOutput("random")
    ),
    
    tabPanel("Plot", sidebarLayout(
      sidebarPanel(
        p("Select a cause of homelessness and see how many are affected each 
          year."),
        fluidRow(
          column(6,
                 radioButtons("color", "Choose color",
                              choices = c("skyblue", "darkseagreen", "khaki", 
                                                   "orange", "tan", "sienna", 
                                                   "salmon", "pink", "thistle"))
          ),
          column(6,
                 uiOutput("checkboxShelter")
          )
        )
      ),
      mainPanel(
        plotOutput("plot"),
        textOutput("years")
      )
    )),
    
    tabPanel("Table", sidebarLayout(
      sidebarPanel(
        p("Here, you may see the homeless data based on the demographic you 
          choose."),
        fluidRow(
          column(6,
                 uiOutput("checkboxDemographics")
          ),
        )
      ),
      mainPanel(
        textOutput("message"),
        tableOutput("data_table")
      )         
    ))
  )
)

server <- function(input, output) {
  ## For the about page
  output$random <- renderTable({
    total %>%  
      sample_n(6)
  })
  
  ## For the plot page
  cleaned_causes <- causes %>% 
    select_if(~!any(is.na(.)))
  
  output$checkboxShelter <- renderUI({
    radioButtons("Cause", "Causes of homelessness",
                 choices = unique(cleaned_causes$Cause),
                 selected = NULL
    )
  })
  
  plot_sample <- reactive({
    cleaned_causes %>%
      filter(Cause == input$Cause)
  })
  
  output$plot <- renderPlot({
    p <- plot_sample() %>%
      ggplot(aes(x = factor(Year), y = Count, fill = factor(Cause))) +
      geom_col() +
      labs(x = "Year", y = "Number of people homeless", fill = "Cause") +
      scale_fill_manual(values = input$color)
    
    if(nrow(plot_sample()) == 0) {
      p <- p + labs(title = "Please select a cause")
    } 
    p
  })
  
  output$years <- renderText({
    years <- plot_sample() %>% pull(Year) %>% unique()
    if (length(years) == 1) {
      paste("This data is from", years)
    } else {
      paste("The data ranges from", min(years), "to", max(years))
    }
  })
  
  ## For the table page
  demographics <- demographic %>% 
    select_if(~ !any(is.na(.)))
  
  output$checkboxDemographics <- renderUI({
    radioButtons("Demographic", "Choose a demographic",
                 choices = unique(demographics$Demographic),
                 selected = NULL)
    
  })
  
  selected_demographics <- reactive({
    demographics %>% 
      filter(Demographic == input$Demographic) %>% 
      filter(!is.na(Count))
  })
  
  output$data_table <- renderTable({
    selected_demographics()
  })
  
  output$message <- renderText({
    n <- nrow(selected_demographics())
    paste("Selected demographic contains", n, "observations.")
  })
}

shinyApp(ui = ui, server = server)