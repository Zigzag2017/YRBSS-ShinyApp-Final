library(shiny)
library(ggplot2)
library(dplyr)

YRBSSdata <- read.csv("YRBSS_clean.csv", stringsAsFactors = FALSE)

YRBSSdata <- YRBSSdata %>%
  rename(
    State = Area,
    Demographic_Type = Demographics_Type,
    Demographic = Demographics_Value
  )

YRBSSdata$Year <- as.numeric(YRBSSdata$Year)
YRBSSdata$Percent <- as.numeric(YRBSSdata$Percent)

YRBSSdata <- YRBSSdata %>%
  filter(!is.na(Percent))

ui <- fluidPage(
  titlePanel("Adolescent Sadness & Hopelessness (YRBSS 2019–2023)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Choose a State:",
                  choices = c("All", sort(unique(YRBSSdata$State))),
                  selected = "All"),
      
      selectInput("demo_type", "Choose Demographic Type:",
                  choices = unique(YRBSSdata$Demographic_Type),
                  selected = unique(YRBSSdata$Demographic_Type)[1])
    ),
    
    mainPanel(
      plotOutput("trendPlot"),
      br(),
      textOutput("note")
    )
  )
)

server <- function(input, output) {
  
  output$trendPlot <- renderPlot({
    
    filtered <- YRBSSdata %>%
      filter(
        (input$state == "All" | State == input$state),
        Demographic_Type == input$demo_type
      )
    
    validate(
      need(nrow(filtered) > 0, "No data available for this selection.")
    )
    
    if (input$state == "All") {
      filtered <- filtered %>%
        group_by(Year, Demographic) %>%
        summarize(Percent = mean(Percent, na.rm = TRUE), .groups = "drop")
    }
    
    ggplot(filtered, aes(x = Year, y = Percent, color = Demographic)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      labs(
        title = paste("Sadness & Hopelessness Trends -",
                      ifelse(input$state == "All", "All States", input$state)),
        subtitle = paste("Demographic Type:", input$demo_type),
        x = "Year",
        y = "Percent of Students",
        color = "Group"
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme_minimal(base_size = 14)
  })
  
  output$note <- renderText({
    "Data Source: CDC YRBSS (Youth Risk Behavior Surveillance System), 2019–2023"
  })
}

shinyApp(ui = ui, server = server)
