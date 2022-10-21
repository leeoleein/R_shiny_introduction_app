options(shiny.maxRequestSize = 30*1024^2)

library(shiny)
library(dplyr) # Package for data manipulations
library(magrittr) # Package for pipe operator
library(ggplot2) # Package for creating graphs
library(shinythemes) #Package for shinythemes 

#course_data <- readRDS("data/europe.rds") %>% # Load the course data
#  mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) # Create a new column with Avg Temperature in Celsius



#Define UI for application ----
ui <- fluidPage(
  theme = shinytheme(theme="darkly"),
  
  
  #Application title
  titlePanel("Course Shiny App"),
  "This is the Course shiny app.",
  
  
  sidebarLayout(
    sidebarPanel(
      #Slider Year ----
      sliderInput(inputId = "Year", label = "Year",
                  min=2000, max=2019, value=2000, step = 1, sep =''),
      
      #Dropdown Country ----
      selectInput(inputId = "Country", label = "Country:",
                  choices = NULL),
      
      #Dropdown City ----
      selectInput(inputId = "City", label = "City:",
                  choices=NULL), 
      
      #Textinput ----
      textInput(inputId = "text_input", label = "Text Input:"),
      
      #Radio Buttons ----
      radioButtons(inputId="Temperature", label ="Temperature Type:",
                   choices = list("Fahrenheit" = "Fahrenheit", "Celsius" ="Celsius"), 
                   selected = "Fahrenheit"),
      
      #ActionButton
      actionButton(inputId ="button", label="LET'S GO!"),
      
      
      #File Upload
      fileInput(inputId ="file", label ="Upload a file (RDS)",
                multiple = FALSE, accept=".rds"),
      
      #Download Button
      downloadButton(outputId = "download", label = "Download Data")
      
      
      
    ),
    
    
    
    mainPanel( "main panel", 
               textOutput(outputId="text_output"),
               tabsetPanel(type = "tabs", 
                           tabPanel(title = "Info",
                                    h3("App description"),
                                    p("This is the shiny app. Data:", br(), strong("Average daily temperature (in Fahrenheit)"),
                                      br(), strong("from cities around Europe from 2000 to 2019")),
                                    hr(),
                                    verbatimTextOutput("data_summary")
                           ),
                           tabPanel(title = "Data",
                                    dataTableOutput("data_table")
                           ),
                           tabPanel(title = "Plots",
                                    fluidRow(
                                      column(width = 12, plotOutput("lineplot"))
                                    ),
                                    fluidRow(
                                      column(width = 6, plotOutput("lineplot_1")),
                                      column(width = 6, plotOutput("lineplot_2"))
                                    ),
                                    fluidRow(
                                      column(width = 12, plotOutput("boxplot"))
                                    )
                                    
                           ))
               
               
    )
  )
)

#Define server side logic ----
server <- function(input, output, session) {
  
  output$download <- downloadHandler(
    filename = "Temperature_data.csv",
    content = function(file){
      write.csv(country_df(), file, row.names = FALSE)
    }
  )
  
  course_data <- eventReactive(input$file, {
    readRDS(input$file$datapath)%>%
      mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) # Create a new column with Avg Temperature in Celsius
  })
  
  #create dataframes:
  #Reactive: data frame Country
  country_df <- eventReactive(input$button, {
    course_data() %>%
      filter(Year >= input$Year) %>% # Subset the rows to keep data more than or equal to a year
      filter(Country == input$Country) # Subset the rows to keep a specific country
  })
  
  #Reactive: Dataset City 
  city_df <- reactive({ 
    country_df() %>% 
      filter(City == input$City) %>%
      filter(Year == input$Year)
  })
  
  #Reactive: Dataset min/mean/max Monthly temperatures
  year_df <- reactive({
    country_df() %>% 
      filter(City == input$City) %>% # Subset the rows for specific City
      filter(Year == input$Year) %>%  # Subset the rows for specific Year
      group_by(Country, City, Year, Month) %>% 
      summarise(MinTempF = min(AvgTemperatureF),
                MeanTempF = round(mean(AvgTemperatureF), 1),
                MaxTempF = max(AvgTemperatureF),
                MinTempC = min(AvgTemperatureC),
                MeanTempC = round(mean(AvgTemperatureC), 1),
                MaxTempC = max(AvgTemperatureC)) %>% 
      ungroup()
  })
  
  #Output
  output$text_output <- renderText({
    print(input$slider_1) #to control, if the user input get right back to the server, its invisible for the user, but we can see in the console what we do.  -> you can see aswell if out server see the answers from the users as its a number or a character
    paste("You typed:", input$text_input, input$Year, input$Country, input$City, input$Temperature)
  })
  output$data_summary <- renderPrint ({
    summary(course_data())
  })
  output$data_table <- renderDataTable({
    country_df()
  })
  output$lineplot <- renderPlot({
    ggplot(data = city_df()) +
      geom_line(mapping = aes(x = Date, y = AvgTemperatureF), size = 1) +
      ylab("Average daily temperatures (in Fahrenheit)")
  })
  output$boxplot <- renderPlot({
    ggplot(data = country_df()) +
      geom_boxplot(mapping = aes(x = Month, y = AvgTemperatureF, group = Year))
  })
  output$lineplot_1 <- renderPlot({
    ggplot(data = year_df()) +
      geom_line(mapping = aes(x = Month, y = MinTempF), size = 1, colour = "red", linetype = "dotted") +
      geom_line(mapping = aes(x = Month, y = MeanTempF), size = 1, colour = "black") +
      geom_line(mapping = aes(x = Month, y = MaxTempF), size = 1, colour = "red", linetype = "dotted") +
      scale_x_discrete(name = "", limits = month.abb) +
      ylab("Average daily temperatures (in Fahrenheit)")
  })
  output$lineplot_2 <- renderPlot({
    ggplot(data = year_df()) +
      geom_line(mapping = aes(x = Month, y = MinTempC), size = 1, colour = "red", linetype = "dotted") +
      geom_line(mapping = aes(x = Month, y = MeanTempC), size = 1, colour = "black") +
      geom_line(mapping = aes(x = Month, y = MaxTempC), size = 1, colour = "red", linetype = "dotted") +
      scale_x_discrete(name = "", limits = month.abb) +
      ylab("Average daily temperatures (in Celsius)")
  })
  
  observe({
    print(input$Country)
    print(input$button)
    print(input$file$datapath)
  })
  
  
  observe({
    new_country_choices <- unique(course_data()$Country)
    updateSelectInput(session, inputId = "Country", choices = new_country_choices)
  })
  
  observe({
    new_city_choices <- unique(course_data()$City[course_data()$Country == input$Country])
    updateSelectInput(session, inputId = "City", choices = new_city_choices)
  })
  
}


#Run the application ----
shinyApp(ui = ui, server = server)