library(tidyverse)
library(shiny)
library(gapminder)
library(countrycode)
library(plotly)
library(scales)
library(DT)

gapminder <- gapminder

gapminder <- gapminder %>% 
  filter(country!='Kuwait')

plot_1 <- function(countries, var){
  
  plt <- gapminder %>%
    pivot_longer(lifeExp:gdpPercap) %>% 
    filter(country %in% countries, name==var) %>%  
    ggplot(aes(year, value, color = continent,group = country))+ 
    geom_line()+
    geom_point()
  
  ggplotly(plt)
}

plot_2 <- function(yr){
  
  plt <- gapminder %>%
    filter(year==yr) %>% 
    ggplot(aes(gdpPercap, lifeExp, color=continent,size=pop, text = paste(country,
                                                                          '<br>GDP per capita: ',number(gdpPercap, 1),
                                                                          '<br>Population: ', number(pop/10000,1),'M'))) +
    geom_point()+
    theme_minimal()
  
  ggplotly(plt, tooltip = 'text')
}



#### ui

ui <- fluidPage(
  titlePanel("Gapminder Data"),
  tabsetPanel(
    tabPanel(title = 'time series',
             sidebarLayout(
               sidebarPanel(
                 selectizeInput(inputId ='countries', 
                                label='Choose the countries', 
                                choices = unique(gapminder$country), 
                                multiple = TRUE, 
                                selected = "United States") ,  
                 selectizeInput(inputId ='inputvariable', 
                                label='Choose the variable to display', 
                                choices =  c("lifeExp","pop","gdpPercap"),
                                multiple = FALSE,
                                selected = "lifeExp")
                 
               ),
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Plot", 
                                      h3("Time series plot"),
                                      plotlyOutput("plot_1")), 
                             tabPanel("Table", 
                                      h3("Time series table"),
                                      dataTableOutput("table"))
                 )
                 
               )
             )
    ),
    tabPanel(title = 'distribution by year',
             wellPanel(
                 sliderInput(inputId ='yr', 
                             label='Choose the year',
                             min = min(gapminder$year),
                             max = max(gapminder$year),
                             value = min(gapminder$year),
                             step = 5,
                             animate = TRUE)
                 ),
                 h3("Distribution by year of life-expectancy and GDP per capita"),
                 plotlyOutput("plot_2") 
          
    )
  )
)


#### server

server <- function(input, output) {
  
  output$plot_1 <- renderPlotly({ 
    plot_1(countries = input$countries, var = input$inputvariable)
  })
  
  output$table <- renderDataTable({
    gapminder %>%
      filter(country %in% input$countries) %>% 
      select(country,continent, year, input$inputvariable )
  }, 
  extensions = 'Buttons',
  options = list(
    paging = TRUE,
    searching = TRUE,
    fixedColumns = TRUE,
    autoWidth = TRUE,
    ordering = TRUE,
    dom = 'tB',
    buttons = c('copy', 'csv', 'excel')
  ),
  class = "display")
  
  output$plot_2 <- renderPlotly({ plot_2(yr = input$yr)})
  
}

shinyApp(ui = ui, server = server)