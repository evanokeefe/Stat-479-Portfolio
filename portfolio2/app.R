library(tidyverse)
library(tsibble)
library(shiny)
library(feasts)

df <- read_csv("../data/processed_data.csv")

gen_data <- function(df, start, end, track, month){
    df %>% 
        filter(endTime > start,
               endTime < end) %>% 
        group_by(artistName) %>% 
        {if (track) 
            group_by(., trackName, .add = TRUE) else .} %>%
        {if (month) 
            group_by(., month, .add = TRUE) else .} %>% 
        summarise(count = n()) %>% 
        arrange(desc(count))}

seasons_plot_fun <- function(df, start, end){
    df %>%
        filter(!are_duplicated(df, index = endTime, key = trackName),
               endTime > start,
               endTime < end) %>%
        mutate(date = as.Date(endTime)) %>% 
        group_by(date) %>% 
        summarise(count = n()) %>%
        tsibble(index = date) %>% 
        fill_gaps(count = 0) %>% 
        gg_season(period = "month") +
        labs(
            title = "Number of Streams By Month",
            x = "Week",
            y = "Number of Streams"
        )
}

ui <- fluidPage(
    titlePanel("My Spotify Listening Data"),
    dateRangeInput("dateRange",
                   "Date Range:",
                   start = min(df$endTime),
                   end = max(df$endTime)),
    checkboxInput("track", "Summarise by Track"),
    checkboxInput("month", "Summarise by Month"),
    plotOutput("season"),
    dataTableOutput("dt")
)

server <- function(input, output) {
    df_subset <- reactive({
        gen_data(df, input$dateRange[1], input$dateRange[2], input$track, input$month)
    })
    seasons_plot <- reactive({
        seasons_plot_fun(df, input$dateRange[1], input$dateRange[2])
    })
    output$season <- renderPlot(seasons_plot())
    output$dt <- renderDataTable(df_subset())
}

shinyApp(ui, server)