library(tidyverse)
library(lubridate)
library(plotly)
library(shiny)
library(zoo)

df <- read_csv("data/processed_data.csv") %>% 
  mutate(startTime = endTime - msPlayed / 1000,
         diff = startTime - lag(endTime),
         date = as.Date(endTime))

days <- df %>% 
  group_by(date) %>% 
  summarize(listen_time = sum(msPlayed / 60000),
            songs = n())

test <- df %>% 
  group_by(date, artistName) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = artistName, values_from = count, values_fill = 0)

days$top_artist <- colnames(test)[apply(test,1,which.max)]

tooltip_fun <- function(day, artist, songs, seconds){
  paste("Date:", format(day, "%a %h %d"), '\n',
        "Top Artist:", artist, '\n',
        "Songs Played:", songs, '\n',
        "Total Playtime:", round(seconds_to_period(seconds)))
}

line_fun <- function(df, df2, start, end){
  p <- df %>% 
    filter(date > start, date < end) %>% 
    ggplot(aes(x = date, y = listen_time)) +
    geom_point(aes(text = tooltip_fun(date,
                                      top_artist,
                                      songs,
                                      listen_time * 60))) +
    geom_line() +
    labs(
      title = "Spotify Listening Data",
      x = "Date",
      y = "Listening Time (Minutes)"
    )
  ggplotly(p, tooltip = c("text"))
}

info_text = "I decided to continue with another visualization from Portfolio 2 on my spotify listening data. For this project
I decided to see how my listening time changed day to day. In order to achieve this I created a line plot of listening time in 
minutes versus the date. On each day I added a tooltip when you hover over the date to display a more readable date (including the 
day of the week), listening time in H:M:S rather than just minutes, number of songs listened to, and top artist of the day. From
these tooltips I was able to observe a lot more information than just the lineplot since it was possible to see if I listen more on
the weekend or during the week. Furthermore I was able to see how my top artist changes over time. To glean even more information I
implemented a datatable containing all of the songs listened to when clicking on a date point. One of the most surprising aspects of
this visualization is just how much music I listened to on the highest days. The maximum of the data was almost 9 hours and while this 
data includes podcasts, I found this very suprising. I hoped to be able to color the point on the graph that was actively selected for
the datatable but I was not able to find the correct method at this time."

ui <- fluidPage(
  titlePanel("Portofolio Project 3: Spotify Listening Data"),
  p(info_text),
  dateRangeInput("dateRange",
                 "Date Range:",
                 start = min(days$date),
                 end = max(days$date)),
  plotlyOutput("lineplot"),
  p("Click a point to open the associated dataframe below."),
  dataTableOutput("click")
)

server <- function(input, output) {
  lineplot <- reactive({
    line_fun(days, df, input$dateRange[1], input$dateRange[2])
  })
  output$lineplot = renderPlotly(lineplot())
  output$click <- renderDataTable({
    d <- event_data("plotly_click")
    if (is.null(d)) "Hello" else{
      hover_date <- as.Date(pull(d, x))
      df %>% 
        filter(date == hover_date) %>% 
        select(startTime, trackName, artistName, msPlayed)
    }
  })
}

shinyApp(ui, server)