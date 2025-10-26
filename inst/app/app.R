#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(BHAI)

data(german_pps_2011_repr)

df_cases <- data.frame(
  infection = names(num_hai_patients),
  cases = as.integer(num_hai_patients)
) %>%
  mutate(rate_per_1000 = 1000 * cases / as.numeric(num_survey_patients))

ui <- fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "minty",
    bg = "lightblue",      
    fg = "black",       
    base_font = bslib::font_google("Lato"),
    heading_font = bslib::font_google("Montserrat")
  ),
  titlePanel("HAI Dashboard – German PPS 2011"),
  fluidRow(
    column(
      4, 
      div(
        style = "background:#F8F9FA; padding:15px; border-radius:10px; text-align:center; box-shadow:0 0 5px #ccc;",
        h4("Survey Patients"), 
        h3(textOutput("survey"))
      )
    ),
    column(
      4, 
      div(
        style = "background:#F8F9FA; padding:15px; border-radius:10px; text-align:center; box-shadow:0 0 5px #ccc;",
        h4("Population (2011)"), 
        h3(textOutput("pop"))
      )
    ),
    column(
      4, 
      div(
        style = "background:#F8F9FA; padding:15px; border-radius:10px; text-align:center; box-shadow:0 0 5px #ccc;",
        h4("Mean Length of Stay (days)"), 
        h3(textOutput("los"))
      )
    )
  ),
  br(),
  
  mainPanel(
    tabsetPanel(
      id = "tabs",   
      tabPanel("Cases (interactive)", plotlyOutput("barplot")),
      tabPanel(
        "Length of Infection",
        plotlyOutput("histogram"),
        br(),
        conditionalPanel(
          condition = "input.tabs == 'Length of Infection'",
          br(),
          selectInput("infection_type", "Select Infection Type:",
                      choices = names(loi_pps),
                      selected = "HAP")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$survey <- renderText(format(num_survey_patients, big.mark = ","))
  output$pop <- renderText(format(population, big.mark = ","))
  output$los <- renderText(round(length_of_stay, 2))
  
  output$barplot <- renderPlotly({
    plot_ly(
      df_cases,
      x = ~infection,
      y = ~cases,
      type = 'bar',
      text = ~paste0("Cases: ", cases, "<br>Rate: ", round(rate_per_1000, 2), " /1k patients"),
      hoverinfo = 'text',
      textposition = "none",     
      color = ~infection,
      colors = "Set2"    ) %>%
      layout(
        title = "HAI Cases by Infection Type (German PPS 2011)",
        xaxis = list(title = ""),
        yaxis = list(title = "Cases"),
        margin = list(l = 50, r = 30, b = 70, t = 50)
      )
  })
  
  output$histogram <- renderPlotly({
    days <- unlist(loi_pps[[input$infection_type]])
    day_seq <- seq(min(days, na.rm = TRUE), max(days, na.rm = TRUE))
    counts <- as.integer(tabulate(factor(days, levels = day_seq)))
    df_day <- data.frame(day = day_seq, count = counts)
    
    plot_ly(
      df_day,
      x = ~day,
      y = ~count,
      type = "bar",
      marker = list(color = 'steelblue'),
      hovertemplate = "Day %{x}<br>Total %{y} cases<extra></extra>"
    ) %>%
      layout(
        title = paste("Length of Infection (per day) for", input$infection_type),
        xaxis = list(title = "Days", tickmode = "linear", dtick = 10),
        yaxis = list(title = "Count"),
        margin = list(l = 50, r = 30, b = 70, t = 50)
      )
  })
  
}
shinyApp(ui = ui, server = server)
