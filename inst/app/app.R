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
      ),
      tabPanel(
        "About & How to Read",
        
        tags$h4("What the fields mean"),
        tags$ul(
          tags$li(tags$b("Infection:"), 
                  " This refers to the type of healthcare-associated infection (HAI) recorded in the German PPS 2011 study. ",
                  "The five categories are:",
                  tags$em(" HAP"), " (Hospital-Acquired Pneumonia), ",
                  tags$em(" SSI"), " (Surgical Site Infection), ",
                  tags$em(" BSI"), " (Bloodstream Infection), ",
                  tags$em(" UTI"), " (Urinary Tract Infection), and ",
                  tags$em(" CDI"), " (Clostridioides difficile Infection)."),
          
          tags$li(tags$b("Cases:"), 
                  " The total number of patients in the survey who were identified as having that particular infection type. ",
                  "This data comes from ", tags$code("num_hai_patients"), 
                  ", representing observed infections in the national sample."),
          
          tags$li(tags$b("Rate per 1,000 patients:"), 
                  " The infection rate standardized per 1,000 survey patients, calculated as ",
                  tags$code('1000 * (cases / num_survey_patients)'), ". ",
                  "This allows fair comparison across infection types despite differences in sample size."),
          
          tags$li(tags$b("Days:"), 
                  " The recorded or simulated duration (in days) that an infection persisted in a patient. ",
                  "Each value in ", tags$code("loi_pps[[type]]"), 
                  " represents one modeled infection duration used to estimate the length-of-infection distribution."),
          
          tags$li(tags$b("Count:"), 
                  " The number of infections (real or simulated) that lasted exactly that number of days. ",
                  "For example, a count of 10 at Day 5 means 10 infections lasted 5 days.")
        ),
        
        tags$h4("How to interpret the outputs"),
        tags$p("The dashboard contains two main interactive visualisations that together help understand how common different HAIs were in 2011 and how long they typically lasted in hospital settings."),
        
        tags$ul(
          tags$li(tags$b("Cases (interactive) bar chart:"),
                  " Each bar represents one infection type. ",
                  "The bar height shows the total number of cases observed in the survey. ",
                  "Hovering over a bar reveals both the absolute number of cases and the standardized rate per 1,000 survey patients. ",
                  "This helps identify which HAIs were most prevalent in the hospital sample. ",
                  "For example, if the 'UTI' bar is tallest, surgical site infections were the most commonly recorded HAI during the survey period."),
          
          tags$li(tags$b("Length of Infection (per day) chart:"),
                  " This plot shows the distribution of infection durations for the selected infection type. ",
                  "Each bar corresponds to a specific infection length (in days), and the bar height shows how many infections lasted exactly that long. ",
                  "Hovering over a bar displays the exact day and number of infections. ",
                  "Long right tails indicate that some patients experienced much longer infections, suggesting potential complications or slower recovery. ",
                  "Days with no bar represent durations where no infections were observed or simulated."),
          
          tags$li(tags$b("Comparing infections:"),
                  " You can select different infection types from the dropdown menu to see how their duration distributions differ. ",
                  "For example, bloodstream infections (BSI) may have shorter median durations than hospital-acquired pneumonia (HAP), ",
                  "reflecting differences in clinical treatment complexity.")
        ),
        
        tags$hr(),
        tags$h4("Additional notes and interpretation guidance"),
        tags$p(
          "The German PPS 2011 dataset was a ", tags$b("point-prevalence survey"), 
          "meaning infections were recorded for patients present in hospitals on the survey day. ",
          "Therefore, the 'number of cases' reflects the prevalence of infections rather than incidence over time. "
        ),
        tags$p(
          "The length-of-infection data (", tags$code("loi_pps"), 
          ") are partly derived from statistical simulation rather than direct patient observation. ",
          "This approach allows researchers to estimate average infection duration and total burden of disease more accurately, ",
          "even when only limited real-world data were available."
        ),
        tags$p(
          "Overall, these plots should be read together: the first shows how frequent each infection type was, ",
          "and the second shows how long infections typically lasted. ",
          "In combination, they provide a quick visual understanding of both the ",
          tags$b("prevalence"), " and ", tags$b("severity (duration)"), " of major healthcare-associated infections in Germany."
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
