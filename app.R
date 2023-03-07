library(tidyverse)
library(shiny)

Artists <- read_delim("best_selling_artists.csv")



ui <- fluidPage(
  titlePanel("Best Selling Artists"),
  tabsetPanel(
    tabPanel("About",
             h2("Debrief"),
             p("This website summarizes the sales of the most popular artists over the past century. There are", nrow(Artists), "artists in the dataset."),
             p("My goal with this page is to display data on these artists and show statistics on who has been successful in the music industry. This way, it's easy to see which artists from which country and what genre are seeing the most success."),
             h2("Sample"),
             mainPanel(
              tableOutput("sample")
             )
    ),
    tabPanel("Plot", 
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("Country", "Select Country",
                              choices = c(unique(Artists$Country))),
                 radioButtons("color", "Choose color",
                              choices = c("#D52D00", "#FF9A56", "#D162A4", "#A30262")),
                 textOutput("plotmsg")
                 ),
               mainPanel(
               plotOutput("plot"))
    )
               ),
    tabPanel("Table", tableOutput("table"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput("top", "Display Top (x) Artists",
                             min = 1,
                             max = 121,
                             value = 10,
                             step = 1)),
                 textOutput("msg")
               ),
               mainPanel(ui = fluidPage(
               fluidRow(
                 column(12,
                        tableOutput('table'))
               )
             ))
  )
)
)

server <- function(input, output) {
  output$checkboxGenre <- renderUI({
    checkboxGroupInput("country", "Choose country",
                       choices = unique(Artists$Country)
    )
  })
  output$plot <- renderPlot({
    Artists %>%
      filter(Country%in%input$Country) %>%
      ggplot(aes(Artist, Sales)) +
      geom_bar(stat='identity', position='dodge', fill=input$color) + 
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  })
  output$table <- renderTable({
    Artists %>%
      select(Artist, Country, Genre, Year, Sales) %>%
      head(input$top)
    })
  output$sample <- renderTable({
    Artists %>%
      select(Artist, Country, Sales) %>% 
      sample_n(10)
  })
  output$msg <- renderText({
    paste("These are the top", input$top, "artists by sales.")
  })
  output$plotmsg <- renderText({
    plottext <- Artists %>%
      group_by(Country, Artist) %>%
      reframe(sale_country=sum((Country%in%input$Country)))
    paste("There are", sum(plottext$sale_country), "artists from", input$Country)
  })
}
shinyApp(ui = ui, server = server)
