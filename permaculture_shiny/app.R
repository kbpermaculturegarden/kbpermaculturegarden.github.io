#Here we use a shinylive app to host on github.io

#install.packages(c("shinylive", "httpuv"))

# to build shinylive, run:
#shinylive::export(appdir = "permaculture_shiny", destdir = "docs")

# to test run, run:-
#httpuv::runStaticServer("docs")

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(tidyverse)
library(igraph)
library(glue)
library(htmltools)
library(networkD3)

library(rjson)
library(jsonlite)


library(ggpubr)

library(ggplot2)
library(ggrepel)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           htmlOutput('networkPlot')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    output$networkPlot <- renderPrint({
      d3ForceNetwork(Nodes = MisNodes,
                     Links = MisLinks,
                     Source = "source", Target = "target",
                     Value = "value", NodeID = "name",
                     Group = "group", width = 400, height = 500,
                     opacity = input$slider, standAlone = FALSE,
                     parentElement = '#networkPlot')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
