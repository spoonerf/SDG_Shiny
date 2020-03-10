#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(maps)
library(mapproj)
library(readr)
library(rnaturalearthdata)
library(rnaturalearth)
library(rnaturalearthhires)
library(janitor)
library(dplyr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(rgeos)

source("sdg_plotter.R")




# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel(strong("SDG Indicators")),
                
                sidebarLayout(sidebarPanel(
                    h3("Indicator Data Availability"),
                    selectInput(
                        "goal_view",
                        h3("Select Goal:"),
                        choices = list(
                            "All Goals" = "All Goals",
                            "Biosphere Pillar" = "the Biosphere Pillar",
                            "Economy Pillar" = "the Economy Pillar",
                            "Society Pillar" = "the Society Pillar",
                            "Goal 1 - No Poverty" = "Goal 1",
                            "Goal 2 - Zero Hunger" = "Goal 2",
                            "Goal 3 - Good Health and wel-being" = "Goal 3",
                            "Goal 4 - Quality Education" = "Goal 4",
                            "Goal 5 - Gender Equality" = "Goal 5",
                            "Goal 6 - Clean Water and Sanitation" = "Goal 6",
                            "Goal 7 - Affordable and Clean Energy" = "Goal 7",
                            "Goal 8 - Decent Work and Economic Growth" = "Goal 8",
                            "Goal 9 - Industry, Innovation and Infrastructure" = "Goal 9",
                            "Goal 10 - Reduced Inequality" = "Goal 10",
                            "Goal 11 - Sustainable Cities and Communities" = "Goal 11",
                            "Goal 12 -Responsible Consumption and Production" = "Goal 12",
                            "Goal 13 - Climate Action" = "Goal 13",
                            "Goal 14 - Life Below Water" = "Goal 14",
                            "Goal 15 - Life on Land" = "Goal 15",
                            "Goal 16 - Peace, Justice and Strong Institutions" = "Goal 16",
                            "Goal 17 - Partnerships to achieve the Goal" = "Goal 17"
                            
                        ),
                        selected = "Goal 14"
                    )
                ),
                
                mainPanel(leafletOutput("map", width = 800, height = 500))
                )
                )


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$selected_var <- renderText({
        paste("You have selected", input$goal_view)
    })
    
    output$map <- renderLeaflet({
        goal_in <- switch(
            input$goal_view,
            "All Goals" = 1:17,
            "the Biosphere Pillar" = c(6, 7, 12, 13, 14, 15),
            "the Economy Pillar" = c(1, 2, 3, 8, 9),
            "the Society Pillar" = c(4, 5, 10, 11, 16, 17),
            "Goal 1" = 1,
            "Goal 2" = 2,
            "Goal 3" = 3,
            "Goal 4" = 4,
            "Goal 5" = 5,
            "Goal 6" = 6,
            "Goal 7" = 7,
            "Goal 8" = 8,
            "Goal 9" = 9,
            "Goal 10" = 10,
            "Goal 11" = 11,
            "Goal 12" = 12,
            "Goal 13" = 13,
            "Goal 14" = 14,
            "Goal 15" = 15,
            "Goal 16" = 16,
            "Goal 17" = 17
        )
        
        sdg_plotter(goal_view = goal_in)
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
