#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(janitor)
library(leaflet)
library(RColorBrewer)
library(readr)
library(rgeos)
library(rnaturalearthdata)
library(rnaturalearth)
library(rnaturalearthhires)
library(shiny)
library(sf)

source("sdg_plotter.R")
source("sdg_useful.R")

# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel(strong("SDG Indicators")),
                
                navbarPage(
                  "",
                  tabPanel("Maps",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Indicator Data Availability"),
                               selectInput(
                                 "goal_view",
                                 h4("Select Goal:"),
                                 choices = list(
                                   "All Goals" = "All Goals",
                                   "Biosphere Pillar" = "the Biosphere Pillar",
                                   "Economy Pillar" = "the Economy Pillar",
                                   "Society Pillar" = "the Society Pillar",
                                   "Goal 1 - No Poverty" = "Goal 1",
                                   "Goal 2 - Zero Hunger" = "Goal 2",
                                   "Goal 3 - Good Health and well-being" = "Goal 3",
                                   "Goal 4 - Quality Education" = "Goal 4",
                                   "Goal 5 - Gender Equality" = "Goal 5",
                                   "Goal 6 - Clean Water and Sanitation" = "Goal 6",
                                   "Goal 7 - Affordable and Clean Energy" = "Goal 7",
                                   "Goal 8 - Decent Work and Economic Growth" = "Goal 8",
                                   "Goal 9 - Industry, Innovation and Infrastructure" = "Goal 9",
                                   "Goal 10 - Reduced Inequality" = "Goal 10",
                                   "Goal 11 - Sustainable Cities and Communities" = "Goal 11",
                                   "Goal 12 - Responsible Consumption and Production" = "Goal 12",
                                   "Goal 13 - Climate Action" = "Goal 13",
                                   "Goal 14 - Life Below Water" = "Goal 14",
                                   "Goal 15 - Life on Land" = "Goal 15",
                                   "Goal 16 - Peace, Justice and Strong Institutions" = "Goal 16",
                                   "Goal 17 - Partnerships to achieve the Goal" = "Goal 17"
                                   
                                 ),
                                 selected = "Goal 14"
                               ),
                               
                               selectInput(
                                 "var",
                                 label = h4("Choose a variable to display:"),
                                 choices = c(
                                   "Number of Indicators with Data" = "num_indicators",
                                   "Mean Age of Most Recent Indicator Data" = "mean_age"
                                 ),
                                 selected = "num_indicators"
                               )
                             ),
                             
                             mainPanel(leafletOutput(
                               "map", width = 800, height = 500
                             ))
                           )),
                  
                  tabPanel("Goal Specific",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("ind_view",
                                             h4("Select Goal:"),
                                             choices = list(
                                               "Biosphere Pillar" = "The Biosphere Pillar",
                                               "Economy Pillar" = "The Economy Pillar",
                                               "Society Pillar" = "The Society Pillar",
                                               "Goal 1 - No Poverty" = "Goal 1",
                                               "Goal 2 - Zero Hunger" = "Goal 2",
                                               "Goal 3 - Good Health and well-being" = "Goal 3",
                                               "Goal 4 - Quality Education" = "Goal 4",
                                               "Goal 5 - Gender Equality" = "Goal 5",
                                               "Goal 6 - Clean Water and Sanitation" = "Goal 6",
                                               "Goal 7 - Affordable and Clean Energy" = "Goal 7",
                                               "Goal 8 - Decent Work and Economic Growth" = "Goal 8",
                                               "Goal 9 - Industry, Innovation and Infrastructure" = "Goal 9",
                                               "Goal 10 - Reduced Inequality" = "Goal 10",
                                               "Goal 11 - Sustainable Cities and Communities" = "Goal 11",
                                               "Goal 12 - Responsible Consumption and Production" = "Goal 12",
                                               "Goal 13 - Climate Action" = "Goal 13",
                                               "Goal 14 - Life Below Water" = "Goal 14",
                                               "Goal 15 - Life on Land" = "Goal 15",
                                               "Goal 16 - Peace, Justice and Strong Institutions" = "Goal 16",
                                               "Goal 17 - Partnerships to achieve the Goal" = "Goal 17"
                                               
                                             ),
                                             selected = "Goal 14"
                                           )),
                                           
                                           # Main panel for displaying outputs ----
                                           mainPanel(plotOutput("countryPlot"))
                                         ))
                ))

# Define server logic required to draw a histogram
server <- function(input, output) {
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
    
    var <- input$var
    
    sdg_plotter(goal_view = goal_in, var_in = var)
  })
  
  output$countryPlot <- renderPlot({
    ind_view <- switch(
      input$ind_view,
      "The Biosphere Pillar" = c(6, 7, 12, 13, 14, 15),
      "The Economy Pillar" = c(1, 2, 3, 8, 9),
      "The Society Pillar" = c(4, 5, 10, 11, 16, 17),
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
    
    glob_inds %>%
      filter(goal %in% {{ind_view}}) %>%
      dplyr::select(goal, indicator, entity, latest_data_year) %>%
      group_by(indicator) %>%
      distinct() %>%
      mutate(
        sum_inds = sum(!is.na(latest_data_year)),
        num_inds = n(),
        prop_inds = 100 * (sum_inds / num_inds)
      ) %>%
      select(goal, sum_inds, num_inds, prop_inds) %>%
      distinct() %>%
      ungroup() %>%
      mutate(goal = factor(goal, levels = 1:17)) %>%
      ggplot(.,
             aes(
               x = indicator,
               y = prop_inds,
               group = indicator,
               fill = goal,
               colour = goal
             )) +
      geom_bar(stat = "identity", colour = "black") +
      sdg_fill_scale +
      sdg_col_scale +
      theme_fiona() +
      theme(legend.position = "none") +
      ylab("Proportion of Countries\n with Data for Indicator") +
      xlab("Indicators ") +
      ylim(0, 100) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
