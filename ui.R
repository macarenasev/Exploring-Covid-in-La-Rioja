
library(shiny)
library(shinythemes)
library(plotly)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

    includeCSS("style.css"),
    
    navbarPage("Covid Status in La Rioja, Spain",
               tabPanel("Overview",
                        
                        fillPage(theme = shinytheme("superhero"),
                                  
                                 fluidRow(
                                     column(10,
                                            fluidRow(
                                              column(5,
                                                     "Date updated:", textOutput("date_updated", inline = T),
                                              )
                                            ),
                                            fluidRow(
                                                column(3, 
                                                       h6("Open Cases in La Rioja")
                                                ),
                                                column(3, 
                                                       h6("Cumulative Incidence (14d) in La Rioja")
                                                ),
                                                column(3, 
                                                       htmlOutput("open_cases_in_city_selected_text")
                                                ),
                                                column(3, 
                                                       htmlOutput("cum_inc_in_city_selected_text")
                                                ),
                                            ),
                                            
                                            
                                            fluidRow(
                                                column(3, 
                                                       wellPanel (
                                                           div(textOutput("total_open_cases"),style = "font-size:105%")
                                                       )
                                                ),
                                                column(3, 
                                                       wellPanel (
                                                         div(textOutput("cumulative_incidence_LR"), style = "font-size:105%")
                                                       )
                                                ),
                                                column(3, 
                                                       htmlOutput("panel_open_cases_in_city_selected")
                                                ),
                                                column(3, 
                                                       htmlOutput("panel_cum_inc_in_city_selected")
                                                )
                                            )
                                     ),
                                 ),

                                 fluidRow(
                                   leafletOutput(outputId = "ByRegion")
                                  )
                        )
               ),
               
               tabPanel("Evolution", 
                        h5("Cumulative Incidence in the last 7 / 14 days"),
                        fluidRow(
                          plotlyOutput("IA")
                          
                        )   
               ),
               
               tabPanel("Testing", 
                        
                        fluidRow(
                          column(5,
                                 plotlyOutput("tests_per_age_range")
                          ),
                          column(6,
                                 
                          ),
                        ),
                        fluidRow(
                          column(6,
                                 
                          ),
                          column(4,
                                 
                          ),
                        ),
               ),
               
               tabPanel("Developers", fluidPage(theme = shinytheme("superhero")),
                        p(a("Macarena", href="", target="_blank"),style = "font-size:25px"))
               
    )
))

