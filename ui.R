
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
               
               tabPanel("Testing", style="overflow-y: visible",
                        
                        fixedRow(
                            column(4,
                                   h6("PCR tests in the last 14 days by Age Range"),
                                   h6("(Positive in orange vs Total in green)"),
                                   plotlyOutput("tests_per_age_range", width = "auto", height = 280))
                            ,
                            column(8,
                                   h6("PCR test evolution in time"),
                                   h6("Total Tests and Positive Ratio per day"),
                                   plotlyOutput("test_evolution", width = "auto", height = 280)
                            ),
                          ),
                        fixedRow(
                              column(8,
                                     h6("PCR tests by City"),
                                     h6("Top 15 Region with higher Cumulative PCR Positive Ratio"),
                                     plotlyOutput("tests_region", width = "auto", height = 280)
                              ),
                              column(4,
                                     h6("Total PCR tests by Weekday"),
                                     h6("(Positive in orange vs Total in green)"),
                                     plotlyOutput("tests_weekday", width = "auto", height = 280)
                              ),
                          ),
               ),
               
               tabPanel("Developers", fluidPage(theme = shinytheme("superhero")),
                        p(a("Macarena", href="", target="_blank"),style = "font-size:25px"))
               
    )
))

