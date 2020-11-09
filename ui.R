
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
                                     column(9,
                                            fluidRow(
                                                column(3, 
                                                       h6("Open Cases")
                                                ),
                                                column(3, 
                                                       h6("Open Cases in Logroño")
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
                                                           div(textOutput("total_open_cases_in_logroño"),style = "font-size:105%")
                                                       )
                                                )
                                            )
                                     ),
                                 ),

                                 fluidRow(
                                     plotlyOutput("IA", height=400, width=1100),
                                  )
                        )
               ),
               tabPanel("PCR testing", 
                        
               ),
               
               tabPanel("Developers", fluidPage(theme = shinytheme("superhero")),
                        p(a("Macarena", href="", target="_blank"),style = "font-size:25px"))
               
    )
))

