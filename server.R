server <- shinyServer(function(input, output) {
  
  output$ByRegion <- renderLeaflet({
    leaflet(tests_region) %>% 
      addTiles() %>% 
      setView(lat = 42.349395, lng = -2.500749, zoom = 9) %>% 
      addCircles(data = tests_region, lat = ~ Lat, lng = ~ Lon, weight = 1, radius = ~5*Cum_Inc_14_Days, popup = ~as.character(City), 
                 label = ~as.character(paste0(City)), 
                 color = "darkred", fillOpacity = 0.3)
  })
  
  output$IA <- renderPlotly({

    plot_cumulative_indidence <- cumulative_incidence %>% 
      ggplot() + 
      geom_bar(data = cumulative_incidence, aes(x= Date, y = New_Pos, color = "New Positives"), stat='identity', color = "lightgrey") +
      geom_line(data = cumulative_incidence, aes(x = Date, y = CumInc_14Days, color = "Cumulative Incidence in the last 14 days"), color = "darkred") +
      theme(legend.title=element_blank(), legend.position="top",
            plot.title = element_text(size=9),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.key.size=unit(0.4, 'cm'),
            strip.text.x=element_text(size=7),
            axis.text=element_text(size=7),
            axis.text.x=element_text(angle=45, hjust=1)) +
      theme_bw() +
      labs(y = "Cumulative Incidence in the last 14 days")
    
    ggplotly(plot_cumulative_indidence)
    
  })  
  output$total_open_cases <- renderText(paste({
    sum(open_cases$Active_Cases, na.rm = T)
  }))
  
  output$cumulative_incidence_LR <- renderText(paste({
    max_date = max(cumulative_incidence$Date)
    round(cumulative_incidence[Date == max_date]$Cum_Inc_14_Days, digits = 2)
  }))
  
  output$total_open_cases_in_logroño <- renderText(paste({
    sum(open_cases[City_w_more_than_1000_people== "LOGROÑO"]$Active_Cases)
  }))
  
  output$date_updated <- renderText(paste({
    max(cumulative_incidence$Date)
  }))

  city_selected <- reactiveValues()
  city_selected <- ""

  output$cumulative_incidence_in_selected <- renderText(paste({"N/A"}))
  output$total_open_cases_in_selected <- renderText(paste({"N/A"}))
  
  observeEvent(input$ByRegion_click, {
    
    output$cumulative_incidence_in_selected <- renderText(paste({"N/A"}))
    output$total_open_cases_in_selected <- renderText(paste({"N/A"}))
    
    output$open_cases_in_city_selected_text <- renderText(paste({""}))
    
    output$cum_inc_in_city_selected_text <- renderText(paste({ ""}))
    
    output$panel_open_cases_in_city_selected <- renderText(paste({""}))
    
    output$panel_cum_inc_in_city_selected <- renderText(paste({""}))
    
  })
  
  observeEvent(input$ByRegion_shape_click, {
    # click event
    lat_selected <- input$ByRegion_shape_click$lat
    lon_selected <- input$ByRegion_shape_click$lng
    
    city_selected <- unique(tests_region[Lat == lat_selected & Lon == lon_selected]$City)
    
    output$cumulative_incidence_in_selected <- renderText(paste({
      aux = round(tests_region[City == city_selected]$Cum_Inc_14_Days, digits = 2)
      if(aux == ""){
        return("N/A")
      }else{
        return(aux)
      }
    }))
    
    output$total_open_cases_in_selected <- renderText(paste({
      aux = open_cases[City_w_more_than_1000_people == city_selected]$Active_Cases
      if(length(aux) == 0){
        aux = open_cases[gsub("'", "", iconv(City_w_more_than_1000_people,to="ASCII//TRANSLIT")) == city_selected]$Active_Cases
      }
      if(length(aux) == 0){
        return("N/A")
      }else{
        return(aux)
      }
    }))
    
    output$open_cases_in_city_selected_text <- renderText(paste({
      h6(paste0("Open Cases in ", city_selected))
    }))
    
    output$cum_inc_in_city_selected_text <- renderText(paste({
      h6(paste0("Cumulative Incidence (14d) in ", city_selected))
    }))
    
    output$panel_open_cases_in_city_selected <- renderText(paste({
      wellPanel (
        div(textOutput("total_open_cases_in_selected"), style = "font-size:105%")
      )
    }))
    
    output$panel_cum_inc_in_city_selected <- renderText(paste({
      wellPanel (
        div(textOutput("cumulative_incidence_in_selected"), style = "font-size:105%")
      )
    }))
  })
  
})