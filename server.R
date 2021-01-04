server <- shinyServer(function(input, output) {
  
  output$ByRegion <- renderLeaflet({
    leaflet(tests_region) %>% 
      addTiles() %>% 
      setView(lat = 42.349395, lng = -2.500749, zoom = 9) %>% 
      addCircles(data = tests_region, lat = ~ Lat, lng = ~ Lon, weight = 1, radius = ~5*Cum_Inc_14_Days, popup = ~as.character(City), 
                 label = ~as.character(paste0(City)), 
                 color = "darkred", fillOpacity = 0.3)
  })
  
  output$tests_per_age_range <- renderPlotly({
    tests_age_range_plot = tests_age_range[Age_Range != "DESCONOCIDO"]
    tests_age_range_plot[, PCR_Total_w_Result := PCR_Pos_Last_14_Days + PCR_Neg_Last_14_Days]
    tests_age_range_plot[, `Percentage Positive` := round(PCR_Pos_Last_14_Days*100/(PCR_Total_w_Result), 2)]
    tests_age_range_plot[, `Percentage Negative` := round(PCR_Neg_Last_14_Days*100/(PCR_Total_w_Result), 2)]
    tests_age_range_plot[, `Total PCR in last 14 days` := PCR_Total_w_Result ]
    tests_age_range_plot[, `Positive PCR in the last 14 days` :=PCR_Pos_Last_14_Days  ]
    
    tests_age_range_plot = melt(tests_age_range_plot, measure.vars = c("Total PCR in last 14 days", "Positive PCR in the last 14 days"))
    
    
    plot <- ggplot(data=tests_age_range_plot, aes(x=Age_Range, y=value, fill=variable, 
                                                  text = paste0(value), group = 1)) +
      geom_bar(stat="identity",position = "identity", alpha=.9)+
      scale_fill_brewer(palette = "Pastel2")+
      scale_x_discrete(labels = tests_age_range$Age_Range)+
      theme_void()+
      theme(legend.title=element_blank(), legend.position="none", 
            axis.text.y = element_text(angle=0, hjust=1), 
            title = element_text(size = 6),
            plot.title = element_text(size=9),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.key.size=unit(0.4, 'cm'),
            strip.text.x=element_text(size=7),
            axis.text=element_text(size=7),
            axis.text.x=element_text(angle=0, hjust=1),
            plot.margin=unit(c(0, 0, 0, 0),"pt"))+
      coord_flip()
    
    ggplotly(plot, tooltip = c("text")) %>% 
      layout(plot_bgcolor='transparent', paper_bgcolor='#a6b3bf',
             hoverlabel=list(bgcolor="white"),
             hovermode = "x unified",
             font = list(color = '#737373'),
             margin = list(l = 0, r = 30, b = 0, t = 30, pad = 0),
             yaxis = list(gridcolor = "#737373"),
             xaxis = list(gridcolor = "#737373"))
    
  })
  
  output$IA <- renderPlotly({

    plot_cumulative_indidence <- cumulative_incidence %>% 
      ggplot() + 
      geom_bar(data = cumulative_incidence, aes(x= Date, y = New_Pos, color = "New Positives",
                                                text = paste0("New Positives:", New_Pos)), stat='identity', color = "#a7b3be") +
      geom_line(data = cumulative_incidence, aes(x = Date, y = Cum_Inc_14_Days, color = "Cumulative Incidence in the last 14 days",
                                                 text = paste0("Cumulative Incidence 14 Days: ", Cum_Inc_14_Days), group = 1), color = "darkred") +
      geom_line(data = cumulative_incidence, aes(x = Date, y = Cum_Inc_7_Days, color = "Cumulative Incidence in the last 7 days",
                                                 text = paste0("Cumulative Incidence 7 Days: ", Cum_Inc_7_Days), group = 1), color = "#cc8800") +
      geom_line(data = cumulative_incidence, aes(x = Date, y = 0, 
                                                 text = paste0("Date: ", Date), group = 1), color = "black") +
      theme(legend.title=element_blank(), legend.position="top",
            plot.title = element_text(size=9),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.key.size=unit(0.4, 'cm'),
            strip.text.x=element_text(size=7),
            axis.text=element_text(size=7),
            axis.text.x=element_text(angle=0, hjust=1),
            plot.margin=unit(c(0, 0, 0, 0),"pt")) +
      scale_x_date(date_breaks = "months" , date_labels = "%Y-%m") +
      theme(
        panel.background = element_rect(colour = "white")
      )
      
    #'#f0f2f4'
    ggplotly(plot_cumulative_indidence, tooltip = "text") %>% 
      layout(plot_bgcolor='transparent', paper_bgcolor='#a6b3bf',
             hoverlabel=list(bgcolor="white"),
             hovermode = "x unified",
             font = list(color = '#737373'),
             margin = list(l = 0, r = 30, b = 0, t = 30, pad = 0),
             yaxis = list(gridcolor = "#737373"),
             xaxis = list(gridcolor = "#737373"))
    
  })  
  
  output$test_evolution <- renderPlotly({
    
    tests[, Pos_Ratio := round(Tests_Pos*100 / Tests_Total, digits = 2)]
    
    plot_cumulative_indidence <- tests %>% 
      ggplot() + 
      geom_bar(data = tests, aes(x= Date, y = Tests_Total, color = "Tests_Total",
                                                text = paste0("Total Tests:", Tests_Total)), stat='identity', color = "lightyellow3") +
      geom_line(data = tests, aes(x = Date, y = Tests_Pos, color = "Positive Tests",
                                                 text = paste0("Positive Tests: ", Tests_Pos), group = 1), color = "sienna4") +
      geom_line(data = tests, aes(x = Date, y = 0, 
                                                 text = paste0("Date: ", Date), group = 1), color = "darkgrey") +
      # Custom the Y scales:
      scale_y_continuous(
        # Features of the first axis
        name = "First Axis",
        # Add a second axis and specify its features
        sec.axis = sec_axis( trans=~.*10, name="Second Axis")
      ) +
      theme(legend.title=element_blank(), legend.position="top",
            plot.title = element_text(size=9),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.key.size=unit(0.4, 'cm'),
            strip.text.x=element_text(size=7),
            axis.text=element_text(size=7),
            axis.text.x=element_text(angle=0, hjust=1),
            plot.margin=unit(c(0, 0, 0, 0),"pt")) +
      scale_x_date(date_breaks = "months" , date_labels = "%Y-%m") +
      theme(
        panel.background = element_rect(colour = "white")
      )
    
    #'#f0f2f4'
    ggplotly(plot_cumulative_indidence, tooltip = "text") %>% 
      layout(plot_bgcolor='transparent', paper_bgcolor='#a6b3bf',
             hoverlabel=list(bgcolor="white"),
             hovermode = "x unified",
             font = list(color = '#737373'),
             margin = list(l = 0, r = 30, b = 0, t = 30, pad = 0),
             yaxis = list(gridcolor = "#737373"),
             xaxis = list(gridcolor = "#737373"))
    
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