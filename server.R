server <- shinyServer(function(input, output) {
  
  output$ByRegion <- renderLeaflet({
    leaflet(tests_region) %>% 
      addTiles() %>% 
      setView(lat = 42.349395, lng = -2.500749, zoom = 9) %>% 
      addCircles(data = tests_region, lat = ~ Lat, lng = ~ Lon, weight = 1, radius = ~5*Cum_Inc_14_Days, popup = ~as.character(City), 
                 label = ~as.character(paste0(City, " - Cumulative Incidence Last 14 Days: ", sep = " ", Cum_Inc_14_Days)), 
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
    sum(open_cases$Active_Cases)
  }))
  output$total_open_cases_in_logroño <- renderText(paste({
    sum(open_cases[City_w_more_than_1000_people== "LOGROÑO"]$Active_Cases)
  }))
})