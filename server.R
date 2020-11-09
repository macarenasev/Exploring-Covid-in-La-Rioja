server <- shinyServer(function(input, output) {
  
  output$IA <- renderPlotly({
    
    plot_cumulative_indidence <- df_cumulative_incidence %>% 
      ggplot() + 
      geom_bar(data = df_cumulative_incidence, aes(x= Date, y = New_Positives, color = "New Positives"), stat='identity', color = "lightgrey") +
      geom_line(data = df_cumulative_incidence, aes(x = Date, y = Cum_Inc_14_Days, color = "Cumulative Incidence in the last 14 days"), color = "darkred") +
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
    sum(df_open_cases$Casos.activos)
  }))
  output$total_open_cases_in_logroño <- renderText(paste({
    sum(df_open_cases[Localidades.de.más.de.1.000.habitantes== "Logroño"]$Casos.activos)
  }))
})