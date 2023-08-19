function(input, output, session) {
  
  ######################## MENU PETA MAPILU ###################################
  
  #access token mapbox
  key <- 'pk.eyJ1IjoibmF0YXNoYXRyYmwiLCJhIjoiY2t6NWM2eTVtMGU5ZjJ1cGtweDkyd2hoMCJ9.2sEmFzTzZIr9qfTrc8cSbw'
  
  #simplified the batas admin
  pituruh_simplified <- st_simplify(pituruh, dTolerance = 0.05)
  
  #validating geometries
  pituruh_cleared <- st_make_valid(pituruh_simplified)
  
  # Rendering mapdeck map
  output$map <- renderMapdeck({
    mapdeck(
      token = key,
      style = 'mapbox://styles/mapbox/streets-v12',
      zoom = 10,
      location = c(110.0093, -7.7129)
    ) %>%
      add_geojson(
        data = pituruh
      )
  })

  
  ######################## MENU DEMOGRAFI #################################
  
  #value box persentase perempuan per kecamatan
  output$persen_perempuan <- renderValueBox({
    selected_kecamatan <- input$kecamatan
    persen_perempuan_round <- round(sum(data_hasil$GENDER=="Perempuan" & 
                                          data_hasil$KECAMATAN == selected_kecamatan)/
                                      sum(data_hasil$KECAMATAN == selected_kecamatan)*100, 2)
    
    valueBox(value = paste0(persen_perempuan_round, "%"),
             subtitle = paste0("Persentase Responden dengan Jenis Kelamin Perempuan di ", input$kecamatan),
             width = "100%",
             color = "olive"
             )
  })
  
  #value box persentase usia 17 per kecamatan
  output$persen_usia17 <- renderValueBox({
    selected_kecamatan <- input$kecamatan
    persen_usia_round <- round(sum(data_hasil$USIA == "17" &
                                     data_hasil$KECAMATAN == selected_kecamatan)/
                                 sum(data_hasil$KECAMATAN == selected_kecamatan)* 100, 2)
    
    valueBox(value = paste0(persen_usia_round, "%"),
             subtitle = paste0("Persentase Responden dengan Usia 17 Tahun di ", input$kecamatan),
             width = "100%",
             color = "navy"
    )
  })
  
  #filtering the data by kecamatan 
  #and plotting the barplot using ggplotly
  #data_hasil filtered by kecamatan
  datahasil_filteredby_kec <- reactive(
    data_hasil %>% filter(KECAMATAN == input$kecamatan)
  )
  
  #barplot kecamatan by pengetahuan seputar pemilu
  output$barplot_kecby_ind2 <- renderPlotly({
    
    pengetahuanpemilu_bykec <- datahasil_filteredby_kec()
    
    #Specify the order of the pengetahuan pemilu categories
    pengetahuanpemilu_order <- c("Sangat mengetahui", "Mengetahui", "Kurang tahu", "Tidak tahu")
    
    # Create pengetahuan pemilu to a factor of the specified order
    pengetahuanpemilu_bykec$IND2 <- factor(pengetahuanpemilu_bykec$IND2, levels = pengetahuanpemilu_order)
    
    #wrap the x-axis labels to prevent overlap
    pengetahuanpemilu_bykec$IND2_wrapped <- str_wrap(pengetahuanpemilu_bykec$IND2, width = 10)
    
    #create ggplot object
    p <- ggplot(data = pengetahuanpemilu_bykec)+
      geom_bar(aes(x = IND2_wrapped), stat = "count", fill = "#2FB380")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = paste("Pengetahuan Pemilu di", input$kecamatan), x = "Tingkat Pengetahuan Pemilih Pemula", y = "Count")
    
    #create plotly plot from ggplot
    ggplotly(p, tooltip = c("count"))
  })
  
  #barplot kecamatan by sumber informasi pemilu
  output$barplot_kecby_ind4 <- renderPlotly({
    p <- ggplot(data = datahasil_filteredby_kec())+
      geom_bar(aes(x = str_wrap(IND4, width = 10)), stat = "count", fill = "#3459E6")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = paste("Sumber Informasi Pemilu di", input$kecamatan), x = "Sumber Informasi Pemilu", y = "Count")
    
    #Create plotly plot from ggplot object
    ggplotly(p, tooltip = c("count"))
  })
  
  #barplot kecamatan by pendapat tentang golput
  output$barplot_kecby_ind5 <- renderPlotly({
    golput_bykec <- datahasil_filteredby_kec()
    
    #Specify the order of the pengetahuan pemilu categories
    golput_order <- c("Tidak Setuju", "Kurang Setuju", "Biasa Saja", "Setuju")
    
    # Create a new variable for ordering
    golput_bykec$golput_var <- factor(golput_bykec$IND5, levels = golput_order)
    
    #wrap the x-axis labels to prevent overlap
    golput_bykec$golput_var_wrapped <- str_wrap(golput_bykec$golput_var, width = 10)
    
    p <- ggplot(data = golput_bykec)+
      geom_bar(aes(x = golput_var_wrapped), stat = "count", fill = "#141414")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = paste("Pendapat tentang Golput di", input$kecamatan), x = "Pendapat Pemilih Pemula", y = "Count")
    
    #Create plotly plot from ggplot object
    ggplotly(p, tooltip = c("count"))
  })

  #Map rendering logic
  #action button to show the map
  
  observeEvent(input$showmap_button, {
  
  #plotting the geografi map using ggplot2
  if (input$demografi_map_option == "Kependudukan") {
    #plotting the demography map using ggplot2
    output$demografi_map <- renderPlot({
      ggplot()+
        geom_sf(data = kependudukan_pwr,
                aes(fill = POPULASI))+
        theme_minimal()+
        theme(panel.grid = element_blank())+
        labs(title = "Populasi di Kabupaten Purworejo Tahun 2022")
    })
  } else if (input$demografi_map_option == "Geografis") {
    #plotting the kependudukan map using ggplot2
    output$demografi_map <- renderPlot({
      ggplot()+
        geom_sf(data = geografi_pwr,
                aes(fill = Jarak_Kab))+
        theme_minimal()+
        theme(panel.grid = element_blank())+
        labs(title = "Jarak ke Pusat Kabupaten Per Kecamatan")
    })
  }
    #plotting the tingkat partisipasi map using ggplot2
    output$tingkatpartisipasi_map <- renderPlot({
      ggplot() +
        geom_sf(data = partisipasi_pwr,
                aes(fill = SKOR_TOTAL)) +
        theme_minimal() +
        theme(panel.grid = element_blank()) +
        labs(title = "Skor Tingkat Partisipasi Kabupaten Purworejo")
    })
})
  

  ######################## MENU ELECTORAL VOTE ###################################
  
  
 
  
}


