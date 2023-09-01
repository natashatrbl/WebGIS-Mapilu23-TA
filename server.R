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
  
  ################ Viewport pertama ##################
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
  
  #filtering the data by kecamatan and plotting the barplot using ggplotly
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

  ################ viewport kedua ###################
  #Map rendering logic
  #action button to show the map
  observeEvent(input$showmap_button, {
  
  #plotting the geografi map using ggplot2
  if (input$demografi_map_option == "Kependudukan") {
    #plotting the demography map using ggplot2
    output$demografi_map <- renderPlot({
      ggplot()+
        geom_sf(data = demografi_pwr,
                aes(fill = POPULASI))+
        theme_minimal()+
        theme(panel.grid = element_blank())+
        labs(title = "Populasi di Kabupaten Purworejo Tahun 2022")
    })
  } else if (input$demografi_map_option == "Geografis") {
    #plotting the kependudukan map using ggplot2
    output$demografi_map <- renderPlot({
      ggplot()+
        geom_sf(data = demografi_pwr,
                aes(fill = JARAK_KAB))+
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
  
  ################ viewport pertama #################
  #valueboxoutput rerata tingkat partisipasi per kecamatan
  output$partisipatif_mean <- renderValueBox({
    kecamatan_pilihan <- input$kecamatan_elect
    filtered_kecamatan <- data_hasil[data_hasil$KECAMATAN == kecamatan_pilihan, ]
    mean_partisipatif <- mean(filtered_kecamatan$SKOR_TOTAL)
    mean_partisipatif_round <- round(mean_partisipatif, 2)
    
    valueBox(value = mean_partisipatif_round,
             subtitle = paste0("Rerata Skor Tingkat Partisipasi Politik di ", input$kecamatan_elect),
             width = "100%",
             color = "olive"
             )
  })
  
  #valueboxoutput persentase parpol legislatif per kecamatan
  output$parpolleg_persentase <- renderValueBox({
    kecamatan_pilihan <- input$kecamatan_elect
    persen_parpolleg_round <- round(sum(data_hasil$PARTAI_LEGISLATIF == "PDIP" &
                                     data_hasil$KECAMATAN == kecamatan_pilihan)/
                                 sum(data_hasil$KECAMATAN == kecamatan_pilihan)* 100, 2)
    
    valueBox(value = paste0(persen_parpolleg_round, "%"),
             subtitle = paste0("Persentase Dominasi Partai PDIP di ", input$kecamatan_elect),
             width = "100%",
             color = "navy"
    )
  })
  
  #valueboxoutput persentase calon presiden per kecamatan
  output$capres_persentase <- renderValueBox({
    kecamatan_pilihan <- input$kecamatan_elect
    persen_capres_round <- round(sum(data_hasil$CAPRES == "Ganjar Pranowo" &
                                          data_hasil$KECAMATAN == kecamatan_pilihan)/
                                      sum(data_hasil$KECAMATAN == kecamatan_pilihan)* 100, 2)
    
    valueBox(value = paste0(persen_capres_round, "%"),
             subtitle = paste0("Persentase Dominasi Ganjar Pranowo di ", input$kecamatan_elect),
             width = "100%",
             color = "black"
    )
  })
  
  #rendering plot output for each parameters that will be shown as bar chart
  #filtering data by kecamatan
  elect_filteredby_kec <- reactive(
    data_hasil %>% filter(KECAMATAN == input$kecamatan_elect)
  ) 
  #barplot kecamatan by parpol legislatif
  output$barplot_kecby_parpolleg <- renderPlotly({
    p <- ggplot(data = elect_filteredby_kec())+
      geom_bar(aes(x = str_wrap(PARTAI_LEGISLATIF, width = 7)), stat = "count", fill = "#2FB380")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = paste("Dominasi Partai Politik Legislatif di", input$kecamatan_elect), x = "Partai Politik Legislatif", y = "Count")
    
    #Create plotly plot from ggplot object
    ggplotly(p, tooltip = c("count"))
  })
 
  #barplot kecamatan by nama capres
  output$barplot_kecby_capres <- renderPlotly({
    p <- ggplot(data = elect_filteredby_kec())+
      geom_bar(aes(x = str_wrap(CAPRES, width = 10)), stat = "count", fill = "#141414")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = paste("Dominasi Capres di", input$kecamatan_elect), x = "Nama Capres", y = "Count")
    
    #Create plotly plot from ggplot object
    ggplotly(p, tooltip = c("count"))
  })
  
  ################ viewport kedua #################
  #rendering map and table for slider input view
  elected_data <- electoral_pwr %>%
    select(kode_dagri, geometry, GP_PCT, PS_PCT, AB_PCT, LAIN_PCT, NA_PCT)
  
  observe({
  #filtering select input
  selected_capres <- input$capres_selector
  
  #creating a column list connected with the selectInput
  column_name <- switch(selected_capres,
                  "Ganjar Pranowo" = "GP_PCT",
                  "Prabowo Subianto" = "PS_PCT",
                  "Anies Baswedan" = "AB_PCT",
                  "Lainnya" = "LAIN_PCT",
                  "Belum Menentukan Pilihan" = "NA_PCT"
           )
  
  #plotting the map
  output$presiden_map <- renderPlot({
    ggplot()+
      geom_sf(data = elected_data, aes(fill = .data[[column_name]]))+
      scale_fill_gradient(name = selected_capres)+
      labs(fill = selected_capres)+
      theme_minimal()
    
    })
  })
    
  #selecting the data for the table
  selected_capres_data <- reactive({
    capres <- input$capres_selector
    if (capres == "Ganjar Pranowo") {
      electoral_csv %>%
        filter(!is.na(GP_PCT)) %>%
        select(Kecamatan, GP_PCT, GP_JML, JML_RESP) %>%
        rename(Persen = GP_PCT, Jumlah = GP_JML, Responden = JML_RESP)
    } else if (capres == "Prabowo Subianto") {
      electoral_csv %>%
        filter(!is.na(PS_PCT)) %>%
        select(Kecamatan, PS_PCT, PS_JML, JML_RESP) %>%
        rename(Persen = PS_PCT, Jumlah = PS_JML, Responden = JML_RESP)
    } else if (capres == "Anies Baswedan") {
      electoral_csv %>%
        filter(!is.na(AB_PCT)) %>%
        select(Kecamatan, AB_PCT, AB_JML, JML_RESP) %>%
        rename(Persen = AB_PCT, Jumlah = AB_JML, Responden = JML_RESP)
    } else if (capres == "Lainnya") {
      electoral_csv %>%
        filter(!is.na(LAIN_PCT)) %>%
        select(Kecamatan, LAIN_PCT, LAIN_JML, JML_RESP) %>%
        rename(Persen = LAIN_PCT, Jumlah = LAIN_JML, Responden = JML_RESP)
    } else if (capres == "Belum Menentukan Pilihan") {
      electoral_csv %>%
        filter(!is.na(NA_PCT)) %>%
        select(Kecamatan, NA_PCT, NA_JML, JML_RESP, Responden = JML_RESP) %>%
        rename(Persen = NA_PCT, Jumlah = NA_JML)
    }
  })
  
  #rendering the table
  output$presiden_table <- renderTable({
    data <- selected_capres_data()
    data <- data[order(-data$Persen), ]
    data
  }, striped = T, bordered = T, digits = 2)
  
  ################ viewport ketiga #################
  #observe event for the action button and checkboxgroup
  observeEvent(input$electoralmap_button, {
    selected_parpol <- input$electoralmap_checkbox
    
    if (length(selected_parpol) > 0) {
      #creating named vector to map party names to column names
      parpol_column_map <- c("PDIP" = "PDIP_PCTL",
                             "Gerindra" = "GE_PCTL",
                             "Nasdem" = "ND_PCTL",
                             "Demokrat" = "DEM_PCTL",
                             "Golkar" = "GK_PCTL",
                             "Perindo" = "PRD_PCTL",
                             "PAN" = "PAN_PCTL")
      
    #creating ggplot list 
      gg_plots <- list()
      
      for (party in selected_parpol) {
        aes_name <- parpol_column_map[party]
        legend_label <- paste(party, "Percentile", sep = " ")
        
        gg <- ggplot(electoral_pwr)+
          geom_sf(aes(fill = .data[[aes_name]]))+
          scale_fill_gradient2(low = "orange", mid = "white", high = "red", name = legend_label)+
          labs(title = legend_label)
        
        gg_plots[[party]] <- gg
      }
      
      #set mfrow layout for multiple plots
      par(mfrow = c(3, length(selected_parpol)))
      
      #rendering main panel for multiple plots
      output$electoral_map <- renderPlot({
        plot_list <- lapply(selected_parpol, function(party){
          plot(gg_plots[[party]])
        })
        
        do.call(grid.arrange, plot_list)
      })
    } else {
      par(mfrow = c(1, 1))
      output$electoral_map <- NULL
    }
  })
  
  
  
}


