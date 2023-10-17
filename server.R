
function(input, output, session) {
  ####################### MENU ABOUT ###################
  blank_map <- leaflet(summarymap) %>%
    setView(lng = 110.0093, lat = -7.7129, zoom = 11) %>%
    addProviderTiles(provider = "CartoDB.Positron",
                     options = providerTileOptions(noWrap = TRUE),
                     group = "Light"
    ) %>%
    addProviderTiles(
      provider = "OpenStreetMap.Mapnik",
      options = providerTileOptions(noWrap = TRUE),
      group = "Street"
    ) %>%
    addLayersControl(
      baseGroups = c("Light", "Street"),
      overlayGroups = c("Batas Administrasi", "Sekolah"),
      options = layersControlOptions(collapsed = F),
      position = "topleft"
    ) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs", 
      title = "Locate Me",
      onClick = JS("function(btn, map){ map.locate({setView: true}); }"))
    )
  output$aoi <- renderLeaflet({
      blank_map %>%
      addPolygons(
        data = summarymap,
        fillColor = "#41b6c4", fillOpacity = 0.7,
        color = "white", opacity = 0.8, weight = 1,
        highlightOptions = highlightOptions(color = "white", weight = 2, fillOpacity = 1, bringToFront = T),
        label = ~paste("Kecamatan", kecamatan),
        group = "Batas Administrasi"
      ) %>%
      addMarkers(
        data = point_sma,
        lng = ~long,
        lat = ~lat,
        popup = ~paste("<h6 style = 'color: #2c7fb8;'> Nama Sekolah: </h6>", point_sma$name, "<br>",
                       "<h6 style = 'color: #2c7fb8;'> Alamat:</h6>", point_sma$address, "<br>",
                       "<h6 style = 'color: #2c7fb8;'> Nomor Telepon:</h6>", point_sma$phone),
        group = "Sekolah"
      )
  })
  
  
  ######################## MENU PETA MAPILU ###################################
  ###### Map View ######
  ##### creating pallete #####
  #demographic palette
  pal1 <- colorNumeric("viridis", NULL)
  #participation map palette
  pal2 <- colorFactor(palette = c("#a1dab4", "#2c7fb8"), domain = c(min(summarymap$KELAS), max(summarymap$KELAS)))
  pal3 <- colorFactor(palette = c("#a1dab4", "#2c7fb8"), domain = c(min(summarymap$KL_FIL), max(summarymap$KL_FIL)))
  pal4 <- colorFactor(palette = c("#a1dab4", "#2c7fb8"), domain = c(min(summarymap$KL_IND), max(summarymap$KL_IND)))
  #electoral palette
  colour_palette <- c("Ganjar Pranowo" = "#b2182b", "Belum Menentukan Pilihan" = "#999999", "Prabowo Subianto" = "#f1a340")
  summarymap$KL_MAXPRES <- factor(summarymap$KL_MAXPRES)
  factpal <- colorFactor(palette = colour_palette, domain = summarymap$KL_MAXPRES)
  summarymap$KLPRES_FIL <- factor(summarymap$KLPRES_FIL)
  factpal2 <- colorFactor(palette = colour_palette, domain = summarymap$KLPRES_FIL)
    
  ##### empty leaflet map #####
  initialmap <- leaflet(summarymap) %>%
    addProviderTiles(
      provider = "CartoDB.Positron",
      options = providerTileOptions(noWrap = TRUE),
      group = "Light"
    ) %>%
    addProviderTiles(
      provider = "OpenStreetMap.Mapnik",
      options = providerTileOptions(noWrap = TRUE),
      group = "Street"
    ) %>%
    addProviderTiles(
      provider = "CartoDB.DarkMatter",
      options = providerTileOptions(noWrap = TRUE),
      group = "Dark"
    ) %>%
    addProviderTiles(
      provider = "Esri.WorldImagery",
      options = providerTileOptions(noWrap = TRUE),
      group = "Satellite"
    ) %>%
    addLayersControl(
      baseGroups = c("Light", "Dark", "Street", "Satellite"),
      overlayGroups = c("Tingkat Partisipasi Unfiltered",
                        "Tingkat Partisipasi Filtered",
                        "Tingkat Partisipasi Indikator",
                        "Demografi",
                        "Persentase Capres"),
      options = layersControlOptions(collapsed = T, groupCheckboxes = T),
      position = "topleft"
    ) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs", 
      title = "Locate Me",
      position = "topleft",
      onClick = JS("function(btn, map){ map.locate({setView: true}); }"))
    )
  
  ##### render leaflet ####
  output$map <- renderLeaflet({
    initialmap %>%
      ##### Peta Demografi #####
      addPolygons(
        data = summarymap,
        fillColor = ~pal1(POP),
        fillOpacity = 0.8,
        stroke = T,
        color = "white",
        weight = 1,
        opacity = 0.7,
        highlightOptions = highlightOptions(color = "white", weight = 2, fillOpacity = 1, bringToFront = T),
        popup = paste("<h6 style = 'color: #2c7fb8;'>Kecamatan: </h6>", summarymap$kecamatan, "<br>",
                       "<h6 style = 'color: #2c7fb8;'>Populasi: </h6>", summarymap$POP, "Jiwa", "<br>",
                       "<h6 style= 'color: #2c7fb8;'>Kepadatan: </h6>", summarymap$KPDTN, "<br>",
                       "<h6 style = 'color: #2c7fb8;'>Ketinggian Wilayah: </h6>", summarymap$TGG_MDPL, "mdpl"),
        group = "Demografi"
      ) %>%
      addLegend(
        pal = pal1,
        values = ~POP,
        title = "Populasi Purworejo",
        position = "bottomright",
        group = "Demografi",
        bins = 3
      ) %>%
      ##### Peta Tingkat Partisipasi Unreclassified #####
      addPolygons(
        data = summarymap,
        fillColor = ~pal2(KELAS),
        fillOpacity = 0.8,
        stroke = T,
        color = "white",
        weight = 1,
        opacity = 0.7,
        highlightOptions = highlightOptions(color = "white", weight = 2, fillOpacity = 1, bringToFront = T),
        popup = paste("<h6 style = 'color: #2c7fb8;'>Kecamatan: </h6>", summarymap$kecamatan, "<br>",
                       "<h6 style = 'color: #2c7fb8;'>Skor: </h6>", summarymap$SK_TOT, "<br>",
                       "<h6 style= 'color: #2c7fb8;'>Kelas: </h6>", summarymap$KELAS, "<br>"),
        group = "Tingkat Partisipasi"
      ) %>%
      addLegend(
        colors = c("#ffffcc", "#a1dab4", "#2c7fb8"),
        labels = c("Rendah (10 - 17)", "Sedang (18 - 25)", "Tinggi (26 - 32)"),
        title = "Kelas Tingkat Partisipasi",
        position = "bottomright",
        group = "Tingkat Partisipasi Unfiltered"
      ) %>%
      addPolygons(
        data = summarymap,
        fillColor = ~pal3(KL_FIL),
        fillOpacity = 0.8,
        stroke = T,
        color = "white",
        weight = 1,
        opacity = 0.7,
        highlightOptions = highlightOptions(color = "white", weight = 2, fillOpacity = 1, bringToFront = T),
        popup = paste("<h6 style = 'color: #2c7fb8;'>Kecamatan: </h6>", summarymap$kecamatan, "<br>",
                      "<h6 style = 'color: #2c7fb8;'>Skor: </h6>", summarymap$SK_TOT_FIL, "<br>",
                      "<h6 style= 'color: #2c7fb8;'>Kelas: </h6>", summarymap$KL_FIL, "<br>"),
        group = "Tingkat Partisipasi Filtered"
      ) %>%
      addLegend(
        colors = c("#ffffcc", "#a1dab4", "#2c7fb8"),
        labels = c("Rendah (10 - 17)", "Sedang (18 - 25)", "Tinggi (26 - 32)"),
        title = "Kelas Tingkat Partisipasi Filtered",
        position = "bottomright",
        group = "Tingkat Partisipasi Filtered"
      ) %>%
      addPolygons(
        data = summarymap,
        fillColor = ~pal4(KL_IND),
        fillOpacity = 0.8,
        stroke = T,
        color = "white",
        weight = 1,
        opacity = 0.7,
        highlightOptions = highlightOptions(color = "white", weight = 2, fillOpacity = 1, bringToFront = T),
        popup = paste("<h6 style = 'color: #2c7fb8;'>Kecamatan: </h6>", summarymap$kecamatan, "<br>",
                      "<h6 style = 'color: #2c7fb8;'>Skor: </h6>", summarymap$SK_IND, "<br>",
                      "<h6 style= 'color: #2c7fb8;'>Kelas: </h6>", summarymap$KL_IND, "<br>"),
        group = "Tingkat Partisipasi Indikator"
      ) %>%
      addLegend(
        colors = c("#ffffcc", "#a1dab4", "#2c7fb8"),
        labels = c("Rendah (2 - 3,67)", "Sedang (3,68 - 5,34)", "Tinggi (5,35 - 7)"),
        title = "Kelas Tingkat Partisipasi Indikator",
        position = "bottomright",
        group = "Tingkat Partisipasi Indikator"
      ) %>%
      ##### Peta Electoral Vote #####
      addPolygons(
        data = summarymap,
        group = "Persentase Capres",
        fillColor = ~factpal(KL_MAXPRES),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(weight = 2, color = "black", fillOpacity = 0.5, bringToFront = T),
        popup = ~paste(
          "<div>",
          "<b>Kecamatan ", summarymap$kecamatan, "</b>",
          "<br>",
          "<div class='circle' style='background-color: #b2182b;'></div> Ganjar Pranowo: ", summarymap$GP_PCT, "%",
          "<br>",
          "<div class='circle' style='background-color: #f1a340;'></div> Prabowo Subianto: ", summarymap$PS_PCT, "%",
          "<br>",
          "<div class='circle' style='background-color: #386cb0;'></div> Anies Baswedan: ", summarymap$AB_PCT, "%",
          "<br>",
          "<div class='circle' style='background-color: #542788;'></div> Lainnya: ", summarymap$LAIN_PCT, "%",
          "<br>",
          "<div class='circle' style='background-color: #999999;'></div> Belum Menentukan Pilihan: ", summarymap$NA_PCT, "%",
          "</div>"
        ),
      ) %>%
      addLegend(
        colors = c("#b2182b", "#f1a340", "#386cb0", "#542788", "#999999"),
        labels = c("Ganjar Pranowo", "Prabowo Subianto", "Anies Baswedan", "Lainnya", "Belum Menentukan Pilihan"),
        title = "Nama Calon Presiden",
        position = "bottomright",
        group = "Persentase Capres"
      ) %>%
      hideGroup(c("Tingkat Partisipasi Filtered", "Tingkat Partisipasi Indikator", "Tingkat Partisipasi Unfiltered", "Demografi "))
  })
  
  ########## Sidebar Menu ##########
  #valuebox output
  output$asal <- renderValueBox({
    valueBox(subtitle = "Responden Berasal Dari Purwodadi",
             value = 96,
             icon = icon("location-dot"),
             width = "100%",
             color = "navy"
    )
  })
  output$usia <- renderValueBox({
    age_count <- table(data_hasil$USIA)
    dominating_age <- as.integer(names(age_count)[which.max(age_count)])
    valueBox(subtitle = "Usia Responden Terbanyak",
             value = paste(dominating_age, "Tahun"),
             icon = icon("users"),
             width = "100%",
             color = "navy"
    )
  })
  output$gender <- renderValueBox({
    percentile_gender <- round(sum(data_hasil$GENDER == "Perempuan") / nrow(data_hasil) * 100, 2)
    valueBox(subtitle = "Persentase Responden Jenis Kelamin Perempuan",
             value = paste(percentile_gender, "%"),
             icon = icon("venus-mars"),
             width = "100%",
             color = "navy"
    )
  })
  output$pekerjaan_ortu <- renderValueBox({
    percentile_work <- round(sum(data_hasil$PKRJ_ORTU == "Buruh") / nrow(data_hasil) * 100, 2)
    valueBox(subtitle = "Persentase Pekerjaan Orang Tua Responden sebagai Buruh",
             value = paste(percentile_work, "%"),
             icon = icon("briefcase"),
             width = "100%",
             color = "navy"
    )
  })
  output$penghasilan_ortu <- renderValueBox({
    percentile_bills <- round(sum(data_hasil$GAJI_ORTU == "<Rp1.000.000,00") / nrow(data_hasil) * 100, 2)
    valueBox(subtitle = "Persentase Penghasilan Orang Tua Responden <Rp1.000.000,00",
             value = paste(percentile_bills, "%"),
             icon = icon("money-bill-wave"),
             width = "100%",
             color = "navy"
    )
  })
  output$skor_partisipasi <- renderValueBox({
    mean_participation <- round(mean(data_hasil$SKOR_TOTAL), 2)
    valueBox(subtitle = "Skor Rata-Rata Tingkat Partisipasi Pemilih Pemula di Kabupaten Purworejo",
             value = mean_participation,
             icon = icon("square-poll-vertical"),
             width = "100%",
             color = "navy"
    )
  })

  #barplot_capres
  color_capres <- c("Ganjar Pranowo" = "#d7191c", "Prabowo Subianto" = "#fdae61", "Anies Baswedan" = "#2c7bb6", "Lainnya" = "#abd9e9", "Belum menentukan pilihan" = "#ffffbf")
  output$barplot_capres <- renderPlotly({
    p <- ggplot(data = data_hasil)+
      geom_bar(aes(x = str_wrap(CAPRES, width = 5), fill = CAPRES), stat = "count")+
      scale_fill_manual(values = color_capres)+
      guides(fill = "none")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = "Jumlah Suara Capres Keseluruhan", x = "Nama Capres", y = "Jumlah Suara")
    ggplotly(p, tooltip = c("count"))
  })
  #barplot_legislatif
  color_legis <- c("Demokrat" = "#1f78b4", "Gerindra" = "#ff7f00", "Golkar" = "#ffff99", "PDIP" = "#e31a1c", "Nasdem" = "#a6cee3", "PAN" = "#fdbf6f", "Perindo" = "#6a3d9a", "PKB" = "#cab2d6", "PSI" = "#fb9a99", "PKS" = "#b2df8a", "PPP" = "#33a02c")
  output$barplot_leg <- renderPlotly({
    p <- ggplot(data = data_hasil)+
      geom_bar(aes(x = str_wrap(PARTAI_LEGISLATIF, width = 7), fill = PARTAI_LEGISLATIF), stat = "count")+
      scale_fill_manual(values = color_legis)+
      guides(fill = "none")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = "Jumlah Suara Parpol Legislatif", x = "Nama Parpol", y = "Jumlah Suara")
    ggplotly(p, tooltip = c("count"))
  })
  #barplot_ind5
  output$barplot_ind5 <- renderPlotly({
    p <- ggplot(data = data_hasil)+
      geom_bar(aes(x = IND5), stat = "count", fill = "#2FB380")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = "Pendapat Responden Tentang Golput", x = "Pendapat Pemilih Pemula Tentang Golput", y = "Jumlah")
    ggplotly(p, tooltip = c("count"))
  })
  #barplot_ind8
  output$barplot_ind8 <- renderPlotly({
    p <- ggplot(data = data_hasil)+
      geom_bar(aes(x = IND8), stat = "count", fill = "#3459E6")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = "Apakah Responden akan Memberikan Hak Suara", x = "Pendapat Tentang Hak Suara", y = "Jumlah")
    ggplotly(p, tooltip = c("count"))
  })
  #barplot_ind9
  output$barplot_ind9 <- renderPlotly({
    p <- ggplot(data = data_hasil)+
      geom_bar(aes(x = IND9), stat = "count", fill = "#141414")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = "Apakah Responden Masa Bodoh Terhadap Hasil Pemilu?", x = "Pendapat Tentang Hasil Pemilu", y = "Jumlah")
    ggplotly(p, tooltip = c("count"))
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
    p <- ggplot(data = datahasil_filteredby_kec())+
      geom_bar(aes(str_wrap(IND2, width = 10)), stat = "count", fill = "#2FB380")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = paste("Pengetahuan Pemilu di", input$kecamatan), x = "Tingkat Pengetahuan Pemilih Pemula", y = "Count")

    ggplotly(p, tooltip = c("count"))
  })
  
  #barplot kecamatan by sumber informasi pemilu
  output$barplot_kecby_ind4 <- renderPlotly({
    p <- ggplot(data = datahasil_filteredby_kec())+
      geom_bar(aes(x = str_wrap(IND4, width = 10)), stat = "count", fill = "#3459E6")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = paste("Sumber Informasi Pemilu di", input$kecamatan), x = "Sumber Informasi Pemilu", y = "Count")
    
    ggplotly(p, tooltip = c("count"))
  })
  
  #barplot kecamatan by pendapat tentang golput
  output$barplot_kecby_ind5 <- renderPlotly({
    p <- ggplot(data = datahasil_filteredby_kec())+
      geom_bar(aes(x = str_wrap(IND6, width = 10)), stat = "count", fill = "#141414")+
      theme_minimal()+
      theme(panel.grid = element_blank())+
      labs(title = paste("Ketertarikan Diskusi Politik di", input$kecamatan), x = "Pendapat Pemilih Pemula", y = "Count")
    
    ggplotly(p, tooltip = c("count"))
  })

  ################ viewport kedua ###################
  #Map rendering logic
  #action button to show the map
  observeEvent(input$showmap_button, {
  #plotting the demografi map using ggplot2
  if (input$demografi_map_option == "Populasi") {
    output$demografi_map <- renderPlotly({
      p <- ggplot()+
        geom_sf(data = demografimap, aes(fill = POPULASI))+
        scale_fill_gradient(low = "#ece2f0", high = "#1c9099")+
        theme_minimal()+
        theme(panel.grid = element_blank())+
        labs(title = "Populasi di Kabupaten Purworejo Tahun 2022")
      ggplotly(p)
    })
  } else if (input$demografi_map_option == "Kepadatan Penduduk") {
    output$demografi_map <- renderPlotly({
      p <- ggplot()+
        geom_sf(data = demografimap, aes(fill = KPDTN_PEND))+
        scale_fill_gradient(low = "#ece2f0", high = "#1c9099")+
        theme_minimal()+
        theme(panel.grid = element_blank())+
        labs(title = "Kepadatan Penduduk Kabupaten Purworejo Tahun 2022")
      ggplotly(p)
    })
  } else if (input$demografi_map_option == "Responden Laki-laki") {
    output$demografi_map <- renderPlotly({
      p <- ggplot()+
        geom_sf(data = demografimap, aes(fill = RES_BOY))+
        scale_fill_gradient(low = "#ece2f0", high = "#1c9099")+
        theme_minimal()+
        theme(panel.grid = element_blank())+
        labs(title = "Jumlah Responden Laki-laki")
      ggplotly(p)
    })
  } else if (input$demografi_map_option == "Responden Perempuan") {
    output$demografi_map <- renderPlotly({
      p <- ggplot()+
        geom_sf(data = demografimap, aes(fill = RES_GIRL))+
        scale_fill_gradient(low = "#ece2f0", high = "#1c9099")+
        theme_minimal()+
        theme(panel.grid = element_blank())+
        labs(title = "Jumlah Responden Laki-laki")
      ggplotly(p)
    })
  } else if (input$demografi_map_option == "Beragama Islam") {
    output$demografi_map <- renderPlotly({
      p <- ggplot()+
        geom_sf(data = demografimap, aes(fill = ISLAM))+
        scale_fill_gradient(low = "#bdc9e1", high = "#045a8d")+
        theme_minimal()+
        theme(panel.grid = element_blank())+
        labs(title = "Jumlah Penduduk Beragama Islam")
      ggplotly(p)
    })
  } else if (input$demografi_map_option == "Beragama Non-Islam") {
    output$demografi_map <- renderPlotly({
      p <- ggplot()+
        geom_sf(data = demografimap, aes(fill = NON_ISLAM))+
        scale_fill_gradient(low = "#bdc9e1", high = "#045a8d")+
        theme_minimal()+
        theme(panel.grid = element_blank())+
        labs(title = "Jumlah Penduduk Beragama Non-Islam")
      ggplotly(p)
    })
  } else if (input$demografi_map_option == "Jarak Kabupaten") {
    output$demografi_map <- renderPlotly({
      p <- ggplot()+
        geom_sf(data = demografimap, aes(fill = JARAK_KAB))+
        scale_fill_gradient(low = "#f0f9e8", high = "#253494")+
        theme_minimal()+
        theme(panel.grid = element_blank())+
        labs(title = "Jarak ke Pusat Kabupaten Per Kecamatan")
      ggplotly(p)
    })
  } else if (input$demografi_map_option == "Ketinggian Wilayah") {
    output$demografi_map <- renderPlotly({
      p <- ggplot()+
        geom_sf(data = demografimap, aes(fill = TINGGI_MDP))+
        scale_fill_gradient(low = "#f0f9e8", high = "#253494")+
        theme_minimal()+
        theme(panel.grid = element_blank())+
        labs(title = "Ketinggian Wilayah Per Kecamatan")
      ggplotly(p)
    })
  }
    #plotting the tingkat partisipasi map using ggplot2
    output$tingkatpartisipasi_map <- renderPlotly({
      p <- ggplot() +
        geom_sf(data = participationmap, aes(fill = SK_TOTAL))+
        scale_fill_gradient2(low = "#2FB380", mid = "white", high = "#3459E6", midpoint = 25,
                             breaks = c(22, 24, 26),
                             labels = c("Kurang Partisipatif", "Cukup Partisipatif", "Sangat Partisipatif"),
                             limits = c(22, 26))+
        theme_minimal() +
        theme(panel.grid = element_blank()) +
        labs(title = "Skor Tingkat Partisipasi Kabupaten Purworejo")
      ggplotly(p)
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
             subtitle = paste0("Rerata Skor Tingkat Partisipasi di ", input$kecamatan_elect),
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
  #rendering map and table for selectinput value
  elected_data <- electoralmap %>%
    select(kode_dagri, kecamatan, geometry, GP_PCT, PS_PCT, AB_PCT, LAIN_PCT, NA_PCT)
  
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
    output$presiden_map <- renderPlotly({
      p <- ggplot()+
        geom_sf(data = elected_data, aes(fill = .data[[column_name]]))+
        scale_fill_gradient(name = selected_capres)+
        labs(fill = selected_capres)+
        theme_minimal()
      ggplotly(p)
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
        
        gg <- ggplot(electoralmap)+
          geom_sf(aes(fill = .data[[aes_name]]))+
          scale_fill_gradient2(low = "#ffffb2", mid = "#fd8d3c", high = "#bd0026", name = legend_label)+
          labs(title = legend_label)+
          theme_void()
        
        gg_plots[[party]] <- gg
      }
      
      #set mfrow layout for multiple plots
      par(mfrow = c(length(selected_parpol), 1))
      
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


