# Welcome to the WebGIS Mapilu! This is my final year project about visualizing the preferences of young voters in Purworejo using R Shiny

# Setup library
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(sf)
library(mapdeck)

#data skoring hasil survei
data_hasil <- read.csv("data/hasil_survei.csv")


#data geojson untuk main map
pituruh <- sf::st_read("data/Pituruh.geojson")


#data untuk demografi map
demografi_pwr <- sf::st_read("data/demografi_map.shp")

partisipasi_pwr <- sf::st_read("data/tingkatpartisipasi_withmap.shp")


#data untuk electoral map
electoral_csv <- read.csv("data/electoral_avg.csv")

electoral_pwr <- sf::st_read("data/electoral_map3.shp")


shinyApp(ui, server)






















#rendering table
#output$hasilsurvei_tabel <- DT::renderDataTable({

#selecting columns to display in the table
#selected_cols <- c("USIA", "GENDER", "KECAMATAN", "SKOR", "KELAS", "CAPRES")

#showing the select input
#kecamatanFilter <- subset(data_hasil, data_hasil$KECAMATAN == input$kecamatan1)
#kecamatanFilter

#select only the desired columns from the filtered user input
#data_subset <- kecamatanFilter %>%
#select(KECAMATAN, !!!selected_cols)  # Using !!! to unquote the column names

#render the data table
#DT::datatable(data_subset, options = list(pageLength = 10))
#})


#Dummy plot in demografi fields
#output$hasilsurvei_plot <- renderPlotly({

#p <- ggplot(data_hasil, 
#aes(x= .data[[input$variables2]]))+
#geom_bar(stat = "count")+
#ggtitle("Perhitungan Statistik Berdasarkan Kategori")+
#coord_flip()

#ggplotly(p)
#})