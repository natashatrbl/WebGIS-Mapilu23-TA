
# Welcome to the WebGIS Mapilu! 
#This is my final year project about visualizing the preferences of young voters in Purworejo using R Shiny

# Setup library
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(plotly)
library(tidyverse)
library(sf)
library(htmltools)
library(htmlwidgets)
library(leaflet)
library(leaflet.extras)
library(mapdeck)


#data skoring hasil survei
data_hasil <- read.csv("data/hasil_survei.csv")
data_hasil_fil <- read.csv("data/hasil_survei_fil.csv")
summarycsv <- read.csv("data/summarymap.csv")

#data geojson untuk main map
electoralmap <- sf::st_read("data/electoralmap_geojson.geojson")
electoralmap_fil <- sf::st_read("data/electoralmapfil_geojson.geojson")
demografimap <- sf::st_read("data/demografimap_geojson.geojson")
participationmap <- sf::st_read("data/participationmapun-fil_geojson.geojson")
summarymap <- sf::st_read("data/summap_un-fil_geojson.geojson")
pointsma_pwr <- sf::st_read("data/point_sma.geojson")


#data untuk demografi map
demografi_pwr <- sf::st_read("data/demografimap_geojson.geojson")

partisipasi_pwr <- sf::st_read("data/participationmapun-fil_geojson.geojson")


#data untuk electoral map
electoral_csv <- read.csv("data/electoral_avg.csv")
electoralfil_csv <- read.csv("data/electoral_avg_fil.csv")

electoral_pwr <- sf::st_read("data/electoralmap_geojson.geojson")
electoral_pwr_fil <- sf::st_read("data/electoralmapfil_geojson.geojson")


shinyApp(ui, server)


