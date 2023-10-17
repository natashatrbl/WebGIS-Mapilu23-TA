
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





#data csv
data_hasil <- read.csv("data/hasil_survei.csv")
data_hasil_fil <- read.csv("data/hasil_survei_fil.csv")
summarycsv <- read.csv("data/summarymap.csv")
point_sma <- read.csv("data/point_sma.csv")
electoral_csv <- read.csv("data/electoral_avg.csv")
electoralfil_csv <- read.csv("data/electoral_avg_fil.csv")

#data geojson
electoralmap <- sf::st_read("data/electoralmap_geojson.geojson")
electoralmap_fil <- sf::st_read("data/electoralmapfil_geojson.geojson")
demografimap <- sf::st_read("data/demografimap_geojson.geojson")
participationmap <- sf::st_read("data/participationmapun-fil_geojson.geojson")
summarymap <- sf::st_read("data/summap_un-fil_geojson.geojson")





shinyApp(ui, server)


