

########################## CREATING dashboard ###########################
dashboardPage(
  
  ###################### DASHBOARD HEADER ###############################
  dashboardHeader(
    #title
    title = "Mapilu Purworejo",
    titleWidth = "90%",
    #listing navbar
    tags$li(class="dropdown", tags$a(href="#home"))
  ),
  
  ###################### DASHBOARD SIDEBAR ############################
  dashboardSidebar(
    #buat menu sidebar
    sidebarMenu(
      id="sidebar",
      
      #Welcome Page
      menuItem(text = "Beranda", tabName = "home", icon = icon("house")),
      #Peta Mapilu Menu
      menuItem(text = "Peta Mapilu", tabName = "viz", icon = icon("map-location-dot")),
      #Demografi menu
      menuItem(text = "Analisis Demografi", tabName = "demography", icon = icon("chart-line")),
      #Electoral vote menu
      menuItem(text = "Electoral Vote", tabName = "electoral", icon = icon("square-poll-vertical"))
    )
  ),
  
  ######################### DASHBOARD BODY ###########################
  dashboardBody(
    
    #changing theme in flatly
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    
    tabItems(
      ####################### Menu homepage ##############################
      tabItem(tabName = "home",
              #home tab box
              tabBox(id="t1", width = 12,
                     tabPanel("Home",
                              fluidRow(
                                column(width = 4, 
                                       tags$img(src="politics.jpg", width = "100%", height = "100%"), 
                                       tags$br(), 
                                       tags$a("Photo by: Rafli Firmansyah on Unsplash", align = "center")),
                                column(width = 8, 
                                       tags$h1("Selamat Datang di Mapilu Purworejo"),
                                       tags$h4("Mapping Analysis of Young Voters Behavior"),
                                       tags$br(),
                                       tags$p("Pernahkah anda mendengar bahwa pemilih pemula cenderung  apatis dalam pemilu? Benarkah pemuda zaman sekarang apatis atau justru frustasi? Website ini akan membantu anda memahami bagaimana kecenderungan pemilih pemula di Purworejo terhadap pemilu 2024 dari sisi geospasial. Penasaran?"))
                                )
                              ),
                     tabPanel("About", h4("Proyek ini menjelaskan tentang...")),
                     tabPanel("Panduan", h4("Pengguna dapat membaca panduan penggunaan website di sini")
                     )
                  )
              ),
      
      ###################### Menu Peta Mapilu #############################
      tabItem(tabName = "viz",
              fluidRow(
                box(title = "Peta Preferensi Pemilih Pemula",
                    status = "primary",
                    width = 9,
                    height = "auto",
                    h2("Peta Preferensi Pemilih Pemula Pemilu 2024"),
                    mapdeckOutput("map", height = "800px")
                )
              )
      ),
      
      ###################### Menu Analisis Demografi - Tingkat Partisipasi #######################
      tabItem(tabName = "demography",
              # Bar plot dan valuebox untuk demografi - tingkat partisipasi
              fluidRow(
                # Sidebar Panel
                sidebarPanel(
                  width = 3,
                  # Selecting kecamatan
                  selectInput("kecamatan", "Pilih Kecamatan",
                              choices = data_hasil$KECAMATAN
                  ),
                  
                  fluidRow(
                    # Value box untuk gender perempuan
                      valueBoxOutput(width = "100%", "persen_perempuan"),
                    
                    #value box untuk usia 17
                      valueBoxOutput(width = "100%","persen_usia17")
                    )
                ),
                # Main Panel
                mainPanel(
                  fluidRow(
                    column(4,
                           plotlyOutput("barplot_kecby_ind2")),
                    column(4, 
                           plotlyOutput("barplot_kecby_ind4")),
                    column(4,
                           plotlyOutput("barplot_kecby_ind5"))
                  )
                )
              ),
              
              #Map for demografi, geografi, and tingkat partisipasi pemilih pemula
              fluidRow(
                sidebarPanel(
                  width = 3,
                  #Select the demografi category
                  radioButtons("demografi_map_option", "Pilih salah satu unsur demografi",
                               choices = c("Geografis", "Kependudukan"),
                               selected = "Geografis"),
                  actionButton("showmap_button", "Tampilkan Peta")
                ),
                mainPanel(
                  width = 9,
                          column(6,
                                 plotOutput("demografi_map")),
                          column(6,
                                 plotOutput("tingkatpartisipasi_map"))
              )
            )
      ),
      
      ########################### Menu Electoral Vote ###########################
      tabItem(tabName = "electoral",
              # Menu bar plot dan infobox
              fluidRow(
                #sidebar panel
                sidebarPanel(
                  width = 3,
                  selectInput("kecamatan_elect", "Pilih Kecamatan",
                              choices = unique(data_hasil$KECAMATAN)
                              ),
                  
                #main panel
                mainPanel(
                  fluidRow(
                    column(4,
                           plotlyOutput("barplot_kecby_parpolleg")),
                    column(4,
                           plotlyOutput("barplot_kecby_parpolpres")),
                    column(4, 
                           plotlyOutput("barplot_kecby_pres"))
                  )
                )
              )
            ),
            
            #Menu map for tingkat partisipasi and presiden percentage
            fluidRow(
              #sidebar panel
              sidebarPanel(
                width = 3,
                sliderInput("skor_partisipasi", "Slide skor tingkat partisipasi pemilih pemula",
                            min = 10,
                            max = 32,
                            value = 17,
                            ticks = TRUE)
                      ),
              #main Panel
              mainPanel(
                  width = 9,
                    column(6,
                          plotOutput("kelaspartisipasi_map")),
                    column(6,
                          plotOutput("presiden_map"))
                )
              ),
            fluidRow(
              #sidebar panel
              sidebarPanel(
                checkboxInput(
                  "electoralmap_checkbox", "Pilih peta yang ingin ditampilkan",
                ),
                actionButton("electoralmap_button", "Tampilkan Peta")
              ),
              #main panel
              mainPanel(
                fluidRow(
                  width = 9,
                  column(6,
                         plotOutput("penduduk_map")),
                  column(6,
                         plotOutput("geografi_map"))
                ),
                fluidRow(
                  width = 9,
                  column(6,
                         plotOutput("partisipasi_map")),
                  column(6,
                         plotOutput("parpol_leg_map"))
                ),
                fluidRow(
                  width = 9,
                  column(6,
                         plotOutput("parpol_pres_map")),
                  column(6,
                         plotOutput("capres_map"))
                )
              )
            )
          )
            
    )
  )
)


