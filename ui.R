

########################## CREATING dashboard ###########################
dashboardPage(
  
  ###################### DASHBOARD HEADER ###############################
  dashboardHeader(
    #title
    title = "Mapilu Purworejo",
    titleWidth = "95%",
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
                                       tags$p("Pernahkah anda mendengar bahwa pemilih pemula cenderung  apatis dalam pemilu?
                                              Benarkah pemuda zaman sekarang apatis atau justru frustasi?
                                              Website ini akan membantu anda memahami bagaimana kecenderungan pemilih pemula
                                              di Purworejo terhadap pemilu 2024 dari sisi geospasial. Penasaran?"))
                                )
                              ),
                     tabPanel("About", h4("Proyek ini menjelaskan tentang...")),
                     tabPanel("Panduan", h4("Pengguna dapat membaca panduan penggunaan website di sini")
                     )
                  )
              ),
      
      ###################### Menu Peta Mapilu #############################
      tabItem(tabName = "viz",
              ##### viewport pertama #####
              fluidRow(
                #title box
                box(
                  title = "Peta Preferensi Pemilih Pemula Kabupaten Purworejo",
                  status = "primary",
                  background = "navy",
                  width = 12,
                  collapsible = T,
                  div(
                    style = "color: white; padding: 2rem; font-size: 15px",
                    "Pada menu ini anda akan melihat bagaimana visualisasi hasil pemetaan preferensi pemilih pemula di Kabupaten Purworejo beserta beberapa informasi dan grafik yang menunjang detail visualisasi")
                ),
                sidebarPanel(
                  width = 3,
                  fluidRow(
                    valueBoxOutput(width = "100%", "asal"),
                    valueBoxOutput(width = "100%", "usia"),
                    valueBoxOutput(width = "100%", "gender"),
                    valueBoxOutput(width = "100%", "pekerjaan_ortu"),
                    valueBoxOutput(width = "100%", "penghasilan_ortu"),
                    valueBoxOutput(width = "100%", "skor_partisipasi")
                  )
                ),
                  mainPanel(
                    width = 9,
                    leafletOutput("map", height = "800px")
                  )
              ),
              ##### viewport kedua #####
              fluidRow(
                column(5, plotlyOutput("barplot_capres")),
                column(7, plotlyOutput("barplot_leg"))
              ),
              fluidRow(
                column(4, plotlyOutput("barplot_ind5")),
                column(4, plotlyOutput("barplot_ind8")),
                column(4, plotlyOutput("barplot_ind9"))
              )
      ),
      
      ###################### Menu Analisis Demografi - Tingkat Partisipasi #######################
      tabItem(tabName = "demography",
              ######## viewport pertama analisis demografi ##########
              fluidRow(
                #title box
                box(
                  title = "Detail Analisis Demografi - Tingkat Partisipasi Tiap Kecamatan",
                  status = "primary",
                  background = "navy",
                  width = 12,
                  collapsible = T,
                  div(
                    style = "color: white; padding: 2rem; font-size: 15px",
                    "Pada menu ini anda akan melihat bagaimana detail analisis demografi seperti persentase jenis kelamin dan beberapa poin indikator tingkat partisipasi pemilih pemula terhadap pemilu berdasarkan kecamatan yang anda pilih")
                ),
                # Sidebar Panel
                sidebarPanel(
                  width = 3,
                  selectInput("kecamatan", "Pilih Kecamatan",
                              choices = data_hasil$KECAMATAN
                  ),
                  fluidRow(
                      valueBoxOutput(width = "100%", "persen_perempuan"),
                      valueBoxOutput(width = "100%","persen_usia17")
                    )
                ),
                # Main Panel
                mainPanel(
                  width = 9,
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
              
              ###### viewport kedua ###### 
              fluidRow(
                #title box
                box(
                  title = "Perbandingan Peta Choropleth Demografi dan Tingkat Partisipasi Pemilih Pemula",
                  status = "primary",
                  background = "navy",
                  width = 12,
                  collapsible = T,
                  div(
                    style = "color: white; padding: 2rem; font-size: 15px",
                    "Pada menu ini anda akan melihat bagaimana perbandingan unsur demografi dan tingkat partisipasi pemilih pemula melalui visualisasi peta choropleth setiap kecamatan. Dapatkah anda menemukan keterkaitan antara tingkat partisipasi pemilih pemula dengan unsur demografinya?")
                ),
                sidebarPanel(
                  width = 3,
                  #Select the demografi category
                  radioButtons("demografi_map_option", "Pilih salah satu unsur demografi",
                               choices = c("Jarak Kabupaten", "Ketinggian Wilayah", "Populasi", 
                                           "Kepadatan Penduduk", "Responden Laki-laki", "Responden Perempuan",
                                           "Beragama Islam", "Beragama Non-Islam"),
                               selected = "Jarak Kabupaten"),
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
              ####### viewport pertama: Menu bar plot dan infobox #########
              fluidRow(
                #title box
                box(
                  title = "Detail Preferensi Pemilih Pemula Tiap Kecamatan",
                  status = "primary",
                  background = "navy",
                  width = 12,
                  collapsible = T,
                  div(
                    style = "color: white; padding: 2rem; font-size: 15px",
                    "Pada menu ini anda akan melihat bagaimana detail preferensi pemilih pemula terhadap partai politik dan calon presiden pada pemilu 2024 berdasarkan kecamatan yang anda pilih")
                ),
                #sidebar panel
                sidebarPanel(
                  width = 3,
                  #select input kecamatan
                  selectInput("kecamatan_elect", "Pilih Kecamatan",
                              choices = unique(data_hasil$KECAMATAN)
                            ),
                  fluidRow(
                    #valuebox untuk rerata tingkat partisipasi pemilih pemula per kecamatan
                    valueBoxOutput(width = "100%", "partisipatif_mean"),
                    #valuebox untuk persentase parpol legislatif per kecamatan
                    valueBoxOutput(width = "100%", "parpolleg_persentase"),
                    #valuebox untuk persentase nama capres per kecamatan
                    valueBoxOutput(width = "100%", "capres_persentase") 
                  )
              ),
              #main panel
                mainPanel(
                  width = 9,
                  fluidRow(
                    column(7,
                           plotlyOutput("barplot_kecby_parpolleg")),
                    column(5, 
                           plotlyOutput("barplot_kecby_capres"))
                  )
                )
            ),
            
            ######## viewport kedua: Menu map for tingkat partisipasi and presiden percentage ########
            fluidRow(
              #title box
              box(
                title = "Detail Dominasi Calon Presiden Berdasarkan Skala",
                status = "primary",
                background = "navy",
                width = 12,
                collapsible = T,
                div(
                  style = "color: white; padding: 2rem; font-size: 15px",
                  "Saatnya melihat dominasi masing-masing capres di setiap kecamatan. Anda dapat memilih salah satu nama calon untuk mengetahui bagaimana visualisasi dominasi atau persentase pemilih di masing-masing kecamatan dari yang paling tinggi hingga ke paling rendah")
              ),
              #sidebar panel
              sidebarPanel(
                width = 3,
                #select input dominasi capres
                selectInput("capres_selector", "Pilih Kandidat Capres",
                            choices = c("Ganjar Pranowo", "Prabowo Subianto", "Anies Baswedan", "Lainnya", "Belum Menentukan Pilihan")
                ),
                box(
                  title = "Visualisasi Spasial Capres",
                  status = "primary",
                  background = "navy",
                  width = "100%",
                  collapsible = F,
                  div(
                    style = "color: white; padding: 1rem",
                    "Silahkan memilih salah satu nama calon presiden, maka anda akan melihat peta visualisasi dominasi setiap nama calon berdasarkan kecamatan")
                )
                      ),
              #main Panel tampilan map detail per kecamatan berdasarkan slider input dan table output 
              mainPanel(
                  width = 9,
                    column(8,
                          plotlyOutput("presiden_map")),
                    column(4,
                           tableOutput("presiden_table"))
                )
              ),
            
            ####### viewport ketiga: menu map untuk tampilan perbandingan persentase parpol #######
            fluidRow(
              #title box
              box(
                title = "Perbandingan Partai Politik untuk Pemilu Legislatif dalam bentuk Peta Choropleth",
                status = "primary",
                background = "navy",
                width = 12,
                collapsible = T,
                div(
                  style = "color: white; padding: 2rem; font-size: 15px",
                  "Saatnya membandingkan masing-masing partai politik dengan melihat peta! Anda dapat melihat bagaimana dominasi setiap partai politik menurut kaum muda berdasarkan kecamatan. Pada menu ini anda dapat memilih lebih dari satu partai politik untuk melihat perbandingannya. Ada 7 partai politik yang paling mendominasi berdasarkan respon dari survei langsung pada pemilih pemula di Purworejo.")
              ),
              #sidebar panel checkboxgroup dan action button
              sidebarPanel(
                  width = 3,
                  #checkbox group untuk opsi peta
                  checkboxGroupInput(
                    "electoralmap_checkbox", "Pilih partai yang ingin ditampilkan",
                    choices = c("PDIP", "Gerindra", "Nasdem", "Demokrat", "Golkar", "Perindo", "PAN"),
                    selected = c("PDIP", "Gerindra")
                  ),
                  #box keterangan
                  box(
                    title = "Visualisasi Spasial Capres",
                    status = "primary",
                    background = "navy",
                    width = "100%",
                    collapsible = F,
                    div(
                      style = "color: white; padding: 1rem",
                      "Silahkan memilih salah satu nama calon presiden, maka anda akan melihat peta visualisasi dominasi setiap nama calon berdasarkan kecamatan")
                  ),
                  #action button untuk menampilkan peta
                  actionButton("electoralmap_button", "Tampilkan Peta")
              ),
              #main panel plot peta
              mainPanel(
                width = 9,
                plotOutput("electoral_map")
              )
            )
          )
            
    )
  )
)