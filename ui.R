ui <- navbarPage(
  # Título de la app
  title = "ANÁLISIS DE INSEGURIDAD CIUDADANA EN COSTA RICA 2013-2023",  
  
  # Crea tabPanels
  tabPanel(title = "Bienvenida", 
           p("El siguiente dashboard presentará un análisis de la seguridad ciudadana en Costa Rica, entre los años 2013 a 2023.", br(),
             "Los datos son tomados de la página del Organismo Investigación Judicial, OIJ y tienen un fin educativo.", br(),
             "Este tablero constituye el proyecto final de graduación del técnico de Análisis y Visualización de datos."),
           downloadButton("download1")
  ),                  
  tabPanel(title = "Análisis General", 
           h1("Análisis General de la seguridad ciudadana en Costa Rica"),
           fluidRow(
             column(3, selectInput(inputId = "provincia", label = "Selecciona una provincia", choices = NULL)),
             column(3, selectInput(inputId = "delito", label = "Delito:", choices = NULL)),
             column(3,selectInput(inputId = "canton",label = "Escoja el cantón: ",choices = NULL,selected = NULL)),
             column(3,selectInput(inputId = "anyo", label = "Escoja el año", choices = NULL))
             
           ),
           fluidRow(
             column(8, withSpinner(plotlyOutput("lineas"), type = 3, color.background = "#060606", color = "#9E94DE")),
             column(4,dataTableOutput("subdelitos"))
           )
  ),  
  
  tabPanel(title = "Delitos por provincia", 
           h1("Análisis de Delitos por provincia"),
           h4("Se realizará una búsqueda de los cinco cantones con mayor cantidad de delitos por provincia"),
           fluidRow(
             column(6, selectInput(inputId = "prov", label = "Selecciona una provincia", choices = NULL))
           ),
           column(6, withSpinner(plotlyOutput("top_cantones"), type = 3, color.background = "#060606", color = "#FF6433")),
           column(6, leafletOutput("provincia_mapa"))
  ), 
  
  tabPanel(title = "Delitos por edad", 
           h1("Delitos según rango etario de las víctimas"),
           h4("Se realizará una búsqueda de los tipos delitos según rango etario de las víctimas, provincia y año"),
           fluidRow(
             column(4, selectInput(inputId = "provin", label = "Selecciona una provincia:", choices = NULL)),
             column(2, selectInput(inputId = "año", label = "Año:", choices = NULL)),
             column(4, selectInput(inputId = "canton1", label = "Seleccione un cantón:", choices = NULL)),
           ),
           fluidRow(
             column(6, withSpinner(plotlyOutput("barra1"), type = 3, color.background = "#060606", color = "blue")),
             column(6, withSpinner(plotlyOutput("barra1.2"), type = 3, color.background = "#060606", color = "red"))
           )
  ), 
  
  tabPanel(title = "Delitos por género", 
           h1("Delitos según género de las víctimas, provincia y año"),
           h4("Se realizará una búsqueda de los tipos delitos según género de las víctimas, provincia y año"),
           fluidRow(
             column(4, selectInput(inputId = "provi", label = "Selecciona una provincia", choices = NULL)),
             column(2, selectInput(inputId = "anio", label = "Año:", choices = NULL)),
             column(4, selectInput(inputId = "canton2", label = "Seleccione un cantón:", choices = NULL)),
           ),
           fluidRow(
             column(6, withSpinner(plotlyOutput("barra2"), type = 3, color.background = "#060606", color = "green")),
             column(6, withSpinner(plotlyOutput("barra2.2"), type = 3, color.background = "#060606", color = "red"))
           )
  ),    
  
  theme = shinythemes::shinytheme("darkly")
  
)