ui <- fluidPage(
  tags$head(
    includeCSS("www/styles.css"),
    tags$title("Sistema de Extracción y Transformación de datos de Consumo electrico (SiExTra).")
  ),
  h2(class = "card-title text-center text-primary", 
    "Sistema de Extracción y Transformación de datos "),
  h3(class = "card-title text-center text-primary", 
    "de Consumo Electrico"),
  br(),
  uiOutput("dynamic_ui"),
  br(),
  br(),
  # Barra de progreso
  progressBar(id = "progress", value = 0, total = 5, title = "Progreso del asistente"),
  
  br(),
  
  # Botones de navegación con el prefijo btn_
  fluidRow(
    column(6, actionButton("btn_back", "⬅️ Atrás", class = "btn btn-warning w-100")),
    column(6, actionButton("btn_next", "Siguiente ➡️", class = "btn btn-success btn-block"))
  )
)
