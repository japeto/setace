ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$script(HTML('
    Shiny.addCustomMessageHandler("disable_button", function(message) {
      var btn = document.getElementById(message.id);
      btn.disabled = true;
      btn.innerHTML = message.text;
    });

    Shiny.addCustomMessageHandler("enable_button", function(message) {
      var btn = document.getElementById(message.id);
      btn.disabled = false;
      btn.innerHTML = message.text;
    });

    Shiny.addCustomMessageHandler("showMessage", (text) => {
      console.log(text)
    });
  ')),
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
  progressBar(id = "progress", value = 0, total = 100, title = "Progreso del asistente", display_pct = TRUE),
  br(),
  fluidRow(
    column(6, actionButton("btn_back", "⬅️ Atrás", class = "btn btn-warning w-100")),
    column(6, actionButton("btn_next", "Siguiente ➡️", class = "btn btn-success btn-block"))
  )
)
