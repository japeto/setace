library(shiny)
library(DT)
library(shinyWidgets)
library(writexl)

# CSS para que todo ocupe el 100% del ancho
custom_css <- "
  .container-fluid {
    max-width: 800px !important;
    margin: 0 auto; /* Centra la página */
    padding: 20px;
  }
  .shiny-input-container {
    width: 100% !important;
  }
"

ui <- fluidPage(
  tags$head(
    tags$style(HTML(custom_css)),
    tags$title("Sistema de Extracción y Transformación de datos de Consumo electrico (SiExTra).")
  ),
  
  h3(class = "card-title text-center text-primary", "Sistema de Extracción y Transformación de datos "),
  h3(class = "card-title text-center text-primary", "de Consumo Electrico"),

    br(),
  
  uiOutput("dynamic_ui"),
  
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

server <- function(input, output, session) {
  step <- reactiveVal(1)
  
  # Para guardar los datos cargados
  data_list <- reactiveValues(file1 = NULL, file2 = NULL, file3 = NULL)
  
  # Para trabajar el dataset
  working_data <- reactiveVal(NULL)
  
  output$dynamic_ui <- renderUI({
    if (step() == 1) {
      tagList(
        h4("Paso 1: Cargar archivos RData"),
        fileInput("file1", "Datos de consumos (Consumos)", accept = ".RData"),
        fileInput("file2", "Datos de administración de Consumos (SAC)", accept = ".RData"),
        fileInput("file3", "Datos de revisiónes (Encriptada)", accept = ".RData"),
        verbatimTextOutput("load_status"),
      )
    } else if (step() == 2) {
      req(working_data())
      tagList(
        h2("Paso 2: Vista previa y limpieza de datos"),
        DTOutput("data_table"),
        br(),
        pickerInput("remove_cols", "Seleccionar columnas a eliminar:", 
                    choices = names(working_data()), 
                    multiple = TRUE, 
                    options = list(`actions-box` = TRUE)),
        br(),
        checkboxInput("remove_na_rows", "Eliminar filas con valores faltantes (NA)", FALSE),
        pickerInput("transform_na", "Reemplazar valores NA por:", 
                    choices = c("No hacer nada", "0", "Media de columna"), 
                    selected = "No hacer nada"),
        br(),
        checkboxInput("normalize_data", "Normalizar columnas numéricas", FALSE)
      )
    } else if (step() == 3) {
      req(working_data())
      tagList(
        h2("Paso 3: Descargar datos limpios"),
        downloadButton("download_clean", "Descargar archivo limpio (.xlsx)")
      )
    }
  })
  
  observeEvent(input$file1, {
    req(input$file1)
    e <- new.env()
    load(input$file1$datapath, envir = e)
    data_list$file1 <- mget(ls(e), envir = e)[[1]]
    working_data(data_list$file1)
  })
  
  observeEvent(input$file2, {
    req(input$file2)
    e <- new.env()
    load(input$file2$datapath, envir = e)
    data_list$file2 <- mget(ls(e), envir = e)[[1]]
  })
  
  observeEvent(input$file3, {
    req(input$file3)
    e <- new.env()
    load(input$file3$datapath, envir = e)
    data_list$file3 <- mget(ls(e), envir = e)[[1]]
  })
  
  output$data_table <- renderDT({
    req(working_data())
    datatable(head(working_data(), 10), options = list(scrollX = TRUE))
  })
  
  observeEvent(input$btn_next, {
    if (step() == 1) {
      if (is.null(working_data())) {
        showModal(modalDialog(
          title = "Error",
          "Por favor carga al menos un archivo antes de continuar.",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        step(2)
        updateProgressBar(session, "progress", value = 66)
      }
    } else if (step() == 2) {
      new_data <- working_data()
      
      # Eliminar columnas seleccionadas
      if (!is.null(input$remove_cols)) {
        new_data <- new_data[, !(names(new_data) %in% input$remove_cols)]
      }
      
      # Eliminar filas con NA
      if (input$remove_na_rows) {
        new_data <- na.omit(new_data)
      }
      
      # Transformar valores NA
      if (input$transform_na == "0") {
        new_data[is.na(new_data)] <- 0
      } else if (input$transform_na == "Media de columna") {
        for (col in names(new_data)) {
          if (is.numeric(new_data[[col]])) {
            mean_value <- mean(new_data[[col]], na.rm = TRUE)
            new_data[[col]][is.na(new_data[[col]])] <- mean_value
          }
        }
      }
      
      # Normalizar columnas numéricas
      if (input$normalize_data) {
        for (col in names(new_data)) {
          if (is.numeric(new_data[[col]])) {
            min_val <- min(new_data[[col]], na.rm = TRUE)
            max_val <- max(new_data[[col]], na.rm = TRUE)
            if (max_val != min_val) {
              new_data[[col]] <- (new_data[[col]] - min_val) / (max_val - min_val)
            }
          }
        }
      }
      
      working_data(new_data)
      step(3)
      updateProgressBar(session, "progress", value = 100)
    }
  })
  
  observeEvent(input$btn_back, {
    if (step() > 1) {
      step(step() - 1)
      updateProgressBar(session, "progress", value = ifelse(step() == 1, 33, 66))
    }
  })
  
  output$download_clean <- downloadHandler(
    filename = function() {
      paste0("datos_limpios_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(working_data(), file)
    }
  )
}

shinyApp(
  ui = ui,
  server = server,
  options = list(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB
)
