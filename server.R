
server <- function(input, output, session) {
  step <- reactiveVal(1)
  
  # Para guardar los datos cargados
  data_list <- reactiveValues(file1 = NULL, file2 = NULL, file3 = NULL)
  # Para presentar el nombre de los objetos en los archivos
  list_messages <- reactiveVal(NULL)
  # Para trabajar el dataset
  working_data <- reactiveVal(NULL)
  
  output$dynamic_ui <- renderUI({
    if (step() == 1) {
      tagList(
        h4("Paso 1: Cargar archivos RData"),
        br(),
        fileInput("file1", "Datos de consumos (Consumos)", accept = ".RData"),
        fileInput("file2", "Datos de administración de Consumos (SAC)", accept = ".RData"),
        fileInput("file3", "Datos de revisiónes (Encriptada)", accept = ".RData"),
        verbatimTextOutput("load_status"),
        verbatimTextOutput("loaded_objects"),
      )
    } else if (step() == 2) {
      req(working_data())
      tagList(
        h4("Paso 2: Vista previa y limpieza de datos"),
        DTOutput("data_table"),
        showNotification(paste("Notification message"), duration = 0),
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
  
  output$loaded_objects <- renderPrint({
    if (is.null(list_messages())) {
      cat("")
    } else {
      cat(list_messages())
    }
  })

  output$data_table <- renderDT({
    req(working_data())
    datatable(head(working_data(), 10), options = list(scrollX = TRUE))
  })
  
  observeEvent(input$btn_next, {
    if (step() == 1) {
      session$sendCustomMessage(type = "disable_button", list(id = "siguiente", text = "Procesando..."))
      
      if (is.null(working_data())) {
        showModal(modalDialog(
          title = tags$div(
            icon("exclamation-triangle", class = "text-danger"),
            " Error en Carga de Datos",
            style = "color: #d9534f;"
          ),
          tags$div(
            tags$p("Debe seleccionar ambos archivos para continuar:"),
            tags$ul(
              tags$li("Archivo de Consumos"),
              tags$li("Archivo de Datos")
            ),
            style = "font-size: 16px;"
          ),
          footer = tagList(
            modalButton("Entendido", icon = icon("check"))
          ),
          easyClose = FALSE,
          size = "m"
        ))
      } else {
        list_messages("Inicia la union de acuerdo al ID del cliente")
        # Deshabilitar el botón (con JavaScript inline)
        
        sac_data <- data_list$file1 #working_data()
        con_sac_data <- data_list$file2
        # print( colnames(sac_data) )
        # print( colnames(con_sac_data) )
        # 1. Union de las dos bases de datos,
        #     con los individuos que esten presentes en amdas bases
        data <- merged_data <- inner_join(
          sac_data, con_sac_data, by = c("CLIENTE" = "CLIENTE_ID"))
        
        updateProgressBar(session, "progress", value = 36)
        
        # 2. Elimina los individuos de la base de datos
        data <- subset(data,
                       !(CICLO %in% c("53", "98", "104") |
                           (CICLO == "56" & TARIFA == "373") |
                           EST_CLIENTE == "Inactivo"))
        updateProgressBar(session, "progress", value = 39)
        
        list_messages(
          paste("Elimina del conjunto de datos todas las filas donde:\n",
                "> El ciclo es '53', '98' o '104'\n",
                "> donde el ciclo es '56' y la tarifa es '373' \n")
        )
        
        # 3. Otros individuos que debo eliminar
        list_messages("Inicia eliminación de Santafe Bogota DC")
        list_messages("Continua eliminación de servicios de Macromedición")
        list_messages("Inicia eliminación de inactivos")
        
        data <- data %>%
          filter(
            DEPTO != "11",
            DES_CLASE_SERVICIO != "Macromedición",
            EST_CLIENTE == "Activo"
          )
        #
        list_messages("Inicia eliminación de variables irrelevantes")
        eliminadas <- colnames(data)[c(3,5,7,10,12,14,16,18,24,31,33,38,40,
                                        42,55,57,60,75,77,79,81,83,
                                        89,91,93,96,99,103,105,109,113,115,
                                        118,120,135,138,142,144,148,
                                        151,153,155,157,159,161,169,185)]
        # 
        cadena_nombres <- paste(eliminadas, collapse = ", ")
        ## Presentar el listado de columnas eliminadas 
        list_messages(cadena_nombres)
        # 
        updateProgressBar(session, "progress", value = 42)
        # step(2)
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
  
  # JavaScript personalizado para deshabilitar/habilitar el botón
  shiny::tags$script(HTML('
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
  '))
  
}
