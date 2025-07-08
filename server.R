
server <- function(input, output, session) {
  step <- reactiveVal(1)
  data_list <- reactiveValues(file1 = NULL, file2 = NULL, file3 = NULL)
  working_data <- reactiveVal(NULL)
  
  shinyjs::disable("btn_back")
  shinyjs::disable("btn_next")
  observeEvent(input$btn_back, { if (step() <= 1) shinyjs::disable("btn_back")})
  
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
        # showNotification(paste("Notification message"), duration = 0),
        br(),
        checkboxInput("remove_na_rows", "Filtrado con y sin datos faltantes.", TRUE),
        pickerInput("remove_cols", "Seleccionar columnas a eliminar:", 
                    choices = names(working_data()), 
                    multiple = TRUE, 
                    options = list(`actions-box` = TRUE))
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
    shinyjs::enable("btn_next")
  })
  
  observeEvent(input$file2, {
    req(input$file2)
    e <- new.env()
    load(input$file2$datapath, envir = e)
    data_list$file2 <- mget(ls(e), envir = e)[[1]]
    shinyjs::enable("btn_next")
  })
  
  observeEvent(input$file3, {
    req(input$file3)
    e <- new.env()
    load(input$file3$datapath, envir = e)
    data_list$file3 <- mget(ls(e), envir = e)[[1]]
  })
  
  # output$loaded_objects <- renderPrint({
  #   if (is.null(list_messages())) {
  #     cat("")
  #   } else {
  #     cat(list_messages())
  #   }
  # })

  output$data_table <- renderDT({
    req(working_data())
    datatable(head(working_data(), 5), options = list(scrollX = TRUE))
  })
  
  observeEvent(input$btn_next, {
    session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Procesando...➡️"))
    
    if (step() == 1) {
      shinyjs::enable("btn_back")
        
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
        shinyWidgets::updateProgressBar(session, "progress", value = 36, title = "Inicia carga de archivos")

        sac_data <- data_list$file1 #working_data()
        con_sac_data <- data_list$file2

        # 1. Union de las dos bases de datos,
        #     con los individuos que esten presentes en amdas bases
        data <- merged_data <- inner_join(
          sac_data, con_sac_data, by = c("CLIENTE" = "CLIENTE_ID"))
        
        session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Unión de datos de ambas DB➡️"))
        # 
        # 2. Elimina los individuos de la base de datos
        data <- subset(data,
                       !(CICLO %in% c("53", "98", "104") |
                           (CICLO == "56" & TARIFA == "373") |
                           EST_CLIENTE == "Inactivo"))
        shinyWidgets::updateProgressBar(
          session, "progress", 
          value = 39, 
          title = "Elimina clientes inactivos, ciclo (53,98,104) y tarifa (373)."
        )
        #
        # 3. Otros individuos que debo eliminar
        #
        # "Inicia eliminación de Santafe Bogota DC"
        # "Continua eliminación de servicios de Macromedición"
        # "Inicia eliminación de inactivos"
        data <- data %>%
          filter(
            DEPTO != "11",
            DES_CLASE_SERVICIO != "Macromedición",
            EST_CLIENTE == "Activo"
          )
        #
        eliminadas <- colnames(data)[c(3,5,7,10,12,14,16,18,24,31,33,38,40,
                                        42,55,57,60,75,77,79,81,83,
                                        89,91,93,96,99,103,105,109,113,115,
                                        118,120,135,138,142,144,148,
                                        151,153,155,157,159,161,169,185)]
        
        shinyWidgets::updateProgressBar(
          session, "progress", 
          value = 42, 
          title = "Elimina variables fuera de estudio."
        )
        #
        cadena_nombres <- paste(eliminadas, collapse = ", ")
        
        working_data(data)
        
        shinyWidgets::updateProgressBar(
          session, "progress", 
          value = 45, 
          title = ""
        )
        session$sendCustomMessage("enable_button", list(id = "btn_next", text = "Continuar...➡️"))
        step(2)
        
      } # if (is.null
      
    } else if (step() == 2) {
      new_data <- working_data()
  
      if (input$remove_na_rows) {
        # Define column indices in a more readable and maintainable way
        selected_columns <- c(
          1, 2, 4, 6:8, 10, 11, 22, 24, 25, 27:31, 54, 58, 62, 67, 71, 
          83, 84, 86:92, 95, 97, 99:101, 107, 110, 112:116, 126, 129, 
          152:154, 156:303
        )
        new_data <- new_data[, selected_columns]
      }
      
      # Eliminar columnas seleccionadas
      if (!is.null(input$remove_cols)) {
        new_data <- new_data[, !(names(new_data) %in% input$remove_cols)]
      }
      
      # NA replacements
      new_data$ACTIVIDAD_COMERCIAL[is.na(new_data$ACTIVIDAD_COMERCIAL)] <- "SIN DATOS"
      new_data$DES_PROPIEDAD_MEDIDOR[is.na(new_data$DES_PROPIEDAD_MEDIDOR)] <- "No definido"
      new_data$DES_LOCALIZACION[is.na(new_data$DES_LOCALIZACION)] <- "Sin Identificar"
      
      # Categoria "Sin identificar"
      new_data[, c(1:47)] <- lapply(new_data[, c(1:47)], function(x) {
        ifelse(is.na(x), "Sin Identificar", x)
      })
      
      # Transformación caracteres
      new_vars = colnames(new_data)
      new_data[new_vars] <- lapply(new_data[new_vars], function(x) iconv(x, "latin1", "UTF-8"))
      
      # Transformación de la estructura
      new_data$ANTIGUEDAD_SALDO = as.numeric(new_data$ANTIGUEDAD_SALDO)
      new_data$REL_CORRIENTE_MAX = as.numeric(new_data$REL_CORRIENTE_MAX) 
      new_data$REL_CORRIENTE_MIN = as.numeric(new_data$REL_CORRIENTE_MIN)
      new_data$CONSTANTE = as.numeric(new_data$CONSTANTE)
      
      # valores decimales estan diferenciados por comas
      m_cols <- grep("^M\\d+$", colnames(new_data), value = TRUE)
      new_data[m_cols] <- lapply(new_data[m_cols], function(x) {
        x <- gsub(",", ".", x)
        x <- as.numeric(x)
        round(x)
      })
      
      periodo_cols <- grep("^(PERIODO_M)\\d+$", colnames(new_data), value = TRUE)
      # new_data[periodo_cols] <- new_data[periodo_cols] %>% 
      #   mutate(across(all_of(periodo_cols), ~ as.POSIXct(paste0(as.character(.), "01"), format = "%Y%m%d")))
      new_data[periodo_cols] <- new_data[periodo_cols] %>% 
        mutate(across(all_of(periodo_cols), ~ ymd(paste0(as.character(.), "01"))))
      
      new_data$DES_MODELO <- as.factor(ifelse(as.numeric(as.character(new_data$DES_MODELO)) < 100, 
                                           paste0("19", as.character(new_data$DES_MODELO)), 
                                           as.character(new_data$DES_MODELO)))
      
      table(new_data$DES_MODELO)
      new_data$DES_MODELO_agrup = cut(as.numeric(as.character(new_data$DES_MODELO)),
                                   breaks = seq(1950,2030,by = 10),
                                   labels = paste(seq(1950,2020,by = 10),seq(1959,2029,by = 10),sep = "-"),
                                   include.lowest = TRUE,
                                   right = FALSE)

      processed_data <- new_data[, c(1:25,196,26:195)]
      
      # Nuevas variables
      processed_data$AGRUPA_ESTRATO <- ifelse(processed_data$ESTRATO %in% c(1, 2), "1 y 2",
                                     ifelse(processed_data$ESTRATO %in% c(3, 4), "3 y 4",
                                            ifelse(processed_data$ESTRATO %in% c(5, 6), "5 y 6", "0")))
      
      processed_data$AGRUPA_DES_CLASE_SERVICIO <- ifelse(processed_data$DES_CLASE_SERVICIO == "Residencial", "Residencial", "No Residencial")
      
      processed_data$AGRUPA_DESC_TARIFA <- ifelse(processed_data$DESC_TARIFA == "PREPAGO", "PREPAGO", "POSPAGO")
      processed_data$AGRUPA_ESTRATO = as.factor(processed_data$AGRUPA_ESTRATO)
      processed_data$AGRUPA_DES_CLASE_SERVICIO = as.factor(processed_data$AGRUPA_DES_CLASE_SERVICIO)
      processed_data$AGRUPA_DESC_TARIFA = as.factor(processed_data$AGRUPA_DESC_TARIFA)
      
      # Ajustando orden
      processed_data <- processed_data[, c(1:12,199,13:198)] #no se corre de nuevo porque se daña el orden
      processed_data <- processed_data[, c(1:15,199,16:198)] #no se corre de nuevo porque se daña el orden
      processed_data <- processed_data[, c(1:17,199,18:198)] #no se corre de nuevo porque se daña el orden

      # Consumos donde no hay periodos
      periodo_col <- paste0("PERIODO_M", seq_len(72))
      consumo_col <- paste0("M", seq_len(72))
      
      processed_data <- reduce(seq_along(periodo_col), function(df, i) {
        df %>% mutate(
          !!consumo_col[i] := if_else(
            is.na(.data[[periodo_col[i]]]) & .data[[consumo_col[i]]] == 0,
            NA_real_,
            .data[[consumo_col[i]]]
          )
        )
      }, .init = processed_data)
      
      var_names = c("REGIONAL", "AGRUPA_DES_CLASE_SERVICIO", "AGRUPA_ESTRATO", "AGRUPA_DESC_TARIFA", "PERIODICIDAD")
      missing_vars = var_names[!var_names %in% names(processed_data)]
      if (length(missing_vars) > 0) {
        stop("Las siguientes variables no están presentes en el dataframe: ", paste(missing_vars, collapse=", "))
      }
      
      processed_data$BD <- do.call(interaction, c(processed_data[var_names], drop = TRUE))
      # Tabla de frecuencias
      categorias_freq <- table(processed_data$BD)

      # Función auxiliar para filtrar categorías por periodicidad y frecuencia
      filtrar_categorias <- function(patron, umbral=50) {
        names(categorias_freq)[grepl(patron, names(categorias_freq)) & categorias_freq < umbral]
      }
      
      # Aplicar la función para cada tipo de periodicidad
      categorias_modificar  <- filtrar_categorias("\\.MENSUAL$")
      categorias_modificar1 <- filtrar_categorias("\\.BIMENSUAL$")
      categorias_modificar2 <- filtrar_categorias("\\.TRIMESUAL$")
      
      Base2$BD = as.character(processed_data)
      Base2$BD[Base2$BD %in% categorias_modificar] <- "agrupacionmensual"
      Base2$BD[Base2$BD %in% categorias_modificar1] <- "agrupacionbimensual"
      Base2$BD[Base2$BD %in% categorias_modificar2] <- "agrupaciontrimesual"
      Base2$BD = as.factor(Base2$BD)
      
      working_data(Base2)
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
