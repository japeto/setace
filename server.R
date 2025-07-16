
server <- function(input, output, session) {
  step <- reactiveVal(1)
  data_list <- reactiveValues(
    file1 = NULL, file2 = NULL, file3 = NULL,
    file4 = NULL, file5 = NULL, file6 = NULL
  )
  working_data <- reactiveVal(NULL)
  
  datasets <- reactiveValues(
    mensual = NULL,
    bimestral = NULL,          
    bimestral_ajustada = NULL,
    trimestral = NULL,
    trimestral_ajustada = NULL 
  )
  
  shinyjs::disable("btn_back")
  # shinyjs::disable("btn_next")
  observeEvent(input$btn_back, { if (step() <= 1) shinyjs::disable("btn_back")})
  
  output$dynamic_ui <- renderUI({
    if (step() == 1) {
      tagList(
        h4("Paso 0: Cargar archivos RData"),
        br(),
        fileInput("file1", "Datos de consumos (Consumos)", accept = ".RData"),
        fileInput("file2", "Datos de administración de Consumos (SAC)", accept = ".RData"),
        verbatimTextOutput("load_status"),
        verbatimTextOutput("loaded_objects"),
      )
    } else if (step() == 2) {
      req(working_data())
      tagList(
        h4("Paso 1: Vista previa y limpieza de datos"),
        DTOutput("data_table"),
        # showNotification(paste("Notification message"), duration = 0),
        br(),
        checkboxInput("remove_na_rows", "Filtrado con y sin datos faltantes.", FALSE),
        pickerInput("remove_cols", "Seleccionar columnas a eliminar:", 
                    choices = names(working_data()), 
                    multiple = TRUE, 
                    options = list(`actions-box` = TRUE))
      )
      
    } else if (step() == 3) {
      req(working_data())
      tagList(
        h4("Paso 2: Preprocesamiento de Datos"),
        p("La base de datos ha sido ", strong("filtrada y etiquetada"), "."),
        div(
          style = "margin-top: 10px;",
          downloadButton("download_clean", "Descargar Base de Datos Limpia (.xlsx)", class = "btn-info")
        )
      )
    } else if (step() == 4) {
      req(working_data())
      req(datasets$mensual)
      req(datasets$bimestral)
      req(datasets$trimestral)
      tagList(
        h2("Paso 3: Descargar datos por periodicidad"),
        p("Seleccione la periodicidad con la que desea descargar los datos procesados:"),
        div(
          style = "margin-top: 10px;",
          # fileInput("file3", "Datos Mensual ajustada", accept = ".xlsx", ),
          fileInput("file4", "Datos Bimensual ajustada", accept = ".xlsx"),
          fileInput("file5", "Datos Trimestral ajustada", accept = ".xlsx"),
        ),
        div(
          style = "margin-top: 10px;",
          downloadButton("download_mensual", "Descargar Mensual (.xlsx)", class = "btn-primary"),
          downloadButton("download_bimensual", "Descargar Bimensual (.xlsx)", class = "btn-success"),
          downloadButton("download_trimestral", "Descargar Trimestral (.xlsx)", class = "btn-info")
        )
      )
    } else if (step() == 5) {
      req(working_data())
      tagList(
        h2("Paso 4: Revisiones"),
        p("Continue con la lectura y procesamiento de las revisiones"),
        div(
          style = "margin-top: 10px;",
          fileInput("file6", "Datos Revisiones (Encriptada)", accept = ".xlsx"),
        )
      )
    }
  })
  
  observeEvent(input$file1, {
    req(input$file1)
    e <- new.env()
    load(input$file1$datapath, envir = e)
    data_list$file1 <- mget(ls(e), envir = e)[[1]]
    working_data(data_list$file1)
    # shinyjs::enable("btn_next")
  })
  
  observeEvent(input$file2, {
    req(input$file2)
    e <- new.env()
    load(input$file2$datapath, envir = e)
    data_list$file2 <- mget(ls(e), envir = e)[[1]]
    # shinyjs::enable("btn_next")
  })
  
  observeEvent(input$file3, {
    req(input$file3)
    e <- new.env()
    load(input$file3$datapath, envir = e)
    data_list$file3 <- mget(ls(e), envir = e)[[1]]
  })
  
  observeEvent(input$file4, {
    req(input$file4)
    e <- new.env()
    load(input$file4$datapath, envir = e)
    datasets$bimestral_ajustada <- mget(ls(e), envir = e)[[1]]
  })
  
  observeEvent(input$file5, {
    req(input$file5)
    e <- new.env()
    load(input$file5$datapath, envir = e)
    datasets$trimestral_ajustada <- mget(ls(e), envir = e)[[1]]
  })
  
  observeEvent(input$file6, {
    req(input$file6)
    e <- new.env()
    load(input$file5$datapath, envir = e)
    data_list$file6 <- mget(ls(e), envir = e)[[1]]
  })

  output$data_table <- renderDT({
    req(working_data())
    datatable(head(working_data(), 5), options = list(scrollX = TRUE))
  })
  
  observeEvent(input$btn_next, {
    session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Procesando...➡️"))
    
    if (step() == 1) {
      shinyjs::enable("btn_back")
        
      # if ( FALSE ) { 
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

        # e <- new.env()
        # load("/Users/japeto/WorkS/pacheco/asistente06072025/data/BD Consumos.RData", envir = e)
        # data_list$file1 <- mget(ls(e), envir = e)[[1]]
        # working_data(data_list$file1)
        # 
        # e <- new.env()
        # load("/Users/japeto/WorkS/pacheco/asistente06072025/data/BD SAC F.RData", envir = e)
        # data_list$file2 <- mget(ls(e), envir = e)[[1]]
        
        sac_data <- data_list$file1 
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
            DES_CLASE_SERVICIO != "Macromedici\xf3n",
            EST_CLIENTE == "Activo"
          )
        # "M1, M2, M3, PERIODO_M5, PERIODO_M6, PERIODO_M7, PERIODO_M8, 
        # PERIODO_M9, PERIODO_M12, M15, M16, PERIODO_M19, PERIODO_M20, 
        # PERIODO_M21, M27, M28, PERIODO_M30, M37, M38, M39, M40, M41, M44, 
        # M45, M46, PERIODO_M48, M49, M51, M52, M54, M56, M57, PERIODO_M59, 
        # PERIODO_M60, M67, PERIODO_M69, PERIODO_M71, PERIODO_M72, EST_CLIENTE, 
        # ESTADO_FACTURACION, DIRECCION, DES_AREA, DES_DEPTO, DES_ZONA,
        # DES_MUNICIPIO, DES_CEN_POBLADO, DES_GRUPO_CU
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
      
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Procesando columnas➡️"))
      if (input$remove_na_rows) {
        # Define column indices in a more readable and maintainable way
        # selected_columns <- c(
        #   "CLIENTE", "PERIODO_M1", "PERIODO_M2", "PERIODO_M3", "M3", "PERIODO_M4", "PERIODO_M5",
        #   "M5", "PERIODO_M11", "PERIODO_M12", "M12", "M13", "PERIODO_M14", "M14", "PERIODO_M15",
        #   "M15", "PERIODO_M27", "PERIODO_M29", "PERIODO_M31", "M33", "M35", "M41", "PERIODO_M42",
        #   "PERIODO_M43", "M43", "PERIODO_M44", "M44", "PERIODO_M45", "M45", "PERIODO_M46", "M47",
        #   "M48", "M49", "PERIODO_M50", "M50", "M53", "PERIODO_M55", "PERIODO_M56", "M56",
        #   "PERIODO_M57", "M57", "PERIODO_M58", "PERIODO_M63", "M64", "EST_FACTURACION",
        #   "DIRECCION", "CODIGO_AREA", "DEPTO", "DES_DEPTO", "ZON", "DES_ZONA", "MUNICIPIO",
        #   "DES_MUNICIPIO", "BARRIO", "DES_BARRIO", "TELEFONO", "TELEFONO_CELULAR",
        #   "TELEFONO_CONTACTO", "FICHA_CATASTRAL", "CENTRO_POBLADO", "DES_CEN_POBLADO",
        #   "NIT", "APELLIDO1", "APELLIDO2", "NOMBRE1", "NOMBRE2", "TIPO_PERSONA", "DESC_TIPO_PER",
        #   "CODIGO_ENTIDAD", "DES_ENTIDAD", "CICLO", "PERIODICIDAD", "RUTA_LECTURA", "TARIFA",
        #   "DESC_TARIFA", "GRUPO_CU", "DES_GRUPO_CU", "CLASE_SERVICIO", "DES_CLASE_SERVICIO",
        #   "ESTRATO", "MEDIDA_TENSION", "NRO_MEDIDORES", "FACTOR_UTILIZACION", "CARGA_CONTRATADA",
        #   "CARGA_INSTALADA", "CARGA_ADICIONAL", "FACTOR_ADICIONAL", "NUMERO_DIAS",
        #   "EXENTO_INTERESES", "IND_SALDO", "IND_CRITICA", "DESC_IND_CRITICA", "IND_FACTURACION",
        #   "DES_IND_FACTURACION", "CANTIDAD_CASAS", "TIPO_BLOQUEO", "DES_TIPO_BLOQUEO",
        #   "CONSECUTIVO", "CORR_CREDITOS", "CORR_PAGOS", "FECHA_CREACION", "FECHA_ACCION", "FECHA_LECTURA_ANT",
        #   "FECHA_LECTURA_ACT", "FECHA_FACTURA_ANT", "FECHA_FACTURA_ACT", "ANTIGUEDAD_SALDO",
        #   "SALDO_ACTUAL", "INTERESES_MES", "INTERESES_ACUMULADOS", "CODIGO_SIE", "ACTIVIDAD_COMERCIAL",
        #   "CODIGO_CIU", "DES_CODIGO_CIU", "CODIGO_SIC", "DES_CODIGO_SIC", "NRO_FORMULARIO", "CODIGO_FORMULARIO",
        #   "PUNTO_SUSPENSION", "DES_PUNTO_SUSPENSION", "MATRICULA_INMOBILIARIA", "GPS_LATITUD", "GPS_LONGITUD",
        #   "CLIENTE_ID_REF", "CALIFICACION", "DES_CALIFICA", "DOCUMENTO_TIPO", "DES_TIPO_DOC", "TIPO_RECIBO",
        #   "DES_TIPO_RECIBO", "AUTORIZA_DATOS", "SEGMENTO_MERCADO", "DES_SEGMENTO_MERCADO", "NUMERO_DOCUMENTO",
        #   "TIPO_FACTURACION", "DES_TIPO_FACTURA", "RUTA_REPARTO", "PUNTAJE_SISBEN", "ESTRATO_CRITERIO",
        #   "DES_ESTRATO_CRITERIO", "TIPO_INFO_PREDIAL", "DES_TIPO_INFO_PREDIAL", "GPS_ALTITUD", "CONDICION_ESPECIAL",
        #   "PERSONA_ID", "DES_PERSONA", "MENSAJERIA_INSTANTANEA", "MEDIDOR_ID", "TIPO_MEDIDOR", "DES_TIPO_MEDIDOR", "TIPO_CONEXION",
        #   "DES_TIPO_CONEXION", "NUMERO_MEDIDOR", "MARCA_MEDIDOR", "DES_MARCA_MEDIDOR", "MODELO", "DES_MODELO", "ENTEROS", "DECIMALES",
        #   "FACTOR_MULTIPLICACION", "REL_CORRIENTE_MIN", "REL_CORRIENTE_MAX", "NRO_FASES", "NRO_HILOS", "TENSION_NOMINAL",
        #   "FRECUENCIA_NOMINAL", "CONSTANTE", "UNIDAD_CONSTANTE", "CLASE_EXACTITUD", "CLASE_EXACTITUD_REACTIVA", "PROPIEDAD_MEDIDOR",
        #   "DES_PROPIEDAD_MEDIDOR", "DEPENDENCIA", "FUNCIONAMIENTO", "DES_FUNCIONAMIENTO", "FECHA_PRI_INSTALA", "FECHA_ULT_INSTALA",
        #   "FORMA_COBRO", "DES_FORMA_COBRO", "MODELO_MEDIDOR", "DES_MODELO_MEDIDOR", "GPS_LATITUD_MED", "GPS_LONGITUD_MED", "LOCALIZACION",
        #   "DES_LOCALIZACION", "RESPONSABLE_ASIGNADO", "FABRICANTE", "DES_FABRICANTE", "TECNOLOGIA_MEDIDOR", "DES_TECNOLOGIA_MEDIDOR",
        #   "SENTIDO_MEDICION", "DES_SENTIDO_MEDICION", "TIPO_REGISTRADOR", "DES_REGISTRADOR", "COMPONENTES"
        # )
        
        selected_columns <- c(
          1, 2, 4, 6:8, 10, 11, 22, 24, 25, 27:31, 54, 58, 62, 67, 71,
          83, 84, 86:92, 95, 97, 99:101, 107, 108, 110, 112:116, 126, 129,
          152:154, 156:303, 347
        )
        new_data <- new_data[, selected_columns]
      }
      # Eliminar columnas seleccionadas
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Eliminando columnas seleccionadas ➡️"))
      if (!is.null(input$remove_cols)) {
        new_data <- new_data[, !(names(new_data) %in% input$remove_cols)]
      }
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Etiquetado de NAs➡️"))
      # NA replacements
      new_data$ACTIVIDAD_COMERCIAL[is.na(new_data$ACTIVIDAD_COMERCIAL)] <- "SIN DATOS"
      new_data$DES_PROPIEDAD_MEDIDOR[is.na(new_data$DES_PROPIEDAD_MEDIDOR)] <- "No definido"
      new_data$DES_LOCALIZACION[is.na(new_data$DES_LOCALIZACION)] <- "Sin Identificar"
      
      # Categoria "Sin identificar"
      # CLIENTE, PERIODO_M1, M1, PERIODO_M2, M2, PERIODO_M3, M3, PERIODO_M4, M4, 
      # PERIODO_M5, M5, PERIODO_M6, M6, PERIODO_M7, M7, PERIODO_M8, M8, PERIODO_M9, 
      # M9, PERIODO_M10, M10, PERIODO_M11, M11, PERIODO_M12, M12, PERIODO_M13, M13, 
      # PERIODO_M14, M14, PERIODO_M15, M15, PERIODO_M16, M16, PERIODO_M17, M17, 
      # PERIODO_M18, M18, PERIODO_M19, M19, PERIODO_M20, M20, PERIODO_M21, M21, 
      # PERIODO_M22, M22, PERIODO_M23, M23
      new_data[, c(1:47)] <- lapply(new_data[, c(1:47)], function(x) {
        ifelse(is.na(x), "Sin Identificar", x)
      })
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Cambio tipado I ➡️"))
      # Transformación caracteres
      new_vars = colnames(new_data)
      new_data[new_vars] <- lapply(new_data[new_vars], function(x) iconv(x, "latin1", "UTF-8"))
      
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Cambio tipado II ➡️"))
      # Transformación de la estructura
      new_data$ANTIGUEDAD_SALDO = as.numeric(new_data$ANTIGUEDAD_SALDO)
      new_data$REL_CORRIENTE_MAX = as.numeric(new_data$REL_CORRIENTE_MAX) 
      new_data$REL_CORRIENTE_MIN = as.numeric(new_data$REL_CORRIENTE_MIN)
      new_data$CONSTANTE = as.numeric(new_data$CONSTANTE)
      
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Cambio separadores ➡️"))
      # valores decimales estan diferenciados por comas
      m_cols <- grep("^M\\d+$", colnames(new_data), value = TRUE)
      new_data[m_cols] <- lapply(new_data[m_cols], function(x) {
        x <- gsub(",", ".", x)
        x <- as.numeric(x)
        round(x)
      })
      
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Cambio de tipado III ➡️"))
      
      periodo_cols <- grep("^(PERIODO_M)\\d+$", colnames(new_data), value = TRUE)
      # new_data[periodo_cols] <- new_data[periodo_cols] %>% 
      #   mutate(across(all_of(periodo_cols), ~ as.POSIXct(paste0(as.character(.), "01"), format = "%Y%m%d")))
      new_data[periodo_cols] <- new_data[periodo_cols] %>% 
        mutate(across(all_of(periodo_cols), ~ ifelse( is.na(.), NA, ymd(paste0(as.character(.), "01")))))
      
      new_data$DES_MODELO <- as.factor(ifelse(as.numeric(as.character(new_data$DES_MODELO)) < 100, 
                                           paste0("19", as.character(new_data$DES_MODELO)), 
                                           as.character(new_data$DES_MODELO)))
      
      # table(new_data$DES_MODELO)
      new_data$DES_MODELO_agrup = cut(as.numeric(as.character(new_data$DES_MODELO)),
                                   breaks = seq(1950,2030,by = 10),
                                   labels = paste(seq(1950,2020,by = 10),seq(1959,2029,by = 10),sep = "-"),
                                   include.lowest = TRUE,
                                   right = FALSE)

      # CLIENTE, PERIODO_M1, M1, PERIODO_M2, M2, PERIODO_M3, M3, PERIODO_M4, M4,
      # PERIODO_M5, M5, PERIODO_M6, M6, PERIODO_M7, M7, PERIODO_M8, M8, PERIODO_M9, 
      # M9, PERIODO_M10, M10, PERIODO_M11, M11, PERIODO_M12, M12, NUMERO_DIAS, 
      # PERIODO_M13, M13, PERIODO_M14, M14, PERIODO_M15, M15, PERIODO_M16, M16, 
      # PERIODO_M17, M17, PERIODO_M18, M18, PERIODO_M19, M19, PERIODO_M20, M20, 
      # PERIODO_M21, M21, PERIODO_M22, M22, PERIODO_M23, M23, PERIODO_M24, M24, 
      # PERIODO_M25, M25, PERIODO_M26, M26, PERIODO_M27, M27, PERIODO_M28, M28, 
      # PERIODO_M29, M29, PERIODO_M30, M30, PERIODO_M31, M31, PERIODO_M32, M32, PERIODO_M33, M33, 
      # PERIODO_M34, M34, PERIODO_M35, M35, PERIODO_M36, M36, PERIODO_M37, M37, PERIODO_M38, M38, PERIODO_M39, M39, 
      # PERIODO_M40, M40, PERIODO_M41, M41, PERIODO_M42, M42, PERIODO_M43, M43, PERIODO_M44, M44, PERIODO_M45, 
      # M45, PERIODO_M46, M46, PERIODO_M47, M47, PERIODO_M48, M48, PERIODO_M49, M49, PERIODO_M50, M50, PERIODO_M51, 
      # M51, PERIODO_M52, M52, PERIODO_M53, M53, PERIODO_M54, M54, PERIODO_M55, M55, PERIODO_M56, M56, PERIODO_M57, 
      # M57, PERIODO_M58, M58, PERIODO_M59, M59, PERIODO_M60, M60, PERIODO_M61, M61, PERIODO_M62, M62, PERIODO_M63, 
      # M63, PERIODO_M64, M64, PERIODO_M65, M65, PERIODO_M66, M66, PERIODO_M67, M67, PERIODO_M68, M68, PERIODO_M69, M69, 
      # PERIODO_M70, M70, PERIODO_M71, M71, PERIODO_M72, M72, NOMBRE, ESTADO_CLIENTE, REGIONAL,
      # EST_CLIENTE, ESTADO_SUMINISTRO, EST_SUMINISTRO, ESTADO_FACTURACION, EST_FACTURACION, DIRECCION, 
      # CODIGO_AREA, DES_AREA, DEPTO, DES_DEPTO, ZON, DES_ZONA, MUNICIPIO, DES_MUNICIPIO, BARRIO, DES_BARRIO, 
      # TELEFONO, TELEFONO_CELULAR, TELEFONO_CONTACTO, FICHA_CATASTRAL, CENTRO_POBLADO, DES_CEN_POBLADO, NIT, 
      # APELLIDO1, APELLIDO2, NOMBRE1, NOMBRE2, TIPO_PERSONA, DESC_TIPO_PER, CODIGO_ENTIDAD, DES_ENTIDAD, CICLO, 
      # PERIODICIDAD, RUTA_LECTURA, TARIFA, DESC_TARIFA, GRUPO_CU, DES_GRUPO_CU, CLASE_SERVICIO, DES_CLASE_SERVICIO, 
      # ESTRATO, MEDIDA_TENSION, NRO_MEDIDORES, FACTOR_UTILIZACION, CARGA_CONTRATADA, CARGA_INSTALADA, CARGA_ADICIONAL, 
      # FACTOR_ADICIONAL
      # processed_data <- new_data[, c(1:25,196,26:195)]
      column_names <- c(
        "CLIENTE", "PERIODO_M1", "M1", "PERIODO_M2", "M2", "PERIODO_M3", "M3", "PERIODO_M4", "M4",
        "PERIODO_M5", "M5", "PERIODO_M6", "M6", "PERIODO_M7", "M7", "PERIODO_M8", "M8", "PERIODO_M9",
        "M9", "PERIODO_M10", "M10", "PERIODO_M11", "M11", "PERIODO_M12", "M12", "NUMERO_DIAS",
        "PERIODO_M13", "M13", "PERIODO_M14", "M14", "PERIODO_M15", "M15", "PERIODO_M16", "M16",
        "PERIODO_M17", "M17", "PERIODO_M18", "M18", "PERIODO_M19", "M19", "PERIODO_M20", "M20",
        "PERIODO_M21", "M21", "PERIODO_M22", "M22", "PERIODO_M23", "M23", "PERIODO_M24", "M24",
        "PERIODO_M25", "M25", "PERIODO_M26", "M26", "PERIODO_M27", "M27", "PERIODO_M28", "M28",
        "PERIODO_M29", "M29", "PERIODO_M30", "M30", "PERIODO_M31", "M31", "PERIODO_M32", "M32", "PERIODO_M33", "M33",
        "PERIODO_M34", "M34", "PERIODO_M35", "M35", "PERIODO_M36", "M36", "PERIODO_M37", "M37", "PERIODO_M38", "M38", "PERIODO_M39", "M39",
        "PERIODO_M40", "M40", "PERIODO_M41", "M41", "PERIODO_M42", "M42", "PERIODO_M43", "M43", "PERIODO_M44", "M44", "PERIODO_M45",
        "M45", "PERIODO_M46", "M46", "PERIODO_M47", "M47", "PERIODO_M48", "M48", "PERIODO_M49", "M49", "PERIODO_M50", "M50", "PERIODO_M51",
        "M51", "PERIODO_M52", "M52", "PERIODO_M53", "M53", "PERIODO_M54", "M54", "PERIODO_M55", "M55", "PERIODO_M56", "M56", "PERIODO_M57",
        "M57", "PERIODO_M58", "M58", "PERIODO_M59", "M59", "PERIODO_M60", "M60", "PERIODO_M61", "M61", "PERIODO_M62", "M62", "PERIODO_M63",
        "M63", "PERIODO_M64", "M64", "PERIODO_M65", "M65", "PERIODO_M66", "M66", "PERIODO_M67", "M67", "PERIODO_M68", "M68", "PERIODO_M69", "M69",
        "PERIODO_M70", "M70", "PERIODO_M71", "M71", "PERIODO_M72", "M72", "NOMBRE", "ESTADO_CLIENTE", "REGIONAL",
        "EST_CLIENTE", "ESTADO_SUMINISTRO", "EST_SUMINISTRO", "ESTADO_FACTURACION", "EST_FACTURACION", "DIRECCION",
        "CODIGO_AREA", "DES_AREA", "DEPTO", "DES_DEPTO", "ZON", "DES_ZONA", "MUNICIPIO", "DES_MUNICIPIO", "BARRIO", "DES_BARRIO",
        "TELEFONO", "TELEFONO_CELULAR", "TELEFONO_CONTACTO", "FICHA_CATASTRAL", "CENTRO_POBLADO", "DES_CEN_POBLADO", "NIT",
        "APELLIDO1", "APELLIDO2", "NOMBRE1", "NOMBRE2", "TIPO_PERSONA", "DESC_TIPO_PER", "CODIGO_ENTIDAD", "DES_ENTIDAD", "CICLO",
        "PERIODICIDAD", "RUTA_LECTURA", "TARIFA", "DESC_TARIFA", "GRUPO_CU", "DES_GRUPO_CU", "CLASE_SERVICIO", "DES_CLASE_SERVICIO",
        "ESTRATO", "MEDIDA_TENSION", "NRO_MEDIDORES", "FACTOR_UTILIZACION", "CARGA_CONTRATADA", "CARGA_INSTALADA", "CARGA_ADICIONAL",
        "FACTOR_ADICIONAL"
      )

      processed_data <- new_data[, column_names]

      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Ajustando orden ➡️"))
      # # Ajustando orden
      # # CLIENTE, PERIODO_M1, M1, PERIODO_M2, M2, PERIODO_M3, M3, PERIODO_M4, M4, PERIODO_M5, M5, PERIODO_M6, AGRUPA_DES_CLASE_SERVICIO, M6, PERIODO_M7, M7, PERIODO_M8, M8, PERIODO_M9, M9, PERIODO_M10, M10, PERIODO_M11, M11, PERIODO_M12, M12, NUMERO_DIAS, PERIODO_M13, M13, PERIODO_M14, M14, PERIODO_M15, M15, PERIODO_M16, M16, PERIODO_M17, M17, PERIODO_M18, M18, PERIODO_M19, M19, PERIODO_M20, M20, PERIODO_M21, M21, PERIODO_M22, M22, PERIODO_M23, M23, PERIODO_M24, M24, PERIODO_M25, M25, PERIODO_M26, M26, PERIODO_M27, M27, PERIODO_M28, M28, PERIODO_M29, M29, PERIODO_M30, M30, PERIODO_M31, M31, PERIODO_M32, M32, PERIODO_M33, M33, PERIODO_M34, M34, PERIODO_M35, M35, PERIODO_M36, M36, PERIODO_M37, M37, PERIODO_M38, M38, PERIODO_M39, M39, PERIODO_M40, M40, PERIODO_M41, M41, PERIODO_M42, M42, PERIODO_M43, M43, PERIODO_M44, M44, PERIODO_M45, M45, PERIODO_M46, M46, PERIODO_M47, M47, PERIODO_M48, M48, PERIODO_M49, M49, PERIODO_M50, M50, PERIODO_M51, M51, PERIODO_M52, M52, PERIODO_M53, M53, PERIODO_M54, M54, PERIODO_M55, M55, PERIODO_M56, M56, PERIODO_M57, M57, PERIODO_M58, M58, PERIODO_M59, M59, PERIODO_M60, M60, PERIODO_M61, M61, PERIODO_M62, M62, PERIODO_M63, M63, PERIODO_M64, M64, PERIODO_M65, M65, PERIODO_M66, M66, PERIODO_M67, M67, PERIODO_M68, M68, PERIODO_M69, M69, PERIODO_M70, M70, PERIODO_M71, M71, PERIODO_M72, M72, NOMBRE, ESTADO_CLIENTE, REGIONAL, EST_CLIENTE, ESTADO_SUMINISTRO, EST_SUMINISTRO, ESTADO_FACTURACION, EST_FACTURACION, DIRECCION, CODIGO_AREA, DES_AREA, DEPTO, DES_DEPTO, ZON, DES_ZONA, MUNICIPIO, DES_MUNICIPIO, BARRIO, DES_BARRIO, TELEFONO, TELEFONO_CELULAR, TELEFONO_CONTACTO, FICHA_CATASTRAL, CENTRO_POBLADO, DES_CEN_POBLADO, NIT, APELLIDO1, APELLIDO2, NOMBRE1, NOMBRE2, TIPO_PERSONA, DESC_TIPO_PER, CODIGO_ENTIDAD, DES_ENTIDAD, CICLO, PERIODICIDAD, RUTA_LECTURA, TARIFA, DESC_TARIFA, GRUPO_CU, DES_GRUPO_CU, CLASE_SERVICIO, DES_CLASE_SERVICIO, ESTRATO, MEDIDA_TENSION, NRO_MEDIDORES, FACTOR_UTILIZACION, CARGA_CONTRATADA, CARGA_INSTALADA, CARGA_ADICIONAL, FACTOR_ADICIONAL, AGRUPA_ESTRATO
      # processed_data <- processed_data[, c(1:12,199,13:198)] #no se corre de nuevo porque se daña el orden
      # # CLIENTE, PERIODO_M1, M1, PERIODO_M2, M2, PERIODO_M3, M3, PERIODO_M4, M4, PERIODO_M5, M5, PERIODO_M6, M6, PERIODO_M7, M7, AGRUPA_DES_CLASE_SERVICIO, PERIODO_M8, M8, PERIODO_M9, M9, PERIODO_M10, M10, PERIODO_M11, M11, PERIODO_M12, M12, NUMERO_DIAS, PERIODO_M13, M13, PERIODO_M14, M14, PERIODO_M15, M15, PERIODO_M16, M16, PERIODO_M17, M17, PERIODO_M18, M18, PERIODO_M19, M19, PERIODO_M20, M20, PERIODO_M21, M21, PERIODO_M22, M22, PERIODO_M23, M23, PERIODO_M24, M24, PERIODO_M25, M25, PERIODO_M26, M26, PERIODO_M27, M27, PERIODO_M28, M28, PERIODO_M29, M29, PERIODO_M30, M30, PERIODO_M31, M31, PERIODO_M32, M32, PERIODO_M33, M33, PERIODO_M34, M34, PERIODO_M35, M35, PERIODO_M36, M36, PERIODO_M37, M37, PERIODO_M38, M38, PERIODO_M39, M39, PERIODO_M40, M40, PERIODO_M41, M41, PERIODO_M42, M42, PERIODO_M43, M43, PERIODO_M44, M44, PERIODO_M45, M45, PERIODO_M46, M46, PERIODO_M47, M47, PERIODO_M48, M48, PERIODO_M49, M49, PERIODO_M50, M50, PERIODO_M51, M51, PERIODO_M52, M52, PERIODO_M53, M53, PERIODO_M54, M54, PERIODO_M55, M55, PERIODO_M56, M56, PERIODO_M57, M57, PERIODO_M58, M58, PERIODO_M59, M59, PERIODO_M60, M60, PERIODO_M61, M61, PERIODO_M62, M62, PERIODO_M63, M63, PERIODO_M64, M64, PERIODO_M65, M65, PERIODO_M66, M66, PERIODO_M67, M67, PERIODO_M68, M68, PERIODO_M69, M69, PERIODO_M70, M70, PERIODO_M71, M71, PERIODO_M72, M72, NOMBRE, ESTADO_CLIENTE, REGIONAL, EST_CLIENTE, ESTADO_SUMINISTRO, EST_SUMINISTRO, ESTADO_FACTURACION, EST_FACTURACION, DIRECCION, CODIGO_AREA, DES_AREA, DEPTO, DES_DEPTO, ZON, DES_ZONA, MUNICIPIO, DES_MUNICIPIO, BARRIO, DES_BARRIO, TELEFONO, TELEFONO_CELULAR, TELEFONO_CONTACTO, FICHA_CATASTRAL, CENTRO_POBLADO, DES_CEN_POBLADO, NIT, APELLIDO1, APELLIDO2, NOMBRE1, NOMBRE2, TIPO_PERSONA, DESC_TIPO_PER, CODIGO_ENTIDAD, DES_ENTIDAD, CICLO, PERIODICIDAD, RUTA_LECTURA, TARIFA, DESC_TARIFA, GRUPO_CU, DES_GRUPO_CU, CLASE_SERVICIO, DES_CLASE_SERVICIO, ESTRATO, MEDIDA_TENSION, NRO_MEDIDORES, FACTOR_UTILIZACION, CARGA_CONTRATADA, CARGA_INSTALADA, CARGA_ADICIONAL, FACTOR_ADICIONAL, AGRUPA_ESTRATO
      # processed_data <- processed_data[, c(1:15,199,16:198)] #no se corre de nuevo porque se daña el orden
      # # CLIENTE, PERIODO_M1, M1, PERIODO_M2, M2, PERIODO_M3, M3, PERIODO_M4, M4, PERIODO_M5, M5, PERIODO_M6, M6, PERIODO_M7, M7, PERIODO_M8, M8, AGRUPA_DES_CLASE_SERVICIO, PERIODO_M9, M9, PERIODO_M10, M10, PERIODO_M11, M11, PERIODO_M12, M12, NUMERO_DIAS, PERIODO_M13, M13, PERIODO_M14, M14, PERIODO_M15, M15, PERIODO_M16, M16, PERIODO_M17, M17, PERIODO_M18, M18, PERIODO_M19, M19, PERIODO_M20, M20, PERIODO_M21, M21, PERIODO_M22, M22, PERIODO_M23, M23, PERIODO_M24, M24, PERIODO_M25, M25, PERIODO_M26, M26, PERIODO_M27, M27, PERIODO_M28, M28, PERIODO_M29, M29, PERIODO_M30, M30, PERIODO_M31, M31, PERIODO_M32, M32, PERIODO_M33, M33, PERIODO_M34, M34, PERIODO_M35, M35, PERIODO_M36, M36, PERIODO_M37, M37, PERIODO_M38, M38, PERIODO_M39, M39, PERIODO_M40, M40, PERIODO_M41, M41, PERIODO_M42, M42, PERIODO_M43, M43, PERIODO_M44, M44, PERIODO_M45, M45, PERIODO_M46, M46, PERIODO_M47, M47, PERIODO_M48, M48, PERIODO_M49, M49, PERIODO_M50, M50, PERIODO_M51, M51, PERIODO_M52, M52, PERIODO_M53, M53, PERIODO_M54, M54, PERIODO_M55, M55, PERIODO_M56, M56, PERIODO_M57, M57, PERIODO_M58, M58, PERIODO_M59, M59, PERIODO_M60, M60, PERIODO_M61, M61, PERIODO_M62, M62, PERIODO_M63, M63, PERIODO_M64, M64, PERIODO_M65, M65, PERIODO_M66, M66, PERIODO_M67, M67, PERIODO_M68, M68, PERIODO_M69, M69, PERIODO_M70, M70, PERIODO_M71, M71, PERIODO_M72, M72, NOMBRE, ESTADO_CLIENTE, REGIONAL, EST_CLIENTE, ESTADO_SUMINISTRO, EST_SUMINISTRO, ESTADO_FACTURACION, EST_FACTURACION, DIRECCION, CODIGO_AREA, DES_AREA, DEPTO, DES_DEPTO, ZON, DES_ZONA, MUNICIPIO, DES_MUNICIPIO, BARRIO, DES_BARRIO, TELEFONO, TELEFONO_CELULAR, TELEFONO_CONTACTO, FICHA_CATASTRAL, CENTRO_POBLADO, DES_CEN_POBLADO, NIT, APELLIDO1, APELLIDO2, NOMBRE1, NOMBRE2, TIPO_PERSONA, DESC_TIPO_PER, CODIGO_ENTIDAD, DES_ENTIDAD, CICLO, PERIODICIDAD, RUTA_LECTURA, TARIFA, DESC_TARIFA, GRUPO_CU, DES_GRUPO_CU, CLASE_SERVICIO, DES_CLASE_SERVICIO, ESTRATO, MEDIDA_TENSION, NRO_MEDIDORES, FACTOR_UTILIZACION, CARGA_CONTRATADA, CARGA_INSTALADA, CARGA_ADICIONAL, FACTOR_ADICIONAL, AGRUPA_ESTRATO
      # processed_data <- processed_data[, c(1:17,199,18:198)] #no se corre de nuevo porque se daña el orden

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
      
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Nuevas variables ➡️"))
      # Nuevas variables
      processed_data$AGRUPA_ESTRATO <- ifelse(processed_data$ESTRATO %in% c(1, 2), "1 y 2",
                                              ifelse(processed_data$ESTRATO %in% c(3, 4), "3 y 4",
                                                     ifelse(processed_data$ESTRATO %in% c(5, 6), "5 y 6", "0")))
      processed_data$AGRUPA_ESTRATO = as.factor(processed_data$AGRUPA_ESTRATO)
      
      processed_data$AGRUPA_DES_CLASE_SERVICIO <- ifelse(processed_data$DES_CLASE_SERVICIO == "Residencial", "Residencial", "No Residencial")
      processed_data$AGRUPA_DES_CLASE_SERVICIO = as.factor(processed_data$AGRUPA_DES_CLASE_SERVICIO)
      
      processed_data$AGRUPA_DESC_TARIFA <- ifelse(processed_data$DESC_TARIFA == "PREPAGO", "PREPAGO", "POSPAGO")
      processed_data$AGRUPA_DESC_TARIFA = as.factor(processed_data$AGRUPA_DESC_TARIFA)
      
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Agrupaciones ➡️"))
      var_names = c("REGIONAL", "AGRUPA_DES_CLASE_SERVICIO", "AGRUPA_ESTRATO", "AGRUPA_DESC_TARIFA", "PERIODICIDAD")
      missing_vars = var_names[!var_names %in% names(processed_data)]
      if (length(missing_vars) > 0) {
        stop("Las siguientes variables no están presentes en el dataframe: ", paste(missing_vars, collapse=", "))
      }
      
      processed_data$BD <- do.call(interaction, c(processed_data[var_names], drop = TRUE))
      # Tabla de frecuencias
      categorias_freq <- table(processed_data$BD)
      
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Filtrando y agrupando ➡️"))
      
      # Función auxiliar para filtrar categorías por periodicidad y frecuencia
      filtrar_categorias <- function(patron, umbral=50) {
        names(categorias_freq)[grepl(patron, names(categorias_freq)) & categorias_freq < umbral]
      }
      
      # Aplicar la función para cada tipo de periodicidad
      categorias_modificar  <- filtrar_categorias("\\.MENSUAL$")
      categorias_modificar1 <- filtrar_categorias("\\.BIMENSUAL$")
      categorias_modificar2 <- filtrar_categorias("\\.TRIMESUAL$")
      processed_data$BD <- as.character(processed_data$BD)
      
      Base2 <- processed_data
      #Base2$BD = as.character(processed_data$BD)
      Base2$BD[Base2$BD %in% categorias_modificar] <- "agrupacionmensual"
      Base2$BD[Base2$BD %in% categorias_modificar1] <- "agrupacionbimensual"
      Base2$BD[Base2$BD %in% categorias_modificar2] <- "agrupaciontrimesual"
      Base2$BD = as.factor(Base2$BD)
      
      mensual <- Base2[grepl("\\.MENSUAL$", Base2$BD) | Base2$BD == "agrupacionmensual", ]
      bimensuales <- Base2[grepl("\\.BIMENSUAL$", Base2$BD) | Base2$BD == "agrupacionbimensual", ]
      trimensuales <- Base2[grepl("\\.TRIMESUAL$", Base2$BD) | Base2$BD == "agrupaciontrimesual", ]
      
      datasets$mensual <- mensual
      datasets$bimestral <- bimensuales
      datasets$trimestral <- trimensuales
      
      working_data(Base2)
      updateProgressBar(session, "progress", value = 85)
      session$sendCustomMessage("enable_button", list(id = "btn_next", text = "Filtrado por periodicidad ➡️"))
      step(3)
    
    } else if (step() == 3) {
      
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Generando data mensual ➡️"))
      
      base3Mensual = datasets$mensual[,c(1:103,200)]
      variables <- paste("PERIODO_M", 1:24, sep = "")
      base3Mensual <- base3Mensual[complete.cases(base3Mensual[variables]), ]

      tiene_consecutivos_cero <- function(fila, n_consecutivos = 5) {
        # Extraer consumos (columnas pares) y periodos (columnas impares)
        consumos <- as.numeric(fila[seq(2, length(fila), by = 2)])
        periodos <- as.numeric(fila[seq(1, length(fila), by = 2)])
        
        # Filtrar solo donde el período es válido (no NA y diferente de 0)
        consumos_validos <- consumos[!is.na(periodos) & periodos != 0]
        
        # Detectar secuencias consecutivas de ceros
        rle_result <- rle(consumos_validos == 0)
        any(rle_result$values & rle_result$lengths >= n_consecutivos)
      }
      
      base3Mensual$consecutivo0 <- apply(base3Mensual[, 56:103], 1, tiene_consecutivos_cero, n_consecutivos = 5)
      datasets$mensual <- base3Mensual
      
      # leer bim ajustada
      bimestral <- datasets$bimestral
      if(!is.null(datasets$bimestra_ajustada)){
        bimestral_ajustada <- datasets$bimestra_ajustada
        bimestral <- merge(bimestral[,-c(56:199)],
                           bimestral_ajustada, by.x = "CLIENTE_ID", by.y = "CLIENTE_ID", all = FALSE)
      }
      
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Generando data bimestral ➡️"))
      # se analizaran solo 24 meses es decir 13 periodos 
      base3Bimensual = bimestral[,c(1:82)]
      variables <- paste("PERIODO_M", 1:13, sep = "")
      base3Bimensual <- base3Bimensual[complete.cases(base3Bimensual[variables]), ]
      #Se observa si el individuo presenta o no mas de 5 consumos consecutivos en 0 
      base3Bimensual$consecutivo0 <- sapply(1:nrow(base3Bimensual), function(i) {
        tiene_consecutivos_cero(base3Bimensual[i, 57:82], n_consecutivos = 5)
      })
      datasets$bimestral <- base3Bimensual
      
      # leer trim ajustada
      trimestral <- datasets$trimestral
      if(!is.null(datasets$trimestral_ajustada)){
        trimestral_ajustada <- datasets$trimestral_ajustada
        trimestral <- merge(trimestral[,-c(56:199)],
                            trimestral_ajustada, by.x = "CLIENTE_ID", by.y = "CLIENTE_ID", all = FALSE)
      }
      
      session$sendCustomMessage("disable_button", list(id = "btn_next", text = "Generando data trimestral ➡️"))
      # se analizaran solo 24 meses es decir 7 periodos 
      base3Trimensual = trimestral[,c(1:74)]
      variables <- paste("PERIODO_M", 1:9, sep = "")
      base3Trimensual <- base3Trimensual[complete.cases(base3Trimensual[variables]), ]
      #Se observa si el individuo presenta o no mas de 5 consumos consecutivos en 0 
      base3Trimensual$consecutivo0 <- sapply(1:nrow(base3Trimensual), function(i) {
        tiene_consecutivos_cero(base3Trimensual[i, 57:74], n_consecutivos = 5)
      })
      
      
      updateProgressBar(session, "progress", value = 95)      
      session$sendCustomMessage("enable_button", list(id = "btn_next", text = "Continuar con revisiones ✅ "))
      step(4)
    } else if (step() == 4) {
      
      session$sendCustomMessage("enable_button", list(id = "btn_next", text = "Continuar ➡️"))
      
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
      # paste0("datos_limpios_", Sys.Date(), ".xlsx")
      paste0("Base2", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(working_data(), file)
    }
  )

  output$download_mensual <- downloadHandler(
    filename = function() {
      paste0("Base3_Mensual_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(datasets$mensual, file)
    }
  )

  output$download_bimensual <- downloadHandler(
    filename = function() {
      paste0("Base3_Bimensual_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(datasets$bimestral, file)
    }
  )

  output$download_trimestral <- downloadHandler(
    filename = function() {
      paste0("Base3_Trimestral_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(datasets$trimestral, file)
    }
  )
  
}
