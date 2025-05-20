#------------------------------------------------------------------------------#
#           Centrales electricas de Santander - Universidad del Valle          #
#                          Proyecto: Indicador de fraude                       #
#                   Grupo de perdidas - Escuela de estadística                 #
#                 Practicante - Jenifer Marcela Vivas Guengue                  #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#                    1. Carga de las bases de datos                         ####
#------------------------------------------------------------------------------#

load("./data/BD Consumos.RData") #Bd relacionada con los consumos de los 72 periodos atras
load("./data/BD SAC F.RData") #Bd relacionada con las caracteristicas de los usuarios 

View(BD_SAC_F)
View(ConsSac)

#union de las dos bases de datos, con los individuos que esten presentes en amdas bases
BaseunidaOriginal <- merge(BD_SAC_F,ConsSac, by.x = "CLIENTE_ID", by.y = "CLIENTE", all = FALSE)
View(BaseunidaOriginal)

#------------------------------------------------------------------------------#
#                    2. Limpieza de la base de datos                        ####
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
##                    2.1. Individuos                                       ####
#------------------------------------------------------------------------------#


#existen individuos que no deben estar presentes en la base de datos
#Individuos que debo eliminar
#elimina los individuos de la base de datos
Base1 = BaseunidaOriginal[-which(BaseunidaOriginal$CICLO == "53" | BaseunidaOriginal$CICLO == "98" | BaseunidaOriginal$CICLO == "104" | 
                                   BaseunidaOriginal$CICLO == "56" & BaseunidaOriginal$TARIFA == "373" | BaseunidaOriginal$EST_CLIENTE == "Inactivo"),]

#Observamos de la base de datos de departamentos que el departamento 11 hace referencia 
#a Santafe Bogota DC, el cual no debe ir en la base de datos, por tanto debe ser eliminado
Base1 = Base1[-(which(Base1$DEPTO == "11")),]

#aquellos clientes que son macromedidores no son relevantes en el analisis

Base1 = Base1[-(which(Base1$DES_CLASE_SERVICIO == "Macromedici\xf3n")),]

#se analizan solo aquellos clientes que esten vinculados 
Base1 = Base1[(which(Base1$EST_CLIENTE == "Activo")),]
#------------------------------------------------------------------------------#
## 2.2. Variables irrelevantes                                              ####
#------------------------------------------------------------------------------#
#se elimina aquellas variables que son codificación de otras
Base2 = Base1[,-c(3,5,7,10,12,14,16,18,24,31,33,38,40,42,55,57,60,75,77,79,81,83,
                  89,91,93,96,99, 103,105,109,113,115,118,120,135,138,142,144,148,
                  151,153,155,157,159,161,169,185)]

#------------------------------------------------------------------------------#
## 2.3. Valores Faltantes                                                   ####
#------------------------------------------------------------------------------#

#Se ponen las fechas en formato caracter para poder correr la función 
for (col in names(Base2)) {
  if (inherits(Base2[[col]], "POSIXt")) {
    Base2[[col]] <- as.character(Base2[[col]])
  }
}

#Datos Faltantes de toda la base de datos completa

#Funcion para contar la cantidad de Na y celdas vacias y su respectivo %
contar_nas <- function(data) {
  nas_por_variable <- colSums(is.na(data))
  celdasvacias <- colSums(data == "")
  total_filas <- nrow(data)
  porcentaje_nas <- round((nas_por_variable / total_filas) * 100, 4)
  porcentaje_vacias <-  round((celdasvacias / total_filas) * 100, 4)
  resultado <- data.frame(nas_por_variable,porcentaje_nas,celdasvacias,porcentaje_vacias)
  return(resultado)
}
#cantidad de nas y celdas vacias del data frame
nas0 <- contar_nas(Base2)


#analizando las variables con y sin datos faltantes se hace un filtro quedando la siguiente base de datos
Base2 = Base2[,c(1,2,4,6:8,10,11,22,24,25,27:31,54,58,62,67,71,83,84,86:92,95,97,
                 99:101,107,110,112:116,126,129,152:154,156:303)]



#valores faltantes son establecidos en la categoria sin identificar o no definido
unique = sapply(Base2,function (x){
  unique(x)
})

#aquellas variables que ya tienen una categoria de datos faltantes, se les anexa los Na encontrados a dicha categoria 
Base2$ACTIVIDAD_COMERCIAL <- ifelse(is.na(Base2$ACTIVIDAD_COMERCIAL), "SIN DATOS", Base2$ACTIVIDAD_COMERCIAL)
Base2$DES_PROPIEDAD_MEDIDOR <- ifelse(is.na(Base2$DES_PROPIEDAD_MEDIDOR), "No definido", Base2$DES_PROPIEDAD_MEDIDOR)
Base2$DES_LOCALIZACION = ifelse(is.na(Base2$DES_LOCALIZACION),"Sin Identificar",Base2$DES_LOCALIZACION)


#aquellas variables que no presentan una categoria para los Na se les crea una categoria titulada x"Sin identificar"
Base2[, c(1:47)] <- lapply(Base2[, c(1:47)], function(x) {
  ifelse(is.na(x), "Sin Identificar", x)
})

#------------------------------------------------------------------------------#
## 2.4 TRANSFORMACIÓN DE CARACTERES ESPECIALES                              ####
#------------------------------------------------------------------------------#
#La mayoria de las variables presentan caracteres especiales (?) por tanto se 
#realiza una conversion del formato de datos de latin a utf8
nombrevariables = colnames(Base2)
Base2[nombrevariables] <- lapply(Base2[nombrevariables], function(x) iconv(x, "latin1", "UTF-8"))

#------------------------------------------------------------------------------#
## 2.5 Transformación de la estructura de los datos                         ####
#------------------------------------------------------------------------------#
str(Base2) 
Base2[, c(3,4,6:16,17,19:25,27,29:47)] <- lapply(Base2[, c(3,4,6:16,17,19:25,27,29:47)], as.factor)

#se debe transormas los datos categoricos de caracter a factor
Base2$ANTIGUEDAD_SALDO = as.numeric(Base2$ANTIGUEDAD_SALDO)
Base2$REL_CORRIENTE_MAX = as.numeric(Base2$REL_CORRIENTE_MAX) 
Base2$REL_CORRIENTE_MIN = as.numeric(Base2$REL_CORRIENTE_MIN)
Base2$CONSTANTE = as.numeric(Base2$CONSTANTE)


#los consumos con valores decimales estan diferenciados por comas, la coma no es numerica, por tanto se ponen los valores redondeados
columnas_M <- grep("^M\\d+$", colnames(Base2), value = TRUE) #filtra los nombres de las variables de consumos M1:M72
Base2[columnas_M] <- lapply(Base2[columnas_M], function(x) {
  x <- gsub(",", ".", x)
  x <- as.numeric(x)
  round(x)
})

library(dplyr)
columnas_Periodos <- grep("^(PERIODO_M)\\d+$", colnames(Base2), value = TRUE) #filtra los nombres de las variables de periodo de consumos M1:M72
Base2[columnas_Periodos] <- Base2[columnas_Periodos] %>% 
  mutate(across(all_of(columnas_Periodos), as.character)) # Convierte las columnas seleccionadas a caracter
Base2[columnas_Periodos] <- lapply(Base2[columnas_Periodos], function(columna) {
  as.Date(paste0(substr(columna, 1, 4), "-", substr(columna, 5, 6), "-01"))
}) #este codigo demora #conviere los periodos a tipo date


#------------------------------------------------------------------------------#
## 2.6 Reescripción de la variable Modelo Medidor                           ####
#------------------------------------------------------------------------------#

#las categorias de la variable DES_MODELO son reescritas dado que 
#presentan diferente tipo de escritura del año
Base2$DES_MODELO <- as.factor(ifelse(as.numeric(as.character(Base2$DES_MODELO)) < 100, 
                                     paste0("19", as.character(Base2$DES_MODELO)), 
                                     as.character(Base2$DES_MODELO)))

#se agrupan el año del medidor (por 10 años) para hacer la variable con menos categorias 
table(Base2$DES_MODELO)
Base2$DES_MODELO_agrup = cut(as.numeric(as.character(Base2$DES_MODELO)),
                             breaks = seq(1950,2030,by = 10),
                             labels = paste(seq(1950,2020,by = 10),seq(1959,2029,by = 10),sep = "-"),
                             include.lowest = TRUE,
                             right = FALSE)
#orden de la variable
Base2 <- Base2[, c(1:25,196,26:195)] #no se corre mas de una vez
#------------------------------------------------------------------------------#
#                    3. Nuevas variables                                    ####
#------------------------------------------------------------------------------#
#primero se van a crear nuevas variables para poder dividir la base de datos
#a traves de ellas 
Base2$AGRUPA_ESTRATO <- ifelse(Base2$ESTRATO %in% c(1, 2), "1 y 2",
                               ifelse(Base2$ESTRATO %in% c(3, 4), "3 y 4",
                                      ifelse(Base2$ESTRATO %in% c(5, 6), "5 y 6", "0")))

Base2$AGRUPA_DES_CLASE_SERVICIO <- ifelse(Base2$DES_CLASE_SERVICIO == "Residencial", "Residencial", "No Residencial")

Base2$AGRUPA_DESC_TARIFA <- ifelse(Base2$DESC_TARIFA == "PREPAGO", "PREPAGO", "POSPAGO")

#vuelvo estas 3 variables factor
Base2$AGRUPA_ESTRATO = as.factor(Base2$AGRUPA_ESTRATO)
Base2$AGRUPA_DES_CLASE_SERVICIO = as.factor(Base2$AGRUPA_DES_CLASE_SERVICIO)
Base2$AGRUPA_DESC_TARIFA = as.factor(Base2$AGRUPA_DESC_TARIFA)


#Se acomoda el orden de las variables
Base2 <- Base2[, c(1:12,199,13:198)] #no se corre de nuevo porque se daña el orden
Base2 <- Base2[, c(1:15,199,16:198)] #no se corre de nuevo porque se daña el orden
Base2 <- Base2[, c(1:17,199,18:198)] #no se corre de nuevo porque se daña el orden

b2= Base2


#------------------------------------------------------------------------------#
#      4. División de las bases de datos                                    ####
#------------------------------------------------------------------------------#

#aquellos consumos donde no hay periodos se ponen en na
for (i in 1:72) {
  periodo_col <- paste0("PERIODO_M", i)  # Nombre de la columna de período
  consumo_col <- paste0("M", i)         # Nombre de la columna de consumo
  
  Base2 <- Base2 %>%
    mutate(!!consumo_col := ifelse(is.na(.[[periodo_col]]) & .[[consumo_col]] == 0, NA, .[[consumo_col]]))
}


#Funcion para clasificar las variables segun a la base de datos que pertenece
agregar_identificador_combinacion <- function(df) {
  var_names = c("REGIONAL", "AGRUPA_DES_CLASE_SERVICIO", "AGRUPA_ESTRATO", "AGRUPA_DESC_TARIFA", "PERIODICIDAD")
  missing_vars = var_names[!var_names %in% names(df)]
  if (length(missing_vars) > 0) {
    stop("Las siguientes variables no están presentes en el dataframe: ", paste(missing_vars, collapse=", "))
  }
  df$BD <- with(df, interaction(REGIONAL, AGRUPA_DES_CLASE_SERVICIO, AGRUPA_ESTRATO, AGRUPA_DESC_TARIFA, PERIODICIDAD, drop = TRUE))
  return(df)
}

#Nueva variable llamada BD que identifica la base de datos a la que pertenece el individuo
Base2 = agregar_identificador_combinacion(Base2)

#agrupar las bd con menos de 50 individuos, divididas por periodicidad
categorias_freq <- table(Base2$BD)
categorias_modificar <- names(categorias_freq)[grepl("\\.MENSUAL$", names(categorias_freq)) & categorias_freq < 50]
categorias_modificar1 <- names(categorias_freq)[grepl("\\.BIMENSUAL$", names(categorias_freq)) & categorias_freq < 50]
categorias_modificar2 <- names(categorias_freq)[grepl("\\.TRIMESUAL$", names(categorias_freq)) & categorias_freq < 50]

Base2$BD = as.character(Base2$BD)
Base2$BD[Base2$BD %in% categorias_modificar] <- "agrupacionmensual"
Base2$BD[Base2$BD %in% categorias_modificar1] <- "agrupacionbimensual"
Base2$BD[Base2$BD %in% categorias_modificar2] <- "agrupaciontrimesual"
Base2$BD = as.factor(Base2$BD)

library(openxlsx)
write.xlsx(Base2, file = "Base2.xlsx") #bajo la base de datos a excel

#------------------------------------------------------------------------------#
#            5. Bases de datos por periodicidad                             ####
#------------------------------------------------------------------------------#

mensual <- Base2[grepl("\\.MENSUAL$", Base2$BD) | Base2$BD == "agrupacionmensual", ]
bimensuales <- Base2[grepl("\\.BIMENSUAL$", Base2$BD) | Base2$BD == "agrupacionbimensual", ]
trimesuales <- Base2[grepl("\\.TRIMESUAL$", Base2$BD) | Base2$BD == "agrupaciontrimesual", ]

#con la base de datos bimensual y trimesual se realizaron las reconversiones de los consumos 
#------------------------------------------------------------------------------#
#             6. Base de datos con 24 periodos Mensual                      ####
#------------------------------------------------------------------------------#

Base3_Mensual = mensual[,c(1:103,200)]
variables <- paste("PERIODO_M", 1:24, sep = "")
Base3_Mensual <- Base3_Mensual[complete.cases(Base3_Mensual[variables]), ]

#Analizo si el cliente tiene o no mas de 5 consumos consecutivos en 0 

#Funcion para mirar mas de 5 consumos consecutivos en 0 
tiene_consecutivos_cero <- function(fila, n_consecutivos = 5) {
  consumos <- fila[seq(2, length(fila), by = 2)]   # Acceder a los consumos (columnas pares)
  periodos <- fila[seq(1, length(fila), by = 2)]   # Acceder a los periodos (columnas impares)
  
  consecutivos <- 0
  tiene_patron <- FALSE
  
  for (i in 1:length(consumos)) {
    if (!is.na(periodos[i]) && periodos[i] != 0) { # Verificar si el periodo no es NA o 0
      if (consumos[i] == 0) {
        consecutivos <- consecutivos + 1
        if (consecutivos >= n_consecutivos) {
          tiene_patron <- TRUE
          break
        }
      } else {
        consecutivos <- 0
      }
    }
  }
  
  return(tiene_patron)
}

#Consecutivos 0
Base3_Mensual$consecutivo0 <- sapply(1:nrow(Base3_Mensual), function(i) {
  tiene_consecutivos_cero(Base3_Mensual[i, 56:103], n_consecutivos = 5)
})


write.xlsx(Base3_Mensual, file = "Base3_Mensual.xlsx") #bajo la base de datos a excel


#------------------------------------------------------------------------------#
##5. Integración de variables categoricas a la base de datos bimensual de solo consumos (bd_bim_Ajus.xlsx)
#------------------------------------------------------------------------------#
#En el script titulado Reconversion consumos segun su periodicidad se presenta la base de datos de clientes bimensuales
#acomodados de manera que solo se tengan clientes con fechas consecutivas cada dos meses
bd_bim_Ajus <- read_excel("bd_bim_Ajus.xlsx")
bimensuales <- merge(bimensuales[,-c(56:199)],bd_bim_Ajus, by.x = "CLIENTE_ID", by.y = "CLIENTE_ID", all = FALSE)


# se analizaran solo 24 meses es decir 13 periodos 
Base3_Bimensual = bimensuales[,c(1:82)]
variables <- paste("PERIODO_M", 1:13, sep = "")
Base3_Bimensual <- Base3_Bimensual[complete.cases(Base3_Bimensual[variables]), ]


#Se observa si el individuo presenta o no mas de 5 consumos consecutivos en 0 
Base3_Bimensual$consecutivo0 <- sapply(1:nrow(Base3_Bimensual), function(i) {
  tiene_consecutivos_cero(Base3_Bimensual[i, 57:82], n_consecutivos = 5)
})

write.xlsx(Base3_Bimensual, file = "Base3_bimensual.xlsx") #bajo la base de datos a excel

#------------------------------------------------------------------------------#
##5. Integración de variables categoricas a la base de datos trimensual de solo consumos (bd_trim_Ajus.xlsx)
#------------------------------------------------------------------------------#
bd_trim_Ajus <- read_excel("bd_trim_Ajus.xlsx")
trimesuales <- merge(trimesuales[,-c(56:199)],bd_trim_Ajus, by.x = "CLIENTE_ID", by.y = "CLIENTE_ID", all = FALSE)



# se analizaran solo 24 meses es decir 7 periodos 
Base3_Trimensual = trimesuales[,c(1:74)]
variables <- paste("PERIODO_M", 1:9, sep = "")
Base3_Trimensual <- Base3_Trimensual[complete.cases(Base3_Trimensual[variables]), ]


#Se observa si el individuo presenta o no mas de 5 consumos consecutivos en 0 
Base3_Trimensual$consecutivo0 <- sapply(1:nrow(Base3_Trimensual), function(i) {
  tiene_consecutivos_cero(Base3_Trimensual[i, 57:74], n_consecutivos = 5)
})




write.xlsx(Base3_Trimensual, file = "Base3_trimensual.xlsx") #bajo la base de datos a excel
