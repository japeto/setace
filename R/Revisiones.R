install.packages("lubridate")
install.packages("dplyr")
install.packages("readxl")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("patchwork")
install.packages("VIM")
#------------------------------------------------------------------------------#
## Lectura de la base de datos  de revisiones                               ----
#------------------------------------------------------------------------------#
load("BD SAC Rev Encriptada.RData")
str(Rev_encript)

#elimino las variables no relevantes
sapply(Rev_encript,function (x) {length(unique(x))} )
Rev_encript = Rev_encript[,-c(1,3,6,8,10,13,16,19,20,21)] #no correr dos veces seguidas

#paso a utf-8
nombrevariables = colnames(Rev_encript)
Rev_encript[nombrevariables] <- lapply(Rev_encript[nombrevariables], function(x) iconv(x, "latin1", "UTF-8"))


#transformación de variables: 
library(lubridate)
Rev_encript$FECHA_REVISION = dmy(Rev_encript$FECHA_REVISION)
Rev_encript$CONTRATISTA = as.factor(Rev_encript$CONTRATISTA)
Rev_encript$REVISOR = as.factor(Rev_encript$REVISOR)
Rev_encript$DES_MOTIVO = as.factor(Rev_encript$DES_MOTIVO)
Rev_encript$RESULTADO = as.factor(Rev_encript$RESULTADO)
str(Rev_encript)

#cantidad y porcentaje de valores faltantes por variable
contar_nas <- function(data) {
  nas_por_variable <- colSums(is.na(data))
  celdasvacias <- colSums(data == "")
  total_filas <- nrow(data)
  porcentaje_nas <- round((nas_por_variable / total_filas) * 100, 4)
  porcentaje_vacias <-  round((celdasvacias / total_filas) * 100, 4)
  resultado <- data.frame(nas_por_variable,porcentaje_nas,celdasvacias,porcentaje_vacias)
  return(resultado)
}
contar_nas(Rev_encript) 

View(Rev_encript)
#creo una variable fraude o no fraude 
Rev_encript$Fraude <- ifelse(Rev_encript$RESULTADO == "Fraude", "Fraude", "No Fraude")
#seleccionar una sola revision del cliente por mes (priorizando si es fraude dejar la revision de fraude)
Rev_unica_por_mes <- Rev_encript %>%
  mutate(MES = format(FECHA_REVISION, "%Y-%m")) %>% # Crear columna de mes en formato "YYYY-MM"
  group_by(CLIENTE, MES) %>% # Agrupar por cliente y mes
  arrange(desc(RESULTADO == "Fraude")) %>% # Priorizar los fraudes
  slice(1) %>% # Seleccionar la primera fila (fraude si existe, sino cualquier otro)
  ungroup() # Desagrupar

#pongo todas las fechas en el dia 01 para poder asociarlas a las otras base de datos
Rev_unica_por_mes <- Rev_unica_por_mes %>%
  mutate(FECHA_REVISION = as.Date(paste0(format(FECHA_REVISION, "%Y-%m"), "-01")))



#Presento una base de datos con el total de revisiones del cliente y cuantos de estos son fraudes
Total_revisiones = Rev_unica_por_mes %>%
  group_by(CLIENTE) %>%
  summarise(
    total_revisiones = n(),  # Contar cuántas veces aparece el cliente (cantidad de revisiones)
    total_fraudes = sum(Fraude == "Fraude", na.rm = TRUE)  # Contar cuántos fraudes tiene
  ) %>%
  mutate(
    total_revisiones = ifelse(is.na(total_revisiones), 0, total_revisiones),  # Para los que no tienen revisiones, poner 0
    total_fraudes = ifelse(is.na(total_fraudes), 0, total_fraudes)  # Para los que no tienen fraudes, poner 0
  )



#------------------------------------------------------------------------------#
### cantidad de clientes que no estan en la base de datos SAC inicial        ----
#------------------------------------------------------------------------------#
library(readxl)


Base2 <- read_excel("Base2.xlsx")
View(Base2)

library(dplyr)
clientes_NO_Base2 = anti_join(Rev_encript,Base2,by = c("CLIENTE" = "CLIENTE_ID"))
#hay 2964 clientes que no estan en la base de datos del SAC que se me proporciono inicialmente




#------------------------------------------------------------------------------#
## Lectura de las base de datos (consumos e información del cliente)  por periodicidad  ----
#------------------------------------------------------------------------------#
# los codigos relacionados a la lectura de las bases de datos son corridos si las bases de datos no estan presenten en el enviroment

Base3_Mensual <- read.delim("Base3_mensual.csv", sep = ";", stringsAsFactors = FALSE)

###Base de datos mensual----
#library(readxl)
#Base3_Mensual <- read_excel("Base3_Mensual.xlsx")
View(Base3_Mensual)
###decodificacion de latin a utf8
nombrevariables = colnames(Base3_Mensual)
Base3_Mensual[nombrevariables] <- lapply(Base3_Mensual[nombrevariables], function(x) iconv(x, "latin1", "UTF-8"))

###Transformacion de la estructura de los datos 
str(Base2) 
Base2[, c(1:55)] <- lapply(Base2[, c(1:55)], as.factor)

###se debe transormas los datos categoricos de caracter a factor
Base3_Mensual$CLIENTE_ID = as.character(Base3_Mensual$CLIENTE_ID)
Base3_Mensual$NOMBRE = as.character(Base3_Mensual$NOMBRE)
Base3_Mensual$DIRECCION = as.character(Base3_Mensual$DIRECCION)
Base3_Mensual$ANTIGUEDAD_SALDO = as.numeric(Base3_Mensual$ANTIGUEDAD_SALDO)
Base3_Mensual$REL_CORRIENTE_MAX = as.numeric(Base3_Mensual$REL_CORRIENTE_MAX) 
Base3_Mensual$REL_CORRIENTE_MIN = as.numeric(Base3_Mensual$REL_CORRIENTE_MIN)
Base3_Mensual$CONSTANTE = as.numeric(Base3_Mensual$CONSTANTE)


#los consumos con valores decimales estan diferenciados por comas, la coma no es numerica, por tanto se ponen los valores redondeados
columnas_M <- grep("^M\\d+$", colnames(Base3_Mensual), value = TRUE) #filtra los nombres de las variables de consumos M1:M72
Base3_Mensual[columnas_M] <- lapply(Base3_Mensual[columnas_M], function(x) {
 x <- gsub(",", ".", x)
 x <- as.numeric(x)
round(x)
})

#library(dplyr)
columnas_Periodos <- grep("^(PERIODO_M)\\d+$", colnames(Base3_Mensual), value = TRUE) #filtra los nombres de las variables de periodo de consumos M1:M72
Base3_Mensual[columnas_Periodos] <- lapply(Base3_Mensual[columnas_Periodos], function(columna) {
as.Date(columna, format="%Y-%m-%d")
}) #este codigo demora #conviere los periodos a tipo date


###Base de datos bimensual----

#library(readxl)
#Base3_Bimensual <- read_excel("Base3_Bimensual.xlsx")
#View(Base3_Bimensual)
###decodificacion de latin a utf8
#nombrevariables = colnames(Base3_Bimensual)
#Base3_Bimensual[nombrevariables] <- lapply(Base3_Bimensual[nombrevariables], function(x) iconv(x, "latin1", "UTF-8"))

###Transformacion de la estructura de los datos 
#str(Base2) 
#Base2[, c(1:55)] <- lapply(Base2[, c(1:55)], as.factor)

###se debe transormas los datos categoricos de caracter a factor
#Base3_Bimensual$CLIENTE_ID = as.character(Base3_Bimensual$CLIENTE_ID)
#Base3_Bimensual$NOMBRE = as.character(Base3_Bimensual$NOMBRE)
#Base3_Bimensual$DIRECCION = as.character(Base3_Bimensual$DIRECCION)
#Base3_Bimensual$ANTIGUEDAD_SALDO = as.numeric(Base3_Bimensual$ANTIGUEDAD_SALDO)
#Base3_Bimensual$REL_CORRIENTE_MAX = as.numeric(Base3_Bimensual$REL_CORRIENTE_MAX) 
#Base3_Bimensual$REL_CORRIENTE_MIN = as.numeric(Base3_Bimensual$REL_CORRIENTE_MIN)
#Base3_Bimensual$CONSTANTE = as.numeric(Base3_Bimensual$CONSTANTE)


#los consumos con valores decimales estan diferenciados por comas, la coma no es numerica, por tanto se ponen los valores redondeados
#columnas_M <- grep("^M\\d+$", colnames(Base3_Bimensual), value = TRUE) #filtra los nombres de las variables de consumos M1:M72
#Base3_Bimensual[columnas_M] <- lapply(Base3_Bimensual[columnas_M], function(x) {
# x <- gsub(",", ".", x)
# x <- as.numeric(x)
#round(x)
#})

#library(dplyr)
#columnas_Periodos <- grep("^(PERIODO_M)\\d+$", colnames(Base3_Bimensual), value = TRUE) #filtra los nombres de las variables de periodo de consumos M1:M72
#Base3_Bimensual[columnas_Periodos] <- lapply(Base3_Bimensual[columnas_Periodos], function(columna) {
#as.Date(columna, format="%Y-%m-%d")
#}) #este codigo demora #conviere los periodos a tipo date



###Base de datos trimensual----
#library(readxl)
#Base3_Trimensual <- read_excel("Base3_Trimensual.xlsx")
#View(Base3_Trimensual)
###decodificacion de latin a utf8
#nombrevariables = colnames(Base3_Trimensual)
#Base3_Trimensual[nombrevariables] <- lapply(Base3_Trimensual[nombrevariables], function(x) iconv(x, "latin1", "UTF-8"))

###Transformacion de la estructura de los datos 
#str(Base2) 
#Base2[, c(1:55)] <- lapply(Base2[, c(1:55)], as.factor)

###se debe transormas los datos categoricos de caracter a factor
#Base3_Trimensual$CLIENTE_ID = as.character(Base3_Trimensual$CLIENTE_ID)
#Base3_Trimensual$NOMBRE = as.character(Base3_Trimensual$NOMBRE)
#Base3_Trimensual$DIRECCION = as.character(Base3_Trimensual$DIRECCION)
#Base3_Trimensual$ANTIGUEDAD_SALDO = as.numeric(Base3_Trimensual$ANTIGUEDAD_SALDO)
#Base3_Trimensual$REL_CORRIENTE_MAX = as.numeric(Base3_Trimensual$REL_CORRIENTE_MAX) 
#Base3_Trimensual$REL_CORRIENTE_MIN = as.numeric(Base3_Trimensual$REL_CORRIENTE_MIN)
#Base3_Trimensual$CONSTANTE = as.numeric(Base3_Trimensual$CONSTANTE)


#los consumos con valores decimales estan diferenciados por comas, la coma no es numerica, por tanto se ponen los valores redondeados
#columnas_M <- grep("^M\\d+$", colnames(Base3_Trimensual), value = TRUE) #filtra los nombres de las variables de consumos M1:M72
#Base3_Trimensual[columnas_M] <- lapply(Base3_Trimensual[columnas_M], function(x) {
# x <- gsub(",", ".", x)
# x <- as.numeric(x)
#round(x)
#})

#library(dplyr)
#columnas_Periodos <- grep("^(PERIODO_M)\\d+$", colnames(Base3_Trimensual), value = TRUE) #filtra los nombres de las variables de periodo de consumos M1:M72
#Base3_Trimensual[columnas_Periodos] <- lapply(Base3_Trimensual[columnas_Periodos], function(columna) {
#as.Date(columna, format="%Y-%m-%d")
#}) #este codigo demora #conviere los periodos a tipo date


#------------------------------------------------------------------------------#
## Pasar a formato largo la base de datos y union con las revisiones        ----
#------------------------------------------------------------------------------#


library(dplyr)
library(tidyr)
library(lubridate)
procesar_base <- function(base, nombre_bd) {
  base_filtrada <- base[base$BD == nombre_bd, ] %>%
    pivot_longer(
      cols = all_of(c(columnas_Periodos, columnas_M)),
      names_to = c(".value", "number"),
      names_pattern = "^(PERIODO_M|M)(\\d+)$"
    ) %>%
    mutate(number = as.numeric(number)) %>%  # Asegura que 'number' sea numérico
    arrange(as.numeric(CLIENTE_ID), number) %>%  # Organiza por CLIENTE_ID y luego por 'number'
    group_by(CLIENTE_ID) %>%
    mutate(
      diferencia_meses = interval(lag(PERIODO_M), PERIODO_M) %/% months(1),
      media_consumo = mean(M, na.rm = TRUE),
      mediana_consumo = median(M, na.rm = TRUE),
      sd_consumo = sd(M, na.rm = TRUE),
      CoefVar = (sd(M, na.rm = TRUE)/mean(M, na.rm = TRUE))*100,
      Q1_consumo = quantile(M, 0.25, na.rm = TRUE),
      Q3_consumo = quantile(M, 0.75, na.rm = TRUE),
      IQR_consumo = IQR(M, na.rm = TRUE),
      min_consumo = min(M, na.rm = TRUE),
      max_consumo = max(M, na.rm = TRUE),
      variabilidad = ifelse(CoefVar> 50, "Alta Variabilidad", "Baja Variabilidad"),
      outliers = case_when(
        M < (Q1_consumo - 1.5 * IQR_consumo) ~ "Atípico por debajo",
        M > (Q3_consumo + 1.5 * IQR_consumo) ~ "Atípico por encima",
        TRUE ~ "Sin atípicos"
      )
    ) %>%
    ungroup()
  
  return(base_filtrada)
}





#---------------------------------------------
#REALIZAR EL PROCESO PARA LAS DEMAS CAEGORIAS Y MESES 
#---------------------------------------------

# Es necesario poner la bd que se quiere analizar independientemente de la periodicidad 
bd = procesar_base(Base3_Mensual,"TIBU.Residencial.1 y 2.POSPAGO.MENSUAL")
bd$CLIENTE_ID = as.character(bd$CLIENTE_ID)
#Se procede a unir la base de datos con las revisiones que se han realizado 
#estas revisiones son puestas en las filas de los clientes donde hubo o no revision, de lo contrario seran presentados NA(referentes a que en otros periodos  no hubo revision)
baseunion <- bd %>%
  left_join(Rev_unica_por_mes, by = c("CLIENTE_ID" = "CLIENTE", "PERIODO_M" = "FECHA_REVISION")) %>%
  mutate(
    Revision = ifelse(!is.na(RESULTADO), "si", "no")  # Si existe un resultado, entonces hubo revisión
  )


#------------------------------------------------------------------------------#
###primeros analisis                                                        ----
#------------------------------------------------------------------------------#
library(dplyr)
# Agrupar por cliente para identificar cuántos tienen al menos una revisión
base_resumida <- baseunion %>%
  group_by(CLIENTE_ID) %>%
  summarise(
    total_revisiones = sum(Revision == "si", na.rm = TRUE),  # Cantidad de revisiones por cliente
    fraudes = sum(Fraude == "Fraude", na.rm = TRUE)       # Cantidad de fraudes por cliente
  ) %>%
  mutate(
    total_revisiones = ifelse(is.na(total_revisiones), 0, total_revisiones),  # Para los que no tienen revisiones, poner 0
    fraudes = ifelse(is.na(fraudes), 0, fraudes)  # Para los que no tienen fraudes, poner 0
  )


 

library(ggplot2)

# Gráfico de barras para visualizar cuantos clientes presentan revisiones y fraudes


ggplot(base_resumida) +
  geom_bar(aes(x = total_revisiones, fill = "Revisiones"), position = "dodge", stat = "count", alpha = 0.7, color = "blue") +
  geom_bar(aes(x = fraudes, fill = "Fraudes"), position = "dodge", stat = "count", alpha = 0.7, color = "red") +
  labs(title = "Distribución de Revisiones y Fraudes por Cliente",
       x = "Cantidad de Revisiones/Fraudes",
       y = "Número de Clientes",
       fill = "Tipo") +
  scale_fill_manual(values = c("Revisiones" = "blue", "Fraudes" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(base_resumida$total_revisiones, base_resumida$fraudes), 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

table(base_resumida$total_revisiones,base_resumida$fraudes)




#------------------------------------------------------------------------------#
###Clientes que tienen al menos una fila con revisión                                       ----
#------------------------------------------------------------------------------#
clientes_con_revision <- baseunion %>%
  group_by(CLIENTE_ID) %>%
  filter(any(Revision == "si")) %>%
  ungroup()

#------------------------------------------------------------------------------#
###graficos temporales                                                      ----
#------------------------------------------------------------------------------#
#Filtro de clientes fraudulentos
clientes_fraudulentos <- clientes_con_revision %>%
  group_by(CLIENTE_ID) %>%
  filter(any(Fraude == "Fraude")) %>%
  ungroup()

# Tamaño de la muestra
n_muestra = 10
set.seed(444)
# Seleccionar muestra de clientes fraudulentos
muestra_fraude <- sample(unique(clientes_fraudulentos$CLIENTE_ID), n_muestra)
# Filtrar los clientes seleccionados en la base de datos
muestra_fraude <- clientes_fraudulentos %>%
  filter(CLIENTE_ID %in% muestra_fraude)


#creacionn de limite para valores atipicos para la mediana 
mediana_consumos <- mean(bd$mediana_consumo, na.rm = TRUE)


###valores atipicos para la mediana 
#calculo de los cuartiles Q1 y Q3 de esta mediana
Q1 <- quantile(bd$mediana_consumo, 0.25, na.rm = TRUE)
Q3 <- quantile(bd$mediana_consumo, 0.75, na.rm = TRUE)


IQR_consumo <- Q3 - Q1
# Calculo de los límites para los valores atípicos
limite_inferior <- Q1 - 1.5 * IQR_consumo
limite_superior <- Q3 + 1.5 * IQR_consumo


#se presenta un grafico para analizar fraudes donde es puesto de color purpura los consumos que fueron fraudulentos
#tener presente que si este grafico es realizado para clientes bimensuales o trimensuales se debe cambiar el rango de periodos
#ya no serian 24 sino 13 o 9 respectivamente. 
Graficofraudes = ggplot(muestra_fraude, aes(x = as.factor(number), y = M, group = CLIENTE_ID, color = as.factor(CLIENTE_ID))) +
  geom_line() +
  geom_hline(yintercept = mediana_consumos, linetype = "dashed", color = "blue", size = 0.5) +
  geom_hline(yintercept = Q1, linetype = "dotted", color = "green", size = 0.5) +
  geom_hline(yintercept = Q3, linetype = "dotted", color = "green", size = 0.5) +
  geom_hline(yintercept = limite_inferior, linetype = "dotted", color = "red", size = 0.5) +
  geom_hline(yintercept = limite_superior, linetype = "dotted", color = "red", size = 0.5) +
   # Añadimos los puntos para los fraudes
  geom_point(data = muestra_fraude %>% filter(Fraude == Fraude), aes(x = as.factor(number), y = M), color = "purple", size = 2) +
  scale_x_discrete(labels = paste("Periodo", 1:24, sep = "_")) +  # Etiquetas de periodos
  scale_y_continuous(n.breaks = 20) +
  labs(title = "Serie de Tiempo de Clientes con Fraudes",
       x = "Periodo", y = "Consumo",
       subtitle = "Líneas azules: Mediana | Líneas verdes: Q1 y Q3 | Líneas rojas: Límites de Atípicos | Puntos púrpura: Fraudes") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



#-------------------------------------------------------------------------------#
#grafico para el caso bimensual 

#Graficofraudes = ggplot(muestra_fraude, aes(x = as.factor(number), y = M, group = CLIENTE_ID, color = as.factor(CLIENTE_ID))) +
 # geom_line() +
#  geom_hline(yintercept = mediana_consumos, linetype = "dashed", color = "blue", size = 0.5) +
 # geom_hline(yintercept = Q1, linetype = "dotted", color = "green", size = 0.5) +
  #geom_hline(yintercept = Q3, linetype = "dotted", color = "green", size = 0.5) +
  #geom_hline(yintercept = limite_inferior, linetype = "dotted", color = "red", size = 0.5) +
  #geom_hline(yintercept = limite_superior, linetype = "dotted", color = "red", size = 0.5) +
  # Añadimos los puntos para los fraudes
  #geom_point(data = muestra_fraude %>% filter(Fraude == Fraude), aes(x = as.factor(number), y = M), color = "purple", size = 2) +
  #scale_x_discrete(labels = paste("Periodo", 1:13, sep = "_")) +  # Etiquetas de periodos
  #scale_y_continuous(n.breaks = 20) +
  #labs(title = "Serie de Tiempo de Clientes con Fraudes",
   #    x = "Periodo", y = "Consumo",
    #   subtitle = "Líneas azules: Mediana | Líneas verdes: Q1 y Q3 | Líneas rojas: Límites de Atípicos | Puntos púrpura: Fraudes") +
  #theme_minimal() +
  #theme(legend.position = "none",
   #     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#-------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
#grafico para el caso trimensual 

#Graficofraudes = ggplot(muestra_fraude, aes(x = as.factor(number), y = M, group = CLIENTE_ID, color = as.factor(CLIENTE_ID))) +
# geom_line() +
#  geom_hline(yintercept = mediana_consumos, linetype = "dashed", color = "blue", size = 0.5) +
# geom_hline(yintercept = Q1, linetype = "dotted", color = "green", size = 0.5) +
#geom_hline(yintercept = Q3, linetype = "dotted", color = "green", size = 0.5) +
#geom_hline(yintercept = limite_inferior, linetype = "dotted", color = "red", size = 0.5) +
#geom_hline(yintercept = limite_superior, linetype = "dotted", color = "red", size = 0.5) +
# Añadimos los puntos para los fraudes
#geom_point(data = muestra_fraude %>% filter(Fraude == Fraude), aes(x = as.factor(number), y = M), color = "purple", size = 2) +
#scale_x_discrete(labels = paste("Periodo", 1:9, sep = "_")) +  # Etiquetas de periodos
#scale_y_continuous(n.breaks = 20) +
#labs(title = "Serie de Tiempo de Clientes con Fraudes",
#    x = "Periodo", y = "Consumo",
#   subtitle = "Líneas azules: Mediana | Líneas verdes: Q1 y Q3 | Líneas rojas: Límites de Atípicos | Puntos púrpura: Fraudes") +
#theme_minimal() +
#theme(legend.position = "none",
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#-------------------------------------------------------------------------------#




# Mostrar el gráfico interactivo
library(plotly)
plotly_fraude <- ggplotly(Graficofraudes, tooltip = c("x", "y", "CLIENTE_ID"))

plotly_fraude



#------------------------------------------------------------------------------#
## Base de datos para modelos                                               ----
#------------------------------------------------------------------------------#
#se usa la mediana de los consumos de cada cliente para representarlos

Base_final_revisiones<- clientes_con_revision %>%
  group_by(CLIENTE_ID) %>%
  summarise(
    mediana_consumo = median(M, na.rm = TRUE),
    across(where(is.character), first),  # Aplicar `first()` a todas las variables de tipo carácter
    across(where(is.factor), first),     # Aplicar `first()` a todas las variables de tipo factor
    across(where(is.numeric) & !matches("M"), first)
  )


fraudes_resumidos <- Rev_unica_por_mes %>%
  group_by(CLIENTE) %>%
  summarise(Fraude = case_when(
    any(Fraude == "Fraude", na.rm = TRUE) ~ "Fraude",  # Si hay al menos un "Fraude"
    all(Fraude == "No Fraude", na.rm = TRUE) ~ "No Fraude",  # Si todas las revisiones son "No Fraude"
    TRUE ~ "Desconocido"  # En caso de que haya solo NA o no haya información clara
  )) 


Base_final_revisiones <- Base_final_revisiones %>%
  inner_join(fraudes_resumidos, by = c("CLIENTE_ID" = "CLIENTE"))


























