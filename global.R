# global.R

library(shiny) # 
library(DT)
library(shinyWidgets)
library(writexl)
library(dplyr)

options(shiny.maxRequestSize = 1024 * 1024^2)  # Aumenta el tamaño máximo de carga a 1 GB
options(shiny.autoreload = TRUE)
options(shiny.autoreload.pattern = glob2rx("server.R"))
options(shiny.port = 9856)

