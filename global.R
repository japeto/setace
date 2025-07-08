# global.R

# install.packages(c("shiny", "DT", "shinyWidgets", "writexl", "dplyr"))

library(shiny) # 
library(DT)
library(shinyWidgets)
library(writexl)
library(openxlsx)
library(dplyr)
# library(lubridate)

options(shiny.maxRequestSize = 1024 * 1024^2)  # Aumenta el tamaño máximo de carga a 1 GB
options(shiny.autoreload = TRUE)
options(shiny.autoreload.pattern = glob2rx("server.R"))
options(shiny.port = 9856)
