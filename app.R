# app.R
source("global.R")

source("ui.R")
source("server.R")

# Ejecutar la aplicación Shiny
app <- shinyApp(ui = ui, server = server)
runApp(app)