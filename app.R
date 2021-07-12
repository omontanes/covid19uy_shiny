# Load packages ----
library(shiny)


# Source helpers ----
source("helpers.R" , encoding = "utf-8")
source("ui.R")
source("server.R")


# Run the app
shinyApp(ui, server)
