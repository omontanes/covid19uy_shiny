library(shinydashboard)

library(jsonlite)
library(curl)

library(dplyr)
library(ggplot2)
library(ggsci)
library(eeptools)

library(stringr)
library(tidyverse)

library(shinyWidgets)
library(EpiEstim)
library(lubridate)

library(sf)


  
header <- dashboardHeader(title = "Covid-19 UY")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Datos Nacionales", tabName = "nacionales" ),
    menuItem("Datos Departamentales",  tabName = "departamentales")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "nacionales",
            
            h2("Datos nacionales"),
            h5(textOutput("fecha")),
            fluidRow(
              column(width = 9,
                infoBoxOutput("casosNuevosBox", width = 3),
                infoBoxOutput("testBox", width = 3),
                infoBoxOutput("positividad", width = 3),
                infoBoxOutput("casosActivos", width = 3),
                infoBoxOutput("cantPerCTI", width = 3),
                infoBoxOutput("personasFallecidas", width = 3)
              ),
              box(
                title = "Parámetros", status = "warning", solidHeader = TRUE, width = 3, 
                dateRangeInput('dateRange',
                     label = paste('Rango de fechas:',
                                   'dd/mm/yy' ),
                     start = '2021-01-01', end = Sys.Date()-1 ,
                     min = '2020-03-25', max = Sys.Date(),
                     separator = " - ", format = "dd/mm/yy",
                     startview = 'year', language = 'es', weekstart = 1
                ),
                sliderInput("tam_span", "Tamaño span loess:",min = 0, max = 1, value = 0.2, step = 0.1)
              )
            ),
            fluidRow(
              box(
                title = "P7 por departamento", width = 4, 
                plotOutput("p7")
              ),
              box(
                title = "Casos nuevos", width = 4, 
                plotOutput("cantCasosNuevos")
              ),
              box(
                title = "Test realizados", width = 4, 
                plotOutput("cantTest")
              )
              
              
            ),
            fluidRow( 
              box( align = "center", 
                title = "10 dias", width = 4, 
                tableOutput("tabla")
              ),
              box( 
                title = "Personas en CTI", width = 4, 
                plotOutput("casosCTI")
              ),
              box( 
                title = "Personas fallecidas", width = 4, 
                plotOutput("fallecimientos")
              )
            ),
            fluidRow( 
              box( 
                   title = "Cálculo R0", width = 8, 
                   plotOutput("graficoEpiEst")
              ),
              box( 
                title = "Positividad", width = 4, 
                plotOutput("PlotPositividad")
              )
            )
            
    ),
    
    tabItem(tabName = "departamentales",
            h2("Datos departamentales"),
            fluidPage(
            column(width = 3, 
              fluidRow( 
                box( width = 12,
                  title = "Parámetros", status = "warning", solidHeader = TRUE,  
                  dateRangeInput('dateRangeDptal',
                                 label = paste('Rango de fechas:',
                                               'dd/mm/yy' ),
                                 start = '2021-01-01', end = Sys.Date() ,
                                 min = '2020-03-25', max = Sys.Date(),
                                 separator = " - ", format = "dd/mm/yy",
                                 startview = 'year', language = 'es', weekstart = 1
                  ),
                  sliderInput("tam_span_deptal", "Tamaño span loess:",min = 0, max = 1, value = 0.2, step = 0.1),
                  radioButtons("radio", label = h3("Departamento"),
                               choices = list("Artigas"=	"Artigas",
                                              "Canelones"=	"Canelones",
                                              "Cerro Largo"=	"Cerro Largo",
                                              "Colonia"=	"Colonia",
                                              "Durazno"=	"Durazno",
                                              "Flores"=	"Flores",
                                              "Florida"=	"Florida",
                                              "Lavalleja"=	"Lavalleja",
                                              "Maldonado"=	"Maldonado",
                                              "Montevideo"=	"Montevideo",
                                              "Paysandú"=	"Paysandú",
                                              "Río Negro"=	"Río Negro",
                                              "Rivera"=	"Rivera",
                                              "Rocha"=	"Rocha",
                                              "Salto"=	"Salto",
                                              "San José"=	"San José",
                                              "Soriano"=	"Soriano",
                                              "Tacuarembó"=	"Tacuarembó",
                                              "Treinta y Tres"=	"Treinta y Tres"
                                              ), 
                               selected = "Artigas")
                ),
              )
            ),
            column(width = 9,
                   fluidRow(
                     box( width = 8,
                       title = "Evolución P7",  
                       plotOutput("dptalP7")
                     ),
                     box( width = 4,
                          title = "Últ. 10 días",  
                          tableOutput("tablaDptal")
                     )
                   )
                   ,
                   fluidRow(
                     box( width = 4,
                       title = "Dpto. Casos Nuevos",  
                       plotOutput("dptalcasosNuevos")
                     ),
                     box( width = 4,
                       title = "Dpto. Activos",  
                       plotOutput("dptalActivos")
                     ),
                     box( width = 4,
                       title = "Dpto Fallecidos",
                       plotOutput("dptalacumFallecidos")
                     )
                   )
                   
            )
    )
    )
  ) 
)

ui <- dashboardPage(skin = "purple",title="COVID-19 UY", header, sidebar, body)