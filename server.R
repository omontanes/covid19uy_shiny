server <- function(input, output) {
  source("helpers.R" , encoding = "utf-8")
  #URI Visualizador APP
  uri_datos_covid_app <- "https://raw.githubusercontent.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/master/datos/estadisticasUY.csv"
  uri_datos_p7_app <- "https://raw.githubusercontent.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/master/datos/estadisticasUY_p7.csv"
  uri_datos_covid_dptal <- "https://raw.githubusercontent.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/master/datos/estadisticasUY_porDepto_detalle.csv"
  #GET Datos
  covid_app_json <- read.csv(url(uri_datos_covid_app) , encoding = "UTF-8")
  covid_app_json <- covid_app_json %>% mutate(fecha = as.Date(fecha, format= "%d/%m/%Y") )
  covid_p7_json <- read.csv(url(uri_datos_p7_app), encoding = "UTF-8")
  covid_p7_json <- covid_p7_json %>% mutate(fecha = as.Date(fecha, format= "%d/%m/%Y") )
  covid_p7_json <- covid_p7_json %>% mutate(id= str_replace(covid_p7_json$iso, "-", "."))
  covid_dptal <- read.csv(url(uri_datos_covid_dptal), encoding = "UTF-8")
  covid_dptal <- covid_dptal %>% mutate(fecha = as.Date(fecha, format= "%d/%m/%Y") )

  #Shape file UY

  Uruguay <- st_read("uy-all.shp")


  p7_ayer <- covid_p7_json %>% filter(fecha == tail(covid_p7_json, 1)$fecha)
  p7_ayer <- p7_ayer %>% mutate(franja = case_when(p7_cada100k <  1 ~ 1, 
                                           p7_cada100k >= 1  & p7_cada100k < 10 ~ 2,
                                           p7_cada100k >= 10 & p7_cada100k < 25 ~ 3,
                                          TRUE ~ 4)
                                  )
 
  p2 <- p7_ayer %>% mutate( FID =  id ) 
  UyData <-  merge(Uruguay,p2, by = "FID")
  covid_app_json <- covid_app_json %>% mutate(cantCasosNuevosAjustado= ifelse(cantCasosNuevosAjustado<0, 0,cantCasosNuevosAjustado)) 
  est <- estimate_R(covid_app_json$cantCasosNuevosAjustado , method = "parametric_si",config = make_config(list(mean_si = 3.95, std_si = 4.75)))
 
  
  
  ##### OUTPUTS ##########
  
  output$fecha <- renderText(paste("Datos actualizados al:", tail ( covid_app_json$fecha , 1)) ) 
  output$casosNuevosBox <- renderInfoBox({ infoBox("Casos nuevos", tail ( covid_app_json$cantCasosNuevosAjustado , 1) , icon = icon("plus"),color = "yellow"  ) })
  output$testBox <- renderInfoBox({ infoBox("Tests realizados hoy", tail ( covid_app_json$cantTest , 1) , icon = icon("vial"),color = "purple"  ) })
  output$casosActivos <- renderInfoBox({ infoBox("Casos activos", tail ( covid_app_json$cantPersonasConInfeccionEnCurso , 1) , icon = icon("users"),color = "green"  ) })
  output$cantPerCTI <- renderInfoBox({ infoBox("Pacientes en CTI", tail ( covid_app_json$cantCTI , 1) , icon = icon("procedures"),color = "red"  ) })
  output$personasFallecidas <- renderInfoBox({ infoBox("Pers. Fallecidas hoy", tail ( covid_app_json$cantFallecidos , 1) , icon = icon("heart-broken"),color = "black"  ) })
  output$positividad <- renderInfoBox({ infoBox("Positividad", paste( as.character(tail(covid_app_json$Positividad , 1)*100), "%")  , color = "blue"  ) })
  
  
  
  
  
  output$cantCasosNuevos<-renderPlot({
      covid_app_json %>% 
      filter(fecha >= input$dateRange[1] & fecha <= input$dateRange[2]) %>%
      ggplot( aes(x=fecha,y=cantCasosNuevosAjustado))+geom_point(colour='steelblue') + 
      geom_smooth(span = input$tam_span)+labs(x="Fecha",y="Cantidad") + theme_minimal()
  })
  output$cantTest<-renderPlot({
      covid_app_json %>% 
      filter(fecha >= input$dateRange[1] & fecha <= input$dateRange[2]) %>%
      ggplot(aes(x=fecha,y=cantTest))+geom_point(colour='steelblue') + 
      geom_smooth(span = input$tam_span)+labs(x="Fecha",y="Cantidad") + theme_minimal()
  })
  output$casosCTI<-renderPlot({
      covid_app_json %>% 
      filter(fecha >= input$dateRange[1] & fecha <= input$dateRange[2]) %>%
      ggplot(aes(x=fecha,y=cantCTI))+geom_point(colour='steelblue') + 
      geom_smooth(span = input$tam_span)+labs(x="Fecha",y="Cantidad") + theme_minimal()
  })
  output$fallecimientos<-renderPlot({
    covid_app_json %>% 
    filter(fecha >= input$dateRange[1] & fecha <= input$dateRange[2]) %>%
    ggplot(aes(x=fecha,y=acumFallecidos))+geom_point(colour='steelblue') + 
    geom_smooth(span = input$tam_span)+labs(x="Fecha",y="Cantidad") + theme_minimal()
  })
  output$p7<-renderPlot({
    ggplot(data = UyData) + geom_sf(aes(fill = factor(franja)),  colour = "white") +  
    theme_void() + geom_sf_text(aes(label = p7_cada100k)) +  
    scale_fill_manual(values = c("#00C851", "#ffbb33", "#FF8800","#CC0000"), name= "Indice de Harvard", limits = c("1", "2", "3", "4"), labels = c("Menor a 1", "Entre 1 y 10", "Entre 10 y 25", "Mayor que 25"))
  })
  output$tabla<-renderTable({
    tail(covid_app_json, 10) %>% 
    mutate(fecha = as.character(fecha)) %>% 
    mutate(casos =  as.character(round(cantCasosNuevosAjustado,0))  )%>% 
    mutate( Positividad =   paste(as.character(Positividad*100),"%" ) )%>% 
    rename("Fecha"= fecha, "Casos" = casos ,"Tests" = cantTest ,  "Fallecidos" = cantFallecidos  ) %>% 
    select("Fecha","Casos","Tests", "Fallecidos", "Positividad") 
  })
  output$PlotPositividad<-renderPlot({
    covid_app_json %>% 
      filter(fecha >= input$dateRange[1] & fecha <= input$dateRange[2]) %>%
      ggplot(aes(x=fecha,y= Positividad))+geom_point(colour='steelblue') + 
      geom_smooth(span = input$tam_span)+labs(x="Fecha",y="Porcentaje") + theme_minimal() + scale_y_continuous(labels = scales::percent)
  })
  output$graficoEpiEst<-renderPlot({
    graficoR0(est)
  })
  output$dptalP7<-renderPlot({ 
    fp7dptal(covid_p7_json, input)
  })
  output$dptalcasosNuevos<-renderPlot({ 
    covid_dptal %>% 
      filter(fecha >= input$dateRangeDptal[1] & fecha <= input$dateRangeDptal[2]) %>%
      filter(str_detect(departamento, input$radio)) %>% 
      ggplot(aes(x=fecha,y=cantCasosNuevosCALC))+geom_point(colour='steelblue') + 
      geom_smooth(span = input$tam_span_deptal)+labs(x="Fecha",y="Cantidad") + theme_minimal()
  })
  output$dptalacumFallecidos<-renderPlot({ 
    covid_dptal %>% 
      filter(fecha >= input$dateRangeDptal[1] & fecha <= input$dateRangeDptal[2]) %>%
      filter(str_detect(departamento, input$radio)) %>% 
      ggplot(aes(x=fecha,y=acumFallecidos))+geom_point(colour='steelblue') + 
      geom_smooth(span = input$tam_span_deptal)+labs(x="Fecha",y="Cantidad") + theme_minimal()
  })
  output$dptalActivos<-renderPlot({ 
    covid_dptal %>% 
      filter(fecha >= input$dateRangeDptal[1] & fecha <= input$dateRangeDptal[2]) %>%
      filter(str_detect(departamento, input$radio)) %>% 
      ggplot(aes(x=fecha,y=enCurso))+geom_point(colour='steelblue') + 
      geom_smooth(span = input$tam_span_deptal)+labs(x="Fecha",y="Cantidad") + theme_minimal()
  })
  output$tablaDptal<-renderTable({
    tail(filter(covid_dptal,str_detect(departamento, input$radio)),10) %>% 
      mutate(fecha = as.character(fecha)) %>% 
      rename("Fecha"= fecha, "Activos" = enCurso ,"Nuevos" = cantCasosNuevosCALC ,  "Fallecidos" = cantFallecidos  ) %>% 
      select("Fecha","Activos","Nuevos", "Fallecidos") 
  })
  
}

