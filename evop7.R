#URI Visualizador APP



uri_datos_euy <- "http://guiad.psico.edu.uy:3000/estadisticasUY"


covid_p7_json <- fromJSON(uri_datos_euy) 
covid_p7_json <- covid_p7_json %>% mutate(fecha = as.Date(fecha, format= "%Y-%m-%d") )

ggplot(covid_p7_json, aes(x=covid_p7_json$cantCasosNuevos, y=cantTest)) + geom_point()

uri_datos_p7_app <- "http://guiad.psico.edu.uy:3000/p7"
#GET Datos
covid_p7_json <- fromJSON(uri_datos_p7_app) 
covid_p7_json <- covid_p7_json %>% mutate(fecha = as.Date(fecha, format= "%Y-%m-%d") )


coso <- covid_p7_json %>%
  filter (fecha > as.Date('2021-02-01', format= "%Y-%m-%d")  )

rect_data <- data.frame(xmin_r=min(coso$fecha),
                        xmax_r=max(coso$fecha),
                        ymin_r=c(-1,1,10,25),
                        ymax_r=c(1,10,25,Inf),
                        col=c("1","2","3","4")  )


  
  
  ggplot(coso,aes(x=fecha, y=p7_cada100k) ) + geom_line(size=1) + facet_wrap(~departamento)+
  geom_rect( data=rect_data, inherit.aes = F,
                            aes(xmin=xmin_r,
                                xmax=xmax_r,
                                ymin=ymin_r,
                                ymax=ymax_r,
                                fill=col ),alpha=0.3) + scale_fill_manual(values=c("green","yellow", "orange","red") )+
    labs(x="Fecha",y="Cantidad",title= "Evolución del P7 a nivel departamental", subtitle = "Promedio de cantidad de casos nuevos cada 100 mil habitantes, en los últimos 7 días") + 
    theme_minimal() +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  theme(legend.position = "none") 
 
  
  
  
  
  
  
  
  coso <- covid_p7_json %>%
    filter (fecha > as.Date('2020-11-01', format= "%Y-%m-%d") , fecha < as.Date('2021-01-01', format= "%Y-%m-%d") )
  
  
  
  
  

  dptos %>% 
    ggplot( aes(x=p7_cada100k, y=(CantVacunas/poblacion)*10000, label = departamento )  ) + geom_point() +  geom_text(hjust = 0, nudge_x = 0.05) +    geom_smooth(method = "lm")
  
  