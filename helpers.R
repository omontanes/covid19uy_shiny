

graficoR0 <- function(data) {
ggplot(data$R) +
  aes(t_end+12, `Median(R)`) + 
  geom_point(colour = "steelblue") + geom_line(colour = "steelblue") +
  geom_hline(yintercept = 1) + theme_minimal() + 
  xlab("Días desde el primer caso") + 
  ylab("R móvil") +  
  geom_smooth(method = "loess", method.args = list(degree = 2), 
              aes(colour = "Mediana"), size = 1,
              span = .3, se = FALSE) + 
  
  geom_smooth(aes(x = t_end+12, y = `Quantile.0.025(R)`,colour = "Percentil 2.5%"), 
              size = 1, 
              method = "loess", method.args = list(degree = 2), 
              span = .3, se = FALSE) +
  
  geom_smooth(aes(x = t_end+12, y = `Quantile.0.975(R)`,
                  colour = "Percentil 97.5%"), 
              # linetype = "dotted", 
              size = 1,
              method = "loess", method.args = list(degree = 2), 
              span = .3, se = FALSE) +
  scale_discrete_manual(
    values = c("Mediana" = "#B6D4E7",
               "Percentil 2.5%" = "#B0B0B0",
               "Percentil 97.5%" = "#B0B0B0"),
    aesthetics = c("colour", "linetype"),
    name = " "
  ) +
  xlab("Días desde el primer caso") + theme(legend.position = "bottom")
}



fp7dptal <- function(data, input) {
    filtro <- data %>% 
    filter(departamento == input$radio) %>% 
    filter(fecha >= input$dateRangeDptal[1] & fecha <= input$dateRangeDptal[2])
  
    rect_data <- data.frame(xmin_r=min(filtro$fecha),
                            xmax_r=max(filtro$fecha),
                            ymin_r=c(-1,1,10,25),
                            ymax_r=c(1,10,25,Inf),
                            col=c("1","2","3","4")  )
    
    ggp <- ggplot(filtro,aes(x=fecha, y=p7_cada100k) ) + geom_line(size=1) + facet_wrap(~departamento)+
      geom_rect( data=rect_data, inherit.aes = F,
                 aes(xmin=xmin_r,
                     xmax=xmax_r,
                     ymin=ymin_r,
                     ymax=ymax_r,
                     fill=col ),alpha=0.3) + scale_fill_manual(name = "Indice Harvard",values=c("green","yellow", "orange","red") )+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +labs(x="Fecha",y="Casos cada 100 mil habitantes")
  
  
    return(ggp)
}

# 
# #Muertes por año
# covid_app_json %>%
#   mutate(year = format(fecha, "%Y"))%>%
#   group_by(year) %>%
#   summarise(sumfallecimientos = sum(cantFallecidos) ) %>%
#   ggplot(aes(x=year,y=sumfallecimientos, fill=year) )+geom_bar(stat='identity')  +labs(x="Año",y="Cantidad", fill = "Año")+
#   ggtitle("Cantidad de fallecimientos por año")
# 
# 
# #CTI desde Diciembre
# covid_app_json %>%
#   filter(fecha >= '2020-12-01') %>%
#   ggplot(aes(x=fecha,y=cantCTI))+geom_point(colour='steelblue') +
#   geom_smooth(span = 0.2)+labs(x="Fecha",y="Cantidad") +
#   ggtitle("Cantidad de personas en CTI (desde el 1ro de Diciembre)")  + coord_cartesian( ylim = c(0, 120))+theme_light()+
#   annotate("text", x = Sys.Date()-10, y = 0, label = "@guiad_covid") +  theme_light()
