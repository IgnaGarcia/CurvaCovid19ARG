# Curva de contagios en Argentina

# Bibliotecas a importar
check_packages <- function(packages) {
  if (all(packages %in% rownames(installed.packages()))) {
    TRUE
  } else{
    cat(
      "Instalar los siguientes packages antes de ejecutar el presente script\n",
      packages[!(packages %in% rownames(installed.packages()))],
      "\n"
    )
  }
}
packages_needed <- c("ggplot2", "ggrepel", "plotly", "sqldf",
                     "lubridate", "htmlwidgets" , "RColorBrewer",
                     "grid", "data.table", "readr" )
check_packages(packages_needed)

library(ggplot2)
library(ggrepel)
library(plotly)
library(sqldf)
library(lubridate)
library(htmlwidgets)
library(RColorBrewer)
library(grid)
library(data.table)
# library(reshape2)

####### leer los datos

# URL donde se cargan los datos a adquirir
URL          <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

# time_series_covid19_confirmed_global.csv    este es el archivo que a leer
url_archivo  <- paste(URL,"time_series_covid19_confirmed_global.csv", sep = "")

COVID_19_h   <- read.csv(url_archivo, sep = ",", header = T)

########## preparar los datos
COVID_19_h$Lat  <- NULL
COVID_19_h$Long <- NULL
COVID_19_h$Province.State <- NULL
setnames(COVID_19_h, "Country.Region", "fi")
ARG_COVID_19_H <- sqldf("select * from COVID_19_h where fi LIKE 'Argentina'")
ARG_COVID_19_H$fi <- 0


####pasar de formato "anchos" a "largo"

library(tidyr)
ARG_COVID_19 <- ARG_COVID_19_H %>% gather(fecha, Acum_fi, 2:ncol(ARG_COVID_19_H))

#Formatear fecha
ARG_COVID_19$fecha <- as.Date(ARG_COVID_19$fecha, format = "X%m.%d.%y")
ARG_COVID_19$mes <- as.numeric(format(ARG_COVID_19$fecha,'%m'))
ARG_COVID_19$semana <- strftime(ARG_COVID_19$fecha, format = "%V")
ARG_COVID_19$dia <- as.numeric(format(ARG_COVID_19$fecha,'%d'))

#Reordenar columnas
ARG_COVID_19 <- sqldf("SELECT mes, semana, fecha, fi, Acum_fi FROM ARG_COVID_19 WHERE mes > 2")

#Agregar col para nuevos casos
ARG_COVID_19$fr <- 0 
ARG_COVID_19$frPorcent <- 0
ARG_COVID_19$Acum_fr <- 0
ARG_COVID_19$Acum_frPorcent <- 0
ARG_COVID_19$factor <- 1


options(scipen = 6) #para evitar notacion cientifica
options(digits=4)

#Contador y tope
i <- 1
max <- nrow(ARG_COVID_19)
maxValor <- max(ARG_COVID_19$Acum_fi)
#Recorrer y calcular casos diarios de cada 10.000 habitantes
while (i<=max) {
  if(ARG_COVID_19$Acum_fi[i] > 0){
    ARG_COVID_19$fi[i] <- ARG_COVID_19$Acum_fi[i] - ARG_COVID_19$Acum_fi[i-1]
    ARG_COVID_19$fr[i] <- round(ARG_COVID_19$fi[i] / maxValor, 4)
    ARG_COVID_19$frPorcent[i] <- ARG_COVID_19$fr[i] * 100
    if(ARG_COVID_19$fr[i-1] > 0) ARG_COVID_19$factor[i-1] <- ARG_COVID_19$Acum_fi[i] / ARG_COVID_19$Acum_fi[i-1]
  }
  ARG_COVID_19$Acum_fr[i] <- round(ARG_COVID_19$Acum_fi[i] / maxValor, 4)
  ARG_COVID_19$Acum_frPorcent[i] <- round(ARG_COVID_19$Acum_fr[i] * 100, 4)
  i <- i+1
}

proyeccion <- sqldf("SELECT fecha, fi, Acum_fi, factor FROM ARG_COVID_19 ORDER BY fecha desc LIMIT 1 ")
proyeccion$factor <- 1.027294
j <- 2
while(j<31){
  proyeccion[j,] <- list((proyeccion$fecha[j-1]+1), (proyeccion$fi[j-1]*1.027294), (proyeccion$Acum_fi[j-1]*1.027294), 1.027294)
  j <- j+1
}
#####nuevas tablas con datos agrupados por mes y por semana

## agrupamiento por mes
XMES_COVID_19 <- sqldf( "select mes,
   SUM(fi) fi,
   MAX(Acum_fi) Acum_fi,
   SUM(fr) fr ,
   SUM(frPorcent) frPorcent,
   MAX(Acum_fr) Acum_fr,
   MAX(Acum_frPorcent) Acum_frPorcent
                  from ARG_COVID_19
                  GROUP BY mes
                         ")

XSEMANA_COVID_19 <- sqldf( "select semana, 
   SUM(fi) fi,
   MAX(Acum_fi) Acum_fi,
   SUM(fr) fr ,
   SUM(frPorcent) frPorcent,
   MAX(Acum_fr) Acum_fr,
   MAX(Acum_frPorcent) Acum_frPorcent
                  from ARG_COVID_19
                  GROUP BY semana
                         ")

#######
#   grabar los datos en un archivo .csv
write.csv2(ARG_COVID_19, "datos_XDia_ARG.csv",  row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(XMES_COVID_19, "datos_XMes_ARG.csv",  row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(XSEMANA_COVID_19, "datos_XSemana_ARG.csv",  row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(proyeccion, "datos_ProyeccionCasos_ARG.csv",  row.names = FALSE, fileEncoding = "UTF-8")

############## genero  figura dinamica

#Casos Diarios
xdia <- plot_ly(
  type = "scatter",
  x = ARG_COVID_19$fecha, 
  y = ARG_COVID_19$fi,
  name = 'Casos Diarios',
  mode= "lines"
)
xdia <- xdia %>%
  layout(
    title = "COVID-19 Casos Diarios Argentina",
    labs(caption = "\nFuente: The Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)"),
    xaxis = list(
      title= "Dia",
      type = "date",
      tick0 = min(ARG_COVID_19$fecha),
      tickformat = '%d/%m'
    ),
    yaxis = list(
      title= "Casos",
      tick0 = 0, 
      dtick = 500
    )
  )
xdia

#Casos Mensuales 
xmes <- plot_ly(
  type = "scatter",
  x = XMES_COVID_19$mes, 
  y = XMES_COVID_19$fi,
  name = 'Casos Mensuales',
  mode= "lines+markers"
)
xmes <- xmes %>%
  layout(
    title = "COVID-19 Casos Mensuales Argentina",
    xaxis = list(
      title= "Mes",
      type = "month",
      tick0 = min(XMES_COVID_19$mes), 
      dtick = 1 
    ),
    yaxis = list(
      title= "Casos",
      tick0 = 0, 
      dtick = 10000
    )
  )
xmes


#Casos Semanales 
xsemana <- plot_ly(
  type = "scatter",
  x = strtoi(XSEMANA_COVID_19$semana)-9, 
  y = XSEMANA_COVID_19$fi,
  name = 'Casos Semanales',
  mode= "lines+markers"
)
xsemana <- xsemana %>%
  layout(
    title = "COVID-19 Casos Semanales Argentina",
    xaxis = list(
      title= "Semana",
      type = "number",
      tick0 = min(XSEMANA_COVID_19$semana), 
      dtick = 3 
    ),
    yaxis = list(
      title= "Casos",
      tick0 = min(XSEMANA_COVID_19$fi), 
      dtick = 5000
    )
  )
xsemana

#Casos Acum
fig1 <- plot_ly(data = ARG_COVID_19,  x = ~fecha, y = ~Acum_fi, name = "Casos Totales",
                 type = 'scatter', mode = 'lines') %>%
  layout(title = "Casos Acumulados ARG",
         xaxis = list(title = "Fecha", type = "date",
                      tickmode = "linear", tickformat = "%d/%m", dtick = 86400*10000,
                      tickangle = 75),
         yaxis = list (title = "Casos Acumulados", tickangle = -45))
fig1

#PROYECCION DE CASOS
fig10 <- plot_ly(data = proyeccion,  x = ~fecha, y = ~Acum_fi, name = "Proyeccion",
                 type = 'scatter', mode = 'lines') %>%
  add_trace(x = ~ARG_COVID_19$fecha, y = ~ARG_COVID_19$Acum_fi, name = 'Actualidad', mode = 'lines') %>%
  layout(title = "Proyeccion de contagiados con factor de aumento= 1.027294",
         xaxis = list(title = "Fecha", type = "date",
                      tickmode = "linear", tickformat = "%d/%m", dtick = 86400*10000,
                      tickangle = 75),
         yaxis = list (title = "Casos Acumulados", tickangle = -45))
fig10

#PROYECCION DE CASOS Diarios
fig11 <- plot_ly(data = proyeccion,  x = ~fecha, y = ~fi, name = "Proyeccion",
                 type = 'scatter', mode = 'lines') %>%
  add_trace(x = ~ARG_COVID_19$fecha, y = ~ARG_COVID_19$fi, name = 'Actualidad', mode = 'lines') %>%
  layout(title = "Proyeccion de contagiados con factor de aumento= 1.027294",
         xaxis = list(title = "Fecha", type = "date",
                      tickmode = "linear", tickformat = "%d/%m", dtick = 86400*10000,
                      tickangle = 75),
         yaxis = list (title = "Casos Acumulados", tickangle = -45))
fig11
