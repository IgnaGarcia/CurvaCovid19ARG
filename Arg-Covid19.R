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

options(scipen = 6) #para evitar notacion cientifica
options(digits=4)

maxValor <- max(ARG_COVID_19$Acum_fi)#maximo valor para dividir luego

#iteracion para los fi, fr, y demas
i <- 1

for (caso in ARG_COVID_19$Acum_fi) {
  if(caso>0){
    ARG_COVID_19$fi[i] <- ARG_COVID_19$Acum_fi[i] - ARG_COVID_19$Acum_fi[i-1]
    ARG_COVID_19$fr[i] <- round(ARG_COVID_19$fi[i] / maxValor, 4)
    ARG_COVID_19$frPorcent[i] <- ARG_COVID_19$fr[i] * 100
  }
  ARG_COVID_19$Acum_fr[i] <- round(ARG_COVID_19$Acum_fi[i] / maxValor, 4)
  ARG_COVID_19$Acum_frPorcent[i] <- round(ARG_COVID_19$Acum_fr[i] * 100, 4)
  i <- i+1
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
