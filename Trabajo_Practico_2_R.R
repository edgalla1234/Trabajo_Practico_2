library(ggplot2)
library(dplyr)
library(lubridate)

##dataset >> https://drive.google.com/file/d/1flV8VZrT4-O2PrRd10VHzNyAWD509Vc-/view?usp=drive_link ## 

#Primero descargo el dataset y creo el objeto# (tambien podria haber usado el link)

csv_hoteles <- "C:\\Users\\juanb\\OneDrive\\Documents\\Directorio de Trabajo_R\\Datasets\\6_reservas_hoteles.csv"
reservas<- read.table(csv_hoteles, header =T, sep = ",", stringsAsFactors = T)

#luego hago una recorrida por las variables, los encabezados para identificar los tipos de datos que asigno R por defecto.
#para iniciar el EDA

head(reservas) # para ver los primeros registros del conjunto de datos y comprender la estructura.#
View(head(reservas))
str(reservas)
View(reservas)
summary(reservas)

resumen_avg_price<- summary(reservas$avg_price_per_room)
resumen_avg_price
plot(reservas$booking_status)


##Histograma de la variable $arrival_date# y $no_of_special_request

Histograma_Dias <- hist(reservas$arrival_date, main = "Distribucion de dias", xlab = "Dias", ylab = "Frecuencia")
Histograma_Pedidos_Especiales <- hist(reservas$no_of_special_requests, main = "Pedidos Especiales", xlab = "Pedidos", ylab = "frecuencia")

hist(reservas$avg_price_per_room)

#grafico de densidad variable precio promedio
densidad_PPR <- density(reservas$avg_price_per_room)
plot(densidad_PPR)

plot(density(reservas$avg_price_per_room), main = "precio promedio reserva", xlab = "precio promedio", col = "blue")


Histograma_Precio_Promedio <- hist(reservas$avg_price_per_room, main = "Precio Promedio", 
                                   xlab = "precio promedio de la reserva",
                                   ylab = "frecuencia",
                                   col = "blue",
                                   breaks = 100,
                                   xlim = c(0,600))



#grafico de dispersion entre 'tipo de habitaciones' y 'precios promedio'#

Dispersion <-plot(reservas$room_type_reserved, reservas$avg_price_per_room, main = "Relación entre tipo de habitacion y precio", xlab = "habitaciones", ylab= "precio")

##Creo una matriz de correlacion entre variables del dataset#

matriz_cor <- cor(reservas[,c("no_of_week_nights", "no_of_weekend_nights", "avg_price_per_room")])
matriz_cor

library("ggplot2")
library("magrittr")


#Otros analisis exploratorios#

Promedio_costo_por_Habit <-mean(reservas$avg_price_per_room)
Promedio_costo_por_Habit
Maximo_monto_Reserva <-max(reservas$avg_price_per_room)
Maximo_monto_Reserva


## analizado el dataset, las preguntas que me hago son las siguientes ##

## ¿Cual es el periodo de mayor demanda del Hotel?##
## ¿Como se distribuyen las reservas en el año?##
## ¿Cual es el precio promedio de reservas segun segmentos de mercado? ##
## ¿Cual es la estadpia promedio?  ##


boxplot(reservas$avg_price_per_room)
boxplot(reservas$avg_price_per_room ~ factor(reservas$market_segment_type),
        main = "precio promedio por segmento",
        xlab = "segmento de mercado",
        ylab = "precio promedio"
        )

ggplot(data = reservas)
reservas %>%
  ggplot(mapping = aes(x=avg_price_per_room, y=market_segment_type)) +
  geom_point(mapping = aes(size=no_of_children),
             alpha=0.3,
             color= "red")

Histograma_segmento_ <- plot(reservas$market_segment_type,
                             main = "Segmento",
                             xlab = "Segmento",
                             ylab = "Frecuencia",
                             pch = 2,
                             col = "gray",
                             ylim= c(1,26000))

grafico_demanda <- hist(reservas$arrival_month, main ="Mes arribo", 
                        xlab = "Mes Arribo",
                        ylab = "frecuencia",
                        col = "red",
                        breaks = 10,
                        xlim = c(1,12))

#Analisis de noches de estadia#
noches_estadia <- c (reservas$no_of_week_nights + reservas$no_of_weekend_nights)
noches_estadia  
class(noches_estadia) <- as.numeric(noches_estadia)
noches_estadia
str(noches_estadia)
summary(noches_estadia)

Hist_noches_estadia <- hist(noches_estadia, main = "noches de estadia",
                               xlab = "Noches",
                               ylab = "frecuencia",
                               col = "blue",
                               breaks = 30,
                               xlim = c(0,25)
)

##Densidad Noches de estadia ##
densidad_estadia <- density(noches_estadia)
plot(densidad_estadia)

Plot2<- plot(reservas$booking_status, reservas$avg_price_per_room,
              main = "relación entre estado de la reserva vs precio promedio",
              xlab = "estado de la reserva",
              ylab = "precio promedio",
              pch = 2,
              col = "red")

Plot3<- plot(reservas$market_segment_type, noches_estadia,
              main ="relación entre segmento del mercado y noches de estadia",
              xlab = "segmento del mercado",
              ylab = "noches de estadia",
              col = "black")

Plot4 <- plot(reservas$room_type_reserved, noches_estadia,
              main = "relación entre tipo de habitación y noches de estadia",
              xlab = "habitación reservada",
              ylab = "noches de estadia",
              pch = 2,
              col = "green")


set.seed(1)
x <- reservas$avg_price_per_room
y <- noches_estadia

scatter_Plot_1 <- plot(x,y,pch=19, col="lightblue")
                  abline(lm(y ~ x), col = "red", lwd = 3)
                  text(paste("Correlacion", round (cor(x,y),2)), x = 15, y = 90)     
                       
                  

## Github = https://github.com/edgalla1234/Trabajo_Practico_2 ##
