## 0. BORRA TODAS LAS VARIABLES DE MEMORIA
rm(list=ls(all=TRUE)) # Elimina todas las variables del entorno de trabajo

# Limpia consola
cat("\014") # Limpia la consola

# Limpiar el área de gráficos
graphics.off() # Limpia el área de gráficos

## APLICANDO LIMPIEZA DE DATOS EN R/RSTUDIO

# 1. Configuramos nuestro directorio
setwd("C:/UPC/DataScience/TP/code") # Establece el directorio de trabajo

# Librerias

install.packages("mlr",dependencies=TRUE)

install.packages("ggplot2",dependencies=TRUE)

install.packages("agricolae",dependencies=TRUE)

install.packages("DescTools",dependencies=TRUE)

install.packages("VIM",dependencies=TRUE)

#Para añadir columnas en un índice especifico
install.packages("tibble",dependencies=TRUE)

#Para facilitar la conversión de las columnas año, mes  y dia a tipo date
install.packages("lubridate",dependencies=TRUE)

install.packages("dplyr")


library(mlr) #summary

library(ggplot2) 

library(agricolae)## tabla de frecuencia

library(DescTools)

library(VIM)

library(tibble)

library(lubridate)

library(dplyr)

# Eliminar datos sin valor ->Leemos el archivo csv con el parámetro na.string = “” para que durante la carga de los datos se
# rellene con NA los valores vacíos o faltantes.

####CARGAR DATOS####

data <- read.csv("hotel_bookings.csv", header = TRUE, sep=',', dec='.', na.strings = "" , stringsAsFactors = FALSE)

# Inspeccionar Data:
str(data)

summary(data)

head(data)


# Ahora debemos limpiar este dataframe con la instrucción na.omit(). Esta instrucción eliminará las
# observaciones (filas) que contengan en algún atributo de tipo string un NA.
data.limpia <- na.omit(data)

# Ver en tabla
View(data.limpia)

# En lugar de utilizar la función na.omit(), podemos utilizar la funcion complete.cases() que nos retorna
# un vector booleano con los valores True o False si encuentra una valor NA
complete.cases(data)

# Calcula el número de casos incompletos en el dataset
incompletos <- sum(!complete.cases(data))
print(incompletos)


##########TIPOS DE DATOS################


####"hotel"####

# "hotel" debe ser factor ya que es una variable categórica
data.limpia$hotel<-as.factor(data.limpia$hotel)
str(data.limpia)

####"is_canceled"####

# "is_canceled" son 0 o 1, y es una variable booleana, así que debe ser logical
unique(data.limpia$is_canceled)

data.limpia$is_canceled<-as.logical(data.limpia$is_canceled)
str(data.limpia)

####"lead_time"####


# "lead_time" debe ser integer, porque son días entre la fecha que se registra una
# reserva y la fecha de llegada al hotel de la reserva por parte del cliente

str(data.limpia)

####"arrival_date_day_of_month", "arrival_date_month", "arrival_date_week_number" y "arrival_date_year"####

# Nos dimos cuenta que "arrival date" tiene 4 columnas que representan:
# año, mes, semana y día(en ese orden). 
# Así que las hemos juntado en una sola columna tipo DATE:

# data.limpia$arrival_date <- dmy(paste(data.limpia$arrival_date_day_of_month,
#                                      data.limpia$arrival_date_month, 
#                                      data.limpia$arrival_date_year, sep = "-"))

# Insertamos en la 4ta posición la columna "arrival_date"
# Ej. dmy("31-Jan-2017")

data.limpia <- data.limpia %>%
  add_column(arrival_date = dmy(paste(data.limpia$arrival_date_day_of_month,
                                      data.limpia$arrival_date_month,
                                      data.limpia$arrival_date_year,
                                      sep = "-")), .after = 3)

####"stays_in_weekend_nights" y "stays_in_week_nights"####

# "stays_in_weekend_nights" y "stays_in_week_nights" deben ser integer, porque
# representan conteos enteros de noches.

unique(data.limpia$stays_in_weekend_nights)

unique(data.limpia$stays_in_week_nights)

str(data.limpia)

####"adults"####

# "adults" debe ser integer, ya que representa número de adultos

unique(data.limpia$adults)

str(data.limpia)

####"children"####

# "children" En este caso, el read.csv lo ha reconocido como char, cuando debe
# ser integer, ya que representa número de niños

# Primero analizamos qué valores están causando el problema de conversión

unique(data$children)

# Obtener los valores únicos y la cantidad de ocurrencias
conteo_children <- table(data.limpia$children)

# Mostrar los valores únicos y la cantidad de ocurrencias
print(conteo_children)

# Nos sale -> [1] "0"  "1"  "2"  "10" "3"  "NA"

# Lo que indica, que se han encontrado valores "NA"

# Convertimos los chr "NA" en NA
data.limpia$children[!grepl("^\\d+$", data.limpia$children)] <- NA

# Convertimos "children" en variable Integer
data.limpia$children<-as.integer(data.limpia$children)
str(data.limpia$children)

# Tenemos que analizar qué hacer con los NA, puede ser
# 1. Borrar las filas con NA
# 2. Hallar la media y reemplazar los NA con ella
# 3. Hallar la mediana y reemplazar los NA con ella
# 4. No se puede aplicar valor aleatoreo ya que no es categórica

# Como "children" es una variable numérica, se puede optar por
# eliminar, media o mediana.
# Calculamos media y mediana, para saber el valor:
mediana_children <- median(data.limpia$children, na.rm = TRUE)

media_children <-mean(data.limpia$children, na.rm = TRUE)

# La mediana nos sale 0, y la media sale 0.103..
# lo cual es un valor que se redondea a 0. 
# Esto se entiende porque al contar 
print(conteo_children)
# 0      1      2      3     10 
# 110796   4861   3652     76      1 
# La gran mayoría de ocurrencias, son 0. Es lógico que la mediana
# salga 0 y la media salga un valor muy cercano a 0.

# Entonces, aplicando tanto la media como la mediana sería
# Reemplazar los valores NA por 0

# Haremos eso, ya que si borramos la filas con NA, a pesar
# De ser pocos datos, son muchas columnas, y se perdería información
# Conviene más utilizar la mediana o media.

# Reemplazar NA con la mediana
data.limpia$children[is.na(data.limpia$children)] <- mediana_children

# Verificamos si hay NA en la variable "children"
sum(is.na(data.limpia$children))

####"babies"####

# "babies" debe ser integer, ya que representa número de bebés

unique(data.limpia$babies)

str(data.limpia)

####"meal"####

unique(data.limpia$meal)

# "meal" es variable categórica, por lo que debe ser tipo Factor
data.limpia$meal<-as.factor(data.limpia$meal)
str(data.limpia)

####"country"####

unique(data.limpia$country)

# En este caso, como "Country" representa el país de origen y está 
# categorizado según el estándar ISO 3155–3:2013, Factor es la elección adecuada
data.limpia$country<-as.factor(data.limpia$country)
str(data.limpia)

####"market_segment"####

unique(data.limpia$market_segment)
# market_segment representa la designación del segmento de mercado al
# que pertenece cada reserva, por lo cual es categórica y debe ser Factor
data.limpia$market_segment<-as.factor(data.limpia$market_segment)
str(data.limpia)

####"distribution_channel"####

unique(data.limpia$distribution_channel)
# "distribution_channel" representa el canal de distribución a través del
# cual se realizó la reserva. Clasifica cómo se vendieron o distribuyeron
# las reservas. Es categórica, por lo que debe ser Factor.
data.limpia$distribution_channel<-as.factor(data.limpia$distribution_channel)
str(data.limpia)

####"is_repeated_guest"####

# "is_repeated_guest" son 0 o 1, y es una variable booleana, así que debe ser logical
data.limpia$is_repeated_guest<-as.logical(data.limpia$is_repeated_guest)
str(data.limpia)

####"previous_cancellations"####

unique(data.limpia$previous_cancellations)
# "previous_cancellations" representa el número de reservas anteriores que fueron
# canceladas por el cliente antes de la reserva actual. Es una variable entera
# que proporciona información sobre el historial de cancelaciones del cliente
# Por lo tanto, debe ser integer

str(data.limpia)

####"previous_bookings_not_cancelled"####

unique(data.limpia$previous_bookings_not_canceled)
# "previous_bookings_not_cancelled" representa el número de reservas anteriores que
# no fueron canceladas por el cliente antes de la reserva actual. Es una variable
# numérica que indica la cantidad de reservas anteriores con el mismo perfil de
# cliente que fueron creadas antes de la reserva actual y que no fueron canceladas.
# Por lo tanto, debe ser integer

str(data.limpia)

####"reserved_room_type"####

unique(data.limpia$reserved_room_type)

# "ReservedRoomType" representa el código del tipo de habitación reservada. 
# Se trata de una variable categórica que indica el tipo de habitación que ha 
# sido reservada por el cliente. El código se presenta en lugar de la 
# designación real con el fin de preservar el anonimato.
# Es Categórica, por lo tanto, debe ser Factor
data.limpia$reserved_room_type<-as.factor(data.limpia$reserved_room_type)
str(data.limpia)

####"assigned_room_type"####

unique(data.limpia$assigned_room_type)

# "assigned_room_type"  representa el código del tipo de habitación asignada
# a la reserva.  A veces, el tipo de habitación asignada puede diferir del tipo 
# de habitación reservada debido a razones operativas del hotel 
# (como el exceso de reservas) o por solicitud del cliente.
# Es Categórica, por lo tanto, debe ser Factor
data.limpia$assigned_room_type<-as.factor(data.limpia$assigned_room_type)
str(data.limpia)

####"booking_changes"####

unique(data.limpia$booking_changes)

# "booking_changes" representa el número de cambios o modificaciones realizadas 
# en la reserva desde el momento en que se ingresó la reserva en el Sistema de 
# Gestión de Propiedades (PMS, por sus siglas en inglés) hasta el momento del 
# check-in o la cancelación.
# Es una variable entera, por ello debe ser Integer
str(data.limpia)

####"deposit_type"####
unique(data.limpia$deposit_type)

# "deposit_type" es una variable categórica que indica si el cliente realizó un depósito para
# garantizar la reserva. Esta variable puede asumir tres categorías:
# 1.No Deposit: Indica que no se realizó ningún depósito. Esto puede significar
# que la reserva se hizo sin ningún tipo de garantía financiera o que se
# utilizaron otros métodos de garantía, como una tarjeta de crédito válida sin cobro previo.
# 2.Non Refund: Indica que se realizó un depósito por un valor igual o superior
# al costo total de la estadía. Esto significa que el depósito es no reembolsable 
# y se considera como un pago completo por adelantado.
# 3.Refundable: Indica que se realizó un depósito por un valor inferior al costo
# total de la estadía. Esto significa que el depósito es reembolsable, 
# lo que sugiere que el cliente tiene la opción de recibir un reembolso si 
# cancela la reserva dentro de ciertos términos y condiciones.
# Como es categórica le corresponde Factor
data.limpia$deposit_type<-as.factor(data.limpia$deposit_type)
str(data.limpia)

####"agent"####
unique(data.limpia$agent)

# "agent" es una variable categórica que representa el ID de la agencia de viajes
# que realizó la reserva.
# En este caso, la categoría "NULL" en "agent" no debe considerarse como un valor
# faltante, sino como "no aplicable". Por ejemplo, si una reserva tiene "Agent"
# definido como "NULL", significa que la reserva no provino de una agencia de viajes. 
# Como es categórica le corresponde Factor
data.limpia$agent<-as.factor(data.limpia$agent)
str(data.limpia)

####"company"####

# "company" representa la compañía que realizó la reserva o es responsable de pagar
# la reserva, Los "NULL" tampoco debería considerarse como un valor faltante,
# sino como "no aplicable".
# Como es categórica le corresponde Factor
data.limpia$company<-as.factor(data.limpia$company)
str(data.limpia)

####"days_in_waiting_list"####

# "days_in_waiting_list" representa el número de días que la reserva 
# estuvo en la lista de espera antes de ser confirmada al cliente.
# Como es una cantidad entera, debe ser Integer
str(data.limpia)

####"customer_type"####

# "customer_type" representa el tipo de reserva y puede asumir una de cuatro categorías:
# Contract: Cuando la reserva tiene una asignación u otro tipo de contrato asociado.
# Group: Cuando la reserva está asociada a un grupo.
# Transient: Cuando la reserva no es parte de un grupo o contrato, y no está asociada a otra reserva transitoria.
# Transient-party: Cuando la reserva es transitoria, pero está asociada al menos a otra reserva transitoria.
# Como es categórica, le corresponde Factor
data.limpia$customer_type<-as.factor(data.limpia$customer_type)
str(data.limpia)

####"adr"####

# "adr" representa la Tarifa Diaria Promedio (Average Daily Rate), como se define
# en el sistema de cuentas uniformes para la industria hotelera de la American 
# Hotel & Lodging Association. Esta métrica proporciona información sobre el precio
# promedio pagado por una habitación de hotel en un día determinado. Es una medida
# importante para evaluar el rendimiento financiero y la gestión de precios en 
# la industria hotelera.
# Como es un valor con decimales, debe tener tipo Numeric
data.limpia$adr<-as.numeric(data.limpia$adr)
str(data.limpia)

####"required_car_parking_spaces"####

unique(data.limpia$required_car_parking_spaces)

# "required_car_parking_spaces" representa el número de espacios de estacionamiento
# para automóviles requeridos por el cliente. Esta variable indica cuántos espacios
# de estacionamiento fueron solicitados por el cliente al momento de hacer la reserva.
# Es útil para comprender las necesidades de estacionamiento de los clientes y para
# la gestión de la disponibilidad de estacionamiento en el hotel.
# Como es una variable numérica entera, debe ser Integer

####"total_of_special_requests"####

unique(data.limpia$total_of_special_requests)

# "total_of_special_requests" representa el número total de solicitudes especiales
# realizadas por el cliente al momento de hacer la reserva. 
# Como es una variable numérica entera, debe ser Integer

####"reservation_status"####

unique(data.limpia$reservation_status)

# "reservation_status" representa el estado más reciente de la reserva 
# y puede tener uno de tres valores:
# "Canceled": La reserva fue cancelada por el cliente.
# "Check-Out": El cliente ha realizado el check-in pero ya ha partido.
# "No-Show": El cliente no se presentó y no informó al hotel sobre el motivo de su ausencia.
# Esta variable proporciona información importante sobre el estado actual de las reservas
# y puede ser útil para realizar seguimientos, gestionar la disponibilidad de 
# habitaciones y evaluar la tasa de cancelación y no presentación.
# Como es categórica, le corresponde Factor
data.limpia$reservation_status<-as.factor(data.limpia$reservation_status)
str(data.limpia)

####"reservation_status_date"####

# "reservation_status_date" representa la fecha en la que se estableció el último 
# estado de la reserva. Esta fecha puede utilizarse en conjunto con la variable 
# "ReservationStatus" para comprender cuándo se canceló la reserva o cuándo
# realizó el cliente el check-out del hotel.
# Es una fecha, así que debe convertirse a Date
data.limpia$reservation_status_date <- as.Date(data.limpia$reservation_status_date, format = "%Y-%m-%d")
str(data.limpia)

summary(data.limpia)

####EVITAR DUPLICACION DE OBSERVACIONES####

duplicated(data.limpia)

data.unica <- unique(data.limpia)

####OUTLIERS####
str(data.unica)

####"lead_time"####
summary(data.unica$lead_time)
boxplot(data.unica$lead_time)$out
boxplot(data.unica$lead_time, boxwex = 0.5)$out

#Observamos que a pesar de haber muchos outliers, son valores que se encuentran
#dentro de los rangos razonables de días entre reserva y llegada del cliente

####"arrival_date_year"####
summary(data.unica$arrival_date_year)
boxplot(data.unica$arrival_date_year)$out
unique(data.unica$arrival_date_year)

####"arrival_date_week_number"####
summary(data.unica$arrival_date_week_number)
boxplot(data.unica$arrival_date_week_number)$out
unique(data.unica$arrival_date_week_number)

####"arrival_date_day_of_month"####
summary(data.unica$arrival_date_day_of_month)
boxplot(data.unica$arrival_date_day_of_month)$out
unique(data.unica$arrival_date_day_of_month)

####"stays_in_weekend_nights"####
summary(data.unica$stays_in_weekend_nights)
boxplot(data.unica$stays_in_weekend_nights)$out
unique(data.unica$stays_in_weekend_nights)

####"stays_in_week_nights"####
summary(data.unica$stays_in_week_nights)
boxplot(data.unica$stays_in_week_nights)$out
unique(data.unica$stays_in_week_nights)

####"adults"####
summary(data.unica$adults)
boxplot(data.unica$adults)$out
unique(data.unica$adults)

fix_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.00, 0.95), na.rm = removeNA)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- mean(x, na.rm = removeNA)
  x
}

sin.outliers <- fix_outliers(data.unica$adults)

par(mfrow = c(1,2))
boxplot(data.unica$adults, main = "adults con Outliers")
boxplot(fix_outliers(data.unica$adults), main = "adults sin Outliers
")

#data.unica$adults <- as.integer(fix_outliers(data.unica$adults))

View(data.unica)

summary(data.unica$adults)
boxplot(data.unica$adults)$out
unique(data.unica$adults)


####"children"####
summary(data.unica$children)
boxplot(data.unica$children)$out
unique(data.unica$children)

# Analizar outliers para babies
summary(data.unica$babies)
boxplot(data.unica$babies)$out
unique(data.unica$babies)

# Analizar outliers para previous_cancellations
summary(data.unica$previous_cancellations)
boxplot(data.unica$previous_cancellations)$out
unique(data.unica$previous_cancellations)

# Analizar outliers para previous_bookings_not_canceled
summary(data.unica$previous_bookings_not_canceled)
boxplot(data.unica$previous_bookings_not_canceled)
unique(data.unica$previous_bookings_not_canceled)

cont_pbnc <- table(data.unica$previous_bookings_not_canceled)
print(cont_pbnc)

# Analizar outliers para booking_changes
summary(data.unica$booking_changes)
boxplot(data.unica$booking_changes)
unique(data.unica$booking_changes)

print(table(data.unica$booking_changes))

# Analizar outliers para days_in_waiting_list
summary(data.unica$days_in_waiting_list)
boxplot(data.unica$days_in_waiting_list)
unique(data.unica$days_in_waiting_list)
print(table(data.unica$days_in_waiting_list))

# Analizar outliers para adr
summary(data.unica$adr)
boxplot(data.unica$adr)$out
unique(data.unica$adr)

# Calcular la media de los valores válidos de adr (menores o iguales a 1000)
mean_adr <- mean(data.unica$adr[data.unica$adr <= 1000], na.rm = TRUE)

# Reemplazar los valores de adr mayores que 1000 con la media, dejando los demás valores intactos
data.unica$adr <- ifelse(data.unica$adr > 1000, mean_adr, data.unica$adr)

# Analizar outliers para required_car_parking_spaces
summary(data.unica$required_car_parking_spaces)
boxplot(data.unica$required_car_parking_spaces)$out
unique(data.unica$required_car_parking_spaces)

# Analizar outliers para total_of_special_requests
summary(data.unica$total_of_special_requests)
boxplot(data.unica$total_of_special_requests)
unique(data.unica$total_of_special_requests)
print(table(data.unica$total_of_special_requests))

####EXPORTAR DATAFRAME LIMPIO####
write.csv(data.unica,"hotel_bookings_limpio.csv", row.names = FALSE)

#PREGUNTAS DEL INFORME



# 1. Contar reservas por tipo de hotel
r_tipo_hotel <- table(data.limpia$hotel)
View(r_tipo_hotel)

# Ver en tabla
View(data.unica)

# Ver en tabla
View(data)


str(data.limpia)

str(data.unica)


