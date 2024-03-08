#Cargando librerias de R
library(ggplot2)
library(dplyr)
library(biglm)

#setwd("D:/iacd/Documents/Uni/Big data/Tema 4")
setwd("F:/Raíz/uni/big data/Proyecto/datasets")

#Cargando los datasets en dataframes
DT_infectados <- read.csv("covid-data.csv")
DT_escuelas <- read.csv("cierres-de-escuelas.csv")
DT_trabajos <- read.csv("cierres-de-trabajo.csv")
DT_interno <- read.csv("movimiento-interno.csv")
DT_transporteP <- read.csv("transporte-publico.csv")
DT_viajes <- read.csv("viaje-internacional.csv")

head(DT_infectados)

DT_inf_filtrado <- filter(DT_infectados, date >= "2021-01-01" & date <="2021-12-31" & location == "Mexico")
#DT_esc_filtrado <- filter(DT_escuelas, Code == "MEX")
#DT_trab_filtrado <- filter(DT_trabajos, Code == "MEX")
#DT_int_filtrado <- filter(DT_interno, Code == "MEX")
#DT_tranP_filtrado <- filter(DT_transporteP, Code == "MEX")
#DT_via_filtrado <- filter(DT_viajes, Code == "MEX")

write.csv(DT_inf_filtrado,"covid-data-Mex2021.csv")
#write.csv(DT_esc_filtrado,"cierres-de-escuelas-Mex.csv")
#write.csv(DT_trab_filtrado,"cierres-de-trabajo-Mex.csv")
#write.csv(DT_int_filtrado,"movimiento-interno-Mex.csv")
#write.csv(DT_tranP_filtrado,"transporte-publico-Mex.csv")
#write.csv(DT_via_filtrado,"viaje-internacional-Mex.csv")

#Filtrando los datasets POR A?O (2020)
DT_inf_filtrado2020 <- filter(DT_infectados[c(3:9)], date <= "2020-12-31" & total_cases > 0 & location == "Mexico")
DT_esc_filtrado2020 <- filter(DT_escuelas[c(2:4)], Day >= "2020-01-01" & Day <= "2020-12-31" & school_closures > 0 & Code == "MEX")
DT_trab_filtrado2020 <- filter(DT_trabajos[c(2:4)], Day >= "2020-01-01" & Day <= "2020-12-31" & workplace_closures > 0 & Code == "MEX")
DT_int_filtrado2020 <- filter(DT_interno[c(2:4)], Day >= "2020-01-01" & Day <= "2020-12-31" & restrictions_internal_movements > 0 & Code == "MEX")
DT_tranP_filtrado2020 <- filter(DT_transporteP[c(2:4)], Day >= "2020-01-01" & Day >= "2020-12-31" & close_public_transport > 0 & Code == "MEX")
DT_via_filtrado2020 <- filter(DT_viajes[c(2:4)], Day >= "2020-01-01" & Day <= "2020-12-31" & international_travel_controls > 0 & Code == "MEX")

#Filtrando los datasets POR A?O (2021)
DT_inf_filtrado2021 <- filter(DT_infectados[c(3:9)], date >= "2021-01-01" & date <= "2021-12-31" & total_cases > 0 & location == "Mexico")
DT_esc_filtrado2021 <- filter(DT_escuelas[c(2:4)], Day >= "2021-01-01" & Day <= "2021-12-31" & school_closures >0 & Code == "MEX")
DT_trab_filtrado2021 <- filter(DT_trabajos[c(2:4)], Day >= "2021-01-01" & Day <= "2021-12-31" & workplace_closures > 0 & Code == "MEX")
DT_int_filtrado2021 <- filter(DT_interno[c(2:4)], Day >= "2021-01-01" & Day <= "2021-12-31" & restrictions_internal_movements > 0 & Code == "MEX")
DT_tranP_filtrado2021 <- filter(DT_transporteP[c(2:4)], Day >= "2021-01-01" & Day <= "2021-12-31" & close_public_transport > 0 & Code == "MEX")
DT_via_filtrado2021 <- filter(DT_viajes[c(2:4)], Day >= "2021-01-01" & Day <= "2021-12-31" & international_travel_controls > 0 & Code == "MEX")

#Filtrando los datasets POR A?O (2022)
DT_inf_filtrado2022 <- filter(DT_infectados[c(3:9)], date >= "2022-01-01" & total_cases > 0 & location == "Mexico")
DT_esc_filtrado2022 <- filter(DT_escuelas[c(2:4)], Day >= "2022-01-01" & Day <= "2022-03-02" & school_closures > 0 & Code == "MEX")
DT_trab_filtrado2022 <- filter(DT_trabajos[c(2:4)], Day >= "2022-01-01" & Day <= "2022-03-02" & workplace_closures > 0 & Code == "MEX")
DT_int_filtrado2022 <- filter(DT_interno[c(2:4)], Day >= "2022-01-01" & Day <= "2022-03-02" & restrictions_internal_movements > 0 & Code == "MEX")
DT_tranP_filtrado2022 <- filter(DT_transporteP[c(2:4)], Day >= "2022-01-01" & Day <= "2022-03-02" & close_public_transport > 0 & Code == "MEX")
DT_via_filtrado2022 <- filter(DT_viajes[c(2:4)], Day >= "2022-01-01" & Day <= "2022-03-02" & international_travel_controls > 0 & Code == "MEX")

#Datasets de 2020 para pruebas de prediccion
inf_escuela2020 <- merge(x=DT_inf_filtrado2020, y=DT_esc_filtrado2020[c(3)])
inf_trabajo2020 <- merge(x=DT_inf_filtrado2020, y=DT_trab_filtrado2020[c(3)])
inf_interno2020 <- merge(x=DT_inf_filtrado2020, y=DT_int_filtrado2020[c(3)])
inf_transP2020 <- merge(x=DT_inf_filtrado2020, y=DT_tranP_filtrado2020[c(3)])
inf_viajes2020 <- merge(x=DT_inf_filtrado2020, y=DT_via_filtrado2020[c(3)])

inf_1 <- merge(x=DT_inf_filtrado2020, y=DT_esc_filtrado2020[c(3)])
inf_2 <- merge(x=inf_2, y=DT_trab_filtrado2020[c(3)])
inf_3 <- merge(x=inf_3, y=DT_int_filtrado2020[c(3)])
inf_4 <- merge(x=inf_4, y=DT_tranP_filtrado2020[c(3)])
inf_5 <- merge(x=inf_5, y=DT_via_filtrado2020[c(3)])

#Datasets de 2021 para pruebas de prediccion
inf_escuela2021 <- merge(x=DT_inf_filtrado2021, y=DT_esc_filtrado2021[c(3)])
inf_trabajo2021 <- merge(x=DT_inf_filtrado2021, y=DT_trab_filtrado2021[c(3)])
inf_interno2021 <- merge(x=DT_inf_filtrado2021, y=DT_int_filtrado2021[c(3)])
inf_transP2021 <- merge(x=DT_inf_filtrado2021, y=DT_tranP_filtrado2021[c(3)])
inf_viajes2021 <- merge(x=DT_inf_filtrado2021, y=DT_via_filtrado2021[c(3)])

#Datasets de 2022 para pruebas de prediccion
inf_escuela2022 <- merge(x=DT_inf_filtrado2022, y=DT_esc_filtrado2022[c(3)])
inf_trabajo2022 <- merge(x=DT_inf_filtrado2022, y=DT_trab_filtrado2022[c(3)])
inf_interno2022 <- merge(x=DT_inf_filtrado2022, y=DT_int_filtrado2022[c(3)])
inf_transP2022 <- merge(x=DT_inf_filtrado2022, y=DT_tranP_filtrado2022[c(3)])
inf_viajes2022 <- merge(x=DT_inf_filtrado2022, y=DT_via_filtrado2022[c(3)])

######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################

#Funciones de prediccion 2020

plot(inf_escuela2020$new_cases ~ inf_escuela2020$new_deaths , col = "red")

cor.test(inf_escuela2020$new_deaths, inf_escuela2020$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_escuela2020)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred
######################################################################################################
plot(inf_trabajo2020$new_cases ~ inf_trabajo2020$new_deaths , col = "red")

cor.test(inf_trabajo2020$new_deaths, inf_trabajo2020$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_trabajo2020)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred
######################################################################################################
plot(inf_interno2020$new_cases ~ inf_interno2020$new_deaths , col = "red")

cor.test(inf_interno2020$new_deaths, inf_interno2020$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_interno2020)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred
######################################################################################################
plot(inf_transP2020$new_cases ~ inf_transP2020$new_deaths , col = "red")

cor.test(inf_transP2020$new_deaths, inf_transP2020$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_transP2020)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred
######################################################################################################
plot(inf_viajes2020$new_cases ~ inf_viajes2020$new_deaths , col = "red")

cor.test(inf_viajes2020$new_deaths, inf_viajes2020$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_viajes2020)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred

######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################

#Funciones de prediccion 2021

plot(inf_escuela2021$new_cases ~ inf_escuela2021$new_deaths , col = "red")

cor.test(inf_escuela2021$new_deaths, inf_escuela2021$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_escuela2021)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred
######################################################################################################
plot(inf_trabajo2021$new_cases ~ inf_trabajo2021$new_deaths , col = "red")

cor.test(inf_trabajo2021$new_deaths, inf_trabajo2021$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_trabajo2021)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred
######################################################################################################
plot(inf_interno2021$new_cases ~ inf_interno2021$new_deaths , col = "red")

cor.test(inf_interno2021$new_deaths, inf_interno2021$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_interno2021)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred
######################################################################################################
plot(inf_transP2021$new_cases ~ inf_transP2021$new_deaths , col = "red")

cor.test(inf_transP2021$new_deaths, inf_transP2021$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_transP2021)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred
######################################################################################################
plot(inf_viajes2021$new_cases ~ inf_viajes2021$new_deaths , col = "red")

cor.test(inf_viajes2021$new_deaths, inf_viajes2021$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_viajes2021)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred

######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################

#Funciones de prediccion 2022

plot(inf_escuela2022$new_cases ~ inf_escuela2022$new_deaths , col = "red")

cor.test(inf_escuela2022$new_deaths, inf_escuela2022$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_escuela2022)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred
######################################################################################################
plot(inf_escuela2022$new_cases ~ inf_escuela2022$new_deaths , col = "red")

cor.test(inf_escuela2022$new_deaths, inf_escuela2022$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_escuela2022)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred
######################################################################################################
plot(inf_interno2022$new_cases ~ inf_interno2022$new_deaths , col = "red")

cor.test(inf_interno2022$new_deaths, inf_interno2022$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_interno2022)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred
######################################################################################################
plot(inf_transP2022$new_cases ~ inf_transP2022$new_deaths , col = "red")

cor.test(inf_transP2022$new_deaths, inf_transP2022$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_transP2022)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred
######################################################################################################
plot(inf_viajes2022$new_cases ~ inf_viajes2022$new_deaths , col = "red")

cor.test(inf_viajes2022$new_deaths, inf_viajes2022$new_cases)

modelo <- lm(new_cases ~ new_deaths, data = inf_viajes2022)
modelo
abline(modelo, col = "blue")

summary(modelo)

data.frame(new_cases = c(20, 45, 80))
pred <- predict(modelo, data.frame(new_deaths = c(20, 45, 80)), interval = "confidence")
pred