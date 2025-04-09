#instalar y descargar las librerias
library(tidyverse)
library(ggplot2)
library(pyramid)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(fs)


rm(list = ls())
#Subir la base de datos 
library(readxl)
i=10
Distrito <- read_excel("20TOMO_01.xlsx",i)
#Omitir valores NA
Distrito <- na.omit(Distrito)
head(Distrito)
habitantes <- Distrito[c(2:18), c(1:4)]
head(habitantes)

#============================================================
#            PIRAMIDE POBLACIONAL
pob <- habitantes %>%
  mutate(totH = sum(Hombres),
         totM = sum(Mujeres)) %>%
  mutate(Hombres = (Hombres/totH)*100,
         Mujeres = (Mujeres/totM)*100
  ) %>%
  select(-totH, -totM) %>%
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo")
# Elaboracion de la grafica
ggplot(pob, aes(x = `grupo de edades`,
                y = `Poblacion por Sexo`,
                fill = Sexo)) +
  geom_bar(data = subset(pob, Sexo == "Hombres") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           stat = "identity", width = 0.5, fill = "blue") +
  geom_bar(data = subset(pob, Sexo == "Mujeres"),
           stat = "identity", width = 0.5, fill = "pink")  +                                                                 
  coord_flip()  +
  labs(title =            "Piramide Poblacional",
       x = "",
       y = "Hombres              Mujeres",
       caption = "Fuente: INEI") +
  scale_y_continuous(breaks = seq(-12, 12, by = 2), labels = paste0(c(seq(-12, 0, by = 2)*-1, seq(2, 12, by = 2)), "%"))

#=============================================================
#       INDICADORES DE LA ESTRUCTURA DE LA POBLACION
Indicadores<-function(c){
  #edad media
  x<-numeric()
  x[1]<-2.5
  for (i in 2:17) {
    x[i]<-x[i-1]+5
    x[i]
  }
  edad_media<-sum(x*c)/sum(c)
  edad_media
  #Envejecimiento de la poblacion
  Indice_envenjecimiento<-sum(c[14:17])/sum(c[1:3])*100
  Indice_envenjecimiento
  #Tasa de dependecia
  n1<-sum(c[14:17])+sum(c[1:3])
  n2<-sum(c[4:13])
  TDI<-n1/n2*100
  TDI
  
  resultados <- c(edad_media, Indice_envenjecimiento, TDI)
  names(resultados) <- c("Edad media", "Indice de envejecimiento","Tasa de dependencia")
  return(resultados)
}
c<-habitantes$Total
Indicadores(c)

#=====================================================================================================
library(readxl)
teoria_de_la_poblacion <- read_excel("teoria de la poblacion.xlsx")
head(teoria_de_la_poblacion)
teoria_de_la_poblacion=teoria_de_la_poblacion[-c(11),]

#====================================================================================================
#              TASA BRUTA DE NATALIDAD
T_nata_Por_distrito<-teoria_de_la_poblacion$`Nacimientos registrados`[i] /teoria_de_la_poblacion$Poblacion[i] *1000
T_nata_Por_distrito
#=======================================================================================================
library(readxl)
fecundidad <- read_excel("20TOMO_08.xlsx", i,
                           col_types = c("text", "numeric", "numeric", 
                                         "numeric", "numeric"))
#Omitir valores NA
fecundidad<- na.omit(fecundidad)
head(fecundidad)
view(fecundidad)
#============================================================
#                TASA GENERAL DE FECUNDIDAD
Pob_edad_fertil<- sum(fecundidad$`Total de mujeres`[3:9])
Nacidos_vivos<-fecundidad$`Últimos hijos o hijas nacidos vivos en el último año` [1]
TFG<-Nacidos_vivos/Pob_edad_fertil*1000
TFG
#===========================================================
#           TASA DE FECUNDIDAD ESPECIFICA
edad_fertil<-fecundidad$`Total de mujeres`[3:9]
Nacimientos<-fecundidad$`Últimos hijos o hijas nacidos vivos en el último año`[3:9]
Tasa_fecundidad_especifica<-Nacimientos/edad_fertil*1000

tabla_grupos <- data.frame(grupo=as.factor( c("15-19", "20-24", "25-29","30-34", "35-39","40-44", "45-49")),Tasa_fecundidad_especifica)
tabla_grupos

plot(as.numeric(tabla_grupos$grupo), tabla_grupos$Tasa_fecundidad_especifica,type = "b" , col="blue", ylab = "Tasa de fecundidad", xlab = "Grupos de edad", xaxt="n")
axis(1, labels = as.character(tabla_grupos$grupo), at=as.numeric(tabla_grupos$grupo))

#===============================================================
#           EDAD MEDIA DE LA FECUNDIDAD
fx<-Nacimientos/edad_fertil
Marca_de_clase<-c(17.5,22.5,27.5,32.5,37.7,42.5,47.5)
fx_Marca<-fx*Marca_de_clase
EDF<-sum(fx_Marca)/sum(fx)
EDF

#===========================================================
#            TASA GLOBAL DE FECUNDIDAD
TGF<-5*sum(fx)
TGF
#=====================================================================
#           TASA BRUTA DE REPRODUCCION
TBR<-TGF*0.4878
TBR
#=============================================
#           TASA NETA DE REPRODUCCION
TEFF<-fx*0.4878
TEFF_Ajustada<-TEFF*fecundidad$`Total de mujeres`[3:9]
TNR<-sum(TEFF_Ajustada)/1000
TNR
#=============================================
#           PARIDEZ MEDIA
nacimientos1<-fecundidad$`Hijos e hijas nacidos vivos`[3:9]
Paridez_media<-nacimientos1 /edad_fertil

tabla_Paridez <- data.frame(grupo=as.factor( c("15-19", "20-24", "25-29","30-34", "35-39","40-44", "45-49")),Paridez_media)
tabla_Paridez

