#####INSUMO PARA GRAFICAR A NIVEL DE BARRIO####

#Author: William Alexander Aguirre Antolínez
#Date: 10/11/2024
#Descripcion: Este script transforma los datos utilizados en la investigación
#sobre el efecto de la migración en el precio de los arriendos, de manera que 
#sirva de insumo para la generación de gráficos a nivel de barrio. 

##Para detalles y comentarios: alexander.aguirre@udea.edu.co



#Paquetes necesarios
library(sf)
library(patchwork)
library(ggthemes)

#Cargar datos consolidados
source("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Scripts/Importar%20datos/Datos%20consolidados%20ECV%20OIME.R")

#Agregar las variables de las ECV
source("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Scripts/Agregaci%C3%B3n%20de%20variables/Variables%20ECV.R")

#Agregar las variables del OIME
source("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Scripts/Agregaci%C3%B3n%20de%20variables/Variables%20OIME.R")


#Cargar el shp
mapa<-read_sf("C:/Users/HP-Laptop/OneDrive - Universidad de Antioquia/Maestría en Economía/Tesis/1. Procesamiento/MigracionMedellin/Data/Shapemaps/shp_limite_barrio_vereda_cata/limite_barrio_vereda_cata.shp")

#Tratar las bases barrio para el left join
bases_barrio<-bases_barrio %>%
  mutate(codigo=as.character(codigoBarrioComuna)) %>% 
  mutate(
    codigo=ifelse(nchar(codigo)<4, 
                  paste0(0,codigo), codigo)
  )

#Unir el mapa con la información de los barrios
mapa_barrios<-mapa %>% left_join(bases_barrio, by="codigo")
mediciones<-sort(unique(mapa_barrios$medicion))

#Filtrar solo para el área urbana
comunas<-c("01","02","03","04",
           "05","06","07","08",
           "09","10","11","12",
           "13","14","15","16")
mapa_barrios<-mapa_barrios %>% filter(indicador_=="U" & comuna %in% comunas)