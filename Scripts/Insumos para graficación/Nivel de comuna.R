#####INSUMO PARA GRAFICAR A NIVEL DE COMUNA####

#Author: William Alexander Aguirre Antolínez
#Date: 10/11/2024
#Descripcion: Este script transforma los datos utilizados en la investigación
#sobre el efecto de la migración en el precio de los arriendos, de manera que 
#sirva de insumo para la generación de gráficos a nivel de comuna. 

##Para detalles y comentarios: alexander.aguirre@udea.edu.co



#Paquetes necesarios
library(sf)
library(patchwork)
library(ggthemes)

#Cargar el shp
mapa<-read_sf("C:/Users/HP-Laptop/OneDrive - Universidad de Antioquia/Maestría en Economía/Tesis/1. Procesamiento/MigracionMedellin/Data/Shapemaps/shp_limite_barrio_vereda_cata/limite_barrio_vereda_cata.shp")

#Tratar las bases barrio para el left join
bases_comuna<-bases_comuna %>%
  mutate(comuna=as.character(Cod_comuna)) %>% 
  mutate(
    comuna=ifelse(nchar(Cod_comuna)<2, 
                  paste0(0,comuna), comuna)
  )

#Unir el mapa con la información de los barrios
mapa_comunas<-mapa %>% left_join(bases_comuna, by="comuna")
mediciones<-sort(unique(mapa_comunas$medicion))

#Filtrar solo para el área urbana
comunas<-c("01","02","03","04",
           "05","06","07","08",
           "09","10","11","12",
           "13","14","15","16")
mapa_comunas<-mapa_comunas %>% filter(indicador_=="U" & comuna %in% comunas)