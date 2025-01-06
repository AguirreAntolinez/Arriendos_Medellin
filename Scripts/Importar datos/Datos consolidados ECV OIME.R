#IMPORTAR DATAS###
t1<-Sys.time()
library(tidyverse)

options(timeout = 120)

data_consolidada<-list() 
  
chunks <- sprintf("%03d", 1:10)
  
for (i in chunks){
  #Poner la ruta base del repositorio 
  ruta<-paste0("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/ECV/Data_Consolidada/data_consolidada_",i,".csv")
  #Descargar los chunk y añadirles la medición  
  data_consolidada[[i]]<-read.csv2(ruta,header = TRUE,sep = ",") %>%
    mutate(across(everything(), as.character))
  }

#Unir los chunks
data_consolidada<-bind_rows(data_consolidada)

#Asignar el factor de expansión
factor_expansion<-read.csv2("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/ECV/Data_Consolidada/factor_expansion.csv",header = TRUE,sep = ",")

data_consolidada<-data_consolidada %>% 
  mutate(
    GrupoEdad=case_when(
      Edad <=  4 ~ 1,
      Edad >= 5 & Edad <=9 ~ 2,
      Edad >= 10 & Edad <=14 ~ 3,
      Edad >= 15 & Edad <=19 ~ 4,
      Edad >= 20 & Edad <=24 ~ 5,
      Edad >= 25 & Edad <=29 ~ 6,
      Edad >= 30 & Edad <=34 ~ 7,
      Edad >= 35 & Edad <=39 ~ 8,
      Edad >= 40 & Edad <=44 ~ 9,
      Edad >= 45 & Edad <=49 ~ 10,
      Edad >= 50 & Edad <=54 ~ 11,
      Edad >= 55 & Edad <=59 ~ 12,
      Edad >= 60 & Edad <=64 ~ 13,
      Edad >= 65 & Edad <=69 ~ 14,
      Edad >= 70 & Edad <=74 ~ 15,
      Edad >= 75 & Edad <=79 ~ 16,
      Edad >= 80  ~ 17),
    GrupoEdad2=case_when(
      GrupoEdad==1 | GrupoEdad==2 | GrupoEdad==3 ~ 1,
      GrupoEdad==4 | GrupoEdad==5 ~ 2,
      GrupoEdad==6 | GrupoEdad==7 ~ 3,
      GrupoEdad==8 | GrupoEdad==9 ~ 4,
      GrupoEdad==10 | GrupoEdad==11 ~ 5,
      GrupoEdad==12 | GrupoEdad==13 ~ 6,
      GrupoEdad==14 | GrupoEdad==15 | GrupoEdad==16  | GrupoEdad==17 ~ 7),
    key=paste0(codigoBarrioComuna,"_",Sexo,"_",GrupoEdad2,"_",medicion)
    ) %>%
  left_join(factor_expansion, by = "key")

rentas_depurado<-read.csv2("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/RENTAS%20OIME/rentas_depurado.csv",header = TRUE,sep = ";")

t2<-Sys.time()

tiempo_descarga<-t2-t1
tiempo_descarga