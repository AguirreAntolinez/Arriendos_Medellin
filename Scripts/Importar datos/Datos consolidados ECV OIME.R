#IMPORTAR DATAS###
t1<-Sys.time()
library(tidyverse)


download_data_consolidada<- function(n_chunk){
  
  data<-list() 
  
  chunks <- sprintf("%03d", 1:n_chunk)
  
  for (i in chunks){
    #Poner la ruta base del repositorio 
    ruta<-paste0("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/ECV/Data_Consolidada/data_consolidada_",i,".csv")
    #Descargar los chunk y añadirles la medición  
    data[[i]]<-read.csv2(ruta,header = TRUE,sep = ",") %>%
      mutate(across(everything(), as.character))
  }
  #Unir los chunks
  data_consolidada<-bind_rows(data)
  #Nombrar el dataframe de acuerdo a la medición
  }
download_data_consolidada(n_chunk = 10)

rentas_depurado<-read.csv2("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/RENTAS%20OIME/rentas_depurado.csv",header = TRUE,sep = ";")

t2<-Sys.time()

tiempo_descarga<-t2-t1
tiempo_descarga