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

#data_consolidada_respaldo<-data_consolidada
