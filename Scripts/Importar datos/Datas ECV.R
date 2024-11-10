#IMPORTAR DATAS###
t1<-Sys.time()
library(tidyverse)

#Función para descargar la data 
download_data<- function(wage,n_chunk){
  
  data<-list() 
  
  chunks <- sprintf("%03d", 1:n_chunk)
  
    for (i in chunks){
  #Poner la ruta base del repositorio 
    ruta<-paste0("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/main/Datos/ECV/",wage,"/ECV_",wage,"_",i,".csv")
  #Descargar los chunk y añadirles la medición  
    data[[i]]<-read.csv2(ruta,header = TRUE,sep = ",") %>%
          mutate(across(everything(), as.character),
           medicion=wage)
  }
  #Unir los chunks
  data<-bind_rows(data)
  id<-c(1:nrow(data))
  data<-data %>% mutate(skPersona=paste0(medicion,"_",id))
  #Nombrar el dataframe de acuerdo a la medición
  assign(paste0("ECV_",wage),data,envir = .GlobalEnv)
}

#2004
download_data(wage = 2004,n_chunk = 9)

#2005
download_data(wage = 2005,n_chunk = 9)

#2006
download_data(wage = 2006,n_chunk = 9)

#2007
download_data(wage = 2007,n_chunk = 9)

#2008
download_data(wage = 2008,n_chunk = 9)

#2009
download_data(wage = 2009,n_chunk = 9)

#2010
download_data(wage = 2010,n_chunk = 6)

#2011
download_data(wage = 2011,n_chunk = 6)

#2012
download_data(wage = 2012,n_chunk = 6)

#2013
download_data(wage = 2013,n_chunk = 6)

#2014
download_data(wage = 2014,n_chunk = 5)

#2015
download_data(wage = 2015,n_chunk = 5)

#2016
download_data(wage = 2016,n_chunk = 4)

#2017
download_data(wage = 2017,n_chunk = 2)

#2018
download_data(wage = 2018,n_chunk = 2)

#2019
download_data(wage = 2019,n_chunk = 2)

#2021
download_data(wage = 2021,n_chunk = 2)

#2022
download_data(wage = 2022,n_chunk = 2)

#2023
download_data(wage = 2023,n_chunk = 2)

object_names <- ls()
df_names <- object_names[sapply(object_names, function(x) is.data.frame(get(x)))]
ECV <- lapply(df_names, get)
names(ECV) <- df_names
rm(list = df_names, envir = .GlobalEnv)

t2<-Sys.time()

tiempo_descarga<-t2-t1
print(tiempo_descarga)