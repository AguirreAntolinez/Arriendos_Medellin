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
comunas<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")
#Quitar barrios que no van en el estudio
barrios_me_alto<-c('1005', '1006', '1008', '1011', '1401', '1408',
                   '1418', '1419', '1420', '517', '702', '725')


barrios_mediciones_incompletas<-c('312','408','705','724','805','1007',
                                  '1502','1621')

barrios_sin_personas_en_arriendo<-c('108','917','1103','1414','1416',
                                    '1604','1618')

data_consolidada<- data_consolidada %>% 
  filter(!codigoBarrioComuna %in% barrios_me_alto &
           !codigoBarrioComuna %in% barrios_mediciones_incompletas &
           !codigoBarrioComuna %in% barrios_sin_personas_en_arriendo &
           zona =="U"&
           Cod_comuna %in% comunas
         
  ) 


#Quitar mediciones que no van en el estudio
data_consolidada<-data_consolidada %>% filter(medicion>2007)

#Validar valores unicos
data_consolidada %>%
  group_by(across(c(codigoBarrioComuna, Sexo, GrupoEdad2, medicion))) %>%
  mutate(duplicado = duplicated(across(everything()))) %>%
  ungroup() %>%
  count(duplicado)


#Asignar el factor de expansión
factor_expansion<-read.csv2("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/ECV/Data_Consolidada/factor_expansion.csv",header = TRUE,sep = ",")
factor_expansionCiudad<-read.csv2("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/ECV/Data_Consolidada/factor_expansionCiudad.csv",header = TRUE,sep = ",")


data_consolidada<-data_consolidada %>% 
  mutate(
    Sexo=as.numeric(Sexo),
    codigoBarrioComuna=as.numeric(codigoBarrioComuna),
    medicion=as.numeric(medicion),
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
      Edad >= 80  ~ 17
      , .default = NA),
    GrupoEdad2=case_when(
      GrupoEdad==1 | GrupoEdad==2 | GrupoEdad==3 ~ 1,
      GrupoEdad==4 | GrupoEdad==5 ~ 2,
      GrupoEdad==6 | GrupoEdad==7 ~ 3,
      GrupoEdad==8 | GrupoEdad==9 ~ 4,
      GrupoEdad==10 | GrupoEdad==11 ~ 5,
      GrupoEdad==12 | GrupoEdad==13 ~ 6,
      GrupoEdad==14 | GrupoEdad==15 | GrupoEdad==16  | GrupoEdad==17 ~ 7
      , .default = NA),
    key=paste0(codigoBarrioComuna,"_",Sexo,"_",GrupoEdad2,"_",medicion)
    ) 
%>%
  inner_join(factor_expansion, by =c("codigoBarrioComuna","Sexo","GrupoEdad2","medicion")) %>% 
  #inner_join(factor_expansionCiudad, by =c("medicion","Sexo","GrupoEdad2")) %>% 
  mutate(
    codigoBarrioComunaUnificado=case_when(
      codigoBarrioComuna==315 ~ 314,
      codigoBarrioComuna==915 ~ 914,
      codigoBarrioComuna==916 ~ 914,
      .default = codigoBarrioComuna),
    nombreBarrioUnificado=case_when(
      codigoBarrioComunaUnificado==314~ "San José la Cima",
      codigoBarrioComunaUnificado==914~ "Asomadera",
      .default = nombreBarrio),
    codigoBarrioComunaAnteriorUnificado=case_when(
      codigoBarrioComunaAnterior=="315" ~ "314",
      codigoBarrioComunaAnterior=="915" ~ "914",
      codigoBarrioComunaAnterior=="916" ~ "914",
      .default = codigoBarrioComunaAnterior)
      ) %>% 
  filter(medicion>2007)
  
  

#rentas_depurado<-read.csv2("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/RENTAS%20OIME/rentas_depurado.csv",header = TRUE,sep = ";")

t2<-Sys.time()

tiempo_descarga<-t2-t1
tiempo_descarga