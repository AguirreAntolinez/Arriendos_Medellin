#WORKING PAPER ECONOMETRIA ESPACIAL
#Author: William Alexander Aguirre Antolínez
#Date 1-12-2024
#Description: this scrpit contains spatial analysis of the migration effect
#on housing rents in neighborhoods of Medellín.  

#Cargar paquetes
Paquetes<-c("spdep","Ecdat","plm","spatialreg","splm","tidyverse","sf")
sapply(Paquetes, require, character.only=TRUE)  

#Cargar los datos
t1<-Sys.time()

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

mediciones<-c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
comunas0<-c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16")
comunas<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")

data_consolidada<-data_consolidada %>% filter(medicion %in% mediciones & 
                                              Cod_comuna %in% comunas & 
                                              zona=="U")

#Modificar tipos de variables
data_consolidada<-data_consolidada %>% 
  mutate(factorExpHogares=as.numeric(factorExpHogares),
         factorExpViviendas=as.numeric(factorExpViviendas),
         Edad=as.numeric(Edad),
         anios_en_barrio=as.numeric(anios_en_barrio),
         vida_en_barrio=as.numeric(vida_en_barrio),
         vive_arriendo=as.numeric(vive_arriendo),
         valor_arriendo=as.numeric(valor_arriendo),
         vivia_en_otro_pais=as.numeric(vivia_en_otro_pais),
         vivia_en_otro_municipio=as.numeric(vivia_en_otro_municipio),
         vivia_en_otro_barrio=as.numeric(vivia_en_otro_barrio),
         migrante_en_arriendo=as.numeric(migrante_en_arriendo)
  )

table(is.na(data_consolidada$Edad),data_consolidada$medicion)
table(is.na(data_consolidada$Sexo),data_consolidada$medicion)
table(is.na(data_consolidada$codigoBarrioComuna),data_consolidada$medicion)

#Se eliminan los registros que tienen NA en la edad
data_consolidada<-data_consolidada %>% filter(!is.na(Edad))
table(is.na(data_consolidada$Edad),data_consolidada$medicion)

#Asignar el factor de expansión
factor_expansion<-read.csv2("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/ECV/Data_Consolidada/factor_expansion_personas.csv",header = TRUE,sep = ",")

factor_expansion<- factor_expansion %>% mutate(FEP_barrio=as.numeric(FEP_barrio))

data_consolidada<-data_consolidada %>% 
  filter(codigoBarrioComuna!="512") %>% #Se filtra el barrio Oleoducto que en proyecciones del DANE no tiene población
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

table(is.na(data_consolidada$FEP_barrio))
summary(as.numeric(data_consolidada$FEP_barrio))

#Agregar a nivel de barrio
data_barrios<-data_consolidada %>% 
  mutate(base_personas=1) %>%   
  group_by(medicion,codigoBarrioComuna,nombreBarrio,skBarrio) %>% 
  summarise(
    Base_Personas=sum(base_personas),
    Poblacion=sum(FEP_barrio,na.rm = TRUE),
    Base_Viviendas= n_distinct(skVivienda),
    Base_Hogares= n_distinct(skHogar),
    total_migrantes_internal = sum(vivia_en_otro_pais * FEP_barrio, na.rm = TRUE)+1,   
    porcentaje_migrantes_internacionales = (total_migrantes_internal / Poblacion),
    porcentaje_migrantes_internacionales =ifelse(porcentaje_migrantes_internacionales ==0,NA,porcentaje_migrantes_internacionales),
    
    total_migrantes_intermun = sum(vivia_en_otro_municipio * FEP_barrio, na.rm = TRUE)+1,  
    porcentaje_migrantes_intermun = (total_migrantes_intermun / Poblacion),
    porcentaje_migrantes_intermun =ifelse(porcentaje_migrantes_intermun ==0,NA,porcentaje_migrantes_intermun),
    total_migrantes_intraurb = sum(vivia_en_otro_barrio * FEP_barrio, na.rm = TRUE)+1,   
    porcentaje_migrantes_intraurb = (total_migrantes_intraurb / Poblacion),
    portentaje_migrantes_total=(total_migrantes_intermun+total_migrantes_internal) /Poblacion,
    total_viven_arriendo = sum(vive_arriendo * FEP_barrio, na.rm = TRUE) +1,   
    porcentaje_viven_arriendo = (total_viven_arriendo / Poblacion),
    porcentaje_viven_arriendo = ifelse(porcentaje_viven_arriendo==0,NA,porcentaje_viven_arriendo),
    media_arriendo = weighted.mean(valor_arriendo,FEP_barrio, na.rm = TRUE),
    log_arriendo=log(media_arriendo))
    

summary(data_barrios$log_arriendo)
data_barrios<-data_barrios %>% filter(!is.na(media_arriendo)) #Se filtran barrios donde no hay valores de arriendo
summary(data_barrios$log_arriendo)

#Cargar el shape de la ciudad
mapa_barrios<-read_sf("C:/Users/HP-Laptop/OneDrive - Universidad de Antioquia/Maestría en Economía/Tesis/1. Procesamiento/MigracionMedellin/Data/Shapemaps/shp_limite_barrio_vereda_cata/limite_barrio_vereda_cata.shp")

mapa_barrios<-mapa_barrios %>% filter(indicador_=="U" & comuna %in% comunas0)

data_barrios<-data_barrios %>% mutate(codigo=as.numeric(codigo))

mapa_barrios<-mapa_barrios %>% 
  mutate(codigo=as.numeric(codigo)) %>% 
  inner_join(data_barrios,by = "codigo")

# Calcular la tasa de variación de la migración y la población local
mapa_barrios <- mapa_barrios %>%
  mutate(
    total_migrantes=total_migrantes_internal+total_migrantes_intermun+1,
    total_locales=Poblacion - total_migrantes +1 
  ) %>%  
  arrange(codigo, medicion) %>% 
  group_by(codigo) %>% 
  mutate(
    delta_migrantes = (total_migrantes - lag(total_migrantes)) / lag(total_migrantes),
    delta_migrantes_internal= (total_migrantes_internal - lag(total_migrantes_internal)) / lag(total_migrantes_internal),
    delta_migrantes_intermun= (total_migrantes_intermun - lag(total_migrantes_intermun)) / lag(total_migrantes_intermun),
    delta_locales=(total_locales - lag(total_locales)) / lag(total_locales)
    ) %>% 
  filter(all(mediciones %in% medicion)) %>%  #Filtro los barrios que están en todas las mediciones
  ungroup() %>% 
  filter(medicion !="2008") #Se la primera medición que se pierde en el momento de sacar los deltas


table(is.na(mapa_barrios$delta_locales),mapa_barrios$medicion)

summary(mapa_barrios$delta_locales)
summary(mapa_barrios$delta_migrantes)
summary(mapa_barrios$delta_migrantes_intermun)
summary(mapa_barrios$delta_migrantes_intermun)

n_distinct(mapa_barrios$codigo) #Luego de eso quedo con un panel de 218 barrios
n_distinct(mapa_barrios$medicion) #Y 11 barrios

#Se define las formulas

fm1 <- log_arriendo ~ delta_migrantes 
fm2 <- log_arriendo ~ delta_migrantes_internal
fm3 <- log_arriendo ~ delta_migrantes_intermun
fm4 <- delta_locales ~ delta_migrantes_internal + delta_migrantes_intermun



#Crear la matriz de pesos espaciales
data_distinct<-mapa_barrios %>% select(codigo) %>% distinct()

nb_barrios <- poly2nb(data_distinct, 
                      row.names=data_distinct$codigo)

xaborrar <- card(nb_barrios)!=0
table(xaborrar)

data_distinct <- data_distinct[xaborrar,]

nb_barrios <- poly2nb(data_distinct, 
                      row.names=data_distinct$codigo)

nbw_barrios <- nb2listw(nb_barrios)
summary(nbw_barrios)

#Filtrar en el dataframe el barrio que tocó eliminar

data<- mapa_barrios %>% filter(codigo %in% data_distinct$codigo)

model_1 <- spml(formula = fm1, data = data, index = c('codigo','medicion'),
                 listw = nbw_barrios, lag = TRUE, spatial.error = "b",
                 model = "within", effect = "individual",
                 method = "eigen", na.action = na.fail) 


model_2 <- spml(formula = fm2, data = data, index = c('codigo','medicion'),
                listw = nbw_barrios, lag = TRUE, spatial.error = "b",
                model = "within", effect = "individual",
                method = "eigen", na.action = na.fail) 

model_3 <- spml(formula = fm3, data = data, index = c('codigo','medicion'),
                listw = nbw_barrios, lag = TRUE, spatial.error = "b",
                model = "within", effect = "individual",
                method = "eigen", na.action = na.fail) 

model_4 <- spml(formula = fm4, data = data, index = c('codigo','medicion'),
                listw = nbw_barrios, lag = TRUE, spatial.error = "b",
                model = "within", effect = "individual",
                method = "eigen", na.action = na.fail) 


summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)

