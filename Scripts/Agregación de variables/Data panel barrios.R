####DATA PANEL BARRIOS######

#Author: William Alexander Aguirre Antolínez
#Date: 05/01/2025
#Description: Este script realiza la agregación de variables a nivel de barrio
#para las mediciones entre el 2004 y 2019 de la ECV de Medellín. 
#Estos datos se utilizan para la investigación sobre el efecto de la migraición
#sobre el precio de los arriendos.

##Para detalles y comentarios: alexander.aguirre@udea.edu.co

#Modificar tipos de variables
data_consolidada<-data_consolidada %>% 
  mutate(FEP_barrio=as.numeric(FEP_barrio),
         factorExpHogares=as.numeric(factorExpHogares),
         factorExpViviendas=as.numeric(factorExpViviendas),
         Edad=as.numeric(Edad),
         anios_en_barrio=as.numeric(anios_en_barrio),
         vida_en_barrio=as.numeric(vida_en_barrio),
         vive_arriendo=as.numeric(vive_arriendo),
         valor_arriendo=as.numeric(valor_arriendo),
         vivia_en_otro_pais=as.numeric(vivia_en_otro_pais),
         vivia_en_otro_municipio=as.numeric(vivia_en_otro_municipio),
         vivia_en_otro_barrio=as.numeric(vivia_en_otro_barrio),
         migrante_en_arriendo=as.numeric(migrante_en_arriendo),
         Estrato=as.numeric(Estrato),
         posee_gas=as.numeric(posee_gas),
         posee_aseo=as.numeric(posee_aseo),
         total_cuartos=as.numeric(total_cuartos),
         cuartos_dormir=as.numeric(cuartos_dormir),
         posee_sanitario=as.numeric(posee_sanitario),
         cantidad_personas_hogar=as.numeric(cantidad_personas_hogar),
         personas_cuarto=as.numeric(personas_cuarto),
         personas_cuarto=ifelse(is.infinite(personas_cuarto),cantidad_personas_hogar,personas_cuarto),
         hacinamiento=as.numeric(hacinamiento),
         posee_energia=as.numeric(posee_energia),
         posee_acueducto=as.numeric(posee_acueducto),
         posee_alcantarillado=as.numeric(posee_alcantarillado),
         paredes_material=as.numeric(paredes_material),
         pisos_material=as.numeric(pisos_material),
         servicios=as.numeric(servicios),
         material_vivienda=as.numeric(material_vivienda)
         )
         
  
# Agregar las variables de personas a nivel de barrio
Personas_barrio<-data_consolidada %>%
  mutate(
    base_personas=1,
    vivia_en_otro_pais = vivia_en_otro_pais * FEP_barrio,
    vivia_en_otro_municipio = vivia_en_otro_municipio * FEP_barrio,
    vivia_en_otro_barrio = vivia_en_otro_barrio * FEP_barrio,
    vive_arriendo = vive_arriendo * FEP_barrio,
    cambio_barrio = case_when(
        anios_en_barrio>=1~0,
        .default = 1),
    cambio_barrio = cambio_barrio * FEP_barrio) %>%
  group_by(medicion,codigoBarrioComunaUnificado,nombreBarrioUnificado) %>%
  summarise(
    Base_Personas=sum(base_personas, na.rm = TRUE),
    Poblacion=sum(FEP_barrio,na.rm = TRUE),
    Base_Viviendas= n_distinct(skVivienda),
    Base_Hogares= n_distinct(skHogar),
    total_migrantes_internal = sum(vivia_en_otro_pais, na.rm = TRUE),
    total_migrantes_intermun = sum(vivia_en_otro_municipio, na.rm = TRUE),
    anios_en_barrio=weighted.mean(x = anios_en_barrio,w = FEP_barrio,na.rm=TRUE),
    total_migrantes_intraurb = sum(vivia_en_otro_barrio, na.rm = TRUE),
    total_viven_arriendo = sum(vive_arriendo, na.rm = TRUE),
    total_cambio_barrio = sum(cambio_barrio, na.rm = TRUE)
    ) %>% 
  mutate(porcentaje_migrantes_internacionales = (total_migrantes_internal / Poblacion),
         porcentaje_migrantes_intermun = (total_migrantes_intermun / Poblacion),
         porcentaje_migrantes_intraurb = (total_migrantes_intraurb / Poblacion),
         porcentaje_viven_arriendo = (total_viven_arriendo / Poblacion),
         siguen_en_barrio=Poblacion-total_cambio_barrio
         )



Personas_barrio <- Personas_barrio %>%
  group_by(codigoBarrioComunaUnificado) %>%  # Agrupar por barrio/comuna
  arrange(codigoBarrioComunaUnificado, medicion) %>%  # Ordenar por año
  mutate(PoblacionAnterior = lag(Poblacion),
         MigrantesAnterior = lag(total_migrantes_internal),
         MigrantesAcumulados= cumsum(total_migrantes_internal),
         Poblacion2019= max(if_else(medicion == 2019, Poblacion, NA_real_), na.rm = TRUE))  # Crear columna con valor anterior


Personas_barrio<- Personas_barrio %>% mutate(
  delta_migracion= total_migrantes_internal-MigrantesAnterior,
  
  tasa_migracion_delta=delta_migracion/PoblacionAnterior,
  
  tasa_permanencia=siguen_en_barrio/Poblacion,
  tasa_permanencia2=siguen_en_barrio/PoblacionAnterior,
  tasa_migracion=total_migrantes_internal/PoblacionAnterior)

Personas_barrio <- Personas_barrio %>%
  group_by(codigoBarrioComunaUnificado) %>%  # Agrupar por barrio/comuna
  arrange(codigoBarrioComunaUnificado, medicion) %>%  # Ordenar por año
  mutate(tasa_migracion_anterior=lag(tasa_migracion))


Viviendas_barrio<-data_consolidada %>% filter(!is.na(skBarrio)) %>% 
  select(codigoBarrioComunaUnificado,
         skVivienda,
         factorExpViviendas,
         medicion,
         Estrato,
         posee_gas,
         posee_aseo,
         posee_sanitario,
         posee_energia,
         posee_acueducto,
         posee_alcantarillado,
         total_cuartos,
         cantidad_personas_hogar,
         personas_cuarto,
         cuartos_dormir,
         hacinamiento,
         paredes_material,
         pisos_material,
         servicios,
         material_vivienda
  ) %>% 
  distinct() %>% 
  group_by(codigoBarrioComunaUnificado,medicion) %>% 
  summarise(Viviendas=sum(factorExpViviendas),
            
            Estrato_promedio=mean(Estrato,na.rm=TRUE),
            
            Estrato_alto=ifelse(round(Estrato_promedio)>4,1,0),
            
            Estrato_medio=ifelse(round(Estrato_promedio)>2 & round(Estrato_promedio)<5 ,1,0),
            
            posee_gas=sum(posee_gas*factorExpViviendas),
            por_posee_gas=posee_gas/Viviendas,
            
            posee_aseo=sum(posee_aseo*factorExpViviendas),
            por_posee_aseo=posee_aseo/Viviendas,
            
            posee_sanitario=sum(posee_sanitario*factorExpViviendas),
            por_posee_sanitario=posee_sanitario/Viviendas,
            
            posee_energia=sum(posee_energia*factorExpViviendas),
            por_posee_energia=posee_energia/Viviendas,
            
            posee_acueducto=sum(posee_acueducto*factorExpViviendas),
            por_posee_acueducto=posee_acueducto/Viviendas,
            
            posee_alcantarillado=sum(posee_alcantarillado*factorExpViviendas),
            por_posee_alcantarillado=posee_alcantarillado/Viviendas,
            
            total_cuartos=mean(total_cuartos, na.rm=TRUE),
            cuartos_dormir=mean(cuartos_dormir, na.rm=TRUE),
            
            cantidad_personas_hogar=mean(cantidad_personas_hogar,na.rm=TRUE),
            personas_cuarto=mean(personas_cuarto,na.rm=TRUE),
            
            hacinamiento=sum(hacinamiento*factorExpViviendas),
            por_hacinamiento=hacinamiento/Viviendas,
            
            paredes_material=sum(paredes_material*factorExpViviendas),
            por_paredes_material=paredes_material/Viviendas,
            
            pisos_material=sum(pisos_material*factorExpViviendas),
            por_pisos_material=pisos_material/Viviendas,
            
            servicios=mean(servicios, na.rm=TRUE),
            
            material_vivienda=sum(material_vivienda*factorExpViviendas),
            por_material_vivienda=material_vivienda/Viviendas
  )


ipc<-read.csv2("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/IPC/IPC.csv")
ipc<-ipc %>% rename(medicion=Año) %>% select(medicion,Indice) 

Hogares_barrio<-data_consolidada %>% filter(!is.na(codigoBarrioComunaUnificado) & !is.na(valor_arriendo)  ) %>% 
  select(codigoBarrioComunaUnificado,medicion,skHogar,valor_arriendo,factorExpHogares #,FEP_barrio
  ) %>% 
  distinct() %>% 
  group_by(codigoBarrioComunaUnificado,medicion) %>% 
  summarise(Hogares=sum(factorExpHogares),
            media_arriendo=mean(valor_arriendo,na.rm=TRUE)
  ) %>% 
  inner_join(ipc, by="medicion") %>% 
  mutate(
    media_arriendo_real=(media_arriendo/Indice)*100,
    log_arriendo=log(media_arriendo_real))


viviendas<-read.csv2("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/ESTADISTICAS%20CATASTRALES/Viviendas.csv")

viviendas<-viviendas %>% 
  mutate(across(starts_with("X"),as.numeric)) %>% 
  pivot_longer(cols = starts_with("X"),
               names_to = "medicion",
               values_to = "viviendas") %>% 
  mutate(medicion=as.character(sub("X","",medicion)),
         medicion=as.numeric(medicion),
         codigoBarrioComuna=as.character(codigoBarrioComuna)) %>% 
  select(codigoBarrioComuna,medicion,viviendas) %>% 
  mutate(
    codigoBarrioComunaUnificado=case_when(
      codigoBarrioComuna=="315" ~ 314,
      codigoBarrioComuna=="915" ~ 914,
      codigoBarrioComuna=="916" ~ 914,
      .default = as.numeric(codigoBarrioComuna)
    )
  ) %>% 
  select(codigoBarrioComunaUnificado,medicion,viviendas) %>% 
  group_by(codigoBarrioComunaUnificado,medicion) %>% 
  summarise(viviendas=sum(viviendas,na.rm = TRUE)) 
  


# tasa_salida<- data_consolidada %>% 
#   mutate(
#     cambio_barrio=case_when(
#       vivia_en_otro_pais==0 &
#       vivia_en_otro_municipio==0 &
#       vivia_en_otro_barrio==0 ~0, .default = 1),
#     cambio_barrio_ponderado=cambio_barrio*FEP_barrio
#     ) %>% 
#   arrange(codigoBarrioComunaUnificado, medicion) %>% 
#   group_by(medicion,codigoBarrioComunaUnificado) %>% 
#   summarise(poblacion=sum(FEP_barrio, na.rm = TRUE),
#             cambio_barrio=sum(cambio_barrio_ponderado, na.rm = TRUE),
#             ) %>% 
#   group_by(codigoBarrioComunaUnificado) %>% 
#   mutate(
#     poblacion_anterior=lag(poblacion,n=1)
#     ) %>% 
#   ungroup() %>% 
#   mutate(
#     tasa_salida=cambio_barrio/poblacion_anterior
#     ) %>% 
#   select(medicion,codigoBarrioComunaUnificado,poblacion_anterior,tasa_salida)
#   
#   

#Consolidar el panel de barrios
data_barrios<-Personas_barrio %>% 
  left_join(Viviendas_barrio,by=c("codigoBarrioComunaUnificado","medicion")) %>%   
  left_join(Hogares_barrio,by=c("codigoBarrioComunaUnificado","medicion")) %>%   
  left_join(viviendas,by=c("codigoBarrioComunaUnificado","medicion")) #%>% 
#  left_join(tasa_salida,by=c("codigoBarrioComunaUnificado","medicion")) 


#Aqui se rellenan mientras tantos los NA con la cantidad de viviendas de 2014
data_barrios <- data_barrios %>% 
  group_by(codigoBarrioComunaUnificado) %>%  
  mutate(viviendas = ifelse(is.na(viviendas), viviendas[medicion == 2014], viviendas)) %>%
  ungroup()  

faltantes<-anti_join(data_barrios,Hogares_barrio, by = c("codigoBarrioComunaUnificado","medicion"))


#Agregar migrantes 2005
censo2005<-read.csv2("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/CENSO/2005/PoblacionMigranteBarrios2005.csv",header = TRUE,sep = ",")

data_barrios %>% 
  anti_join(censo2005,by = "codigoBarrioComunaUnificado")

data_barrios<-data_barrios %>% 
  inner_join(censo2005,by = "codigoBarrioComunaUnificado")


migrantes_medicion<-data_consolidada %>% 
  mutate(
    FEP_barrio=as.numeric(FEP_barrio),
    factorExpPersonas=as.numeric(factorExpPersonas),
    poblacionMigrante=vivia_en_otro_pais*FEP_barrio
    ) %>% 
  group_by(medicion) %>% 
  summarise(poblacionMigranteMedicion=sum(poblacionMigrante,na.rm = TRUE)) 
  

data_barrios<-data_barrios %>%
  inner_join(migrantes_medicion, by="medicion")

data_barrios<-data_barrios %>%
  mutate(
    VI_migracion=(as.numeric(shiftShareMigrantes)*poblacionMigranteMedicion)/Poblacion,
    VI_otros=(as.numeric(shiftShareResto)*poblacionMigranteMedicion)/Poblacion,
    VI_Poblacion=VI_migracion+VI_otros
    )




write.csv(data_barrios,"C:/Users/HP-Laptop/OneDrive - Universidad de Antioquia/Maestría en Economía/Tesis/1. Procesamiento/Arriendos_Medellin/Datos/ECV/Data_Consolidada/data_barrios.csv")

#writexl::write_xlsx(data_barrios,"data_barrio.xlsx")
#############
