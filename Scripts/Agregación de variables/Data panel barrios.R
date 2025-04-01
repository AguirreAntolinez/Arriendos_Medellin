####DATA PANEL BARRIOS######

#Author: William Alexander Aguirre Antolínez
#Date: 05/01/2025
#Description: Este script realiza la agregación de variables a nivel de barrio
#para las mediciones entre el 2004 y 2019 de la ECV de Medellín. 
#Estos datos se utilizan para la investigación sobre el efecto de la migraición
#sobre el precio de los arriendos.

##Para detalles y comentarios: alexander.aguirre@udea.edu.co

#Excluir los corregimientos
comunas<-c('1','2','3','4',
           '5','6','7','8',
           '9','10','11','12',
           '13','14','15','16')

mediciones<-c('2008','2009','2010',
              '2011','2012','2013',
              '2014','2015','2016',
              '2017','2018','2019')

# barrios_sin_arrendamiento<-c('1008','108','1103','1401','1408',
#                              '1414','1416','1418','1419','1420',
#                              '1502','1604','1618','1621','314',
#                              '315','517','612','702','705',
#                              '725','805','915','916','917')

data_consolidada<-data_consolidada %>% filter(
  zona=='U' & 
    Cod_comuna %in% comunas & 
    medicion %in% mediciones #&
    #!codigoBarrioComuna %in% barrios_sin_arrendamiento
    )




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
         
  


Personas_barrio<-data_consolidada %>% 
  mutate(base_personas=1) %>%   
  group_by(medicion,codigoBarrioComunaUnificado,nombreBarrioUnificado) %>% 
  summarise(
    Base_Personas=sum(base_personas),
    Poblacion=sum(FEP_barrio),
    Base_Viviendas= n_distinct(skVivienda),
    Base_Hogares= n_distinct(skHogar),
    
    total_migrantes_internal = sum(vivia_en_otro_pais * FEP_barrio, na.rm = TRUE),
    porcentaje_migrantes_internacionales = (total_migrantes_internal / Poblacion),
    
    total_migrantes_intermun = sum(vivia_en_otro_municipio * FEP_barrio, na.rm = TRUE),   
    porcentaje_migrantes_intermun = (total_migrantes_intermun / Poblacion),
    
    total_migrantes_intraurb = sum(vivia_en_otro_barrio * FEP_barrio, na.rm = TRUE),  
    porcentaje_migrantes_intraurb = (total_migrantes_intraurb / Poblacion),
    
    total_viven_arriendo = sum(vive_arriendo * FEP_barrio, na.rm = TRUE),   
    porcentaje_viven_arriendo = (total_viven_arriendo / Poblacion),
    
    )


calcular_moda <- function(x) {
  tabla <- table(x)  
  moda <- as.numeric(names(tabla)[tabla == max(tabla)])  
  return(moda)
}

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
ipc<-ipc %>% rename(medicion=Año) %>% select(medicion,Indice) %>% mutate(medicion=as.character(medicion))

Hogares_barrio<-data_consolidada %>% filter(!is.na(codigoBarrioComunaUnificado) & !is.na(valor_arriendo)  ) %>% 
  select(codigoBarrioComunaUnificado,medicion,skHogar,valor_arriendo,factorExpHogares,FEP_barrio) %>% 
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
         codigoBarrioComuna=as.character(codigoBarrioComuna)) %>% 
  select(codigoBarrioComuna,medicion,viviendas) %>% 
  mutate(
    codigoBarrioComunaUnificado=case_when(
      codigoBarrioComuna=="315" ~ "314",
      codigoBarrioComuna=="915" ~ "914",
      codigoBarrioComuna=="916" ~ "914",
      .default = codigoBarrioComuna
    )
  )


#Consolidar el panel de barrios
data_barrios<-Personas_barrio %>% 
  left_join(Viviendas_barrio,by=c("codigoBarrioComunaUnificado","medicion")) %>%   
  left_join(Hogares_barrio,by=c("codigoBarrioComunaUnificado","medicion")) %>%   
  left_join(viviendas,by=c("codigoBarrioComunaUnificado","medicion"))

#Aqui se rellenan mientras tantos los NA con la cantidad de viviendas de 2014
data_barrios <- data_barrios %>% 
  group_by(codigoBarrioComunaUnificado) %>%  
  mutate(viviendas = ifelse(is.na(viviendas), viviendas[medicion == 2014], viviendas)) %>%
  ungroup()  

faltantes<-anti_join(data_barrios,Hogares_barrio, by = c("codigoBarrioComunaUnificado","medicion"))

#Calcular las tasas de variacion
data_barrios <- data_barrios %>%
  arrange(codigoBarrioComunaUnificado, medicion) %>%  # Asegurar que los datos estén ordenados
  group_by(codigoBarrioComunaUnificado) %>%  # Agrupar por barrio
  mutate(
    viviendas_lag=lag(viviendas),
    var_viviendas = case_when(
      viviendas_lag==0~100,
      is.na(viviendas_lag)~NA,
      TRUE~ (viviendas - viviendas_lag) / viviendas_lag * 100),
    
    poblacion_lag=lag(Poblacion),
    var_poblacion = case_when(
      poblacion_lag==0~100,
      is.na(poblacion_lag)~NA,
      TRUE~ (Poblacion - poblacion_lag) / poblacion_lag * 100),
    
    migrante_internal_lag=lag(total_migrantes_internal),
    var_migrante_internal = case_when(
      migrante_internal_lag==0~100,
      is.na(migrante_internal_lag)~NA,
      TRUE~ (total_migrantes_internal - migrante_internal_lag) / migrante_internal_lag * 100),
    
    migrante_intermun_lag=lag(total_migrantes_intermun),
    var_migrante_intermun = case_when(
      migrante_intermun_lag==0~100,
      is.na(migrante_intermun_lag)~NA,
      TRUE~ (total_migrantes_intermun - migrante_intermun_lag) / migrante_intermun_lag * 100),
    
    migrante_intraurb_lag=lag(total_migrantes_intraurb),
    var_migrante_intraurb = case_when(
      migrante_intraurb_lag==0~100,
      is.na(migrante_intraurb_lag)~NA,
      TRUE~ (total_migrantes_intraurb - migrante_intraurb_lag) / migrante_intraurb_lag * 100)) %>% 
    
  ungroup() 



