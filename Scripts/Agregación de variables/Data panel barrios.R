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
         
  


Personas_barrio<-data_consolidada %>% 
  mutate(base_personas=1) %>%   
  group_by(medicion,codigoBarrioComuna,nombreBarrio) %>% 
  summarise(
    Base_Personas=sum(base_personas),
    Poblacion=sum(FEP_barrio),
    Base_Viviendas= n_distinct(skVivienda),
    Base_Hogares= n_distinct(skHogar),
    
    total_migrantes_internal = sum(vivia_en_otro_pais * FEP_barrio, na.rm = TRUE),
    porcentaje_migrantes_internacionales = (total_migrantes_internal / Poblacion),
    porcentaje_migrantes_internacionales =ifelse(porcentaje_migrantes_internacionales ==0,NA,porcentaje_migrantes_internacionales),
    
    total_migrantes_intermun = sum(vivia_en_otro_municipio * FEP_barrio, na.rm = TRUE),   
    porcentaje_migrantes_intermun = (total_migrantes_intermun / Poblacion),
    porcentaje_migrantes_intermun =ifelse(porcentaje_migrantes_intermun ==0,NA,porcentaje_migrantes_intermun),
    
    total_migrantes_intraurb = sum(vivia_en_otro_barrio * FEP_barrio, na.rm = TRUE),  
    porcentaje_migrantes_intraurb = (total_migrantes_intraurb / Poblacion),
    porcentaje_migrantes_intraurb =ifelse(porcentaje_migrantes_intraurb ==0,NA,porcentaje_migrantes_intraurb),
    
    total_viven_arriendo = sum(vive_arriendo * FEP_barrio, na.rm = TRUE),   
    porcentaje_viven_arriendo = (total_viven_arriendo / Poblacion),
    porcentaje_viven_arriendo = ifelse(porcentaje_viven_arriendo==0,NA,porcentaje_viven_arriendo),
    
    )


calcular_moda <- function(x) {
  tabla <- table(x)  
  moda <- as.numeric(names(tabla)[tabla == max(tabla)])  
  return(moda)
}

Viviendas_barrio<-data_consolidada %>% filter(!is.na(skBarrio)) %>% 
  select(codigoBarrioComuna,
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
  group_by(codigoBarrioComuna,medicion) %>% 
  summarise(Viviendas=sum(factorExpViviendas),
            
            Estrato_predominante=calcular_moda(Estrato),
            Estrato_promedio=mean(Estrato,na.rm=TRUE),
            
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
  


Hogares_barrio<-data_consolidada %>% filter(!is.na(codigoBarrioComuna) & !is.na(valor_arriendo)  ) %>% 
  select(codigoBarrioComuna,medicion,skHogar,valor_arriendo,factorExpHogares,FEP_barrio) %>% 
  distinct() %>% 
  group_by(codigoBarrioComuna,medicion) %>% 
  summarise(Hogares=sum(factorExpHogares),
            media_arriendo=mean(valor_arriendo,na.rm=TRUE),
            log_arriendo=log(media_arriendo)
            ) 
  



viviendas<-read.csv2("https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/ESTADISTICAS%20CATASTRALES/Viviendas.csv")

viviendas<-viviendas %>% 
  mutate(across(starts_with("X"),as.numeric)) %>% 
  pivot_longer(cols = starts_with("X"),
               names_to = "medicion",
               values_to = "viviendas") %>% 
  mutate(medicion=as.character(sub("X","",medicion)),
         codigoBarrioComuna=as.character(codigoBarrioComuna)) %>% 
  select(codigoBarrioComuna,medicion,viviendas)


#Consolidar el panel de barrios
data_barrios<-Personas_barrio %>% 
  left_join(Viviendas_barrio,by=c("codigoBarrioComuna","medicion")) %>%   
  left_join(Hogares_barrio,by=c("codigoBarrioComuna","medicion")) %>%   
  left_join(viviendas,by=c("codigoBarrioComuna","medicion"))

#Aqui se rellenan mientras tantos los NA con la cantidad de viviendas de 2014
data_barrios <- data_barrios %>% 
  group_by(codigoBarrioComuna) %>%  
  mutate(viviendas = ifelse(is.na(viviendas), viviendas[medicion == 2014], viviendas)) %>%
  ungroup()  


