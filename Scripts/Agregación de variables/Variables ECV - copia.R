####AGREGACIÓN DE VARIABLES ECV######

#Author: William Alexander Aguirre Antolínez
#Date: 10/11/2024
#Description: Este script realiza la agregación de variables a nivel de barrio
#y de comuna para las mediciones entre el 2004 y 2019 de la ECV de Medellín. 
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
        hacinamiento=as.numeric(hacinamiento),
        posee_energia=as.numeric(posee_energia),
        posee_acueducto=as.numeric(posee_acueducto),
        posee_alcantarillado=as.numeric(posee_alcantarillado),
        paredes_material=as.numeric(paredes_material),
        pisos_material=as.numeric(pisos_material),
        servicios=as.numeric(servicios),
        material_vivienda=as.numeric(material_vivienda)
        )

#Por medición
Personas_medicion<-data_consolidada %>% 
  mutate(base_personas=1) %>%   
  group_by(medicion) %>% 
  summarise(
    Base_Personas=sum(base_personas),
    Poblacion=sum(FEP_barrio,na.rm = TRUE),
    Base_Viviendas= n_distinct(skVivienda),
    Base_Hogares= n_distinct(skHogar),
    
    total_migrantes_internal = sum(vivia_en_otro_pais * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_internacionales = (total_migrantes_internal / Poblacion),
    
    total_migrantes_intermun = sum(vivia_en_otro_municipio * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_intermun = (total_migrantes_intermun / Poblacion),
    
    total_migrantes_intraurb = sum(vivia_en_otro_barrio * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_intraurb = (total_migrantes_intraurb / Poblacion),
    
    total_viven_arriendo = sum(vive_arriendo * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    porcentaje_viven_arriendo = (total_viven_arriendo / Poblacion)
  )


Viviendas_medicion<-data_consolidada %>% 
  select(medicion,skVivienda,factorExpViviendas) %>% 
  distinct() %>% 
  group_by(medicion) %>% 
  summarise(Viviendas=sum(factorExpViviendas))


Hogares_medicion<-data_consolidada %>% 
  select(medicion,skHogar,factorExpHogares) %>% 
  distinct() %>% 
  group_by(medicion) %>% 
  summarise(Hogares=factorExpHogares)

bases_frecuencias<-Personas_medicion %>% 
  left_join(Viviendas_medicion,by="medicion") %>%   
  left_join(Hogares_medicion,by="medicion")

#Por barrio
Personas_barrio<-data_consolidada %>% 
  mutate(base_personas=1) %>%   
  group_by(medicion,codigoBarrioComuna,nombreBarrio,skBarrio) %>% 
  summarise(
    Base_Personas=sum(base_personas),
    Poblacion=sum(FEP_barrio),
    Base_Viviendas= n_distinct(skVivienda),
    Base_Hogares= n_distinct(skHogar),
    total_migrantes_internal = sum(vivia_en_otro_pais * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_internacionales = (total_migrantes_internal / Poblacion),
    porcentaje_migrantes_internacionales =ifelse(porcentaje_migrantes_internacionales ==0,NA,porcentaje_migrantes_internacionales),
    
    total_migrantes_intermun = sum(vivia_en_otro_municipio * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_intermun = (total_migrantes_intermun / Poblacion),
    porcentaje_migrantes_intermun =ifelse(porcentaje_migrantes_intermun ==0,NA,porcentaje_migrantes_intermun),
    total_migrantes_intraurb = sum(vivia_en_otro_barrio * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_intraurb = (total_migrantes_intraurb / Poblacion),
    
    total_viven_arriendo = sum(vive_arriendo * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    porcentaje_viven_arriendo = (total_viven_arriendo / Poblacion),
    porcentaje_viven_arriendo = ifelse(porcentaje_viven_arriendo==0,NA,porcentaje_viven_arriendo),
    
    
    
  )

Viviendas_barrio<-data_consolidada %>% filter(!is.na(skBarrio)) %>% 
  select(skBarrio,skVivienda,factorExpViviendas) %>% 
  distinct() %>% 
  group_by(skBarrio) %>% 
  summarise(Viviendas=sum(factorExpViviendas))


Hogares_barrio<-data_consolidada %>% filter(!is.na(skBarrio) & !is.na(valor_arriendo)  ) %>% 
  select(skBarrio,skHogar,valor_arriendo,factorExpHogares,FEP_barrio) %>% 
  distinct() %>% 
  group_by(skBarrio) %>% 
  summarise(Hogares=sum(factorExpHogares),
            media_arriendo = weighted.mean(valor_arriendo,factorExpHogares, na.rm = TRUE),
            log_arriendo=log(media_arriendo))

bases_barrio<-Personas_barrio %>% 
  left_join(Viviendas_barrio,by="skBarrio") %>%   
  left_join(Hogares_barrio,by="skBarrio")

#Por comuna
Personas_comuna<-data_consolidada %>% 
  mutate(base_personas=1) %>%   
  group_by(medicion,Cod_comuna,nombreComuna) %>% 
  summarise(
    Base_Personas=sum(base_personas),
    Poblacion=sum(FEP_barrio, na.rm = TRUE),
    Base_Viviendas= n_distinct(skVivienda),
    Base_Hogares= n_distinct(skHogar),
    total_migrantes_internal = sum(vivia_en_otro_pais * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_internacionales = (total_migrantes_internal / Poblacion),
    porcentaje_migrantes_internacionales =ifelse(porcentaje_migrantes_internacionales ==0,NA,porcentaje_migrantes_internacionales),
    
    total_migrantes_intermun = sum(vivia_en_otro_municipio * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_intermun = (total_migrantes_intermun / Poblacion),
    porcentaje_migrantes_intermun =ifelse(porcentaje_migrantes_intermun ==0,NA,porcentaje_migrantes_intermun),
    total_migrantes_intraurb = sum(vivia_en_otro_barrio * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_intraurb = (total_migrantes_intraurb / Poblacion),
    
    total_viven_arriendo = sum(vive_arriendo * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    porcentaje_viven_arriendo = (total_viven_arriendo / Poblacion),
    porcentaje_viven_arriendo = ifelse(porcentaje_viven_arriendo==0,NA,porcentaje_viven_arriendo),
    
    
    
  )

Viviendas_comuna<-data_consolidada %>% 
  select(Cod_comuna,skVivienda,factorExpViviendas) %>% 
  distinct() %>% 
  group_by(Cod_comuna) %>% 
  summarise(Viviendas=sum(factorExpViviendas))


Hogares_comuna<-data_consolidada %>% filter(!is.na(valor_arriendo)  ) %>% 
  select(Cod_comuna,skHogar,valor_arriendo,factorExpHogares) %>% 
  distinct() %>% 
  group_by(Cod_comuna) %>% 
  summarise(Hogares=sum(factorExpHogares),
            media_arriendo = weighted.mean(valor_arriendo,factorExpHogares, na.rm = TRUE),
            log_arriendo=log(media_arriendo))

bases_comuna<-Personas_comuna %>% 
  left_join(Viviendas_comuna,by="Cod_comuna") %>%   
  left_join(Hogares_comuna,by="Cod_comuna")


#Variable de interés
vida_en_barrio <- data_consolidada %>%
  filter(zona=="U") %>%
  group_by(medicion) %>%
  summarize(
    vida_en_barrio_promedio =
      weighted.mean(vida_en_barrio,
                    FEP_barrio,
                    na.rm = TRUE)
  )

vivia_en_otro_pais <- data_consolidada %>%
  filter(zona=="U") %>% 
  group_by(medicion) %>%
  summarize(
    total_migrantes = sum(vivia_en_otro_pais * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    total_personas = sum(FEP_barrio, na.rm = TRUE),               # Total de personas ponderado
    porcentaje_migrantes_internacionales = (total_migrantes / total_personas) 
  )

vivia_en_otro_municipio <- data_consolidada %>%
  filter(zona=="U") %>% 
  group_by(medicion) %>%
  summarize(
    total_migrantes = sum(vivia_en_otro_municipio * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    total_personas = sum(FEP_barrio, na.rm = TRUE),               # Total de personas ponderado
    porcentaje_migrantes_interregionales = (total_migrantes / total_personas) 
  )

vivia_en_otro_barrio <- data_consolidada %>%
  filter(zona=="U") %>% 
  group_by(medicion) %>%
  summarize(
    total_migrantes = sum(vivia_en_otro_barrio * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    total_personas = sum(FEP_barrio, na.rm = TRUE),               # Total de personas ponderado
    porcentaje_migrantes_intraurbanos = (total_migrantes / total_personas) 
  )


vive_en_arriendo <- data_consolidada %>%
  filter(zona=="U") %>% 
  group_by(medicion) %>%
  summarize(
    total_en_arriendo = sum(vive_arriendo * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    total_personas = sum(FEP_barrio, na.rm = TRUE),               # Total de personas ponderado
    porcentaje_en_arriendo = (total_en_arriendo / total_personas) 
  )

vive_en_arriendo<-vive_en_arriendo %>% mutate(
  porcentaje_en_arriendo=ifelse(porcentaje_en_arriendo==0,NA,porcentaje_en_arriendo)
)

migrante_en_arriendo<-data_consolidada %>% 
  filter(zona=="U") %>% 
  group_by(medicion) %>%
  summarize(
    total_migrante_en_arriendo = sum(migrante_en_arriendo * FEP_barrio, na.rm = TRUE),   # Peso de migrantes
    total_personas = sum(FEP_barrio, na.rm = TRUE),               # Total de personas ponderado
    porcentaje_en_arriendo = (total_migrante_en_arriendo / total_personas) 
  )

arriendo_por_tipo_persona <- data_consolidada %>%
  group_by(medicion, tipo_persona) %>%
  summarize(media_arriendo = mean(valor_arriendo, na.rm = TRUE)) %>%
  ungroup()

arriendo_por_tipo_persona<-arriendo_por_tipo_persona %>% filter(!is.na(media_arriendo))
