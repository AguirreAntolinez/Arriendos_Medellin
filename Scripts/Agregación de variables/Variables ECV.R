####AGREGACIÓN DE VARIABLES ECV######

#Author: William Alexander Aguirre Antolínez
#Date: 10/11/2024
#Description: Este script realiza la agregación de variables a nivel de barrio
#y de comuna para las mediciones entre el 2004 y 2019 de la ECV de Medellín. 
#Estos datos se utilizan para la investigación sobre el efecto de la migraición
#sobre el precio de los arriendos.

##Para detalles y comentarios: alexander.aguirre@udea.edu.co

#Por medición
Personas<-data_consolidada %>% 
  mutate(base_personas=1) %>%   
  group_by(medicion) %>% 
  summarise(
    Base_Personas=sum(base_personas),
    Poblacion=sum(as.numeric(factorExpPersonas)),
    Base_Viviendas= n_distinct(skVivienda),
    Base_Hogares= n_distinct(skHogar),
    
    total_migrantes_internal = sum(vivia_en_otro_pais * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_internacionales = (total_migrantes_internal / Poblacion),
    
    total_migrantes_intermun = sum(vivia_en_otro_municipio * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_intermun = (total_migrantes_intermun / Poblacion),
    
    total_migrantes_intraurb = sum(vivia_en_otro_barrio * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_intraurb = (total_migrantes_intraurb / Poblacion),
    
    total_viven_arriendo = sum(vive_arriendo * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    porcentaje_viven_arriendo = (total_viven_arriendo / Poblacion)
  )


Viviendas<-data_consolidada %>% 
  select(medicion,skVivienda,factorExpViviendas) %>% 
  distinct() %>% 
  group_by(medicion) %>% 
  summarise(Viviendas=sum(as.numeric(factorExpViviendas)))


Hogares<-data_consolidada %>% 
  select(medicion,skHogar,factorExpHogares) %>% 
  distinct() %>% 
  group_by(medicion) %>% 
  summarise(Hogares=sum(as.numeric(factorExpHogares)))

bases_frecuencias<-Personas %>% 
  left_join(Viviendas,by="medicion") %>%   
  left_join(Hogares,by="medicion")

#Por barrio
Peresonas<-data_consolidada %>% 
  mutate(base_personas=1) %>%   
  group_by(medicion,codigoBarrioComuna,nombreBarrio,skBarrio) %>% 
  summarise(
    Base_Personas=sum(base_personas),
    Poblacion=sum(as.numeric(factorExpPersonas)),
    Base_Viviendas= n_distinct(skVivienda),
    Base_Hogares= n_distinct(skHogar),
    total_migrantes_internal = sum(vivia_en_otro_pais * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_internacionales = (total_migrantes_internal / Poblacion),
    porcentaje_migrantes_internacionales =ifelse(porcentaje_migrantes_internacionales ==0,NA,porcentaje_migrantes_internacionales),
    
    total_migrantes_intermun = sum(vivia_en_otro_municipio * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_intermun = (total_migrantes_intermun / Poblacion),
    porcentaje_migrantes_intermun =ifelse(porcentaje_migrantes_intermun ==0,NA,porcentaje_migrantes_intermun),
    total_migrantes_intraurb = sum(vivia_en_otro_barrio * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_intraurb = (total_migrantes_intraurb / Poblacion),
    
    total_viven_arriendo = sum(vive_arriendo * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    porcentaje_viven_arriendo = (total_viven_arriendo / Poblacion),
    porcentaje_viven_arriendo = ifelse(porcentaje_viven_arriendo==0,NA,porcentaje_viven_arriendo),
    
    
    
  )

Viviendas<-data_consolidada %>% filter(!is.na(skBarrio)) %>% 
  select(skBarrio,skVivienda,factorExpViviendas) %>% 
  distinct() %>% 
  group_by(skBarrio) %>% 
  summarise(Viviendas=sum(as.numeric(factorExpViviendas)))


Hogares<-data_consolidada %>% filter(!is.na(skBarrio) & !is.na(valor_arriendo)  ) %>% 
  select(skBarrio,skHogar,valor_arriendo,factorExpHogares) %>% 
  distinct() %>% 
  group_by(skBarrio) %>% 
  summarise(Hogares=sum(as.numeric(factorExpHogares)),
            media_arriendo = weighted.mean(valor_arriendo,as.numeric(factorExpHogares), na.rm = TRUE),
            log_arriendo=log(media_arriendo))

bases_barrio<-Personas %>% 
  left_join(Viviendas,by="skBarrio") %>%   
  left_join(Hogares,by="skBarrio")

#Por comuna
Personas<-data_consolidada %>% 
  mutate(base_personas=1) %>%   
  group_by(medicion,Cod_comuna,nombreComuna) %>% 
  summarise(
    Base_Personas=sum(base_personas),
    Poblacion=sum(as.numeric(factorExpPersonas)),
    Base_Viviendas= n_distinct(skVivienda),
    Base_Hogares= n_distinct(skHogar),
    total_migrantes_internal = sum(vivia_en_otro_pais * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_internacionales = (total_migrantes_internal / Poblacion),
    porcentaje_migrantes_internacionales =ifelse(porcentaje_migrantes_internacionales ==0,NA,porcentaje_migrantes_internacionales),
    
    total_migrantes_intermun = sum(vivia_en_otro_municipio * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_intermun = (total_migrantes_intermun / Poblacion),
    porcentaje_migrantes_intermun =ifelse(porcentaje_migrantes_intermun ==0,NA,porcentaje_migrantes_intermun),
    total_migrantes_intraurb = sum(vivia_en_otro_barrio * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    porcentaje_migrantes_intraurb = (total_migrantes_intraurb / Poblacion),
    
    total_viven_arriendo = sum(vive_arriendo * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    porcentaje_viven_arriendo = (total_viven_arriendo / Poblacion),
    porcentaje_viven_arriendo = ifelse(porcentaje_viven_arriendo==0,NA,porcentaje_viven_arriendo),
    
    
    
  )

Viviendas<-data_consolidada %>% 
  select(Cod_comuna,skVivienda,factorExpViviendas) %>% 
  distinct() %>% 
  group_by(Cod_comuna) %>% 
  summarise(Viviendas=sum(as.numeric(factorExpViviendas)))


Hogares<-data_consolidada %>% filter(!is.na(valor_arriendo)  ) %>% 
  select(Cod_comuna,skHogar,valor_arriendo,factorExpHogares) %>% 
  distinct() %>% 
  group_by(Cod_comuna) %>% 
  summarise(Hogares=sum(as.numeric(factorExpHogares)),
            media_arriendo = weighted.mean(valor_arriendo,as.numeric(factorExpHogares), na.rm = TRUE),
            log_arriendo=log(media_arriendo))

bases_comuna<-Personas %>% 
  left_join(Viviendas,by="Cod_comuna") %>%   
  left_join(Hogares,by="Cod_comuna")


#Variable de interés
vida_en_barrio <- data_consolidada %>%
  filter(zona=="U") %>%
  group_by(medicion) %>%
  summarize(
    vida_en_barrio_promedio =
      weighted.mean(vida_en_barrio,
                    as.numeric(factorExpPersonas),
                    na.rm = TRUE)
  )

vivia_en_otro_pais <- data_consolidada %>%
  filter(zona=="U") %>% 
  group_by(medicion) %>%
  summarize(
    total_migrantes = sum(vivia_en_otro_pais * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    total_personas = sum(as.numeric(factorExpPersonas), na.rm = TRUE),               # Total de personas ponderado
    porcentaje_migrantes_internacionales = (total_migrantes / total_personas) 
  )

vivia_en_otro_municipio <- data_consolidada %>%
  filter(zona=="U") %>% 
  group_by(medicion) %>%
  summarize(
    total_migrantes = sum(vivia_en_otro_municipio * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    total_personas = sum(as.numeric(factorExpPersonas), na.rm = TRUE),               # Total de personas ponderado
    porcentaje_migrantes_interregionales = (total_migrantes / total_personas) 
  )

vivia_en_otro_barrio <- data_consolidada %>%
  filter(zona=="U") %>% 
  group_by(medicion) %>%
  summarize(
    total_migrantes = sum(vivia_en_otro_barrio * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    total_personas = sum(as.numeric(factorExpPersonas), na.rm = TRUE),               # Total de personas ponderado
    porcentaje_migrantes_intraurbanos = (total_migrantes / total_personas) 
  )


vive_en_arriendo <- data_consolidada %>%
  filter(zona=="U") %>% 
  group_by(medicion) %>%
  summarize(
    total_en_arriendo = sum(vive_arriendo * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    total_personas = sum(as.numeric(factorExpPersonas), na.rm = TRUE),               # Total de personas ponderado
    porcentaje_en_arriendo = (total_en_arriendo / total_personas) 
  )

vive_en_arriendo<-vive_en_arriendo %>% mutate(
  porcentaje_en_arriendo=ifelse(porcentaje_en_arriendo==0,NA,porcentaje_en_arriendo)
)

migrante_en_arriendo<-data_consolidada %>% 
  filter(zona=="U") %>% 
  group_by(medicion) %>%
  summarize(
    total_migrante_en_arriendo = sum(migrante_en_arriendo * as.numeric(factorExpPersonas), na.rm = TRUE),   # Peso de migrantes
    total_personas = sum(as.numeric(factorExpPersonas), na.rm = TRUE),               # Total de personas ponderado
    porcentaje_en_arriendo = (total_migrante_en_arriendo / total_personas) 
  )

arriendo_por_tipo_persona <- data_consolidada %>%
  group_by(medicion, tipo_persona) %>%
  summarize(media_arriendo = mean(valor_arriendo, na.rm = TRUE)) %>%
  ungroup()

arriendo_por_tipo_persona<-arriendo_por_tipo_persona %>% filter(!is.na(media_arriendo))
