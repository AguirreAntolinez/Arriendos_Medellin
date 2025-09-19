#### ANALISIS ECONOMETRICO ####

#Author: William Alexander Aguirre Antolínez
#Date: 05/01/2024
#Description: Este script permite realizar el análisis econométrico de
#la investigación acerca del efecto de la migración en el precio de los 
#alquileres.

##Para detalles y comentarios: alexander.aguirre@udea.edu.co

library(tidyverse)
library(fixest)
library(AER)


#PASO 1 IMPORTAR DATOS####

#Importar los datos
data_barrios<-read.csv2(
  'https://raw.githubusercontent.com/AguirreAntolinez/Arriendos_Medellin/refs/heads/main/Datos/ECV/Data_Consolidada/data_barrios.csv',
  header = TRUE,sep = ",")

#PASO 2 AJUSTAR TIPOS DE VARIABLES####
data_barrios <- data_barrios %>% 
  mutate(
    codigoBarrioComunaUnificado=as.numeric(codigoBarrioComunaUnificado),
    medicion=as.factor(medicion),
    total_migrantes_internal=as.numeric(total_migrantes_internal),
    tasa_migracion=as.numeric(tasa_migracion),
    tasa_migracion_anterior=as.numeric(tasa_migracion_anterior),
    tasa_permanencia=as.numeric(tasa_permanencia),
    log_arriendo=as.numeric(log_arriendo),
    MigrantesAcumulados=as.numeric(MigrantesAcumulados),
    Poblacion2019=as.numeric(Poblacion2019),
    tasa_migracion_delta=as.numeric(tasa_migracion_delta),
    MigrantesAnterior=as.numeric(MigrantesAnterior),
    VI_Migrantes=as.numeric(VI_Migrantes),
    VI_Poblacion=as.numeric(VI_Poblacion),
    anios_en_barrio=as.numeric(anios_en_barrio)
  )



#PASO 3 VALIDAR QUE EL PANEL SEA BALANCEADO####

panel_check <- data_barrios %>%
  count(codigoBarrioComunaUnificado)  # Cuenta cuántas mediciones hay por cada código

# Verificar si todos tienen el mismo número de observaciones
if (length(unique(panel_check$n)) == 1) {
  print("El panel es balanceado")
} else {
  print("El panel es desbalanceado")
}


#Verificar barrios completos 
años_completos <- expand.grid(codigoBarrioComunaUnificado = unique(data_barrios$codigoBarrioComunaUnificado), 
                              medicion = unique(data_barrios$medicion))
# Unir con los datos reales
completo_check <- full_join(años_completos, data_barrios, by = c("codigoBarrioComunaUnificado", "medicion"))
# Filtrar los NA en 'viviendas', que indican datos faltantes
datos_faltantes <- completo_check %>% filter(is.na(nombreBarrioUnificado))


#PASO 4: REALIZAR ESTIMACIONES

data_modelo <- data_barrios %>% 
  # Crear estratos (tu código original)
  mutate(
    Estrato_bajo = case_when(
      Estrato_alto == 0 & Estrato_medio == 0 ~ 1,
      .default = 0
    ),
    Estrato = as.factor(case_when(
      Estrato_alto == 1 ~ 3,
      Estrato_medio == 1 ~ 2,
      Estrato_bajo == 1 ~ 1
    )),
    # Crear comuna (tu código original)
    comuna = ifelse(
      nchar(codigoBarrioComunaUnificado) == 4, substr(codigoBarrioComunaUnificado, 1, 2),
      ifelse(nchar(codigoBarrioComunaUnificado) == 3, substr(codigoBarrioComunaUnificado, 1, 1), NA)
    ),
    comuna = as.factor(comuna))

anios<-2008:2019

#### VI con TWFE #####


# Loop por pares de años
# Iterar por cada año
#Efecto directo
vi_efecto_directo <- list()

for (i in seq_along(anios)) {
  anio_ini <- as.numeric(as.character(anios[i]))
  anio_fin <- anio_ini + 1
  
  # Filtrar datos para el par de años actual
  datos_par <- data_modelo %>%
    filter(medicion %in% c(as.character(anio_ini), as.character(anio_fin)))
  
  # Intentar estimar el modelo IV con efectos fijos
  resultado <- tryCatch(
    {
      modelo <- feols(
        log_arriendo ~ 1 
        | codigoBarrioComunaUnificado + medicion 
        | tasa_migracion ~ VI_Migrantes,
        data = datos_par,
        cluster = ~comuna
      )
      # Extraer estadísticas del coeficiente de tasa_migracion
      coef_val <- modelo$coefficients
      se_val   <- modelo$se
      t_val    <- coef_val / se_val
      p_val    <- pvalue(modelo)
      primera_etapa<-fitstat(modelo, type = "ivf1")
      f<-primera_etapa$`ivf1::tasa_migracion`[1]                                     
      f<-as.numeric(f)
      F_KP<-fitstat(modelo, type = "ivwald")
      f_kp<-F_KP$`ivwald1::tasa_migracion`$stat
      F_CD<-fitstat(modelo, type = "ivf1")
      f_cd<-F_CD$`ivf1::tasa_migracion`$stat
      R2<-r2(modelo, type = "r2")
      wR2<-r2(modelo, type ="wr2")
      
      
      # Crear data.frame con resultados
      data.frame(
        periodo = paste0(anio_ini, "-", anio_fin),
        beta    = coef_val,
        se      = se_val,
        t       = t_val,
        p       = p_val,
        f_primer_etapa = f,
        f_primer_etapa_kp=f_kp,
        f_primer_etapa_cd=f_cd,
        R2      = R2,
        wR2     = wR2
      )
      
    }, error = function(e) {
      # En caso de error, devolver fila con NAs
      data.frame(
        periodo = paste0(anio_ini, "-", anio_fin),
        beta = NA,
        se = NA,
        t = NA,
        p = NA,
        f_primer_etapa = NA,
        f_primer_etapa_kp=NA,
        f_primer_etapa_cd=NA,
        R2 = NA,
        wR2= NA
      )
    })
  
  # Guardar el resultado en la lista
  vi_efecto_directo[[i]] <- resultado
}


# Unir todos los resultados
tabla_resultados_vi_efecto_directo <- bind_rows(vi_efecto_directo)

tabla_resultados_vi_efecto_directo<- tabla_resultados_vi_efecto_directo %>% 
  mutate(
    sig=case_when(
      p<=0.001~"***",
      p<=0.01~"**",
      p<=0.05~"*",
      p<=0.1~".",
      p>0.1~"",
    ),
    modelo="Efecto directo",
    metodo="VI"
  )


comparacion_F<-tabla_resultados_vi_efecto_directo %>% select(periodo,f_primer_etapa_cd,f_primer_etapa_kp)

writexl::write_xlsx(comparacion_F,"C:/Users/HP-Laptop/OneDrive - Universidad de Antioquia/Maestría en Economía/Tesis/1. Procesamiento/MigracionMedellin/Scripts/Analisis econométrico/Comparacion Prueba F.xlsx")

# DID Efecto inducido
vi_efecto_inducido <- list()


# Loop por pares de años
for (i in seq_along(anios)) {
  anio_ini <- as.numeric(as.character(anios[i]))
  anio_fin <- anio_ini + 1
  
  # Filtrar datos para el par de años actual
  datos_par <- data_modelo %>%
    filter(medicion %in% c(as.character(anio_ini), as.character(anio_fin)))
  
  # Intentar estimar el modelo IV con efectos fijos
  resultado <- tryCatch(
    {
      modelo <- feols(
        tasa_permanencia ~ 1 
        | codigoBarrioComunaUnificado + medicion 
        | tasa_migracion ~ VI_Migrantes,
        data = datos_par,
        cluster = ~comuna
      )
      # Extraer estadísticas del coeficiente de tasa_migracion
      coef_val <- modelo$coefficients
      se_val   <- modelo$se
      t_val    <- coef_val / se_val
      p_val    <- pvalue(modelo)
      primera_etapa<-fitstat(modelo, type = "ivf1")
      f<-primera_etapa$`ivf1::tasa_migracion`[1]                                     
      f<-as.numeric(f)
      R2<-r2(modelo, type = "r2")
      wR2<-r2(modelo, type ="wr2")
      
      # Crear data.frame con resultados
      data.frame(
        periodo = paste0(anio_ini, "-", anio_fin),
        beta    = coef_val,
        se      = se_val,
        t       = t_val,
        p       = p_val,
        f_primer_etapa = f,
        R2      = R2,
        wR2     = wR2
      )
      
    }, error = function(e) {
      # En caso de error, devolver fila con NAs
      data.frame(
        periodo = paste0(anio_ini, "-", anio_fin),
        beta = NA,
        se = NA,
        t = NA,
        p = NA,
        f_primer_etapa = NA,
        R2 = NA,
        wR2 = NA
      )
    })
  
  # Guardar el resultado en la lista
  vi_efecto_inducido[[i]] <- resultado
}
a<-fitstat(modelo, "kpr")
a$`ivwald1::tasa_migracion`
# Unir todos los resultados
tabla_resultados_vi_efecto_inducido <- bind_rows(vi_efecto_inducido)

tabla_resultados_vi_efecto_inducido<- tabla_resultados_vi_efecto_inducido %>% 
  mutate(
    sig=case_when(
      p<=0.001~"***",
      p<=0.01~"**",
      p<=0.05~"*",
      p<=0.1~".",
      p>0.1~""
    ),
    modelo="Efecto inducido",
    metodo="VI"
  )

#### TWFE ####
# FE Efecto directo
fe_efecto_directo <- list()

# Loop por pares de años
for (i in seq_along(anios)) {
  anio_ini <- anios[i]
  anio_ini<-  as.numeric(as.character(anio_ini))
  anio_fin <- anio_ini + 1
  anio_ini <-as.character(anio_ini)
  anio_fin <-as.character(anio_fin)
  
  # Filtrar datos para el par de años actual
  datos_par <- data_modelo %>%
    filter(medicion %in% c(anio_ini, anio_fin))
  
  # Intentar estimar el modelo
  resultado <- tryCatch(
    {
      modelo <- feols(
        log_arriendo ~ tasa_migracion | codigoBarrioComunaUnificado + medicion,
        data = datos_par,
        cluster = ~comuna)
      
      # Extraer estadísticas
      coef <- coef(modelo)["tasa_migracion"]
      se <- se(modelo)["tasa_migracion"]
      tval <- coef / se
      pval <- pvalue(modelo)["tasa_migracion"]
      R2<-r2(modelo, type = "r2")
      wR2<-r2(modelo, type ="wr2")
      
      # Crear data frame de resultado
      data.frame(
        periodo = paste0(anio_ini, "-", anio_fin),
        beta = coef,
        se = se,
        t = tval,
        p = pval,
        f_primer_etapa = NA,
        R2 = R2,
        wR2 = wR2
      )
      
    }, error = function(e) {
      # Si hay error, devolver fila con NAs
      data.frame(
        periodo = paste0(anio_ini, "-", anio_fin),
        beta = NA,
        se = NA,
        t = NA,
        p = NA,
        f_primer_etapa = NA,
        R2 = NA,
        wR2 = NA
      )
    })
  
  # Guardar resultado
  fe_efecto_directo[[i]] <- resultado
}

# Unir todos los resultados
tabla_resultados_fe_efecto_directo <- bind_rows(fe_efecto_directo)

tabla_resultados_fe_efecto_directo<- tabla_resultados_fe_efecto_directo %>% 
  mutate(
    sig=case_when(
      p<=0.001~"***",
      p<=0.01~"**",
      p<=0.05~"*",
      p<=0.1~".",
      p>0.1~""
    ),
    modelo="Efecto directo",
    metodo="FE"
  )

# FE Efecto inducido
fe_efecto_inducido <- list()

# Loop por pares de años
for (i in seq_along(anios)) {
  anio_ini <- anios[i]
  anio_ini<-  as.numeric(as.character(anio_ini))
  anio_fin <- anio_ini + 1
  anio_ini <-as.character(anio_ini)
  anio_fin <-as.character(anio_fin)
  
  # Filtrar datos para el par de años actual
  datos_par <- data_modelo %>%
    filter(medicion %in% c(anio_ini, anio_fin))
  
  # Intentar estimar el modelo
  resultado <- tryCatch(
    {
      modelo <- feols(
        tasa_permanencia ~ tasa_migracion | codigoBarrioComunaUnificado + medicion,
        data = datos_par,
        cluster = ~comuna)
      
      # Extraer estadísticas
      coef <- coef(modelo)["tasa_migracion"]
      se <- se(modelo)["tasa_migracion"]
      tval <- coef / se
      pval <- pvalue(modelo)["tasa_migracion"]
      R2<-r2(modelo, type = "r2")
      wR2<-r2(modelo, type ="wr2")
      
      # Crear data frame de resultado
      data.frame(
        periodo = paste0(anio_ini, "-", anio_fin),
        beta = coef,
        se = se,
        t = tval,
        p = pval,
        f_primer_etapa = NA,
        R2 = R2,
        wR2 = wR2
      )
      
    }, error = function(e) {
      # Si hay error, devolver fila con NAs
      data.frame(
        periodo = paste0(anio_ini, "-", anio_fin),
        beta = NA,
        se = NA,
        t = NA,
        p = NA,
        f_primer_etapa = NA,
        R2 = NA,
        wR2 = NA
      )
    })
  
  # Guardar resultado
  fe_efecto_inducido[[i]] <- resultado
}

# Unir todos los resultados
tabla_resultados_fe_efecto_inducido <- bind_rows(fe_efecto_inducido)

tabla_resultados_fe_efecto_inducido<- tabla_resultados_fe_efecto_inducido %>% 
  mutate(
    sig=case_when(
      p<=0.001~"***",
      p<=0.01~"**",
      p<=0.05~"*",
      p<=0.1~".",
      p>0.1~""
    ),
    modelo="Efecto inducido",
    metodo="FE"
  )


### OLS ###
# OLS Efecto directo
ols_efecto_directo <- list()

# Loop por pares de años
for (i in seq_along(anios)) {
  anio_ini <- anios[i]
  anio_ini<-  as.numeric(as.character(anio_ini))
  anio_fin <- anio_ini + 1
  anio_ini <-as.character(anio_ini)
  anio_fin <-as.character(anio_fin)
  
  # Filtrar datos para el par de años actual
  datos_par <- data_modelo %>%
    filter(medicion %in% c(anio_ini, anio_fin))
  
  # Intentar estimar el modelo
  resultado <- tryCatch(
    {
      modelo <- feols(
        log_arriendo ~ tasa_migracion,
        data = datos_par,
        cluster = ~comuna)
      
      # Extraer estadísticas
      coef <- coef(modelo)["tasa_migracion"]
      se <- se(modelo)["tasa_migracion"]
      tval <- coef / se
      pval <- pvalue(modelo)["tasa_migracion"]
      R2<-r2(modelo, type = "r2")
      
      
      # Crear data frame de resultado
      data.frame(
        periodo = paste0(anio_ini, "-", anio_fin),
        beta = coef,
        se = se,
        t = tval,
        p = pval,
        f_primer_etapa = NA,
        R2 = R2,
        wR2 = NA
      )
      
    }, error = function(e) {
      # Si hay error, devolver fila con NAs
      data.frame(
        periodo = paste0(anio_ini, "-", anio_fin),
        beta = NA,
        se = NA,
        t = NA,
        p = NA,
        f_primer_etapa = NA,
        R2 = NA,
        wR2 = NA
      )
    })
  
  # Guardar resultado
  ols_efecto_directo[[i]] <- resultado
}

# Unir todos los resultados
tabla_resultados_ols_efecto_directo <- bind_rows(ols_efecto_directo)

tabla_resultados_ols_efecto_directo<- tabla_resultados_ols_efecto_directo %>% 
  mutate(
    sig=case_when(
      p<=0.001~"***",
      p<=0.01~"**",
      p<=0.05~"*",
      p<=0.1~".",
      p>0.1~""
    ),
    modelo="Efecto directo",
    metodo="OLS"
  )

# OLS Efecto inducido
ols_efecto_inducido <- list()

# Loop por pares de años
for (i in seq_along(anios)) {
  anio_ini <- anios[i]
  anio_ini<-  as.numeric(as.character(anio_ini))
  anio_fin <- anio_ini + 1
  anio_ini <-as.character(anio_ini)
  anio_fin <-as.character(anio_fin)
  
  # Filtrar datos para el par de años actual
  datos_par <- data_modelo %>%
    filter(medicion %in% c(anio_ini, anio_fin))
  
  # Intentar estimar el modelo
  resultado <- tryCatch(
    {
      modelo <- feols(
        tasa_permanencia ~ tasa_migracion,
        data = datos_par,
        cluster = ~comuna)
      
      # Extraer estadísticas
      coef <- coef(modelo)["tasa_migracion"]
      se <- se(modelo)["tasa_migracion"]
      tval <- coef / se
      pval <- pvalue(modelo)["tasa_migracion"]
      R2<-r2(modelo, type = "r2")
      
      
      # Crear data frame de resultado
      data.frame(
        periodo = paste0(anio_ini, "-", anio_fin),
        beta = coef,
        se = se,
        t = tval,
        p = pval,
        f_primer_etapa = NA,
        R2 = R2,
        wR2 = NA
        
      )
      
    }, error = function(e) {
      # Si hay error, devolver fila con NAs
      data.frame(
        periodo = paste0(anio_ini, "-", anio_fin),
        beta = NA,
        se = NA,
        t = NA,
        p = NA,
        f_primer_etapa = NA,
        R2 = NA,
        wR2 = NA
      )
    })
  
  # Guardar resultado
  ols_efecto_inducido[[i]] <- resultado
}

# Unir todos los resultados
tabla_resultados_ols_efecto_inducido <- bind_rows(ols_efecto_inducido)

tabla_resultados_ols_efecto_inducido<- tabla_resultados_ols_efecto_inducido %>% 
  mutate(
    sig=case_when(
      p<=0.001~"***",
      p<=0.01~"**",
      p<=0.05~"*",
      p<=0.1~".",
      p>0.1~""
    ),
    modelo="Efecto inducido",
    metodo="OLS"
  )


resultados_finales<-rbind(
  tabla_resultados_vi_efecto_directo,
  tabla_resultados_vi_efecto_inducido,
  tabla_resultados_fe_efecto_directo,
  tabla_resultados_fe_efecto_inducido,
  tabla_resultados_ols_efecto_directo,
  tabla_resultados_ols_efecto_inducido
)

resultados_finales<-resultados_finales %>%
  filter(!is.na(beta)) %>% 
  select(periodo,modelo,metodo,beta,p,sig,se,t,f_primer_etapa,R2)


writexl::write_xlsx(resultados_finales,"resultados modelos consolidado.xlsx")



#PASO 5: REALIZAR PRUEBAS DE ROBUSTEZ

#### VI con TWFE #####
#Primera ventana de tiempo
datos_par <- data_modelo %>%
  filter(medicion %in% c(as.character(2009), as.character(2015)))

#Efecto directo
modelo <- feols(
  log_arriendo ~ 1 
  | codigoBarrioComunaUnificado + medicion 
  | tasa_migracion ~ VI_Migrantes,
  data = datos_par,
  cluster = ~comuna
)

F_KP<-fitstat(modelo, type = "ivwald")
f_kp<-F_KP$`ivwald1::tasa_migracion`$stat


#Efecto inducido
modelo <- feols(
  tasa_permanencia ~ 1 
  | codigoBarrioComunaUnificado + medicion 
  | tasa_migracion ~ VI_Migrantes,
  data = datos_par,
  cluster = ~comuna
)

primera_etapa<-fitstat(modelo, type = "ivf1")
f<-primera_etapa$`ivf1::tasa_migracion`[1]                                     
f<-as.numeric(f)

#Segunda ventana de tiempo
datos_par <- data_modelo %>%
  filter(medicion %in% c(as.character(2016), as.character(2019)))

#Efecto directo
modelo <- feols(
  log_arriendo ~ 1 
  | codigoBarrioComunaUnificado + medicion 
  | tasa_migracion ~ VI_Migrantes,
  data = datos_par,
  cluster = ~comuna
)

F_KP<-fitstat(modelo, type = "ivwald")
f_kp<-F_KP$`ivwald1::tasa_migracion`$stat

#Efecto inducido
modelo <- feols(
  tasa_permanencia ~ 1 
  | codigoBarrioComunaUnificado + medicion 
  | tasa_migracion ~ VI_Migrantes,
  data = datos_par,
  cluster = ~comuna
)

primera_etapa<-fitstat(modelo, type = "ivf1")
f<-primera_etapa$`ivf1::tasa_migracion`[1]                                     
f<-as.numeric(f)
vi_efecto_directo <- list()

