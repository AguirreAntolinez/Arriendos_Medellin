####AGREGACIÓN DE VARIABLES ECV######

#Author: William Alexander Aguirre Antolínez
#Date: 10/11/2024
#Description: Este script realiza la agregación de variables a nivel de barrio
#y de comuna para las muestras de arrendamientos entre el 2008 y 2019 del OIME. 
#Estos datos se utilizan para la investigación sobre el efecto de la migraición
#sobre el precio de los arriendos.

##Para detalles y comentarios: alexander.aguirre@udea.edu.co


#Generar agregación por barrio
rentas_barrio<-rentas_depurado %>%
  group_by(sk_barrio_medicion,barrio,medicion) %>% 
  summarise(
    cantidad_observaciones=n(),
    valor_comercial=mean(valor_comercial),
    valor_m2=mean(valorm2),
    sd_valor_comercial = sd(valorm2, na.rm = TRUE))


#Generar agregación por comuna
rentas_comuna<-rentas_depurado %>%
  mutate(skComunaMedicion=paste0(comuna,"_",medicion)) %>% 
  group_by(skComunaMedicion,comuna,medicion) %>% 
  summarise(
    cantidad_observaciones=n(),
    valor_comercial=mean(valor_comercial),
    valor_m2=mean(valorm2),
    sd_valor_comercial = sd(valorm2, na.rm = TRUE)) 

rentas_comuna %>%  group_by(medicion) %>% summarise(n(comunas))
