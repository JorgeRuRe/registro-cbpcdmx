# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# desp-cdmx/import/src/import.R


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, here, readxl, janitor, stringi, fuzzyjoin, lubridate)



# Files  ------------------------------------------------------------------
files <- list(registro_cbpcdmx = here("import", "input", "REGISTRO INTERNO CBP 1 mayo con claves.xlsx"),
              colonias_cdmx = here("import", "input", "coloniascdmx_ids1.xlsx"),
              registro_clean = here("import", "output", "registro_cdmx_clean.rds"))

clean_text <- function(s){
      str_squish(toupper(stri_trans_general(s, "latin-ascii")))
}

# Data ---------------------------------------------------------------------
registro_cbpcdmx <- read_xlsx(files$registro_cbpcdmx) %>% 
   clean_names()


test <- registro_cbpcdmx  %>% 
   mutate(año_desaparicion = lubridate::year(fecha_de_desaparicion),
          año_localizacion = lubridate::year(fecha_de_localizacion),
          clave_municipio = str_pad(clave_municipio, width = 2, side = "left", pad = "0"),
          condicion_localizacion = replace_na(condicion_localizacion, "sigue desaparecida"),
          origen_de_la_noticia = case_when(
             origen_de_la_noticia %in% c("alcaldia azcapotzalco", "alcaldía azcapotzalco", 
                                       "comision ejecutiva de atencion a victimas",
                                       "comision ejecutiva de atencion a victimas de la ciudad de mexico",
                                       "comision de derechos humanos de la ciudad de mexico",
                                       "comision nacional de derechos humanos", "congreso de la cdmx",
                                       "congreso local", 
                                       "fiscalia especializada en la busqueda, localización e investigación de personas desaparecidas",
                                       "mecanismo de proteccion integral a periodistas y defensores de ddhh",
                                       "procuraduria de proteccion de derechos de niños, niñas y adolescentes de la ciudad de mexico",
                                       "procuraduria general de justicia de la ciudad de mexico",
                                       "secretaria de gobierno de la ciudad de mexico",
                                       "secretaria de las mujeres de la ciudad de méxico",
                                       "secretaria de seguridad ciudadana") ~ "autoridades en general",
             origen_de_la_noticia %in% c("casa frida para juventud lgbti", "centro PRODH",
                                       "fundación para la justicia y el estado democrático de derecho", 
                                       "red por los derechos de la infancia mexico", "ONG", "naciones unidas",
                                       "oficina del alto comisionado para los ddhh", 
                                       "universidad autonoma metropolitana", "Universidad Ibeoramericana",
                                       "consejo ciudadano para la seguridad y justicia de la ciudad de mexico") ~ "sociedad civil y organismos internacionales",
             origen_de_la_noticia %in% c("colectivo", "colectivo 'hasta encontrarles' de la ciudad de méxico", 
                                       "colectivo familiares en busqueda maria herrera", 
                                       "colectivo familias de desaparecidos orizaba cordoba ac", "colectivo mariposas destellando",
                                       "colectivo red eslabones por los derechos humanos", 
                                       "colectivo uniendo esperanzas edomex") ~ "colectivos de familiares",
             origen_de_la_noticia %in% c("comision de busqueda de personas del estado de mexico", 
                                       "comision local de busqueda de morelos", "comision local de busqueda de personas de hidalgo",
                                       "comision local de busqueda de personas de puebla",
                                       "comision local de busqueda de personas de san luis potosi", "comision local de busqueda de personas de tamaulipas",
                                       "comision local de busqueda de personas de veracruz",
                                       "comision local de busqueda de personas del estado de nuevo leon", 
                                       "comision local de busqueda de queretaro\r\n", "comision local de busqueda de veracruz",
                                       "comision local de busqueda del estado de mexico", "comision local del estado de queretaro",
                                       "comision nacional de busqueda") ~ "comisiones de búsqueda",
             origen_de_la_noticia %in% c("facebook", "twitter", "911 ") ~ "redes sociales y 911",
             origen_de_la_noticia %in% c("consejo ciudadano de busqueda de la ciudad de mexico", 
                                       "consejo ciudadano del sistema nacional de busqueda") ~ "consejos ciudadanos de búsqueda",
             origen_de_la_noticia %in% c("consulado de colombia", "consulado de colombia en mexico") ~ "consulados", 
             origen_de_la_noticia %in% c("portal de la comision nacional de busqueda") ~ "portal comision búsqueda cdmx"),
          lugar_de_nacimiento = ifelse(lugar_de_nacimiento == "ciudad de mexico", " ciudad de méxico", 
                                         lugar_de_nacimiento),
          lugar_de_nacimiento = ifelse(lugar_de_nacimiento == "estado de mexico", "méxico", 
                                       lugar_de_nacimiento),
          lugar_de_nacimiento = ifelse(lugar_de_nacimiento == "michoacán de ocampo", "michoacán", 
                                         lugar_de_nacimiento),
          lugar_de_nacimiento = ifelse(lugar_de_nacimiento == "michoacan", "michoacán", 
                                         lugar_de_nacimiento),
          lugar_de_nacimiento = ifelse(lugar_de_nacimiento == "Puebla", "puebla", 
                                       lugar_de_nacimiento),
          lugar_de_nacimiento = ifelse(lugar_de_nacimiento == "tampico", "tamaulipas", 
                                       lugar_de_nacimiento),
          lugar_de_nacimiento = ifelse(lugar_de_nacimiento == "veracruz de ignacio de la llave", "veracruz", 
                                       lugar_de_nacimiento),
          escolaridad = case_when(
             escolaridad %in% c("bachillerato", "c.c.h.") ~ "preparatoria", 
             escolaridad %in% c("comercial", "diplomado", "especialidad", "normal", "vocacional") ~ "otra",
             escolaridad %in% c("tecnico") ~ "profesional tecnico",
             escolaridad %in% c("maestria", "doctorado") ~ "posgrado",
             T ~ escolaridad),
          estado_civil = ifelse(estado_civil == "divorciado", "divorciado(a)", 
                                       estado_civil), 
          parentesco_reportante = case_when(
             parentesco_reportante %in% c("abuela materna", "abuela paterna", "abuelo materno", "abuelo(a)", "cuñada (o)",
                                          "cunado(a)", "hermano(a)", "hijastro(a)", "hijo", "hijo(a)", "madre", "madre adoptiva",
                                          "medio hermano(a) materno(a)", "medio hermano(a) paterno(a)", "nieta materna", 
                                          "nieto (a)", "nieto materno", "nuera", "nuera o yerno", "padrastro", 
                                          "padre", "prima materna", "prima paterna", "primo materno", "primo paterno",
                                          "primo(a)", "sobrina materna", "sobrina paterna", "sobrino materno", 
                                          "sobrino paterno", "sobrino(a)", "suegro(a)", "tia abuela", "tia materna",
                                          "tia materna, madre", "tia paterna", "tio (a)", "tio materno", 
                                          "tio(a)", "yerno") ~ "familiar", 
             parentesco_reportante %in% c("companero(a)/esposo(a)", "compañero(a)/esposo(a)", "concubina", "concubina (o)",
                                          "concubinario", "conyuge", "expareja", "novio(a)") ~ "pareja o expareja", 
             parentesco_reportante %in% c("abogado", "abogado (a)", "amigo/conocido", "contacto de empleo", 
                                          "jefe", "otro", "tutor", "tutor (a)", "vecino", "vecino(a)") ~ "otros"),
          entidad_de_desaparicion = ifelse(entidad_de_desaparicion == "ciudad de mexico", "ciudad de méxico", 
                                           entidad_de_desaparicion), 
          entidad_de_desaparicion = ifelse(entidad_de_desaparicion == "estado de mexico", "méxico", 
                                           entidad_de_desaparicion),
          entidad_de_desaparicion = ifelse(entidad_de_desaparicion == "veracruz de ignacio de la llave", "veracruz", 
                                           entidad_de_desaparicion),
          municipio_alcadia_de_la_desaparicion = ifelse(municipio_alcadia_de_la_desaparicion == "alvaro obregon", "alvaro obregón", 
                                                        municipio_alcadia_de_la_desaparicion),
          estado_de_salud = ifelse(estado_de_salud == "buen estado general", "buen estado", 
                                                        estado_de_salud))


# falta limpiar 1) municipio_alcadia_de_la_localizacion; 2) motivo_referido_de_desaparicion 3) delito_del_que_fue_victima
          
   

p %>% summarise(suma = sum(n))
test %>% count(origen_de_la_noticia, sort = T) %>% 
   summarise(suma = sum(n))


registro_cbpcdmx %>% count(parentesco_reportante, sort = T) %>% 
   na.omit() %>% 
   summarise(suma = sum(n))



colonias_cdmx <- read_csv(files$colonias_cdmx) %>% 
      mutate(nombre = clean_text(nombre),
             alcaldia = clean_text(alcaldia),
             cve_alc = str_pad(cve_alc, width = 3, side = "left", pad = "0")) %>% 
      rename(colonia = nombre)


test_fuzzy <- registro_cbpcdmx %>% 
   filter(!is.na(alcaldia) & !is.na(colonia)) %>% 
   stringdist_left_join(colonias_cdmx, by = c("alcaldia", "colonia"), method = "lv",
                        max_dist = 2, distance_col = "distance") %>% 
   select(alcaldia.x, alcaldia.y, cve_alc, 
          colonia.x, colonia.y, cve_col, 
          alcaldia.distance, colonia.distance, everything()) # X es registro y Y son alcaldías 
   
   
# fin 


