# Maintainers: JRR, OE
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# desp-cdmx/import/src/import.R


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, here, readxl, janitor, stringi, fuzzyjoin, lubridate)



# Files  ------------------------------------------------------------------
files <- list(registro_cbpcdmx = here("import", "input", "REGISTRO INTERNO CBP 31 dic.xlsx"),
              registro_clean_dic = here("import", "output", "registro_cdmx_clean_dic.rds"))

# clean_text <- function(s){
#str_squish(toupper(stri_trans_general(s, "latin-ascii")))
# }


source(here("import", "src", "lists.r"))

# Data ---------------------------------------------------------------------
keep_cols <- c("id", "fecha_de_inicio_de_reporte_en_la_cbpcdmx", "fuente_de_reporte", "origen_de_la_noticia",
               "sexo", "edad", "lugar_de_nacimiento", "nacionalidad", "escolaridad", "estado_civil", 
               "parentesco_reportante", "fecha_de_desaparicion", "año_desaparicion", "entidad_de_desaparicion",
               "clave_municipio_desp","municipio_alcadia_de_la_desaparicion", "clave_colonia_desp",
               "colonia_de_la_desaparicion", "circunstancias_de_la_desaparicion", "estatus_de_localizacion",
               "condicion_localizacion", "fecha_de_localizacion", "año_localizacion", "estado_de_salud",
               "constancia_de_localizacion", "municipio_alcaldia_de_la_localizacion", 
               "tipo_de_lugar_de_la_localizacion", "motivo_referido_de_desaparicion", "delito_del_que_fue_victima")





df <- read_xlsx(files$registro_cbpcdmx) %>% 
      clean_names() %>% 
      mutate_if(is.character, str_to_lower)
      



tempo2 <- df %>% mutate_if(is.character, str_to_lower) %>% 
      mutate(fecha_de_desaparicion = gsub('/', '-', fecha_de_desaparicion),
              fecha_de_desaparicion = lubridate::dmy(fecha_de_desaparicion),
              año_desaparicion = lubridate::year(fecha_de_desaparicion),
              fecha_de_localizacion = gsub('/', '-', fecha_de_localizacion),
              fecha_de_localizacion = lubridate::dmy(fecha_de_localizacion),
              año_localizacion = lubridate::year(fecha_de_localizacion),
              condicion_localizacion = replace_na(condicion_localizacion, "sigue desaparecida"),
       origen_de_la_noticia = case_when(
             origen_de_la_noticia %in% c(autoridades_en_general) ~ "autoridades en general",
             origen_de_la_noticia %in% c(sociedad_civil) ~ "sociedad civil y organismos internacionales",
             origen_de_la_noticia %in% c(colectivos_familias) ~ "colectivos de familiares",
             origen_de_la_noticia %in% c(comisiones_busqueda) ~ "comisiones de búsqueda",
             origen_de_la_noticia %in% c(redes_sociales_telefono) ~ "redes sociales y reportes telefónicos",
             origen_de_la_noticia %in% c(consejos_ciudadanos) ~ "consejos ciudadanos de búsqueda",
             origen_de_la_noticia %in% c(consulados) ~ "consulados", 
             origen_de_la_noticia %in% c(portal_cnb) ~ "portal CNB",
             origen_de_la_noticia %in% c("pendiente", "se desconoce") ~ "no especificado",
             T ~ origen_de_la_noticia),
       escolaridad = case_when(
             escolaridad %in% c(preparatoria) ~ "preparatoria", 
             escolaridad %in% c(otra_educacion) ~ "otra",
             escolaridad %in% c(tecnico) ~ "profesional tecnico",
             escolaridad %in% c(posgrado) ~ "posgrado",
             escolaridad %in% c("indeterminada", "s/d") ~ "no especificado",
             T ~ escolaridad),
       estado_civil = case_when(
             estado_civil %in% c(casado) ~ "casado/a",
             estado_civil %in% c(divorciado) ~ "divorciado/a",
             estado_civil %in% c(soltero) ~ "soltero/a",
             estado_civil %in% c(concubino) ~ "concubino/a",
             estado_civil %in% c(separado) ~ "separaado/a",
             estado_civil %in% c(viudo) ~ "viudo/a",
             estado_civil %in% c("sociedad en con") ~ "en sociedad",
             T ~ estado_civil)) %>% 
      select(estado_civil)
       


# Aqui me quedé -----------------------------------------------------------


       
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
                                estado_de_salud),
       municipio_alcaldia_de_la_localizacion = ifelse(municipio_alcaldia_de_la_localizacion == "ecatepec", "ecatepec de morelos", 
                                                      municipio_alcaldia_de_la_localizacion),
       municipio_alcaldia_de_la_localizacion = ifelse(municipio_alcaldia_de_la_localizacion == "tlalnepantla", "tlalnepantla de baz", 
                                                      municipio_alcaldia_de_la_localizacion),
       tipo_de_lugar_de_la_localizacion = case_when(
             tipo_de_lugar_de_la_localizacion %in% c("a bordo de vehiculo particular", 
                                                     "dentro de un vehiculo") ~ "a bordo de un vehículo",
             tipo_de_lugar_de_la_localizacion %in% c("en casa de un amigo(a)", "en casa de un familiar", 
                                                     "en casa de una amistad o conocido") ~ "en casa de amigo o familiar",
             tipo_de_lugar_de_la_localizacion %in% c("en su domicilio", "en su propio domicilio") ~ "domicilio de la persona",
             tipo_de_lugar_de_la_localizacion %in% c("en un ministerio publico", "en un ministerio publico o lugar administrativo",
                                                     "en una agencia del ministerio publico") ~ "Agencia del ministerio público",
             tipo_de_lugar_de_la_localizacion == "en una central camionera" ~ "en una central o estacion de transporte",
             tipo_de_lugar_de_la_localizacion == "en un centro hospitalario" ~ "en centro de salud u hospital",
             tipo_de_lugar_de_la_localizacion == "en una institucion para adicciones" ~ "en un centro de adicciones",
             T ~ tipo_de_lugar_de_la_localizacion), 
       delito_del_que_fue_victima = case_when(
             delito_del_que_fue_victima %in% c("asalto con violencia", "asalto o robo", "asalto y lesiones") ~ "asalto",
             delito_del_que_fue_victima %in% c("homicidio", "homicidio o tentativa de homicidio", 
                                               "privacion de la libertad personal y homicidio") ~ "homicidio o tentativa de homicidio",
             delito_del_que_fue_victima == "NINGUNO" ~ "ninguno", 
             delito_del_que_fue_victima %in% c("plagio, secuestro o tentativa de secuestro", "secuestro y extorsión",
                                               "secuestro y robo", "tentiva de secuestro", 
                                               "secuestro") ~ "secuestro o tentativa de secuestro",
             delito_del_que_fue_victima %in% c("privacion ilegal de la libertad, lesiones y robo", 
                                               "privacion ilegal de la libertad, robo") ~ "privacion ilegal de la libertad",
             delito_del_que_fue_victima %in% c("retencion de menores", "retención de menores", "retencion y sustraccion de menores",
                                               "sustraccion de menor", "sustraccion o retencion de nna", 
                                               "sustraccion parental", "sustracción parental") ~ "retención o sustracción de menores",
             delito_del_que_fue_victima == "Robo" ~ "robo",
             T ~ delito_del_que_fue_victima)) %>% 
      rename(clave_municipio_desp = clave_municipio,
             clave_colonia_desp = clave_colonia) %>% 
      select(all_of(keep_cols))

     





