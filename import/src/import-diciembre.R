# Maintainers: JRR, OE
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# desp-cdmx/import/src/import-diciembre.R


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, here, readxl, janitor, lubridate)


# Files  ------------------------------------------------------------------
source(here("import", "src", "lists.r"))

files <- list(registro_cbpcdmx = here("import", "input", "REGISTRO INTERNO CBP 31 dic.xlsx"),
              geo_catalog = here("import", "input", "geo_catalog.csv"),
              registro_clean_dic = here("import", "output", "registro_cdmx_clean_dic.rds"),
              registro_clean_csv = here("registro-limpio", "registro_cdmx_clean_dic.csv"))


# clean_text <- function(s){
#str_squish(toupper(stri_trans_general(s, "latin-ascii")))
# }


# Load geo catalog --------------------------------------------------------
geo_catalog <- read_csv(files$geo_catalog) %>% 
      mutate(name_mun = tolower(name_mun),
             name_ent = tolower(name_ent),
             name_ent = ifelse(name_ent == "distrito federal", "ciudad de méxico",
                               name_ent)) %>% 
      select(-name_ent_short)


# clean data ---------------------------------------------------------------------
keep_cols <- c("id", "inegi", "fecha_de_inicio_de_reporte_en_la_cbpcdmx", "fuente_de_reporte", "origen_de_la_noticia",
               "sexo", "edad", "lugar_de_nacimiento", "nacionalidad", "escolaridad", "estado_civil", 
               "parentesco_reportante", "fecha_de_desaparicion", "year_desaparicion", "cve_ent_desp",
               "entidad_de_desaparicion", "cv_mun_desp", "municipio_alcadia_de_la_desaparicion","colonia_de_la_desaparicion",
               "circunstancias_de_la_desaparicion", "estatus_de_localizacion", "condicion_localizacion",
               "fecha_de_localizacion", "year_localizacion", "estado_de_salud", "constancia_de_localizacion",
               "municipio_alcaldia_de_la_localizacion", "tipo_de_lugar_de_la_localizacion", "motivo_referido_de_desaparicion")



clean_desaparicion <- function(files) {
      
      df <- read_xlsx(files$registro_cbpcdmx) %>% 
            clean_names() %>% 
            mutate_if(is.character, str_to_lower) %>% 
            mutate(id = as.character(id),
                   fecha_de_inicio_de_reporte_en_la_cbpcdmx = gsub('/', '-', fecha_de_inicio_de_reporte_en_la_cbpcdmx),
                   fecha_de_inicio_de_reporte_en_la_cbpcdmx = lubridate::dmy(fecha_de_inicio_de_reporte_en_la_cbpcdmx),
                   fecha_de_desaparicion = gsub('/', '-', fecha_de_desaparicion),
                   fecha_de_desaparicion = lubridate::dmy(fecha_de_desaparicion),
                   year_desaparicion = lubridate::year(fecha_de_desaparicion),
                   fecha_de_localizacion = gsub('/', '-', fecha_de_localizacion),
                   fecha_de_localizacion = lubridate::dmy(fecha_de_localizacion),
                   year_localizacion = lubridate::year(fecha_de_localizacion),
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
                         escolaridad %in% c(otra_educacion) ~ "otra",escolaridad %in% c(tecnico) ~ "profesional tecnico",
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
                         T ~ estado_civil),
                   parentesco_reportante = case_when(
                         parentesco_reportante %in% c(familiar) ~ "familiar", 
                         parentesco_reportante %in% c(pareja) ~ "pareja o expareja", 
                         parentesco_reportante %in% c(otro_parentesco) ~ "otros",
                         parentesco_reportante %in% c("s/d") ~ "no especificado",
                         T ~ parentesco_reportante),
                   estado_de_salud = case_when(
                         estado_de_salud %in% c(buen_estado) ~ "buen estado de salud",
                         estado_de_salud %in% c(mal_estado_otros) ~ "mal estado sin violencia",
                         T ~ estado_de_salud),
                   tipo_de_lugar_de_la_localizacion = case_when(
                         tipo_de_lugar_de_la_localizacion %in% c(abordo_vehiculo) ~ "a bordo de un vehículo",
                         tipo_de_lugar_de_la_localizacion %in% c(casa_famcon) ~ "en casa de amigo o familiar",
                         tipo_de_lugar_de_la_localizacion %in% c(domicilio_propio) ~ "domicilio de la persona",
                         tipo_de_lugar_de_la_localizacion %in% c(ministerio_publico) ~ "agencia del ministerio público",
                         tipo_de_lugar_de_la_localizacion %in% c(central_camionera) ~ "en una central o estacion de transporte",
                         tipo_de_lugar_de_la_localizacion %in% c(centro_salud) ~ "en centro de salud u hospital",
                         tipo_de_lugar_de_la_localizacion %in% c(centro_adicciones) ~ "en un centro de adicciones",
                         tipo_de_lugar_de_la_localizacion %in% c(centro_detencion) ~ "en un centro de detención",
                         tipo_de_lugar_de_la_localizacion == "alcaldia gustavo a. madero" ~ "otro",
                         T ~ tipo_de_lugar_de_la_localizacion),
                   municipio_alcadia_de_la_desaparicion = case_when(
                         municipio_alcadia_de_la_desaparicion == "acapulco de juarez" ~ "acapulco de juárez",
                         municipio_alcadia_de_la_desaparicion == "alvaro obregon" ~ "álvaro obregón",
                         municipio_alcadia_de_la_desaparicion == "benito juarez" ~ "benito juárez",
                         municipio_alcadia_de_la_desaparicion == "chimalhuacan" ~ "chimalhuacán",
                         municipio_alcadia_de_la_desaparicion == "coyoacan" ~ "coyoacán",
                         municipio_alcadia_de_la_desaparicion == "cuauhtemoc" ~ "cuauhtémoc",
                         municipio_alcadia_de_la_desaparicion == "tlahuac" ~ "tláhuac",
                         municipio_alcadia_de_la_desaparicion == "tultitlan" ~ "tultitlán",
                         municipio_alcadia_de_la_desaparicion == "atizapan de zaragoza" ~ "atizapán de zaragoza",
                         municipio_alcadia_de_la_desaparicion == "naucalpan de juarez" ~ "naucalpan de juárez",
                         T ~ municipio_alcadia_de_la_desaparicion),
                   entidad_de_desaparicion = case_when(
                         entidad_de_desaparicion == "ciudad de mexico" ~ "ciudad de méxico",
                         entidad_de_desaparicion == "mexico" ~ "méxico",
                         entidad_de_desaparicion == "veracruz" ~ "veracruz de ignacio de la llave",
                         T ~ entidad_de_desaparicion)) %>% 
            rename(name_ent = entidad_de_desaparicion,
             name_mun = municipio_alcadia_de_la_desaparicion) %>% 
            left_join(geo_catalog, by = c("name_ent", "name_mun")) %>% 
            rename(entidad_de_desaparicion = name_ent,
             cve_ent_desp = id_ent,
             municipio_alcadia_de_la_desaparicion = name_mun,
             cv_mun_desp = id_mun) %>%
            mutate(inegi = paste0(cve_ent_desp, cv_mun_desp)) %>% 
            select(all_of(keep_cols))
      
      return(df)
      
      }



main <- function(){
      print("working on registro desaparición cdmx")
      desp_cdmx <- clean_desaparicion(files)
      
      print("writing fosas cdmx")
      saveRDS(desp_cdmx, files$registro_clean_dic) 
      
      print("writing fosas cdmx")
      write_csv(desp_cdmx, files$registro_clean_csv)
}

main()


# DONE.