# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# desp-cdmx/censo/src/procesar-censo.R


# Paquetes ----------------------------------------------------------------
pacman::p_load(tidyverse, here, googledrive)


# Files out ---------------------------------------------------------------
files_out <- list(edades_censo_nacional = here("censo", "output", "edades_censo_nacional.rds"),
                  edades_censo_cdmx = here("censo", "output", "edades_censo_cdmx .rds"),
                  escolaridad_censo_cdmx = here("censo", "output", "escolaridad_censo_cdmx.rds"))


# Dowload censo -----------------------------------------------------------
files_censo <- drive_ls(as_id("14_hMR1u7ndp62O_KiHWZgYthL-cdOquN")) %>% 
      filter(str_detect(name, "Personas"))

tmp <- tempfile(fileext = ".csv")

dl <- drive_download(as_id(files_censo$id),
                     path = tmp,
                     overwrite = T)

censo <- read_csv(dl$local_path, locale = locale(encoding = "latin1")) %>% 
      select(ENT, ID_PERSONA, FACTOR, NUMPER, SEXO, EDAD, NIVACAD)


rm(tmp, dl)


# Edades censo ------------------------------------------------------------

### edades en censo nacional 
grupos_edad_censo <- censo %>%
      filter(EDAD != 9999) %>% 
      mutate(
            grupo_edad = case_when(EDAD<12 ~ "Menores de 12 años",
                                   EDAD %in% 12:17 ~ "De 12 a 17 años",
                                   EDAD %in% 18:23 ~ "De 18 a 23 años",
                                   EDAD %in% 24:29 ~ "De 24 a 29 años",
                                   EDAD %in% 30:44 ~ "De 30 a 44 años",
                                   EDAD %in% 45:59 ~ "De 45 a 59 años",
                                   EDAD>=60 ~ "60 años o más"),
            SEXO = case_when(SEXO == 1 ~ "Hombres",
                             SEXO == 3 ~ "Mujeres", 
                             T ~ "No especificado"),
            grupo_edad = factor(grupo_edad,
                                levels = c("Menores de 12 años",
                                           "De 12 a 17 años",
                                           "De 18 a 23 años",
                                           "De 24 a 29 años",
                                           "De 30 a 44 años",
                                           "De 45 a 59 años",
                                           "60 años o más"))) %>% 
      group_by(SEXO, grupo_edad) %>% 
      summarise(EDAD = sum(FACTOR, na.rm = T)) %>% 
      group_by(SEXO) %>% 
      mutate(pob = sum(EDAD, na.rm =T),
             fuente = "Censo")

write_rds(grupos_edad_censo, files_out$edades_censo_nacional)


### edades CDMX censo 
grupos_edad_cdmx <- censo %>%
      filter(EDAD != 9999 & ENT == "09") %>% 
      mutate(
            grupo_edad = case_when(EDAD<12 ~ "Menores de 12 años",
                                   EDAD %in% 12:17 ~ "De 12 a 17 años",
                                   EDAD %in% 18:23 ~ "De 18 a 23 años",
                                   EDAD %in% 24:29 ~ "De 24 a 29 años",
                                   EDAD %in% 30:44 ~ "De 30 a 44 años",
                                   EDAD %in% 45:59 ~ "De 45 a 59 años",
                                   EDAD>=60 ~ "60 años o más"),
            SEXO = case_when(SEXO == 1 ~ "Hombres",
                             SEXO == 3 ~ "Mujeres", 
                             T ~ "No especificado"),
            grupo_edad = factor(grupo_edad,
                                levels = c("Menores de 12 años",
                                           "De 12 a 17 años",
                                           "De 18 a 23 años",
                                           "De 24 a 29 años",
                                           "De 30 a 44 años",
                                           "De 45 a 59 años",
                                           "60 años o más"))) %>% 
      group_by(SEXO, grupo_edad) %>% 
      summarise(EDAD = sum(FACTOR, na.rm = T)) %>% 
      group_by(SEXO) %>% 
      mutate(pob = sum(EDAD, na.rm =T),
             fuente = "Censo")

write_rds(grupos_edad_cdmx, files_out$edades_censo_cdmx)


# Escolaridad Censo -------------------------------------------------------------
escolaridad_censo_cdmx <- censo %>% 
      filter(ENT == "09") %>% 
      select(NIVACAD, EDAD, SEXO, FACTOR) %>% 
      mutate(NIVACAD = case_when(NIVACAD %in% c("00") ~ "ninguno", 
                                 NIVACAD %in% c("01") ~ "otra", 
                                 NIVACAD %in% c("02") ~ "primaria", 
                                 NIVACAD %in% c("03") ~ "secundaria", 
                                 NIVACAD %in% c("04", "05", "09") ~ "preparatoria",
                                 NIVACAD %in% c("10", "11") ~ "licenciatura",
                                 NIVACAD %in% c("12", "13", "14") ~ "posgrado",
                                 NIVACAD %in% c("06", "07", "08") ~ "profesional técnico",
                                 is.na(NIVACAD) ~ "No aplica",
                                 T ~ "No especificado"),
             SEXO = case_when(SEXO == 1 ~ "Hombres",
                              SEXO == 3 ~ "Mujeres", 
                              T ~ "No especificado")) %>%
      filter(NIVACAD != "No aplica") %>%
      group_by(NIVACAD) %>% 
      summarise(total = sum(FACTOR, na.rm = T)) %>% 
      mutate(pob = sum(total, na.rm =T),
             fuente = "Censo")

write_rds(escolaridad_censo_cdmx, files_out$escolaridad_censo_cdmx)

# DONE. 