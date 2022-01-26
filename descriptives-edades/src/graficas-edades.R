# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2022, Data Cívica GPL v2 or later
# ===========================================================
# desp-cdmx/descriptives-edades/src/gráficas-edades.R

# Paquetes ----------------------------------------------------------------
pacman::p_load(tidyverse, sf, here, svglite, scales, readxl,
               lubridate, patchwork, janitor)



# Files -------------------------------------------------------------------
files <- list(registro_cbpcdmx_clean = here("import", "output", "registro_cdmx_clean_dic.rds"),
              rnpdno_clean = here("descriptives-edades", "input", "BASE-DE-DATOS-Personas-desaparecidas-2011-2020.xlsx"),
              edades_censo_nacional = here("censo", "output", "edades_censo_nacional.rds"),
              edades_censo_cdmx = here("censo", "output", "edades_censo_cdmx.rds"),
              perfiles_ambos_png = here("descriptives-edades", "output", "perfiles_ambos."), 
              perfiles_ambos_estatus_png = here("descriptives-edades", "output", "perfiles_ambos_estatus."))

devices <- c("png", "svg")


# Data Desp ------------------------------------------------------------------
registro_cbpcdmx_clean <- read_rds(files$registro_cbpcdmx_clean)
rnpdno_clean <- read_excel(files$rnpdno_clean) %>% 
      clean_names() %>% 
      mutate(condicion_actual_de_las_personas = 
                   ifelse(condicion_actual_de_las_personas == "LOCALIZADAS CON VIDA", "con vida",
                          condicion_actual_de_las_personas),
             condicion_actual_de_las_personas = 
                   ifelse(condicion_actual_de_las_personas == "LOCALIZADAS SIN VIDA", "sin vida",
                          condicion_actual_de_las_personas),
             condicion_actual_de_las_personas = 
                   ifelse(condicion_actual_de_las_personas == "DESAPARECIDAS Y NO LOCALIZADAS", "sigue desaparecida",
                          condicion_actual_de_las_personas))

edades_censo_nacional <- read_rds(files$edades_censo_nacional) %>% 
      rename(total_poblacion = EDAD,
             pob_censo = pob,
             sexo = SEXO)
edades_censo_cdmx <- read_rds(files$edades_censo_cdmx) %>% 
      rename(total_poblacion = EDAD,
             pob_censo = pob,
             sexo = SEXO)

### referencia pob cdmx 
edades_censo_cdmx %>% 
      mutate(prop = (total_poblacion/pob_censo)*100)

# Graf 1 ------------------------------------------------------------------
p1 <- registro_cbpcdmx_clean %>% 
      mutate(sexo = ifelse(sexo == "hombre", "Hombres",
                           sexo),
             sexo = ifelse(sexo == "mujer", "Mujeres",
                           sexo),
            grupo_edad = case_when(edad<12 ~ "Menores de 12 años",
                                   edad %in% 12:17 ~ "De 12 a 17 años",
                                   edad %in% 18:23 ~ "De 18 a 23 años",
                                   edad %in% 24:29 ~ "De 24 a 29 años",
                                   edad %in% 30:44 ~ "De 30 a 44 años",
                                   edad %in% 45:59 ~ "De 45 a 59 años",
                                   edad>=60 ~ "60 años o más"),
            grupo_edad = factor(grupo_edad,
                                levels = c("Menores de 12 años",
                                           "De 12 a 17 años",
                                           "De 18 a 23 años",
                                           "De 24 a 29 años",
                                           "De 30 a 44 años",
                                           "De 45 a 59 años",
                                           "60 años o más"))) %>% 
      group_by(sexo, grupo_edad) %>%
      summarize(total=n()) %>%
      mutate(den=sum(total, na.rm=T)) %>%
      ungroup() %>%
      na.omit() %>% 
      left_join(edades_censo_cdmx, by = c("sexo", "grupo_edad")) %>% 
      mutate(tasa = (total/total_poblacion)*100000,
             tasa = round(tasa, 2)) %>% 
      mutate(order_edad= as.numeric(grupo_edad)) %>% 
      ggplot(data=., aes(x = sexo, y = reorder(grupo_edad, -order_edad), fill = tasa)) +
      geom_tile(color="black") +
      scale_fill_gradient(low="#F9E0D9", high="#F85A3E", name = "Tasa")+ 
      labs(title= NULL,
           subtitle = "CDMX",
           x="", y="", fill="") +
      geom_text(aes(label= tasa), size=3.5, hjust=.2, vjust=.2, color="black") +
      theme_minimal(base_family = "Courier New") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y = element_text(face = "bold", size=10.5),
            axis.text.x = element_text(face = "bold", size=10.5)) +
      scale_x_discrete(position = "top")




p2 <- rnpdno_clean %>% 
      mutate(
            grupo_edad = case_when(edad<12 ~ "Menores de 12 años",
                                   edad %in% 12:17 ~ "De 12 a 17 años",
                                   edad %in% 18:23 ~ "De 18 a 23 años",
                                   edad %in% 24:29 ~ "De 24 a 29 años",
                                   edad %in% 30:44 ~ "De 30 a 44 años",
                                   edad %in% 45:59 ~ "De 45 a 59 años",
                                   edad>=60 ~ "60 años o más"),
            grupo_edad = factor(grupo_edad,
                                levels = c("Menores de 12 años",
                                           "De 12 a 17 años",
                                           "De 18 a 23 años",
                                           "De 24 a 29 años",
                                           "De 30 a 44 años",
                                           "De 45 a 59 años",
                                           "60 años o más"))) %>% 
      group_by(condicion_actual_de_las_personas, edad, grupo_edad)  %>% 
      summarise(hombre = sum(hombres, na.rm = T),
                mujer = sum(mujeres, na.rm = T)) %>% 
      ungroup() %>% 
      select(-edad) %>% 
      pivot_longer(cols = -c(condicion_actual_de_las_personas, grupo_edad),
                   names_to = "sexo",
                   values_to = "total") %>% 
      mutate(sexo = ifelse(sexo == "hombre", "Hombres",
                          sexo),
            sexo = ifelse(sexo == "mujer", "Mujeres",
                          sexo)) %>% 
      group_by(sexo, grupo_edad) %>%
      summarize(total= sum(total, na.rm = T)) %>%
      mutate(den=sum(total, na.rm=T)) %>%
      ungroup() %>%
      na.omit() %>%
      left_join(edades_censo_nacional, by = c("sexo", "grupo_edad")) %>% 
       mutate(tasa = (total/total_poblacion)*100000,
              tasa = round(tasa, 2)) %>% 
      mutate(order_edad= as.numeric(grupo_edad)) %>% 
      ggplot(data=., aes(x = sexo, y = reorder(grupo_edad, -order_edad), fill = tasa)) +
      geom_tile(color="black") +
      scale_fill_gradient(low="#F9E0D9", high="#F85A3E", name = "Tasa") + 
      labs(title= NULL,
           subtitle = "NACIONAL",
           x="", y="", fill="") +
      geom_text(aes(label=tasa), size=3.5, hjust=.2, vjust=.2, color="black")+
      theme_minimal(base_family = "Courier New") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y = element_text(face = "bold", size=10.5),
            axis.text.x = element_text(face = "bold", size=10.5)) +
      scale_x_discrete(position = "top")


### join plots 
p2 + p1 +
      plot_annotation(
            title = "Perfiles de personas desaparecidas con base en su edad y sexo"
      ) &
      theme(
            plot.title = element_text(face = "bold", hjust = 0.5))

walk(devices, ~ ggsave(filename = file.path(paste0(files$perfiles_ambos_png, .x)),
                       device = .x, width = 24, height = 14))



# Graf 2  -----------------------------------------------------------------
p3 <- rnpdno_clean %>% 
      mutate(
            grupo_edad = case_when(edad<12 ~ "Menores de 12 años",
                                   edad %in% 12:17 ~ "De 12 a 17 años",
                                   edad %in% 18:23 ~ "De 18 a 23 años",
                                   edad %in% 24:29 ~ "De 24 a 29 años",
                                   edad %in% 30:44 ~ "De 30 a 44 años",
                                   edad %in% 45:59 ~ "De 45 a 59 años",
                                   edad>=60 ~ "60 años o más"),
            grupo_edad = factor(grupo_edad,
                                levels = c("Menores de 12 años",
                                           "De 12 a 17 años",
                                           "De 18 a 23 años",
                                           "De 24 a 29 años",
                                           "De 30 a 44 años",
                                           "De 45 a 59 años",
                                           "60 años o más"))) %>% 
      group_by(condicion_actual_de_las_personas, edad, grupo_edad)  %>% 
      summarise(hombre = sum(hombres, na.rm = T),
                mujer = sum(mujeres, na.rm = T)) %>% 
      ungroup() %>% 
      select(-edad) %>% 
      pivot_longer(cols = -c(condicion_actual_de_las_personas, grupo_edad),
                   names_to = "sexo",
                   values_to = "total") %>% 
      mutate(sexo = ifelse(sexo == "hombre", "Hombres",
                           sexo),
             sexo = ifelse(sexo == "mujer", "Mujeres",
                           sexo), 
             condicion_actual_de_las_personas = factor(condicion_actual_de_las_personas,
                                                       levels = c("con vida",
                                                                  "sin vida",
                                                                  "sigue desaparecida"))) %>% 
      group_by(sexo, condicion_actual_de_las_personas, grupo_edad) %>% 
      summarize(total= sum(total, na.rm = T)) %>%
      mutate(den=sum(total, na.rm=T)) %>%
      ungroup() %>%
      na.omit() %>%
      left_join(edades_censo_nacional) %>% 
      mutate(tasa = (total/total_poblacion)*100000,
             tasa = round(tasa, 2)) %>% 
      mutate(order_edad= as.numeric(grupo_edad)) %>% 
      ggplot(data=., aes(x = sexo, y = reorder(grupo_edad, -order_edad), fill = tasa)) +
      geom_tile(color="black") +
      scale_fill_gradient(low="#F9E0D9", high="#F85A3E", name = "Tasa")+ 
      labs(title=NULL,
           subtitle = "NACIONAL",
           x="", y="", fill="") +
      geom_text(aes(label=tasa), size=3.0, hjust=.2, vjust=.2, color="black")+
      theme_minimal(base_family = "Courier New") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y = element_text(face = "bold",  size=10.5),
            axis.text.x = element_text(face = "bold", size=10.5)) +
      scale_x_discrete(position = "top")+
      facet_wrap(~ condicion_actual_de_las_personas)



p4 <- registro_cbpcdmx_clean %>% 
      mutate(sexo = ifelse(sexo == "hombre", "Hombres",
                           sexo),
             sexo = ifelse(sexo == "mujer", "Mujeres",
                           sexo),
            grupo_edad = case_when(edad<12 ~ "Menores de 12 años",
                                   edad %in% 12:17 ~ "De 12 a 17 años",
                                   edad %in% 18:23 ~ "De 18 a 23 años",
                                   edad %in% 24:29 ~ "De 24 a 29 años",
                                   edad %in% 30:44 ~ "De 30 a 44 años",
                                   edad %in% 45:59 ~ "De 45 a 59 años",
                                   edad>=60 ~ "60 años o más"),
            grupo_edad = factor(grupo_edad,
                                levels = c("Menores de 12 años",
                                           "De 12 a 17 años",
                                           "De 18 a 23 años",
                                           "De 24 a 29 años",
                                           "De 30 a 44 años",
                                           "De 45 a 59 años",
                                           "60 años o más")),
            condicion_localizacion = factor(condicion_localizacion,
                                            levels = c("con vida",
                                                       "sin vida",
                                                       "sigue desaparecida"))) %>% 
      group_by(sexo, condicion_localizacion, grupo_edad) %>%
      summarize(total=n()) %>%
      mutate(den=sum(total, na.rm=T)) %>%
      ungroup() %>%
      na.omit() %>% 
      left_join(edades_censo_cdmx) %>% 
      mutate(tasa = (total/total_poblacion)*100000,
             tasa = round(tasa, 2)) %>% 
      mutate(order_edad= as.numeric(grupo_edad)) %>% 
      ggplot(data=., aes(x = sexo, y = reorder(grupo_edad, -order_edad), fill = tasa)) +
      geom_tile(color="black") +
      scale_fill_gradient(low="#F9E0D9", high="#F85A3E", name = "Tasa")+ 
      labs(title=NULL,
           subtitle = "CDMX",
           x="", y="", fill="") +
      geom_text(aes(label= tasa), size=3.0, hjust=.2, vjust=.2, color="black")+
      theme_minimal(base_family = "Courier New") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y = element_text(face = "bold", size=10.5),
            axis.text.x = element_text(face = "bold", size=10.5)) +
      scale_x_discrete(position = "top")+
      facet_wrap(~ condicion_localizacion)


p3 + p4 +
      plot_annotation(
            title = "Perfiles de personas desaparecidas con base en su edad, sexo y estatus de localización"
      ) &
      theme(
            plot.title = element_text(face = "bold", hjust = 0.5))


walk(devices, ~ ggsave(filename = file.path(paste0(files$perfiles_ambos_estatus_png, .x)),
                       device = .x, width = 24, height = 14))


# DONE.