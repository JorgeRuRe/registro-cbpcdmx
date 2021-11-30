# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# desp-cdmx/write/src/graficas-perfiles.R


# Paquetes ----------------------------------------------------------------
pacman::p_load(tidyverse, sf, here, svglite, scales, treemapify, reshape2, rcolorbrewer, readxl,
               biscale, cowplot, lubridate, patchwork, janitor)



# Files -------------------------------------------------------------------
files <- list(registro_cbpcdmx_clean = here("import", "output", "registro_cdmx_clean.rds"),
              rnpdno_clean = here("descriptives-rnpdno", "input", "BASE-DE-DATOS-Personas-desaparecidas-2011-2020.xlsx"),
              perfiles_ambos_png = here("write", "input", "perfiles_ambos."), 
              perfiles_ambos_estatus_png = here("write", "input", "perfiles_ambos_estatus."))
              
devices <- c("png", "svg")



# Data  ------------------------------------------------------------------
registro_cbpcdmx_clean <- read_rds(files$registro_cbpcdmx_clean)
rnpdno_clean <- read_excel(files$rnpdno_clean) %>% 
      clean_names()



# Graf 1 ------------------------------------------------------------------
p1 <- registro_cbpcdmx_clean %>% 
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
      group_by(sexo, grupo_edad) %>%
      summarize(total=n()) %>%
      mutate(den=sum(total, na.rm=T)) %>%
      ungroup() %>%
      mutate(per=round((total/den)*100, 1)) %>%
      na.omit()%>%
      mutate(order_edad= as.numeric(grupo_edad)) %>% 
      ggplot(data=., aes(x = sexo, y = reorder(grupo_edad, -order_edad), fill = per)) +
      geom_tile(color="black") +
      scale_fill_gradient(low="#F9E0D9", high="#F85A3E", name = "Porcentaje")+ 
      labs(title= NULL,
           subtitle = "CDMX",
           x="", y="", fill="") +
      geom_text(aes(label=paste0(per, "%")), size=3.5, hjust=.2, vjust=.2, color="black")+
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
      group_by(sexo, grupo_edad) %>%
      summarize(total= sum(total, na.rm = T)) %>%
      mutate(den=sum(total, na.rm=T)) %>%
      ungroup() %>%
      mutate(per=round((total/den)*100, 1)) %>%
      na.omit() %>%
      mutate(order_edad= as.numeric(grupo_edad)) %>% 
      ggplot(data=., aes(x = sexo, y = reorder(grupo_edad, -order_edad), fill = per)) +
      geom_tile(color="black") +
      scale_fill_gradient(low="#F9E0D9", high="#F85A3E", name = "Porcentaje") + 
      labs(title= NULL,
           subtitle = "NACIONAL",
           x="", y="", fill="") +
      geom_text(aes(label=paste0(per, "%")), size=3.5, hjust=.2, vjust=.2, color="black")+
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
      mutate(condicion_actual_de_las_personas = factor(condicion_actual_de_las_personas,
                                                       levels = c("LOCALIZADAS CON VIDA",
                                                                  "LOCALIZADAS SIN VIDA",
                                                                  "DESAPARECIDAS Y NO LOCALIZADAS"))) %>% 
      group_by(sexo, condicion_actual_de_las_personas, grupo_edad) %>% 
      summarize(total= sum(total, na.rm = T)) %>%
      mutate(den=sum(total, na.rm=T)) %>%
      ungroup() %>%
      mutate(per=round((total/den)*100, 1)) %>%
      na.omit() %>%
      mutate(order_edad= as.numeric(grupo_edad)) %>% 
      ggplot(data=., aes(x = sexo, y = reorder(grupo_edad, -order_edad), fill = per)) +
      geom_tile(color="black") +
      scale_fill_gradient(low="#F9E0D9", high="#F85A3E", name = "Porcentaje")+ 
      labs(title=NULL,
           subtitle = "NACIONAL",
           x="", y="", fill="") +
      geom_text(aes(label=paste0(per, "%")), size=3.0, hjust=.2, vjust=.2, color="black")+
      theme_minimal(base_family = "Courier New") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y = element_text(face = "bold",  size=10.5),
            axis.text.x = element_text(face = "bold", size=10.5)) +
      scale_x_discrete(position = "top")+
      facet_wrap(~ condicion_actual_de_las_personas)



p4 <- registro_cbpcdmx_clean %>% 
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
                                           "60 años o más")),
            condicion_localizacion = factor(condicion_localizacion,
                                            levels = c("con vida",
                                                       "sin vida",
                                                       "sigue desaparecida"))) %>% 
      group_by(sexo, condicion_localizacion, grupo_edad) %>%
      summarize(total=n()) %>%
      mutate(den=sum(total, na.rm=T)) %>%
      ungroup() %>%
      mutate(per=round((total/den)*100, 1)) %>%
      na.omit()%>%
      mutate(order_edad= as.numeric(grupo_edad)) %>% 
      ggplot(data=., aes(x = sexo, y = reorder(grupo_edad, -order_edad), fill = per)) +
      geom_tile(color="black") +
      scale_fill_gradient(low="#F9E0D9", high="#F85A3E", name = "Porcentaje")+ 
      labs(title=NULL,
           subtitle = "CDMX",
           x="", y="", fill="") +
      geom_text(aes(label=paste0(per, "%")), size=3.0, hjust=.2, vjust=.2, color="black")+
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
