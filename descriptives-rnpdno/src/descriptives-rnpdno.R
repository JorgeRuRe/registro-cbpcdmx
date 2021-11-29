#
# Author: JRR
# Maintainer(s):  OE, JRR
# Copyright:   2021, Data Cívica, GPL v2 or later
# ====================================================
# registro-cbpcdmx/descriptives-rnpdno/src/descriptives-rnpdno.R


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, here, readxl, janitor, add2ggplot, magick)




# Files -------------------------------------------------------------------
files <- list(rnpdno_clean = here("descriptives-rnpdno", "input", "BASE-DE-DATOS-Personas-desaparecidas-2011-2020.xlsx"),
              perfiles_desp_cnb = here("descriptives-rnpdno", "output", "perfiles_desp_cnb."),
              perfiles_estatus_cnb = here("descriptives-rnpdno", "output", "perfiles_estatus_cnb."),
              logo = here("import/input/logo-dc.png"))



devices <- c("png", "svg")


add_dclogo <- function(graf, escala){
      graf_con_logo <- add_logo(
            plot_path = graf,
            logo_path = files$logo,
            logo_position = "bottom right",
            logo_scale = escala)
      
      magick::image_write(graf_con_logo, graf)
}


# Datos  ------------------------------------------------------------------
rnpdno_clean <- read_excel(files$rnpdno_clean) %>% 
      clean_names()




# Descriptives ------------------------------------------------------------

# Perfiles nacionales 
rnpdno_clean %>% 
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
      labs(title="Edad y sexo de personas desaparecidas a nivel nacional",
           subtitle = "Registradas por la Comisión Nacional de Búsqueda de Personas Desaparecidas",
           x="", y="", fill="") +
      geom_text(aes(label=paste0(per, "%")), size=2.5, hjust=.2, vjust=.2, color="black")+
      theme_minimal(base_family = "Courier New") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y = element_text(face = "bold"),
            axis.text.x = element_text(face = "bold")) +
      scale_x_discrete(position = "top")


walk(devices, ~ ggsave(filename = file.path(paste0(files$perfiles_desp_cnb, .x)),
                       device = .x, width = 14, height = 10))


# Perfiles nacionales con estatus
rnpdno_clean %>% 
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
      labs(title="Edad y sexo de personas desaparecidas a nivel nacional",
           subtitle = "con base en su estatus de localización",
           x="", y="", fill="") +
      geom_text(aes(label=paste0(per, "%")), size=2.5, hjust=.2, vjust=.2, color="black")+
      theme_minimal(base_family = "Courier New") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y = element_text(face = "bold"),
            axis.text.x = element_text(face = "bold")) +
      scale_x_discrete(position = "top")+
      facet_wrap(~ condicion_actual_de_las_personas)


walk(devices, ~ ggsave(filename = file.path(paste0(files$perfiles_estatus_cnb, .x)),
                       device = .x, width = 14, height = 10))


# DONE. 