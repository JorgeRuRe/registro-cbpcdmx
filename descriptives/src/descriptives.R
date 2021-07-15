# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# desp-cdmx/descriptives/src/descriptives.R



# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, sf, here, svglite, scales, treemapify, reshape2, rcolorbrewer, readxl,  biscale, cowplot)



# Files -------------------------------------------------------------------
files <- list(registro_cbpcdmx_clean = here("import", "output", "registro_cdmx_clean.rds"),
              evalua_cdmx = here("descriptives", "input", "coloniascdmx_ids1.xlsx"),
              shp_cdmx = here("descriptives", "input", "coloniascdmx.shp"),
              perfiles_desp = here("descriptives", "output", "perfiles_desp."),
              perfiles_desp_estatus = here("descriptives", "output", "perfiles_desp_estatus."),
              tree_map_escolaridad = here("descriptives", "output", "escolaridad_desp."),
              tree_map_civil = here("descriptives", "output", "civil_desp."),
              lugar_localizacion = here("descriptives", "output", "lugar_localizacion."))
              


devices <- c("png", "svg")


# Datos  ------------------------------------------------------------------

registro_cbpcdmx_clean <- read_rds(files$registro_cbpcdmx_clean)

evalua_cdmx <- read_excel(files$evalua_cdmx, skip = 1) %>% 
   mutate(habitantes = as.numeric(habitantes)) %>% 
   select(cve_col, habitantes, valor_ids, estrato)

shp_cdmx <- st_read(files$shp_cdmx) %>% 
   left_join(evalua_cdmx, 
             by = c("cve_col" = "cve_col"))




# Análisis quiénes son ----------------------------------------------------------------

# quiénes son sin status 
registro_cbpcdmx_clean %>% 
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
      labs(title="Edad y sexo de personas desaparecidas en la Ciudad de México",
           subtitle = "Registradas por la Comisión de Búsqueda de la CDMX",
           x="", y="", fill="") +
      geom_text(aes(label=paste0(per, "%")), size=2.5, hjust=.2, vjust=.2, color="black")+
      theme_minimal(base_family = "Courier New") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            #  axis.text.y = element_blank(),
            axis.text.x = element_text(face = "bold")) +
      scale_x_discrete(position = "top")

walk(devices, ~ ggsave(filename = file.path(paste0(files$perfiles_desp, .x)),
                       device = .x, width = 14, height = 10))


# quiénes son con estatus 
registro_cbpcdmx_clean %>% 
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
      labs(title="Edad y sexo de personas desaparecidas en la Ciudad de México",
           subtitle = "con base en su estatus de localización",
           x="", y="", fill="") +
      geom_text(aes(label=paste0(per, "%")), size=2.5, hjust=.2, vjust=.2, color="black")+
      theme_minimal(base_family = "Courier New") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            #  axis.text.y = element_blank(),
            axis.text.x = element_text(face = "bold")) +
      scale_x_discrete(position = "top")+
      facet_wrap(~ condicion_localizacion)

walk(devices, ~ ggsave(filename = file.path(paste0(files$perfiles_desp_estatus, .x)),
                       device = .x, width = 14, height = 10))


# escolaridad 

registro_cbpcdmx_clean %>% 
   mutate(condicion_localizacion = factor(condicion_localizacion,
                                           levels = c("con vida",
                                                      "sin vida",
                                                      "sigue desaparecida"))) %>% 
   group_by(condicion_localizacion, escolaridad) %>% 
   summarize(total=n()) %>% 
   na.omit() %>% 
   mutate(den=sum(total, na.rm=T)) %>%
   ungroup() %>%
   mutate(per=round((total/den)*100, 0)) %>% 
   arrange(-per) %>% 
   ggplot(aes(fill = escolaridad, area = per, label = paste0(escolaridad, "\n", per))) +
      geom_treemap() +
      geom_treemap_text( aes(label=paste0(escolaridad, "\n", per, "%")), colour ="black", 
                         place = "centre", size = 12, face = "bold",
                        family = "Courier New Bold") +
      scale_fill_brewer(palette = "Set3") +
   facet_wrap(~ condicion_localizacion) +
      labs(title= "Escolaridad de personas desaparecidas registradas por la Comisión de Búsqueda de la CDMX",
           subtitle = "En porcentaje y con base en estatus de localización") +
      theme_minimal(base_family = "Courier New") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y = element_blank(),
            axis.text.x = element_text(face = "bold")) +
      theme(legend.position = "none") 

walk(devices, ~ ggsave(filename = file.path(paste0(files$tree_map_escolaridad, .x)),
                       device = .x, width = 14, height = 10))



# Estado civil
registro_cbpcdmx_clean %>% 
   mutate(condicion_localizacion = factor(condicion_localizacion,
                                          levels = c("con vida",
                                                     "sin vida",
                                                     "sigue desaparecida"))) %>% 
   group_by(sexo, estado_civil) %>% 
   summarize(total=n()) %>% 
   na.omit() %>% 
   mutate(den=sum(total, na.rm=T)) %>%
   ungroup() %>%
   mutate(per=round((total/den)*100, 0)) %>% 
   arrange(-per) %>% 
   ggplot(aes(fill = estado_civil, area = per, label = paste0(estado_civil, "\n", per))) +
   geom_treemap() +
   geom_treemap_text( aes(label=paste0(estado_civil, "\n", per, "%")), colour ="black", 
                      place = "centre", size = 12, face = "bold",
                      family = "Courier New Bold") +
   scale_fill_brewer(palette = "Set3") +
   facet_wrap(~ sexo) +
   labs(title= "Estado civil de personas desaparecidas registradas por la Comisión de Búsqueda de la CDMX",
        subtitle = "En porcentaje y con base en su sexo") +
   theme_minimal(base_family = "Courier New") +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         axis.text.y = element_blank(),
         axis.text.x = element_text(face = "bold")) +
   theme(legend.position = "none") 

walk(devices, ~ ggsave(filename = file.path(paste0(files$tree_map_civil, .x)),
                       device = .x, width = 14, height = 10))


# Cómo se localizan  ------------------------------------------------------

# lugar de localizacion 
registro_cbpcdmx_clean %>% 
   group_by(tipo_de_lugar_de_la_localizacion) %>% 
   summarize(total=n()) %>% 
   na.omit() %>% 
   mutate(den=sum(total, na.rm=T)) %>%
   ungroup() %>%
   mutate(per=round((total/den)*100, 2)) %>% 
   mutate(tipo_de_lugar_de_la_localizacion = reorder(tipo_de_lugar_de_la_localizacion, per)) %>%
   ggplot(aes(tipo_de_lugar_de_la_localizacion, per)) +
   geom_col(fill = "#F85A3E") +
   geom_text(aes(label=paste0(per, "%")), size=2.5, hjust=.2, vjust=.2, color="black") +
   coord_flip() +
   labs(title= "Lugar de localización de las personas reportadas como desaparecidas \n ante la Comisión de Búsqueda de Personas de la CDMX",
        x = NULL, y = "porcentaje") +
   theme_minimal(base_family = "Courier New") +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         plot.title = element_text(face = "bold", hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         axis.text.x = element_text(face = "bold")) +
   theme(legend.position = "none") 

walk(devices, ~ ggsave(filename = file.path(paste0(files$lugar_localizacion, .x)),
                       device = .x, width = 14, height = 10))


# FIN  