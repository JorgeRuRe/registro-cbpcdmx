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
              tree_map_escolaridad = here("descriptives", "output", "escolaridad_desp."))
              


devices <- c("png", "svg")


# Datos  ------------------------------------------------------------------

registro_cbpcdmx_clean <- read_rds(files$registro_cbpcdmx_clean)

evalua_cdmx <- read_excel(files$evalua_cdmx, skip = 1) %>% 
   mutate(habitantes = as.numeric(habitantes)) %>% 
   select(cve_col, habitantes, valor_ids, estrato)

shp_cdmx <- st_read(files$shp_cdmx) %>% 
   left_join(evalua_cdmx, 
             by = c("cve_col" = "cve_col"))




# Análisis ----------------------------------------------------------------

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
   group_by(escolaridad) %>% 
   summarize(total=n()) %>% 
   na.omit() %>% 
   mutate(den=sum(total, na.rm=T)) %>%
   ungroup() %>%
   mutate(per=round((total/den)*100, 1)) %>% 
   arrange(-per) %>% 
   ggplot(aes(fill = escolaridad, area = per, label = paste0(escolaridad, "\n", per))) +
      geom_treemap() +
      geom_treemap_text( aes(label=paste0(escolaridad, "\n", per, "%")), colour ="black", 
                         place = "centre", size = 12, face = "bold",
                        family = "Courier New Bold") +
      scale_fill_brewer(palette = "Set3") +
      labs(title= "Escolaridad de personas desaparecidas registradas por la Comisión de Búsqueda de la CDMX",
           subtitle = "En porcentaje") +
      theme_minimal(base_family = "Courier New") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.y = element_blank(),
            axis.text.x = element_text(face = "bold")) +
      theme(legend.position = "none") 

walk(devices, ~ ggsave(filename = file.path(paste0(files$tree_map_escolaridad, .x)),
                       device = .x, width = 14, height = 10))



# mapas desigualdad 

cases_per_colonia <- registro_cbpcdmx_clean %>%
   group_by(clave_colonia_desp) %>%
   count(clave_municipio_desp) 



# Datos para mapa
mapa_desp_cdmx <- shp_cdmx %>%
   left_join(cases_per_colonia,
             by = c("cve_col" = "clave_colonia_desp")) %>% 
   mutate(tasa_desp = n*(10000/habitantes),
          valor_ids = as.numeric(valor_ids)) 
   



centroides <- st_coordinates(st_centroid(mapa_desp_cdmx)) %>% as.tibble()


# create classes
bi_map <- bi_class(mapa_desp_cdmx, x = valor_ids, y =  tasa_desp, style = "quantile", dim = 2) 

bi_map <- bi_map %>% 
   filter(bi_class != "1-NA") %>% 
   filter(bi_class != "2-NA") %>% 
   filter(bi_class != "NA-NA")

# create map
map <- ggplot() +
   geom_sf(data = bi_map, mapping = aes(fill = bi_class), color = "grey", size = 0.2, show.legend = FALSE) +
   bi_scale_fill(pal = "DkBlue", dim = 2) +
   labs(
      title = "Tasas de desaparición de personas en CDMX Y estrato social",
      subtitle = "Nivel Colonia"
   ) +
   bi_theme() +
   theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
         plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 10),
         plot.caption = element_text(hjust = 0.5, size = 10)) 

map

legend <- bi_legend(pal = "DkBlue",
                    dim = 2,
                    xlab = "Estrato Socioeconómico",
                    ylab = "Tasa de desaparición por cada 10 mil habitantes",
                    size = 7)


finalPlot <- ggdraw() +
   draw_plot(map, 0, 0, 1, 1) +
   draw_plot(legend, 0.2, .65, 0.2, 0.2)

finalPlot




# FIN 


