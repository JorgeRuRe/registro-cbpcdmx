# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# desp-cdmx/descriptives/src/descriptives.R


test %>% 
      mutate(grupo_edad = case_when(edad<18 ~ "Menores de 18 años",
                                    edad %in% 18:25 ~ "De 18 a 25 años",
                                    edad %in% 26:30 ~ "De 26 a 30 años",
                                    edad %in% 31:44 ~ "De 31 a 44 años",
                                    edad %in% 45:60 ~ "De 45 a 60 años",
                                    edad>60 ~ "Más de 60 años"),
             grupo_edad = factor(grupo_edad,
                                 levels = c("Menores de 18 años",
                                            "De 18 a 25 años",
                                            "De 26 a 30 años",
                                            "De 31 a 44 años",
                                            "De 45 a 60 años",
                                            "Más de 60 años"))) %>% 
      group_by(sexo, condicion_localizacion, grupo_edad) %>%
      summarize(total=n()) %>%
      mutate(den=sum(total, na.rm=T)) %>%
      ungroup() %>%
      mutate(per=round((total/den)*100, 1)) %>%
      na.omit()%>%
      mutate(order_edad= as.numeric(grupo_edad)) %>% 
      ggplot(data=., aes(x = sexo, y = reorder(grupo_edad, -order_edad), fill = per)) +
      geom_tile(color="black") +
      scale_fill_gradient(low="#F9E0D9", high="#F85A3E")+ 
      labs(title="Edad y sexo de personas desaparecidas en CDMX",
           subtitle = "con base en su estatus de localización",
           x="", y="", fill="") +
      geom_text(aes(label=paste0(per, "%")), size=2, hjust=.2, vjust=.2, color="black")+
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle=0)) +
      scale_x_discrete(position = "top")+
      facet_wrap(~ condicion_localizacion)



test %>% 
      mutate(grupo_edad = case_when(edad<18 ~ "Menores de 18 años",
                                    edad %in% 18:25 ~ "De 18 a 25 años",
                                    edad %in% 26:30 ~ "De 26 a 30 años",
                                    edad %in% 31:44 ~ "De 31 a 44 años",
                                    edad %in% 45:60 ~ "De 45 a 60 años",
                                    edad>60 ~ "Más de 60 años"),
             grupo_edad = factor(grupo_edad,
                                 levels = c("Menores de 18 años",
                                            "De 18 a 25 años",
                                            "De 26 a 30 años",
                                            "De 31 a 44 años",
                                            "De 45 a 60 años",
                                            "Más de 60 años"))) %>% 
      group_by(sexo, grupo_edad) %>%
      summarize(total=n()) %>%
      mutate(den=sum(total, na.rm=T)) %>%
      ungroup() %>%
      mutate(per=round((total/den)*100, 1)) %>%
      na.omit()%>%
      mutate(order_edad= as.numeric(grupo_edad)) %>% 
      ggplot(data=., aes(x = sexo, y = reorder(grupo_edad, -order_edad), fill = per)) +
      geom_tile(color="black") +
      scale_fill_gradient(low="#F9E0D9", high="#F85A3E")+ 
      labs(title="Edad y sexo de personas desaparecidas en CDMX",
           subtitle = "con base en su estatus de localización",
           x="", y="", fill="") +
      geom_text(aes(label=paste0(per, "%")), size=2, hjust=.2, vjust=.2, color="black")+
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle=0)) +
      scale_x_discrete(position = "top")
