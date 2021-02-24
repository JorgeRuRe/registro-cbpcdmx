# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# desp-cdmx/import/src/import.R


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, here, readxl, janitor, stringi)



# Files  ------------------------------------------------------------------
files <- list(registro_cbpcdmx = here("import", "input", "REGISTRO INTERNO CBP 1 febrero.xlsx"),
              registro_clean = here("import", "output", "registro_cdmx_clean"))



clean_text <- function(s){
      str_squish(toupper(stri_trans_general(s, "latin-ascii")))
}


# Data ---------------------------------------------------------------------
registro_cbpcdmx <- read_xlsx(files$registro_cbpcdmx) 

colonias_cdmx <- read_csv("import/input/ColoniasCDMX_Oficial_Pobtot.csv") %>% 
      mutate(nombre = clean_text(nombre),
             alcaldia = clean_text(alcaldia)) %>% 
      rename(colonia = nombre)




registro_cbpcdmx <- registro_cbpcdmx %>% 
      mutate(Colonia.de.la.desaparicion = clean_text(Colonia.de.la.desaparicion),
             Municipio.Alcadia.de.la.desaparicion = clean_text(Municipio.Alcadia.de.la.desaparicion)) %>% 
      rename(mun_desp = Municipio.Alcadia.de.la.desaparicion,
             colonia_desp = Colonia.de.la.desaparicion)


registro_final <- left_join(registro_cbpcdmx, colonias_cdmx, 
                            by = c(""))



