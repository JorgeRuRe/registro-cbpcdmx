# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# desp-cdmx/import/src/import.R


# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, here, readxl, janitor, stringi, fuzzyjoin)



# Files  ------------------------------------------------------------------
files <- list(registro_cbpcdmx = here("import", "input", "REGISTRO INTERNO CBP 1 febrero.xlsx"),
              registro_clean = here("import", "output", "registro_cdmx_clean"))

clean_text <- function(s){
      str_squish(toupper(stri_trans_general(s, "latin-ascii")))
}

# Data ---------------------------------------------------------------------
registro_cbpcdmx <- read_xlsx(files$registro_cbpcdmx) %>% 
   mutate(Colonia.de.la.desaparicion = clean_text(Colonia.de.la.desaparicion),
          Municipio.Alcadia.de.la.desaparicion = clean_text(Municipio.Alcadia.de.la.desaparicion)) %>% 
   rename(alcaldia = Municipio.Alcadia.de.la.desaparicion,
          colonia = Colonia.de.la.desaparicion)

colonias_cdmx <- read_csv("import/input/ColoniasCDMX_Oficial_Pobtot.csv") %>% 
      mutate(nombre = clean_text(nombre),
             alcaldia = clean_text(alcaldia),
             cve_alc = str_pad(cve_alc, width = 3, side = "left", pad = "0")) %>% 
      rename(colonia = nombre)


test_fuzzy <- registro_cbpcdmx %>% 
   filter(!is.na(alcaldia) & !is.na(colonia)) %>% 
   stringdist_left_join(colonias_cdmx, by = c("alcaldia", "colonia"), method = "lv",
                        max_dist = 2, distance_col = "distance") %>% 
   select(alcaldia.x, alcaldia.y, cve_alc, 
          colonia.x, colonia.y, cve_col, 
          alcaldia.distance, colonia.distance, everything())
   
   
   



