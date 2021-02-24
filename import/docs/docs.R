# Author: JRR
# Maintainers: JRR, OE
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# 



# Paquetes ----------------------------------------------------------------
if(!require('pacman')) {install.packages('pacman')}
pacman::p_load(tidyverse, here, DataExplorer, readxl)



# Data --------------------------------------------------------------------
desp_cbcdmx <- read_xlsx(here("import", "input", "REGISTRO INTERNO CBP 1 febrero.xlsx"))

p <- desp_cbcdmx %>% count(Colonia.de.la.desaparicion, sort = T)


# Reporte  ----------------------------------------------------------------
DataExplorer::create_report(desp_cbcdmx)



# FIN 