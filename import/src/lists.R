# Maintainers: JRR, OE ----
# Copyright:   2021, PDH-IBERO GPL v2 or later
# ===========================================================
# desp-cdmx/import/src/lists.R


# Origen de la noticia ----------------------------------------------------
autoridades_en_general <- list("alcaldia azcapotzalco", "alcaldía azcapotzalco",
                               "alcaldia alavaro obregon", "alcaldia coyoacan",
                               "alcaldia tlahuac", "desarrollo integral de las familias",
                               "comision ejecutiva de atencion a victimas",
                               "comision ejecutiva de atencion a victimas de la ciudad de mexico",
                               "comision de derechos humanos de la ciudad de mexico",
                               "comision nacional de derechos humanos", "congreso de la cdmx",
                               "congreso local", 
                               "fiscalia de investigacion y persecucion de los delitos en materia de desaparicion forzada de personas y la desaparicion",
                               "fiscalia especializada en la busqueda, localización e investigación de personas desaparecidas",
                               "fiscalia especializada en la busqueda, localizacion e invesitgacion de personas desaparecidas",
                               "fiscalia especializada en la busqueda, localizacion e investigacion de personas desaparecidas",
                               "institucional",
                               "mecanismo de proteccion integral a periodistas y defensores de ddhh",
                               "procuraduria de proteccion de derechos de niños, niñas y adolescentes de la ciudad de mexico",
                               "procuraduria general de justicia de la ciudad de mexico",
                               "secretaria de gobierno de la ciudad de mexico",
                               "secretaria de las mujeres de la ciudad de méxico",
                               "secretaria de seguridad ciudadana")


sociedad_civil <- list("casa frida para juventud lgbti", "centro PRODH",
                       "fundación para la justicia y el estado democrático de derecho", 
                       "red por los derechos de la infancia mexico", "ONG", "naciones unidas",
                       "oficina del alto comisionado para los ddhh", 
                       "universidad autonoma metropolitana", "Universidad Ibeoramericana",
                       "consejo ciudadano para la seguridad y justicia de la ciudad de mexico",
                       "asociacion mexicana de niños robados t desaparecidos", 
                       "asociacion mexicana de niños robados y desaparecidos",
                       "red nacional de defensoras de derechos humanos en mexico")


colectivos_familias <- list("colectivo", "colectivo 'hasta encontrarles' de la ciudad de méxico", 
                            "colectivo familiares en busqueda maria herrera", 
                            "colectivo familias de desaparecidos orizaba cordoba ac", "colectivo mariposas destellando",
                            "colectivo red eslabones por los derechos humanos", 
                            "colectivo uniendo esperanzas edomex",
                            "colectiivo una luz en el camino", "colectivo buscando nos encontramos")

comisiones_busqueda <- list("comision de busqueda de personas del estado de mexico", 
                            "comision local de busqueda de morelos", "comision local de busqueda de personas de hidalgo",
                            "comision local de busqueda de personas de puebla",
                            "comision local de busqueda de personas de san luis potosi", "comision local de busqueda de personas de tamaulipas",
                            "comision local de busqueda de personas de veracruz",
                            "comision local de busqueda de personas del estado de nuevo leon", 
                            "comision local de busqueda de queretaro\r\n", "comision local de busqueda de veracruz",
                            "comision local de busqueda del estado de mexico", "comision local del estado de queretaro",
                            "comision nacional de busqueda", "comsion nacional de busqueda")

redes_sociales_telefono <- list("facebook", "twitter", "911", "medios digitales",
                                "telefonico")


consejos_ciudadanos <- list("consejo ciudadano de busqueda de la ciudad de mexico", 
                            "consejo ciudadano del sistema nacional de busqueda")

consulados <- list("consulado de colombia", "consulado de colombia en mexico")


portal_cnb <- list("portal de la comision nacional de busqueda", "portal comision nacional de busqueda",
                   "portal de la comision de busqueda", "portal de la comision de busqueda de personas",
                   "portal de la comision nacional de busqueda", "portal de la comisionde busqueda de personas",
                   "portal de la comsion nacional de busqueda")


# Escolaridad -------------------------------------------------------------
preparatoria <- list("bachillerato", "c.c.h.", "c.c.h", "c.c.h.", "bachillereato")

otra_educacion <- list("comercial", "diplomado", "normal", "vocacional", "maternal", "preescolar")

tecnico <- list("tecnico")

posgrado <- list("maestria", "doctorado", "especialidad")



# Estado civil ------------------------------------------------------------
casado <- list("casado", "casado(a)")

divorciado <- list("divorciado", "divorciado(a)")

soltero <- list("soltera", "soltero", "soltero(a)", "solterro(a)")

concubino <- list("concubino(a)")

separado <- list("separado(a)")

viudo <- list("viudo(a)")



# Parentesco --------------------------------------------------------------
familiar <- list("abuela materna", "abuela paterna", "abuelo materno", "abuelo(a)", "cuñada (o)",
                 "cunado(a)", "hermano(a)", "hijastro(a)", "hijo", "hijo(a)", "madre", "madre adoptiva",
                 "medio hermano(a) materno(a)", "medio hermano(a) paterno(a)", "nieta materna", 
                 "nieto (a)", "nieto materno", "nuera", "nuera o yerno", "padrastro", 
                 "padre", "prima materna", "prima paterna", "primo materno", "primo paterno",
                 "primo(a)", "sobrina materna", "sobrina paterna", "sobrino materno", 
                 "sobrino paterno", "sobrino(a)", "suegro(a)", "tia abuela", "tia materna",
                 "tia materna, madre", "tia paterna", "tio (a)", "tio materno", 
                 "tio(a)", "yerno", "abuela", "concuña", "cuñada", "hermamo", "hermana", 
                 "hermano", "hija", "hijo", "medio hermano", "nieta", "nieto", "prima",
                 "primo", "sobirna", "sobrina", "sobrino", "tia", "tio", "cuñado(a)")

pareja <- list("companero(a)/esposo(a)", "compañero(a)/esposo(a)", "concubina", "concubina (o)",
               "concubinario", "conyuge", "expareja", "novio(a)", "concubino", "esposa",
               "esposo(a)", "ex concubina", "novia")

otro_parentesco <- list("abogado", "abogado (a)", "amigo/conocido", "contacto de empleo", 
                        "jefe", "otro", "tutor", "tutor (a)", "vecino", "vecino(a)", "abogada",
                        "amgo", "amiga", "amigo", "compañera de trabajo", "conocida", "conocido",
                        "jefe laboral", "ong", "otro")


# Estado de salud  --------------------------------------------------------
buen_estado <- list("buen estado", "buen estado de salud", "buen estado general")

mal_estado_otros <- list("enfermo", "mal estado por otras causas")


# Lugar de la localización ------------------------------------------------
abordo_vehiculo <- list("a bordo de vehiculo particular", 
                      "dentro de un vehiculo")

casa_famcon <- list("en casa de un amigo(a)", "en casa de un familiar", 
                    "en casa de una amistad o conocido")

domicilio_propio <- list("en su domicilio", "en su propio domicilio", "su domicilio")

ministerio_publico <- list("en un ministerio publico", "en un ministerio publico o lugar administrativo",
                           "en una agencia del ministerio publico")

central_camionera <- list("en una central camionera", "en una central o estacion de transporte")

centro_salud <- list("en un centro hospitalario", "en centro de salud u hospital", "hospital")

centro_adicciones <- list("en una institucion para adicciones", "en un centro de atencion de adicciones")

centro_detencion <- list("en un centro de detencion o reclusion", "en un centro de reclusion o detencion")


# DONE.