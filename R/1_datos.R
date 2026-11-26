# =============================================================================
# Datos, catálogo de preguntas y variables de cruce (v2)
# -----------------------------------------------------------------------------
# Hasta noviembre de 2026 la v2 tomaba este fichero de la v1. Desde que la serie
# empieza en 2010 es independiente: la v1 sigue con sus datos de 2020 en adelante.
# =============================================================================

# Primera ola telefónica: antes, la encuesta era presencial
CAMBIO_MODO <- 2020

# Microdatos de la app: datos/oppf_explorador.rds, solo con las columnas que usa.
# Los genera actualizar_datos.R a partir de la ponderación del proyecto, así que
# la carpeta de la app se puede publicar sola.
F_DATOS <- file.path("datos", "oppf_explorador.rds")

cargar_datos <- function() {
  if (!file.exists(F_DATOS))
    stop("Falta ", F_DATOS, ". Desde la raíz del proyecto: Rscript explorador_v2/actualizar_datos.R", call. = FALSE)
  # En el ordenador del proyecto, avisa si la ponderación es más reciente que la copia
  origen <- origen_datos()
  if (length(origen) && file.mtime(origen) > file.mtime(F_DATOS))
    warning("Los datos del explorador son anteriores a ", basename(origen),
            ": ejecuta explorador_v2/actualizar_datos.R", call. = FALSE)
  readRDS(F_DATOS)
}

# El oppf_AAAA_AAAA.RDS del proyecto que cubre más olas (code/OPPF_ponderacion.R)
origen_datos <- function(raices = c("..", ".", "../..")) {
  rutas <- unlist(lapply(raices, function(r) Sys.glob(
    file.path(r, "data", "output longitudinal", "oppf_[0-9][0-9][0-9][0-9]_[0-9][0-9][0-9][0-9].RDS"))))
  if (!length(rutas)) return(character())
  anios <- regmatches(basename(rutas), gregexpr("[0-9]{4}", basename(rutas)))
  ini <- as.numeric(sapply(anios, `[`, 1)); fin <- as.numeric(sapply(anios, `[`, 2))
  rutas[order(ini, -fin)][1]
}

# Columnas que usa la app: pesos, preguntas del catálogo y variables de los cruces
columnas_app <- function() unique(c(
  "año", "peso", "peso_est", "peso_est_voto", CATALOGO$var,
  "edad", "sexo", "h.5", "recuerdo", "recuerdo_r", "est4", "j.4", "j.11", "tamuni",
  "j.9.2", "e.2.2", "h.7.1", "h.7.2"))

# ------------------------------------------------------- Etiquetas limpias ----
# El FID recorta las categorías a 60 caracteres y no es homogéneo entre ítems.
ETIQUETAS <- c(
  "Demasiados pocos" = "Muy pocos", "Demasiado pocos" = "Muy pocos",
  "Los justos" = "Los necesarios", "N.C" = "N.C.",
  "(NO LEER) Regular" = "Regular (no leer)", "(NO LEER) Depende" = "Depende (no leer)",
  "(NO LEER) Depende de qué tipo, persona, magnitud, etc." = "Depende (no leer)",
  "(NO LEER) No sabe, no puede comparar con otros países" = "No puede comparar",
  "N.P." = "No procede",
  "Los impuestos son un medio para distribuir mejor la riqueza"  = "Un medio para redistribuir la riqueza",
  "Los impuestos son algo que el Estado nos obliga a pagar sin"  = "Algo que el Estado nos obliga a pagar sin saber a cambio de qué",
  "Los impuestos son necesarios para que el Estado pueda presta" = "Necesarios para prestar servicios públicos",
  "Sobre todo con impuestos directos, tipo IRPF"   = "Impuestos directos (IRPF)",
  "Sobre todo con impuestos indirectos, tipo IVA"  = "Impuestos indirectos (IVA)",
  "Donde existen pocas desigualdades sociales"     = "Pocas desigualdades",
  "Donde existen algunas desigualdades importantes en algunos a" = "Algunas desigualdades importantes",
  "Donde existen grandes desigualdades en general" = "Grandes desigualdades",
  "El Estado no debe intervenir en la vida económica, debe deja" = "No debe intervenir",
  "El Estado no debe intervenir en la vida económica, salvo par" = "No, salvo para corregir desajustes",
  "El Estado debe intervenir en la vida económica, pero respeta" = "Sí, respetando la iniciativa privada",
  "El Estado debe intervenir en toda la vida económica"          = "Sí, en toda la vida económica",
  "La falta de control de la Administración"                     = "Falta de control de la Administración",
  "Las sanciones que pueden imponerse son de poca importancia, " = "Sanciones de poca importancia",
  "Las sanciones que pueden imponerse son de poca importancia,"  = "Sanciones de poca importancia",
  "La escasez de empleos obliga a aceptar cualquier trabajo aun" = "Escasez de empleos",
  "Los salarios son demasiado bajos y hay que buscar otras opci" = "Salarios demasiado bajos",
  "Los impuestos que hay que pagar a Hacienda son excesivos"     = "Impuestos excesivos",
  "La falta de honradez y conciencia ciudadana"                  = "Falta de honradez y conciencia",
  "Porque los/as que más tienen evitan (o minimizan) el pago de" = "Los que más tienen evitan pagar",
  "Por varias o todas las razones anteriores"                    = "Varias de las anteriores",
  "Por desconfianza de la gestión y destino de los impuestos"    = "Desconfianza en el destino de los impuestos",
  "En general, no tiene unos efectos realmente importantes"      = "Sin efectos importantes",
  "Disminuye los recursos para financiar los servicios públicos" = "Menos recursos para servicios públicos",
  "Obliga a aumentar la presión fiscal sobre quienes cumplen y"  = "Más presión fiscal sobre quienes cumplen",
  "Obliga a aumentar la presión fiscal sobre quienes cumplen y " = "Más presión fiscal sobre quienes cumplen",
  "Desmotiva a quienes pagan correctamente sus impuestos"        = "Desmotiva a quienes pagan",
  "Crea injusticias, pues unos/as tienen que pagar lo que dejan" = "Crea injusticias",
  "Existe mucho fraude fiscal" = "Mucho", "Existe bastante fraude" = "Bastante",
  "Existe poco fraude" = "Poco", "Existe muy poco fraude" = "Muy poco",
  "A ambas (Iglesia católica y fines sociales)"    = "A ambas",
  "Sin asignación/No ha marcado ninguna casilla"   = "Ninguna casilla",
  "Positiva" = "A pagar", "Negativa" = "A devolver",
  "Agnóstico/a (No niegan la existencia de Dios pero tampoco la" = "Agnóstico/a",
  "Ateo/a (Niegan la existencia de Dios)"          = "Ateo/a")

# ------------------------------------------------------ Catálogo de temas -----
# `pre` = agrupación sugerida (ver AGRUPACIONES). Escalas: tipo "esc".
.it <- function(tema, var, etiqueta, pregunta, pre = NA, tipo = "cat")
  data.frame(tema, var, etiqueta, pregunta, pre, tipo)

P_GASTO <- "¿Cree que se dedican demasiados recursos, los necesarios o muy pocos a...?"
P_SAT   <- "¿En qué medida funcionan satisfactoriamente los siguientes servicios públicos?"
P_TOL   <- "¿En qué medida considera tolerable el siguiente comportamiento?"
P_ENT   <- "¿Este tipo de comportamientos podrían ser tolerados por...?"
P_ACU   <- "¿Está más bien de acuerdo o más bien en desacuerdo con la frase?"
P_BUE   <- "Para considerar a alguien buen ciudadano, ¿qué importancia le da a...? (0 = nada importante, 10 = muy importante)"
P_VID   <- "¿En qué medida está satisfecho/a con...? (0 = completamente insatisfecho/a, 10 = completamente satisfecho/a)"

CATALOGO <- rbind(
  .it("Impuestos", "c.1", "Escala servicios públicos - impuestos",
      "Escala 0-10: 0 = mejorar los servicios públicos aunque haya que pagar más impuestos; 10 = pagar menos impuestos aunque haya que reducir los servicios", "tramos", "esc"),
  .it("Impuestos", "c.2", "Cuánto se paga en impuestos", "¿Lo que los españoles pagamos en impuestos es mucho, regular o poco?"),
  .it("Impuestos", "c.3", "Impuestos frente a Europa", "En comparación con los países más avanzados de Europa, ¿en España se pagan más, igual o menos impuestos?"),
  .it("Impuestos", "b.1", "Para qué sirven los impuestos", "¿Cuál de las siguientes frases refleja mejor su opinión sobre los impuestos?"),
  .it("Impuestos", "b.2", "Beneficio para la sociedad", "¿La sociedad se beneficia mucho, bastante, poco o nada de lo que pagamos en impuestos y cotizaciones?", "mb"),
  .it("Impuestos", "b.3", "Beneficio para su familia", "Usted y su familia, ¿reciben de las administraciones más, más o menos lo mismo o menos de lo que pagan?"),
  .it("Impuestos", "b.8", "Impuestos directos o indirectos", "¿Los impuestos se deberían recaudar sobre todo con directos (IRPF) o con indirectos (IVA)?"),
  .it("Impuestos", "b.9", "Justicia en el cobro", "¿Los impuestos se cobran con justicia, esto es, pagan más quienes más tienen?"),
  .it("Impuestos", "b.5", "Responsabilidad de los españoles", "¿Los españoles son conscientes y responsables al pagar impuestos?", "mb"),
  .it("Impuestos", "b.6", "Responsabilidad propia", "¿Y usted, personalmente, se considera consciente y responsable?", "mb"),
  .it("Impuestos", "c.7", "Conoce el tipo general de IVA", "¿Cuál es el tipo general del IVA que se aplica en España?"),
  .it("Impuestos", "d.24", "Esfuerzo en explicar el destino", "¿La Administración hace esfuerzos por explicar el destino de los impuestos?", "mb"),
  .it("Impuestos", "e.2.2", "Resultado de la declaración", "¿La declaración le ha salido a pagar o a devolver?"),
  .it("Impuestos", "e.3", "Casilla de la renta", "¿Qué casilla marcó: Iglesia católica, fines sociales, ambas o ninguna?"),

  .it("Gasto público", "b.4.1",  "Gasto: enseñanza", P_GASTO), .it("Gasto público", "b.4.5", "Gasto: sanidad", P_GASTO),
  .it("Gasto público", "b.4.6",  "Gasto: vivienda", P_GASTO),  .it("Gasto público", "b.4.8", "Gasto: Seguridad Social y pensiones", P_GASTO),
  .it("Gasto público", "b.4.16", "Gasto: dependencia", P_GASTO), .it("Gasto público", "b.4.12", "Gasto: desempleo", P_GASTO),
  .it("Gasto público", "b.4.7",  "Gasto: justicia", P_GASTO),  .it("Gasto público", "b.4.13", "Gasto: seguridad ciudadana", P_GASTO),
  .it("Gasto público", "b.4.3",  "Gasto: defensa", P_GASTO),   .it("Gasto público", "b.4.2", "Gasto: obras públicas", P_GASTO),
  .it("Gasto público", "b.4.9",  "Gasto: transporte y comunicaciones", P_GASTO), .it("Gasto público", "b.4.10", "Gasto: cultura", P_GASTO),
  .it("Gasto público", "b.4.11", "Gasto: medio ambiente", P_GASTO), .it("Gasto público", "b.4.14", "Gasto: cooperación al desarrollo", P_GASTO),
  .it("Gasto público", "b.4.15", "Gasto: ciencia y tecnología", P_GASTO), .it("Gasto público", "b.4.18", "Gasto: medio rural", P_GASTO),
  .it("Gasto público", "b.4.19", "Gasto: servicios sociales", P_GASTO),

  .it("Servicios públicos", "g.2.2.1", "Funciona: enseñanza", P_SAT, "mb"), .it("Servicios públicos", "g.2.2.2", "Funciona: sanidad", P_SAT, "mb"),
  .it("Servicios públicos", "g.2.2.3", "Funciona: pensiones", P_SAT, "mb"), .it("Servicios públicos", "g.2.2.4", "Funciona: justicia", P_SAT, "mb"),
  .it("Servicios públicos", "g.2.2.5", "Funciona: seguridad ciudadana", P_SAT, "mb"), .it("Servicios públicos", "g.2.2.6", "Funciona: servicios sociales", P_SAT, "mb"),
  .it("Servicios públicos", "g.2.2.7", "Funciona: transporte público", P_SAT, "mb"), .it("Servicios públicos", "g.2.2.8", "Funciona: obras públicas", P_SAT, "mb"),
  .it("Servicios públicos", "g.2.2.9", "Funciona: dependencia", P_SAT, "mb"),
  .it("Servicios públicos", "g.1", "Habla de los servicios públicos", "¿Con qué frecuencia habla con personas próximas del funcionamiento de los servicios públicos?", "mb"),

  .it("Fraude", "d.1", "Cuánto fraude fiscal existe", "¿En España existe mucho, bastante, poco o muy poco fraude fiscal?", "mb"),
  .it("Fraude", "d.8", "Esfuerzo contra el fraude", "¿La Administración hace esfuerzos para luchar contra el fraude fiscal?", "mb"),
  .it("Fraude", "d.2", "Conocidos que declaran todo (IRPF)", "De la gente que conoce, ¿cuánta declara realmente todos sus ingresos en la renta?"),
  .it("Fraude", "d.3", "Conocidos que declaran todo (IVA)", "De la gente que conoce obligada a declarar el IVA, ¿cuánta declara todos sus ingresos?"),
  .it("Fraude", "d.5", "Razón para ocultar ingresos", "¿Qué razón principal mueve a ocultar ingresos a Hacienda?"),
  .it("Fraude", "d.6", "Efectos del fraude", "¿Qué efecto tiene principalmente el fraude fiscal?"),
  .it("Fraude", "d.7.1", "Si no se engaña más es por miedo", P_ACU),
  .it("Fraude", "d.7.2", "Casi todo el mundo engaña algo", P_ACU),
  .it("Fraude", "d.7.3", "No está mal ocultar parte de la renta", P_ACU),
  .it("Fraude", "d.7.4", "Engañar a Hacienda es engañar a los demás", P_ACU),
  .it("Fraude", "d.25.1", "Tolerable: no declarar todo en el IRPF", P_TOL, "mb"),
  .it("Fraude", "d.25.2", "Tolerable: pagar sin factura", P_TOL, "mb"),
  .it("Fraude", "d.25.3", "Tolerable: deducción que no corresponde", P_TOL, "mb"),
  .it("Fraude", "d.25.4", "Tolerable: prestación sin derecho", P_TOL, "mb"),
  .it("Fraude", "d.25.5", "Tolerable: autónomo que no cobra IVA", P_TOL, "mb"),
  .it("Fraude", "d.25.6", "Tolerable: autónomo que deduce gastos personales", P_TOL, "mb"),
  .it("Fraude", "d.25.7", "Tolerable: gran empresa que elude impuestos", P_TOL, "mb"),
  .it("Fraude", "d.25.8", "Tolerable: pequeña empresa que elude impuestos", P_TOL, "mb"),
  .it("Fraude", "d.25.9", "Tolerable: empresa en Internet para pagar menos", P_TOL, "mb"),
  .it("Fraude", "d.26.1", "Lo tolerarían sus vecinos", P_ENT),
  .it("Fraude", "d.26.2", "Lo tolerarían sus amigos", P_ENT),
  .it("Fraude", "d.26.3", "Lo toleraría su entorno familiar", P_ENT),
  .it("Fraude", "d.14", "Probabilidad de inspección", "¿Considera probable que le hagan una revisión de la declaración de la renta?", "mb"),

  .it("Valores y sociedad", "h.3", "Esfuerzo u origen familiar",
      "Escala 0-10: 0 = la posición económica depende del esfuerzo, la educación y la valía; 10 = del origen familiar, los contactos o la suerte", "tramos", "esc"),
  .it("Valores y sociedad", "b.24", "Desigualdad en España", "¿España es un país con pocas, algunas o grandes desigualdades?"),
  .it("Valores y sociedad", "b.25", "Intervención del Estado", "¿Debe el Estado intervenir en la vida económica?", "estado"),
  .it("Valores y sociedad", "h.2", "Confianza en la gente", "Escala 0-10: 0 = nunca se es lo bastante prudente; 10 = se puede confiar en la mayoría de la gente", "tramos", "esc"),
  .it("Valores y sociedad", "h.1", "Felicidad", "Escala 0-10: 0 = completamente infeliz; 10 = completamente feliz", "tramos", "esc"),
  .it("Valores y sociedad", "h.5", "Ideología", "Escala 1-10: 1 = lo más a la izquierda; 10 = lo más a la derecha", "ideo", "esc"),
  .it("Valores y sociedad", "b.7.1", "Importancia: cumplir las leyes", P_BUE, "tramos", "esc"),
  .it("Valores y sociedad", "b.7.2", "Importancia: votar", P_BUE, "tramos", "esc"),
  .it("Valores y sociedad", "b.7.3", "Importancia: ser solidario", P_BUE, "tramos", "esc"),
  .it("Valores y sociedad", "b.7.4", "Importancia: no evadir impuestos", P_BUE, "tramos", "esc"),
  .it("Valores y sociedad", "b.7.5", "Importancia: respetar otras opiniones", P_BUE, "tramos", "esc"),
  .it("Valores y sociedad", "b.7.6", "Importancia: ser responsable y honesto", P_BUE, "tramos", "esc"),
  .it("Valores y sociedad", "h.9.1", "Satisfacción: vida familiar", P_VID, "tramos", "esc"),
  .it("Valores y sociedad", "h.9.2", "Satisfacción: salud", P_VID, "tramos", "esc"),
  .it("Valores y sociedad", "h.9.3", "Satisfacción: vida social", P_VID, "tramos", "esc"),
  .it("Valores y sociedad", "h.9.4", "Satisfacción: nivel de vida", P_VID, "tramos", "esc"),
  .it("Valores y sociedad", "j.4",  "Situación económica personal", "¿Cómo calificaría su situación económica personal?", "bm"),
  .it("Valores y sociedad", "j.18", "Situación económica de España", "¿Cómo calificaría la situación económica general de España?", "bm"))

# Agrupaciones sugeridas: categoría -> grupo
agrupar_sugerido <- function(cat, pre) {
  if (is.na(pre)) return(cat)
  n <- suppressWarnings(as.numeric(cat))
  switch(pre,
    mb = dplyr::case_when(
      grepl("^(Muy poc|Muy poco)", cat) ~ "Poco o nada",
      grepl("^(Much|Muy|Bastante)", cat) ~ "Mucho o bastante",
      grepl("^(Poc|Nada|Ninguna)", cat)  ~ "Poco o nada", TRUE ~ cat),
    bm = dplyr::case_when(cat %in% c("Muy buena", "Buena") ~ "Buena",
                          cat %in% c("Muy mala", "Mala") ~ "Mala", TRUE ~ cat),
    estado = dplyr::case_when(grepl("^Sí", cat) ~ "Debe intervenir",
                              grepl("^No( debe|,)", cat) ~ "No debe intervenir", TRUE ~ cat),
    ideo = dplyr::case_when(n <= 4 ~ "Izquierda (1-4)", n <= 6 ~ "Centro (5-6)",
                            n <= 10 ~ "Derecha (7-10)", TRUE ~ cat),
    tramos = dplyr::case_when(n <= 4 ~ "0-4", n == 5 ~ "5", n <= 10 ~ "6-10", TRUE ~ cat),
    cat)
}

# Etiquetas que solo se cambian en algunas preguntas. Satisfacción con los
# servicios (g.2.2): hasta 2019 el FID abrevia las categorías ("Muy", "Mucho",
# "Mucha"...). La pregunta es la misma en todas las olas: "muy, bastante, poco o
# nada satisfactoriamente"; desde 2020 se añade "Regular" sin leer.
ETIQUETAS_VAR <- list(
  "^g\\.2\\.2\\." = c("Muy" = "Muy satisfactoriamente", "Mucho" = "Muy satisfactoriamente",
                      "Mucha" = "Muy satisfactoriamente", "Bastante" = "Bastante satisfactoriamente",
                      "Poco" = "Poco satisfactoriamente", "Poca" = "Poco satisfactoriamente",
                      "Nada" = "Nada satisfactoriamente"))

NSNC      <- c("N.S.", "N.C.", "N.S./N.C.", "No puede comparar", "Regular (no leer)",
               "Depende (no leer)", "No tiene")
SIN_BASE  <- "No procede"                 # preguntas filtradas: fuera de la base
NUM_NSNC  <- c("97" = "N.S.", "98" = "N.S.", "99" = "N.C.")

# ------------------------------------------------- Preparación de la base -----
preparar <- function(d) {
  cat <- CATALOGO[CATALOGO$var %in% names(d), ]
  xx  <- cruces(d)                         # antes de convertir las preguntas a texto
  for (v in cat$var) {
    x <- d[[v]]
    if (is.numeric(x)) {
      txt  <- as.character(x)
      txt  <- ifelse(txt %in% names(NUM_NSNC), NUM_NSNC[txt], txt)
      nivs <- c(as.character(sort(unique(x[x <= 10]))), "N.S.", "N.C.")
    } else {
      f    <- if (is.factor(x)) x else factor(x)
      nivs <- levels(droplevels(f)); txt <- as.character(f)
      if (cat$tipo[cat$var == v] == "esc") {       # "0 Nada importante" -> "0"
        nivs <- sub("^(\\d+) .*", "\\1", nivs); txt <- sub("^(\\d+) .*", "\\1", txt)
      }
      propias <- unlist(unname(ETIQUETAS_VAR[vapply(names(ETIQUETAS_VAR), grepl, TRUE, v)]))
      dic  <- c(propias, ETIQUETAS)
      rec  <- function(z) ifelse(z %in% names(dic), dic[z], z)
      nivs <- unique(unname(rec(nivs))); txt <- unname(rec(txt))
    }
    nivs <- nivs[nivs %in% unique(txt)]
    fin  <- intersect(c("Regular (no leer)", "Depende (no leer)", "No puede comparar",
                        "N.S.", "N.C.", "No procede"), nivs)
    # "Regular" espontáneo va en medio; el resto de no respuestas, al final
    if ("Regular (no leer)" %in% nivs) fin <- setdiff(fin, "Regular (no leer)")
    if (grepl("^g\\.2\\.2", v))
      nivs <- c(grep("^(Muy|Bastante)", nivs, value = TRUE), intersect("Regular (no leer)", nivs),
                grep("^(Poco|Nada)", nivs, value = TRUE), fin)
    else nivs <- c(setdiff(nivs, fin), fin)
    d[[v]] <- factor(txt, levels = nivs)
  }
  list(d = cbind(d[c("año", "peso", "peso_est", "peso_est_voto", cat$var)], xx), cat = cat)
}

# ----------------------------------------------------- Variables de cruce -----
cruces <- function(d) {
  f <- function(x, n) factor(x, levels = n)
  txt <- function(v) as.character(d[[v]])
  edad <- ifelse(d$edad >= 18 & d$edad <= 110, d$edad, NA)
  tramos <- c("18-34", "35-49", "50-64", "65+")
  e4 <- f(dplyr::case_when(edad < 35 ~ tramos[1], edad < 50 ~ tramos[2], edad < 65 ~ tramos[3],
                           edad >= 65 ~ tramos[4]), tramos)
  sx <- f(dplyr::recode(txt("sexo"), Hombre = "Hombres", Mujer = "Mujeres"), c("Hombres", "Mujeres"))
  ab <- txt("j.11"); hb <- txt("tamuni"); rn <- txt("j.9.2"); rl <- txt("h.7.2")
  # Religiosidad: hasta 2018 (h.7.1) no distingue católicos practicantes
  rl_toda <- dplyr::coalesce(rl, txt("h.7.1"))
  # Recuerdo: IU y UPyD (elecciones de 2008 y 2011) con nombre propio; después,
  # IU concurre dentro de Unidos/Unidas Podemos y de Sumar
  rv <- txt("recuerdo")
  voto <- dplyr::case_when(grepl("^IU", rv) ~ "IU", rv == "UPyD" ~ "UPyD",
                           TRUE ~ dplyr::recode(d$recuerdo_r, VOX = "Vox", Ciudadanos = "Cs", NAC = "Nacionalistas",
                                                OTROS = NA_character_, BAI = NA_character_))
  data.frame(check.names = FALSE,
    x_sexo  = sx,
    x_edad  = e4,
    x_ideo  = f(dplyr::case_when(d$h.5 <= 4 ~ "Izquierda (1-4)", d$h.5 <= 6 ~ "Centro (5-6)",
                                 d$h.5 <= 10 ~ "Derecha (7-10)"),
                c("Izquierda (1-4)", "Centro (5-6)", "Derecha (7-10)")),
    x_voto  = f(voto, c("Sumar", "Podemos", "IU", "PSOE", "UPyD", "Cs", "PP", "Vox", "Nacionalistas")),
    x_est   = f(d$est4, c("Primaria o menos", "ESO", "Bach/FP medio", "Superior")),
    x_econ  = f(dplyr::case_when(txt("j.4") %in% c("Muy buena", "Buena") ~ "Buena",
                                 txt("j.4") == "Regular" ~ "Regular",
                                 txt("j.4") %in% c("Mala", "Muy mala") ~ "Mala"),
                c("Buena", "Regular", "Mala")),
    x_lab   = f(dplyr::case_when(ab == "Trabaja" ~ "Trabaja", grepl("^Parad", ab) ~ "En paro",
                                 grepl("^Jubil|^Pension", ab) ~ "Jubilado/a o pensionista",
                                 ab %in% c("Estudiante", "Trabajo doméstico no remunerado", "Otra situación") ~ "Otra situación"),
                c("Trabaja", "En paro", "Jubilado/a o pensionista", "Otra situación")),
    x_hab   = f(dplyr::case_when(grepl("^Menos|^2\\.001", hb) ~ "Hasta 10.000",
                                 grepl("^10\\.001|^50\\.001", hb) ~ "10.001 a 100.000",
                                 grepl("^100\\.001|^400\\.001|^Más", hb) ~ "Más de 100.000"),
                c("Hasta 10.000", "10.001 a 100.000", "Más de 100.000")),
    x_renta = f(dplyr::case_when(grepl("^Menos de 1.100", rn) ~ "< 1.100 €", grepl("^De 1.100", rn) ~ "1.100-1.800 €",
                                 grepl("^De 1.801", rn) ~ "1.801-2.700 €", grepl("^De 2.701", rn) ~ "2.701-3.900 €",
                                 grepl("^De 3.901", rn) ~ "3.901-5.000 €", grepl("^Más de 5.000", rn) ~ "> 5.000 €"),
                c("< 1.100 €", "1.100-1.800 €", "1.801-2.700 €", "2.701-3.900 €", "3.901-5.000 €", "> 5.000 €")),
    x_sxed  = f(ifelse(is.na(sx) | is.na(e4), NA, paste(sx, e4)),
                as.vector(outer(tramos, c("Hombres", "Mujeres"), function(a, b) paste(b, a)))),
    x_dec   = f(dplyr::recode(txt("e.2.2"), Positiva = "A pagar", Negativa = "A devolver"),
                c("A pagar", "A devolver")),
    x_rel   = f(dplyr::case_when(grepl("^Católico", rl_toda) ~ "Católico/a",
                                 rl_toda == "Creyente de otra religión" ~ "Otra religión",
                                 grepl("^Agn|^Indif|^Ateo|^No creyente", rl_toda) ~ "No creyente"),
                c("Católico/a", "Otra religión", "No creyente")),
    x_rel19 = f(dplyr::case_when(rl == "Católico/a practicante" ~ "Católico practicante",
                                 rl == "Católico/a no practicante" ~ "Católico no practicante",
                                 rl == "Creyente de otra religión" ~ "Otra religión",
                                 grepl("^Agn|^Indif|^Ateo", rl) ~ "No creyente"),
                c("Católico practicante", "Católico no practicante", "Otra religión", "No creyente")))
}

CRUCES <- list(
  "Principales" = c("Sexo" = "x_sexo", "Edad" = "x_edad", "Ideología" = "x_ideo",
                    "Recuerdo de voto" = "x_voto", "Estudios" = "x_est",
                    "Situación económica personal" = "x_econ", "Hábitat" = "x_hab",
                    "Situación laboral" = "x_lab", "Ingresos del hogar (desde 2022)" = "x_renta"),
  "Con muestra justa" = c("Sexo y edad" = "x_sxed", "Resultado de la declaración" = "x_dec",
                          "Religiosidad" = "x_rel", "Religiosidad y práctica (desde 2019)" = "x_rel19"))

# Categorías que no salen marcadas al elegir el grupo: solo se ven si el usuario
# las añade. En el recuerdo de voto, las categorías que solo existen en olas
# antiguas (IU y UPyD hasta 2015, Podemos y Cs de 2016 a 2022) y el grupo
# heterogéneo de los nacionalistas.
SIN_MARCAR <- list(x_voto = c("Podemos", "IU", "UPyD", "Cs", "Nacionalistas"))

# ------------------------------------------------------------------ Colores ---
PALETA <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
# IU (granate) y UPyD (orquídea) coinciden en el tiempo con PSOE, PP y los
# nacionalistas: se distinguen de ellos también con daltonismo (dE OKLab >= 13).
PARTIDOS <- c("Sumar" = "#ed4273", "Podemos" = "#a369f3", "IU" = "#7a1d3a", "PSOE" = "#cc2229",
              "UPyD" = "#f066ea", "Cs" = "#fb5000", "PP" = "#164b91", "Vox" = "#6ab32d",
              "Nacionalistas" = "#8a8a8a")
IDEO   <- c("Izquierda (1-4)" = "#cc2229", "Centro (5-6)" = "#8a8a8a", "Derecha (7-10)" = "#164b91")

# Cruces ordinales: de frío (primera categoría) a cálido (última). Los extremos
# son petróleo y teja, y no azul y rojo puros, para que no se lean como PP y PSOE.
ORDINALES <- c("x_edad", "x_est", "x_hab", "x_renta", "x_econ")
GAMAS <- list(
  "3" = c("#125a80", "#e39a2d", "#b5301c"),
  "4" = c("#125a80", "#45a3c4", "#e8892f", "#b5301c"),
  "6" = c("#0f4a6b", "#2a86b0", "#72b8c8", "#efad4d", "#df6a35", "#9c2418"))

# El color sigue a la categoría: se asigna sobre TODOS los niveles, no sobre los visibles
colores_de <- function(niveles, var = "") {
  if (var == "x_voto") return(PARTIDOS[niveles])
  if (var %in% c("x_ideo") || all(niveles %in% names(IDEO))) return(IDEO[niveles])
  k <- length(niveles)
  p <- if (var %in% ORDINALES) { g <- GAMAS[[as.character(k)]]
         if (is.null(g)) grDevices::colorRampPalette(GAMAS[["6"]], space = "Lab")(k) else g }
       else if (k <= 8) PALETA[seq_len(k)]
       else grDevices::hcl.colors(k, "Viridis")
  stats::setNames(p, niveles)
}
