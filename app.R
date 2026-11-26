# =============================================================================
# Explorador OPPF · v2 (versión minimal)
# -----------------------------------------------------------------------------
# Ejecutar desde la raíz del proyecto:   shiny::runApp("explorador_v2")
# Olas de 2010 a la actualidad (presenciales hasta 2019, telefónicas desde 2020).
#
# La carpeta es autónoma, para poder publicarla tal cual:
#   R/1_datos.R    catálogo, cruces y carga de datos   R/2_calculo.R  estimación
#   R/3_grafico.R  gráficos                            R/4_excel.R    Excel
#   datos/         los microdatos que usa (los genera actualizar_datos.R)
#   fuentes/       Roboto, para la interfaz y los gráficos
#   manifest.json  paquetes y versiones, para publicarla en Posit Connect Cloud
# Tras cambiar la ponderación: Rscript explorador_v2/actualizar_datos.R
# =============================================================================
library(shiny)
library(bslib)

BASE <- preparar(cargar_datos())
D    <- BASE$d
CAT  <- BASE$cat
OLAS <- sort(unique(D$año))

pregs <- lapply(split(CAT, factor(CAT$tema, unique(CAT$tema))), function(k) setNames(k$var, k$etiqueta))
otras <- lapply(pregs, function(x) setNames(paste0("p_", x), names(x)))
names(otras) <- paste("Otra pregunta ·", names(otras))
PESOS <- c("Propio, con recuerdo de voto" = "peso_est_voto", "Propio, sin recuerdo de voto" = "peso_est", "CIS" = "peso")
# Variables de cada peso, para la nota al pie
VARS_PESO <- c(
  peso_est_voto = "Ponderado por sexo, edad, comunidad autónoma, hábitat, estudios y recuerdo de voto.",
  peso_est      = "Ponderado por sexo, edad, comunidad autónoma, hábitat y estudios.",
  peso          = "Peso del CIS: sin ponderar hasta 2020; desde 2021, sexo, edad, territorio y hábitat; desde 2024, también estudios.")
MODOS <- c("Mostrar" = "mostrar", "Ocultar" = "ocultar", "Excluir" = "excluir")
addResourcePath("fuentes", "fuentes")       # Roboto para la interfaz (estilo.css)

info <- function(v) CAT[CAT$var == v, ]
# Botones segmentados y bloques plegables
seg    <- function(...) div(class = "seg", radioButtons(..., inline = TRUE))
plegar <- function(titulo, ...) tags$details(class = "plegar", tags$summary(titulo), div(...))

ui <- page_sidebar(
  window_title = "Explorador OPPF",
  theme = bs_theme(version = 5, primary = "#164b91", base_font = "Roboto, 'Helvetica Neue', Arial, sans-serif", "font-size-base" = "0.875rem"),
  tags$head(tags$style(HTML(readLines("estilo.css", encoding = "UTF-8") |> paste(collapse = "\n")))),
  sidebar = sidebar(
    width = 300, open = list(mobile = "closed"), bg = "#fafaf8",
    div(class = "marca", "Explorador OPPF"),
    selectizeInput("var", "Pregunta", pregs, selected = "c.2"),
    conditionalPanel("output.es_escala", seg("medida", NULL, c("Media" = "media", "Porcentajes" = "pct"))),
    selectizeInput("g1", "Grupo", c(list("Población general" = c("Población general" = "no")), CRUCES, otras)),
    conditionalPanel("input.g1 != 'no'",
      selectizeInput("g1_cat", NULL, NULL, multiple = TRUE, options = list(plugins = list("remove_button"))),
      selectizeInput("g2", "Segundo grupo", c(list("Ninguno" = c("Ninguno" = "no")), CRUCES))),
    seg("tipo", "Gráfico", c("Evolución" = "linea", "Una ola" = "barras", "Apiladas" = "apilada")),
    conditionalPanel("input.tipo == 'barras'", selectInput("ola", "Ola", rev(OLAS))),
    conditionalPanel("input.g1 != 'no' && input.tipo != 'apilada'",
      seg("color", "Comparar", c("Grupos" = "grupo", "Respuestas" = "cat"))),
    div(class = "plegables",
      plegar("Recodificar respuestas",
        conditionalPanel("output.usa_recode",
          div(class = "enlaces", actionLink("rec_sug", "Agrupar"), actionLink("rec_ori", "Originales")),
          uiOutput("rec_ui"),
          p(class = "nota", "Para agrupar, escribe el mismo nombre. ", strong("Ocultar"), " mantiene la respuesta en la base del %; ",
            strong("excluir"), " la saca.")),
        conditionalPanel("!output.usa_recode",
          p(class = "nota", "La media usa los valores de la escala, sin N.S./N.C. Cambia a «Porcentajes» para recodificar."))),
      plegar("Peso y formato",
        selectInput("peso", "Peso", PESOS),
        conditionalPanel("input.g1 == 'no'", checkboxInput("comparar", "Comparar los tres pesos", FALSE)),
        checkboxInput("ic", "Intervalos de confianza", TRUE),
        checkboxInput("cero", "Eje desde cero", TRUE),
        sliderInput("anios", "Olas", min(OLAS), max(OLAS), range(OLAS), step = 1, sep = "", ticks = FALSE)))),
  div(class = "cabeza",
    div(class = "titular", uiOutput("titular")),
    div(class = "acciones",
      seg("vista", NULL, c("Gráfico" = "graf", "Tabla" = "tabla")),
      tags$details(class = "exportar", tags$summary("Exportar"),
        div(class = "caja",
          textInput("titulo", "Título", placeholder = "Se añade al gráfico"),
          textAreaInput("subtitulo", "Subtítulo", rows = 2, placeholder = "Si lo dejas vacío, se usa la pregunta"),
          div(class = "d-flex gap-2", numericInput("ancho", "Ancho (cm)", 22, 8, 60), numericInput("alto", "Alto (cm)", 13, 6, 60)),
          div(class = "d-flex gap-2 mt-1",
            downloadButton("png", "Imagen", class = "btn-sm btn-primary"),
            downloadButton("xlsx", "Excel", class = "btn-sm btn-outline-secondary"),
            downloadButton("csv", "CSV", class = "btn-sm btn-outline-secondary")),
          p(class = "nota", "Excel: tabla lista para pegar en los datos de un gráfico de PowerPoint."))))),
  div(class = "lienzo",
    conditionalPanel("input.vista == 'graf'", class = "hoja", ggiraph::girafeOutput("graf", height = "100%")),
    conditionalPanel("input.vista == 'tabla'", class = "hoja tabla", DT::DTOutput("tabla"))),
  uiOutput("pie"))

server <- function(input, output, session) {
  es_escala <- reactive(info(input$var)$tipo == "esc")
  output$es_escala <- reactive(es_escala())
  con_g1 <- reactive(input$g1 != "no")
  medida <- reactive(if (es_escala() && input$tipo != "apilada") input$medida else "pct")
  output$usa_recode <- reactive(medida() == "pct")
  outputOptions(output, "es_escala", suspendWhenHidden = FALSE)
  outputOptions(output, "usa_recode", suspendWhenHidden = FALSE)

  # ---------------------------------------------------------- Recodificación ---
  # Cada (pregunta, agrupación) genera una versión nueva de los campos, para no
  # leer valores de la pregunta anterior mientras la interfaz se actualiza.
  version <- reactiveVal(0)
  base_rec <- reactiveVal(NULL)
  # Niveles presentes en alguna ola (las NS/NC y "no procede" traen su modo por defecto)
  nueva_rec <- function(v, sug) {
    cat <- levels(D[[v]])
    data.frame(cat = cat,
               grupo = if (sug) agrupar_sugerido(cat, info(v)$pre) else cat,
               modo = ifelse(cat %in% SIN_BASE, "excluir", ifelse(cat %in% NSNC, "ocultar", "mostrar")))
  }
  poner_rec <- function(sug) { base_rec(nueva_rec(input$var, sug)); version(isolate(version()) + 1) }
  observeEvent(input$var, poner_rec(TRUE))
  observeEvent(input$rec_sug, poner_rec(TRUE))
  observeEvent(input$rec_ori, poner_rec(FALSE))

  output$rec_ui <- renderUI({
    v <- version(); b <- base_rec(); req(b)
    lapply(seq_len(nrow(b)), function(i) div(class = "rec-fila",
      div(class = "rec-cat", b$cat[i]),
      textInput(paste0("g", v, "_", i), NULL, b$grupo[i]),
      selectInput(paste0("m", v, "_", i), NULL, MODOS, b$modo[i], selectize = FALSE)))
  })
  outputOptions(output, "rec_ui", suspendWhenHidden = FALSE)

  rec <- debounce(reactive({
    b <- base_rec(); req(b); v <- version()
    for (i in seq_len(nrow(b))) {
      g <- input[[paste0("g", v, "_", i)]]; m <- input[[paste0("m", v, "_", i)]]
      if (!is.null(g) && nzchar(trimws(g))) b$grupo[i] <- trimws(g)
      if (!is.null(m)) b$modo[i] <- m
    }
    b
  }), 400)

  # ----------------------------------------------------------------- Grupos ---
  datos <- reactive({
    d <- D
    if (startsWith(input$g1, "p_"))
      d[[input$g1]] <- pregunta_como_cruce(D, sub("^p_", "", input$g1), CAT)
    d
  })
  observeEvent(input$g1, {
    if (con_g1()) {
      n <- levels(datos()[[input$g1]])
      # Todas empiezan marcadas salvo las de SIN_MARCAR (R/1_datos.R)
      updateSelectizeInput(session, "g1_cat", choices = n, selected = setdiff(n, SIN_MARCAR[[input$g1]]))
      updateCheckboxInput(session, "comparar", value = FALSE)
    } else updateSelectizeInput(session, "g2", selected = "no")
  })
  # Las apiladas necesitan porcentajes
  comparando <- reactive(!con_g1() && isTRUE(input$comparar) && input$tipo != "apilada")

  res <- reactive({
    b <- rec(); req(b, identical(b$cat, levels(D[[input$var]])))
    d  <- datos(); d <- d[d$año >= input$anios[1] & d$año <= input$anios[2], ]
    g1 <- if (con_g1()) input$g1; g2 <- if (con_g1() && input$g2 != "no") input$g2
    validate(need(is.null(g1) || g1 != paste0("p_", input$var), "Elige como grupo una pregunta distinta de la que estás viendo."),
             need(is.null(g2) || g2 != g1, "Los dos grupos son el mismo."))
    r <- if (comparando()) {
      do.call(rbind, lapply(names(PESOS), function(p) {
        k <- estimar(d, input$var, b, medida(), peso = PESOS[[p]]); if (is.null(k)) return(NULL)
        k$g1 <- factor(p, levels = names(PESOS)); k }))
    } else estimar(d, input$var, b, medida(), g1, g2, input$peso)
    validate(need(!is.null(r), "No hay nada que mostrar: revisa las respuestas ocultas o excluidas."))
    if (!is.null(g1)) {
      r <- r[r$g1 %in% input$g1_cat, ]
      validate(need(nrow(r) > 0, "Elige al menos una categoría del grupo."))
    }
    r
  })

  # ancho_in: ancho real del gráfico en pulgadas (pantalla o exportación), para
  # que etiquetas y años se adapten a pantallas estrechas
  opciones <- function(r, exportar = FALSE, ancho_in = NULL) {
    g1 <- input$g1
    todos <- if (comparando()) names(PESOS) else if (con_g1()) levels(datos()[[g1]]) else "Total"
    list(tipo = input$tipo, medida = medida(), hay_g1 = con_g1() || comparando(),
         col_g1 = colores_de(todos, g1), color = if (comparando()) "grupo" else input$color,
         ic = input$ic, cero = input$cero, ola = as.integer(input$ola), base = if (exportar) 11 else 10,
         titulo = input$titulo,
         subtitulo = if (exportar && !nzchar(input$subtitulo)) info(input$var)$pregunta else input$subtitulo,
         pie = if (exportar) PIE else NULL,
         ancho_txt = if (exportar) input$ancho * 4.6 else 120,
         ancho_in = if (exportar) input$ancho / 2.54 else ancho_in)
  }

  # --------------------------------------------------------------- Cabecera ---
  # El titular no depende del cálculo: se ve aunque el gráfico dé un aviso
  output$titular <- renderUI({
    i <- info(input$var)
    tagList(h1(i$etiqueta), p(class = "enunciado", i$pregunta))
  })
  # Base, ponderación y avisos: los usan el pie y la ficha del Excel
  notas <- reactive({
    r <- res()
    vis <- if (input$tipo == "barras") r[r$año == as.integer(input$ola), ] else r
    b <- rec()
    lista <- function(m) sub("\\.$", "", paste(unique(b$cat[b$modo == m]), collapse = ", "))
    base <- if (medida() == "media") "Media de quienes responden." else paste0(
      "% sobre el total", if (nzchar(lista("excluir"))) paste0(" sin ", lista("excluir")), ".",
      if (nzchar(lista("ocultar"))) paste0(" No se dibujan: ", lista("ocultar"), "."))
    faltan <- setdiff(seq(input$anios[1], input$anios[2]), r$año)
    serie  <- input$tipo != "barras"
    # ¿La serie que se ve pasa de la ola a-1 a la ola a?
    cruza  <- function(a) serie && input$anios[1] < a && input$anios[2] >= a
    ola_ve <- function(a) if (serie) input$anios[1] <= a && input$anios[2] >= a else as.integer(input$ola) == a
    usa    <- function(cruce, pregunta) input$var == pregunta || paste0("p_", pregunta) %in% input$g1 ||
                                        (con_g1() && cruce %in% c(input$g1, input$g2))
    # El paso a la encuesta telefónica (2020) no lleva aviso: ya lo marca la raya
    # vertical del gráfico
    av <- c(
      if (any(!vis$fiable)) "Hay celdas con n efectiva inferior a 100: en las líneas, punto hueco; en las barras, atenuadas.",
      if (length(faltan)) paste0("Sin datos en: ", paste(faltan, collapse = ", "), "."),
      if (!serie && as.integer(input$ola) < CAMBIO_MODO) "Ola presencial: no es del todo comparable con las telefónicas (desde 2020).",
      if (input$peso == "peso" && !comparando() && (cruza(2021) || cruza(2024)))
        "El peso del CIS cambia en 2021 (empieza a ponderar) y en 2024 (añade los estudios): la serie no es comparable.",
      if (usa("x_econ", "j.4") && cruza(2024))
        "Situación económica personal: desde 2024 «Regular» ya no se lee como respuesta y casi desaparece: la serie se rompe ese año.",
      if (usa("", "e.3") && ola_ve(2011))
        "2011, sin datos de la casilla de la renta: en el fichero del CIS esa variable repite el resultado de la declaración.",
      if (medida() == "pct" && con_g1() && input$tipo == "linea" && length(unique(r$cat)) > 6)
        "Son muchas respuestas para un solo gráfico: prueba a agruparlas.")
    list(avisos = av, texto = paste0(base, " ", if (comparando()) "Comparación de los tres pesos." else VARS_PESO[[input$peso]]))
  })
  output$pie <- renderUI({
    av <- notas()$avisos
    div(class = "pie",
      if (length(av)) tooltip(span(class = "ambar", tabindex = "0", span(class = "punto"),
                                   if (length(av) == 1) "1 aviso" else paste(length(av), "avisos")),
                              lapply(av, p), placement = "top"),
      span(paste0(notas()$texto, " ", if (input$ic) switch(input$tipo, linea = "Bandas: IC 95%.", barras = "Trazos: IC 95%.", ""))))
  })

  # ---------------------------------------------------------------- Salidas ---
  output$graf <- ggiraph::renderGirafe({
    r <- res(); if (input$tipo == "barras") validate(need(as.integer(input$ola) %in% r$año, "No hay datos en esa ola."))
    w <- session$clientData$output_graf_width; h <- session$clientData$output_graf_height
    w <- if (is.null(w) || w < 200) 900 else w; h <- if (is.null(h) || h < 250) 520 else h
    ggiraph::girafe(ggobj = hacer_grafico(r, opciones(r, ancho_in = w / 96)), width_svg = w / 96, height_svg = h / 96,
      options = list(
        ggiraph::opts_tooltip(css = "background:#fff;color:#222;border:1px solid #e3e3e0;border-radius:8px;box-shadow:0 4px 14px rgba(0,0,0,.08);padding:8px 10px;font-family:Roboto,sans-serif;font-size:13px", opacity = 1),
        ggiraph::opts_hover(css = "fill-opacity:.28;stroke-opacity:0;"),
        ggiraph::opts_toolbar(hidden = c("selection", "zoom", "misc"), saveaspng = FALSE),
        ggiraph::opts_sizing(rescale = TRUE, width = 1)))
  })

  tabla <- reactive({
    r <- res(); dec <- if (medida() == "media") 2 else 1
    t <- data.frame(Ola = r$año, Grupo = r$g1, Subgrupo = r$g2, Respuesta = r$cat, Valor = round(r$valor, dec),
                    `IC inf` = round(r$inf, dec), `IC sup` = round(r$sup, dec), n = r$n, `n efectiva` = r$neff, check.names = FALSE)
    if (all(t$Grupo == "Total")) t$Grupo <- NULL
    if (all(t$Subgrupo == "Total")) t$Subgrupo <- NULL
    t
  })
  output$tabla <- DT::renderDT(DT::datatable(tabla(), rownames = FALSE, options = list(pageLength = 25, dom = "ftip",
    language = list(search = "Buscar:", info = "_START_ a _END_ de _TOTAL_", paginate = list(previous = "Anterior", `next` = "Siguiente")))))

  outputOptions(output, "graf", suspendWhenHidden = TRUE)
  nombre <- function(ext) paste0("oppf_", gsub("[^a-z0-9]+", "_", tolower(chartr("áéíóúüñÁÉÍÓÚÑ", "aeiouunAEIOUN", info(input$var)$etiqueta))), ".", ext)
  output$png <- downloadHandler(function() nombre("png"), function(f) {
    r <- res()
    ggplot2::ggsave(f, hacer_grafico(r, opciones(r, TRUE)), device = ragg::agg_png, width = input$ancho,
                    height = input$alto, units = "cm", dpi = 300, bg = "white")
  })
  output$xlsx <- downloadHandler(function() nombre("xlsx"), function(f) {
    r <- res(); if (input$tipo == "barras") req(as.integer(input$ola) %in% r$año)
    op <- opciones(r); i <- info(input$var); grupo <- function(g) {
      todas <- c(unlist(unname(CRUCES)), unlist(unname(otras))); names(todas)[match(g, todas)] }
    ficha <- rbind(
      c("Pregunta", i$pregunta), c("Variable", input$var),
      if (nzchar(input$titulo)) c("Título", input$titulo),
      if (con_g1()) c("Grupo", grupo(input$g1)),
      if (con_g1() && input$g2 != "no") c("Segundo grupo", grupo(input$g2)),
      c("Olas", if (input$tipo == "barras") input$ola else paste(range(r$año), collapse = "-")),
      if (any((if (input$tipo == "barras") as.integer(input$ola) else r$año) < CAMBIO_MODO))
        c("Modo de entrevista", "Presencial hasta 2019; telefónica desde 2020."),
      c("Base y ponderación", notas()$texto),
      if (length(notas()$avisos)) c("Avisos", paste(notas()$avisos, collapse = " ")),
      c("Cifras en gris", "Muestra efectiva inferior a 100: tomar con cautela."),
      c("Margen de error", "Semianchura del intervalo de confianza al 95%."),
      c("Fuente", "CIS (Opinión Pública y Política Fiscal)"))
    por_grupo <- op$hay_g1 && op$color == "grupo" && op$tipo != "apilada"
    col <- if (por_grupo || (op$tipo == "barras" && op$hay_g1)) op$col_g1[levels(droplevels(r$g1))]
           else colores_de(levels(r$cat))
    exportar_excel(f, r, op, as.data.frame(ficha), tabla(), col[!is.na(col)])
  })
  output$csv <- downloadHandler(function() nombre("csv"), function(f)
    write.csv2(tabla(), f, row.names = FALSE, fileEncoding = "UTF-8"))
}

shinyApp(ui, server)
