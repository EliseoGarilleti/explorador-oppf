# =============================================================================
# Gráficos (ggplot + ggiraph) · versión minimal (v2)
# =============================================================================

# Roboto, de la carpeta fuentes/ de la propia app (sin ella, la letra del sistema)
FUENTE <- local({
  dir <- "fuentes"
  ttf <- Sys.glob(file.path(dir, "Roboto-*.ttf"))
  if (length(ttf) >= 8) {
    r <- function(x) file.path(dir, paste0("Roboto-", x, ".ttf"))
    try(systemfonts::register_font("Roboto", r("regular"), r("700"), r("italic"), r("700italic")), silent = TRUE)
    try(systemfonts::register_font("Roboto Light", r("300"), r("500"), r("300italic"), r("500italic")), silent = TRUE)
    try(systemfonts::register_font("Roboto Medium", r("500"), r("700"), r("500italic"), r("700italic")), silent = TRUE)
    c(normal = "Roboto", ligera = "Roboto Light", media = "Roboto Medium")
  } else c(normal = "sans", ligera = "sans", media = "sans")
})

PIE <- "Elaborado por: Eliseo Garilleti\nFuente: CIS (Opinión Pública y Política Fiscal)"

tema_oppf <- function(b = 12) {
  ggplot2::theme_minimal(base_size = b, base_family = FUENTE[["normal"]]) +
    ggplot2::theme(
      legend.position = "top", legend.justification = "left", legend.title = ggplot2::element_blank(),
      legend.margin = ggplot2::margin(0, 0, 4, -6),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "grey50", size = b * .95),
      legend.text = ggplot2::element_text(size = b), legend.key.size = grid::unit(b * 1.2, "pt"),
      panel.grid.minor = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linewidth = .25, color = "grey92"),
      panel.spacing = grid::unit(2, "lines"),
      strip.text = ggplot2::element_text(family = FUENTE[["media"]], face = "bold", size = b * 1.1,
                                         hjust = 0, color = "grey20"),
      plot.title = ggplot2::element_text(family = FUENTE[["media"]], face = "bold", size = b * 1.45,
                                         lineheight = 1.05, margin = ggplot2::margin(b = 5)),
      plot.subtitle = ggplot2::element_text(family = FUENTE[["ligera"]], size = b * 1.1, color = "grey15",
                                            lineheight = 1.1, margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(family = FUENTE[["ligera"]], size = b * .8, color = "grey20",
                                           lineheight = 1.1, margin = ggplot2::margin(t = 10)),
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.margin = ggplot2::margin(12, 14, 10, 12))
}

partir <- function(x, n) if (is.null(x) || !nzchar(x)) NULL else paste(strwrap(x, n), collapse = "\n")
fnum   <- function(x, d) format(round(x, d), nsmall = d, decimal.mark = ",", trim = TRUE)

# Tramos de cada serie: la línea se corta donde falta una ola (la pregunta no se
# hizo) y en el paso de la encuesta presencial a la telefónica (CAMBIO_MODO).
# `puente` son los pares 2019-2020, que se unen con trazo discontinuo.
tramos <- function(r) {
  s <- paste(r$col, r$otro, r$g2, sep = "\r")
  o <- order(s, r$año); r <- r[o, ]; s <- s[o]
  n <- nrow(r); sig <- c(s[-1] == s[-n], FALSE)          # la fila siguiente es de la misma serie
  corta <- c(TRUE, !sig[-n] | diff(r$año) > 1 | (r$año[-1] >= CAMBIO_MODO & r$año[-n] < CAMBIO_MODO))
  r$tramo <- cumsum(corta)
  r$solo  <- stats::ave(r$año, r$tramo, FUN = length) == 1
  i <- which(sig & r$año == CAMBIO_MODO - 1 & c(r$año[-1], NA) == CAMBIO_MODO)
  puente <- data.frame(col = r$col[i], otro = r$otro[i], g2 = r$g2[i], x = r$año[i], xend = r$año[i + 1],
                       y = r$valor[i], yend = r$valor[i + 1])
  list(r = r, puente = puente)
}

# r: salida de estimar(). op: lista de opciones de la interfaz
hacer_grafico <- function(r, op) {
  media <- op$medida == "media"
  dec   <- if (media) 2 else 0
  suf   <- if (media) "" else "%"
  hay1  <- op$hay_g1
  # Qué va al color y qué a los paneles
  por_grupo <- if (op$tipo == "apilada") FALSE
               else if (op$tipo == "barras") isTRUE(op$color == "grupo") && hay1
               else hay1 && op$color == "grupo"
  r$col   <- if (por_grupo) r$g1 else r$cat
  r$otro  <- if (por_grupo) r$cat else r$g1
  pal     <- if (por_grupo) op$col_g1 else colores_de(levels(r$cat))
  r$txt   <- paste0(fnum(r$valor, dec), suf)       # al pasar el ratón, solo el dato
  r$id    <- paste(r$col, r$año, r$otro, r$g2)
  r$alfa  <- ifelse(r$fiable, 1, .35)
  hay_otro <- dplyr::n_distinct(r$otro) > 1
  hay_g2   <- dplyr::n_distinct(r$g2) > 1
  ancho_t  <- op$ancho_txt
  k        <- op$base / ggplot2::.pt       # todo el texto del gráfico depende de op$base
  npan     <- if (hay_otro) min(4, dplyr::n_distinct(r$otro)) else if (hay_g2) min(4, dplyr::n_distinct(r$g2)) else 1
  n_olas   <- dplyr::n_distinct(r$año)
  # Con varios paneles o muchas olas, los años se abrevian para que no se pisen
  anio_txt <- function(x) if (npan >= 3 || (npan == 2 && n_olas > 10) || (op$tipo == "apilada" && n_olas > 12 && npan >= 2))
    paste0("'", substr(x, 3, 4)) else as.character(x)
  # ¿El gráfico cruza el paso de presencial a telefónica?
  cruza_modo <- op$tipo != "barras" && any(r$año < CAMBIO_MODO) && any(r$año >= CAMBIO_MODO)
  rotular_modo <- cruza_modo && npan <= 2

  eje_txt <- function(x) {
    # El símbolo % solo en el valor más alto del eje
    t <- format(x, decimal.mark = ",", trim = TRUE); t[is.na(x)] <- ""
    i <- which.max(x); if (length(i)) t[i] <- paste0(t[i], suf); t
  }

  if (op$tipo == "linea") {
    anios <- sort(unique(r$año))
    tr    <- tramos(r); r <- tr$r
    unico <- !hay_otro && !hay_g2
    ult   <- r[r$año == stats::ave(r$año, r$col, r$otro, r$g2, FUN = max), ]
    nser  <- dplyr::n_distinct(r$col)
    cabe     <- if (unico) 28 else if (npan == 2) 18 else 10
    directas <- nser > 1 && nser <= 8 && npan <= 3 && max(nchar(as.character(r$col))) <= cabe
    r$ultimo <- r$año == stats::ave(r$año, r$col, r$otro, r$g2, FUN = max)
    p <- ggplot2::ggplot(r, ggplot2::aes(año, valor, color = col, fill = col, group = col))
    # Cambio de modo: raya vertical fina, por detrás de las series
    if (cruza_modo) p <- p + ggplot2::geom_vline(xintercept = CAMBIO_MODO - .5, linewidth = .35, color = "grey70")
    if (rotular_modo) p <- p +
      ggplot2::annotate("text", x = CAMBIO_MODO - .62, y = Inf, label = "Presencial", hjust = 1, vjust = 1.5,
                        size = k * .78, color = "grey45", family = FUENTE[["normal"]]) +
      ggplot2::annotate("text", x = CAMBIO_MODO - .38, y = Inf, label = "Telefónica", hjust = 0, vjust = 1.5,
                        size = k * .78, color = "grey45", family = FUENTE[["normal"]])
    if (op$ic) p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = inf, ymax = sup, group = tramo), alpha = .07, color = NA)
    # Puntos: el último de cada serie, los de olas sueltas (entre huecos) y,
    # huecos, los de muestra pequeña. El resto no se dibuja, pero responde al
    # ratón (capa transparente).
    if (nrow(tr$puente)) p <- p + ggplot2::geom_segment(data = tr$puente,
        ggplot2::aes(x = x, xend = xend, y = y, yend = yend, color = col), inherit.aes = FALSE,
        linewidth = .8, linetype = "22", lineend = "butt", show.legend = FALSE)
    p <- p + ggplot2::geom_line(ggplot2::aes(group = tramo), linewidth = .95, lineend = "round", linejoin = "round") +
      ggplot2::geom_point(data = r[(r$ultimo | r$solo) & r$fiable, ], size = op$base * .24, show.legend = FALSE) +
      ggplot2::geom_point(data = r[!r$fiable, ], size = op$base * .2, shape = 21, fill = "white", stroke = .8, show.legend = FALSE) +
      ggiraph::geom_point_interactive(ggplot2::aes(tooltip = txt, data_id = id), size = op$base * .6, alpha = .004, show.legend = FALSE)
    vivas <- ult[ult$año == max(anios), ]; caidas <- ult[ult$año < max(anios), ]
    if (directas) {
      # Series que acaban antes (IU, UPyD, Cs, Podemos...): etiqueta sobre su
      # último punto, con halo blanco porque suele caer encima de otras líneas
      if (nrow(caidas)) p <- p + ggrepel::geom_text_repel(data = caidas, ggplot2::aes(label = if (unico) paste0(col, "  ", fnum(valor, if (media) 1 else 0), suf) else as.character(col)),
        hjust = .5, direction = "y", nudge_y = .035 * max(r$sup), segment.color = NA, seed = 1, size = k, box.padding = .12,
        bg.color = "white", bg.r = .14, family = FUENTE[["media"]], show.legend = FALSE)
      p <- p + ggrepel::geom_text_repel(data = vivas, ggplot2::aes(label = if (unico) paste0(col, "  ", fnum(valor, if (media) 1 else 0), suf) else as.character(col)),
        hjust = 0, direction = "y", nudge_x = .18, segment.color = NA, seed = 1, size = k, box.padding = .12,
        family = FUENTE[["media"]], xlim = c(max(anios) + .12, NA), show.legend = FALSE)
    } else if (nser == 1)
      p <- p + ggrepel::geom_text_repel(data = r, ggplot2::aes(label = paste0(fnum(valor, if (media) 1 else 0), suf)),
        direction = "y", nudge_y = .02 * max(r$valor), segment.color = NA, seed = 1, size = k * .95,
        bg.color = "white", bg.r = .14, family = FUENTE[["media"]], show.legend = FALSE)
    ejes_y <- if (media && op$cero) c(0, 5, 10) else scales::breaks_extended(n = 4)
    lim <- if (media) { if (op$cero) c(0, 10) else NULL } else { if (op$cero) c(0, NA) else NULL }
    # Muchas olas: un año de cada dos (de cada cuatro con 3 o más paneles),
    # contando hacia atrás desde el último
    paso   <- if (length(anios) <= 10) 1 else if (npan >= 3) 4 else 2
    ejes_x <- anios[(max(anios) - anios) %% paso == 0]
    p <- p + ggplot2::scale_x_continuous(breaks = ejes_x, labels = anio_txt,
        expand = ggplot2::expansion(mult = c(.04, if (directas) (.07 + .0125 * max(nchar(as.character(vivas$col)), 4)) * (if (unico) 1 else if (npan == 2) 1.35 else 2.3) else .06))) +
      ggplot2::scale_y_continuous(limits = lim, breaks = ejes_y, labels = eje_txt,
                                  expand = ggplot2::expansion(mult = c(if (op$cero) 0 else .06, if (rotular_modo) .17 else .08)))
    if (op$cero) p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = .9, color = "grey35")   # el panel recorta la mitad
    if (directas || nser == 1) p <- p + ggplot2::theme(legend.position = "none")
    sin_leyenda <- directas || nser == 1

  } else if (op$tipo == "barras") {
    r <- r[r$año == op$ola, ]
    # Por categorías en el eje (distribución) o por grupos en el eje
    r$eje <- if (por_grupo) r$g1 else r$cat
    ejes  <- levels(droplevels(r$eje))
    # Nombres largos o muchas categorías: barras horizontales
    tumbar <- npan == 1 && (length(ejes) > 6 || max(nchar(ejes)) > 24) && max(nchar(ejes)) > 4
    if (!por_grupo && !hay1) { r$col <- factor("Total"); pal <- c(Total = PALETA[1]) }
    p <- ggplot2::ggplot(r, ggplot2::aes(eje, valor, fill = col)) +
      ggiraph::geom_col_interactive(ggplot2::aes(tooltip = txt, data_id = id, alpha = alfa), width = .76) +
      ggplot2::scale_alpha_identity()
    if (op$ic) p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = inf, ymax = sup), width = .16, linewidth = .4, color = "grey30", alpha = .55)
    r$y_txt <- if (op$ic) r$sup else r$valor
    p$data  <- r
    p <- p + ggplot2::geom_text(ggplot2::aes(y = y_txt, label = paste0(fnum(valor, if (media) 1 else 0), if (length(ejes) <= 6) suf)),
                                vjust = if (tumbar) .4 else -.5, hjust = if (tumbar) -.4 else .5, size = k * .95, family = FUENTE[["media"]], color = "grey15") +
      ggplot2::geom_hline(yintercept = 0, linewidth = .3, color = "grey70") +
      ggplot2::scale_x_discrete(limits = if (tumbar) rev(ejes) else ejes,
                                labels = function(x) vapply(x, partir, "", n = if (tumbar) 40 else 14)) +
      ggplot2::scale_y_continuous(limits = if (media) c(0, 10) else c(0, NA),
                                  breaks = if (media) seq(0, 10, 2) else ggplot2::waiver(), labels = eje_txt,
                                  expand = ggplot2::expansion(mult = c(0, .1)))
    if (tumbar) p <- p + ggplot2::coord_flip()
    sin_leyenda <- TRUE

  } else {                                             # apiladas
    p <- ggplot2::ggplot(r, ggplot2::aes(factor(año), valor, fill = col, group = col))
    # Cambio de modo: raya entre la última ola presencial y la primera telefónica
    if (cruza_modo) p <- p + ggplot2::geom_vline(
      xintercept = sum(sort(unique(r$año)) < CAMBIO_MODO) + .5, linewidth = .45, color = "grey55")
    p <- p +
      ggiraph::geom_col_interactive(ggplot2::aes(tooltip = txt, data_id = id), width = .78, color = "white", linewidth = .5,
                                    position = ggplot2::position_stack(reverse = TRUE)) +
      # Con muchas barras estrechas las cifras no caben: queda el dato al pasar el ratón
      ggplot2::geom_text(ggplot2::aes(label = ifelse(valor >= 7 & n_olas * npan <= 36, fnum(valor, 0), "")),
                         position = ggplot2::position_stack(vjust = .5, reverse = TRUE),
                         color = "white", size = k * .9, family = FUENTE[["media"]]) +
      ggplot2::scale_x_discrete(labels = anio_txt, breaks = local({
        a <- sort(unique(r$año)); paso <- if (n_olas <= 12 || npan == 1) 1 else if (npan >= 3) 4 else 2
        as.character(a[(max(a) - a) %% paso == 0]) })) +
      ggplot2::scale_y_continuous(limits = c(0, 100.5), labels = function(x) paste0(x, "%"), expand = c(0, 0))
    sin_leyenda <- FALSE
  }

  # En barras por categoría el color solo distingue categorías si no hay grupos
  if (op$tipo == "barras" && !por_grupo && hay1) {
    r$col <- r$g1; pal <- op$col_g1
    p$data <- r
  }
  p <- p + ggplot2::scale_fill_manual(values = pal, drop = TRUE)
  if (op$tipo == "linea") p <- p + ggplot2::scale_color_manual(values = pal, drop = TRUE)

  etq <- function(x) vapply(as.character(x), partir, "", n = 34)
  if (hay_otro && hay_g2)
    p <- p + ggplot2::facet_grid(g2 ~ otro, labeller = ggplot2::as_labeller(etq))
  else if (hay_otro)
    p <- p + ggplot2::facet_wrap(~ otro, ncol = min(4, dplyr::n_distinct(r$otro)), labeller = ggplot2::as_labeller(etq))
  else if (hay_g2)
    p <- p + ggplot2::facet_wrap(~ g2, ncol = min(4, dplyr::n_distinct(r$g2)), labeller = ggplot2::as_labeller(etq))

  p <- p + ggplot2::labs(title = partir(op$titulo, ancho_t * .78), subtitle = partir(op$subtitulo, ancho_t), caption = op$pie) +
    tema_oppf(op$base)
  if (op$tipo != "linea") {          # el valor ya va rotulado: sobran eje y cuadrícula
    p <- p + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.major.x = ggplot2::element_blank())
    p <- p + if (op$tipo == "barras" && tumbar) ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(color = "grey25"))
             else ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }
  if (sin_leyenda) p <- p + ggplot2::theme(legend.position = "none")
  else p <- p + {
    # Nombres largos: leyenda en vertical para que no se corte
    g <- if (sum(nchar(levels(droplevels(r$col)))) > 85) ggplot2::guide_legend(ncol = 1) else ggplot2::guide_legend(nrow = 1)
    ggplot2::guides(fill = if (op$tipo == "linea") "none" else g, color = g)
  }
  p
}
