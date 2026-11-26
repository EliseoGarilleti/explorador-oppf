# =============================================================================
# Estimación: % o media ponderados, con error que tiene en cuenta los pesos
# =============================================================================
# Varianza linealizada de un estimador de razón (equivale a survey/srvyr con
# diseño ids = ~1, weights = ~peso). n efectiva = (suma w)^2 / suma w^2.

.razon <- function(y, w) {
  n <- length(w); W <- sum(w); m <- sum(w * y) / W
  c(m, if (n > 1) sqrt(n / (n - 1) * sum((w * (y - m))^2)) / W else NA)
}

# d: datos; var: pregunta; rec: data.frame(cat, grupo, modo); g1/g2: cruces (o NULL)
# modo: "mostrar", "ocultar" (no se dibuja, sigue en la base), "excluir" (sale de la base)
estimar <- function(d, var, rec, medida = "pct", g1 = NULL, g2 = NULL, peso = "peso_est_voto") {
  t <- data.frame(año = d$año, cat = as.character(d[[var]]), w = d[[peso]])
  t$g1 <- if (is.null(g1)) "Total" else as.character(d[[g1]])
  t$g2 <- if (is.null(g2)) "Total" else as.character(d[[g2]])
  t <- t[!is.na(t$cat) & !is.na(t$g1) & !is.na(t$g2), ]
  if (!nrow(t)) return(NULL)

  if (medida == "media") {
    t$y <- suppressWarnings(as.numeric(t$cat))
    fuera <- rec$cat[rec$modo == "excluir"]
    t <- t[!is.na(t$y) & !(t$cat %in% fuera), ]
    if (!nrow(t)) return(NULL)
    t$cat <- "Media"
    cats <- "Media"
  } else {
    i <- match(t$cat, rec$cat)
    t$cat <- rec$grupo[i]; modo <- rec$modo[i]
    t <- t[modo != "excluir", ]
    cats <- unique(rec$grupo[rec$modo == "mostrar"])
    if (!nrow(t) || !length(cats)) return(NULL)
  }

  celdas <- split(t, list(t$año, t$g1, t$g2), drop = TRUE, sep = "\r")
  out <- lapply(celdas, function(k) {
    ne <- sum(k$w)^2 / sum(k$w^2)
    r <- if (medida == "media") matrix(.razon(k$y, k$w), 1)
         else t(vapply(cats, function(c) 100 * .razon(as.numeric(k$cat == c), k$w), numeric(2)))
    data.frame(año = k$año[1], g1 = k$g1[1], g2 = k$g2[1], cat = cats, valor = r[, 1], ee = r[, 2],
               n = nrow(k), neff = round(ne), row.names = NULL)
  })
  out <- do.call(rbind, out)
  q <- stats::qt(.975, pmax(out$n - 1, 1))
  out$inf <- out$valor - q * out$ee
  out$sup <- out$valor + q * out$ee
  if (medida == "pct") { out$inf <- pmax(out$inf, 0); out$sup <- pmin(out$sup, 100) }
  niv <- function(v) if (is.null(v)) "Total" else if (is.factor(d[[v]])) levels(d[[v]]) else sort(unique(d[[v]]))
  out$g1  <- factor(out$g1, levels = niv(g1))
  out$g2  <- factor(out$g2, levels = niv(g2))
  out$cat <- factor(out$cat, levels = cats)
  out$fiable <- out$neff >= 100
  out[order(out$cat, out$g1, out$g2, out$año), ]
}

# Cruce por otra pregunta: usa su agrupación sugerida, sin NS/NC
pregunta_como_cruce <- function(d, var, catalogo) {
  x <- as.character(d[[var]]); pre <- catalogo$pre[catalogo$var == var]
  niv <- setdiff(levels(d[[var]]), c(NSNC, SIN_BASE))
  g <- agrupar_sugerido(niv, pre)
  factor(g[match(x, niv)], levels = unique(g))
}
