# =============================================================================
# Exportación a Excel, con la forma que espera la hoja de datos de PowerPoint:
# A1 vacía, categorías del eje en la columna A y una columna por serie.
# Reproduce lo que se ve en pantalla (mismos grupos, respuestas y olas).
# =============================================================================

# Devuelve una lista con una tabla por gráfico: list(nombre, valor, margen, fiable)
tablas_ppt <- function(r, op) {
  grupo <- if (op$tipo == "apilada") FALSE else op$hay_g1 && op$color == "grupo"
  if (op$tipo == "barras") {
    # Una sola tabla: si el gráfico tiene paneles, cada panel es una columna
    r <- r[r$año == op$ola, ]
    r$fila <- if (grupo) r$g1 else r$cat
    r$col  <- if (grupo) r$cat else r$g1
    r$hoja <- as.character(r$g2)
  } else {
    r$fila <- factor(r$año)
    r$col  <- if (grupo) r$g1 else r$cat
    otro   <- droplevels(if (grupo) r$cat else r$g1)
    g2     <- droplevels(r$g2)
    partes <- c(if (nlevels(otro) > 1) "otro", if (nlevels(g2) > 1) "g2")
    r$hoja <- if (!length(partes)) "Datos"
              else interaction(list(otro = otro, g2 = g2)[partes], sep = " · ", lex.order = TRUE, drop = TRUE)
  }
  r$fila <- droplevels(factor(r$fila)); r$col <- droplevels(factor(r$col))
  hojas <- if (is.factor(r$hoja)) levels(r$hoja) else unique(r$hoja)
  lapply(hojas, function(h) {
    k <- r[as.character(r$hoja) == h, ]
    m <- function(v, vacio = NA_real_) {
      x <- matrix(vacio, nlevels(r$fila), nlevels(r$col), dimnames = list(levels(r$fila), levels(r$col)))
      x[cbind(as.character(k$fila), as.character(k$col))] <- v
      x
    }
    v <- m(k$valor)
    ok <- list(rowSums(!is.na(v)) > 0, colSums(!is.na(v)) > 0)
    rec <- function(x) x[ok[[1]], ok[[2]], drop = FALSE]
    list(nombre = h, valor = rec(v), margen = rec(m((k$sup - k$inf) / 2)), fiable = rec(m(k$fiable, NA)))
  })
}

# Excel no admite más de 31 caracteres ni ciertos signos, ni nombres repetidos
nombres_hoja <- function(x) {
  x <- gsub("[\\[\\]:*?/\\\\]", " ", x, perl = TRUE)
  x[x == "Total" | !nzchar(trimws(x))] <- "Datos"
  x <- substr(x, 1, 31)
  for (i in seq_along(x)) {
    j <- 1
    base <- x[i]
    while (tolower(x[i]) %in% tolower(c(x[seq_len(i - 1)], "Margen de error", "Ficha", "Detalle"))) {
      j <- j + 1; x[i] <- paste0(substr(base, 1, 27), " (", j, ")")
    }
  }
  x
}

# ficha: data.frame(Campo, Valor); detalle: la tabla larga del explorador
exportar_excel <- function(f, r, op, ficha, detalle, colores) {
  media <- op$medida == "media"
  esc   <- if (media) 1 else 100                # 45,2 -> 0,452 con formato 45%
  tabs  <- tablas_ppt(r, op)
  hojas <- nombres_hoja(vapply(tabs, `[[`, "", "nombre"))

  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontSize = 11, fontName = "Roboto")
  num  <- openxlsx::createStyle(numFmt = if (media) "0.0" else "0%", halign = "center")
  marg <- openxlsx::createStyle(numFmt = if (media) "0.00" else "0.0%", halign = "center")
  cab  <- openxlsx::createStyle(textDecoration = "bold", halign = "center", border = "bottom", borderColour = "#BFBFBF")
  neg  <- openxlsx::createStyle(textDecoration = "bold")
  duda <- openxlsx::createStyle(fontColour = "#9C9C9C", textDecoration = "italic")

  escribir <- function(hoja, m, fila, estilo) {
    openxlsx::writeData(wb, hoja, t(colnames(m)), startCol = 2, startRow = fila, colNames = FALSE)
    openxlsx::writeData(wb, hoja, rownames(m), startCol = 1, startRow = fila + 1, colNames = FALSE)
    # Las celdas sin dato se dejan vacías: PowerPoint las dibuja como hueco
    openxlsx::writeData(wb, hoja, as.data.frame(m / esc), startCol = 2, startRow = fila + 1, colNames = FALSE)
    openxlsx::addStyle(wb, hoja, cab, rows = fila, cols = seq_len(ncol(m) + 1), gridExpand = TRUE)
    openxlsx::addStyle(wb, hoja, estilo, rows = fila + seq_len(nrow(m)), cols = 1 + seq_len(ncol(m)), gridExpand = TRUE)
  }

  for (i in seq_along(tabs)) {
    m <- tabs[[i]]$valor
    openxlsx::addWorksheet(wb, hojas[i])
    escribir(hojas[i], m, 1, num)
    mal <- which(!tabs[[i]]$fiable, arr.ind = TRUE)
    if (nrow(mal)) openxlsx::addStyle(wb, hojas[i], duda, rows = mal[, 1] + 1, cols = mal[, 2] + 1, stack = TRUE)
    openxlsx::setColWidths(wb, hojas[i], 1, min(45, max(8, nchar(rownames(m)) + 2)))
    openxlsx::setColWidths(wb, hojas[i], 1 + seq_len(ncol(m)), pmin(32, pmax(10, nchar(colnames(m)) + 2)))
  }

  # Márgenes con la misma forma, para las barras de error personalizadas
  openxlsx::addWorksheet(wb, "Margen de error")
  fila <- 1
  for (i in seq_along(tabs)) {
    if (length(tabs) > 1) {
      openxlsx::writeData(wb, "Margen de error", tabs[[i]]$nombre, startRow = fila)
      openxlsx::addStyle(wb, "Margen de error", neg, rows = fila, cols = 1)
      fila <- fila + 1
    }
    escribir("Margen de error", tabs[[i]]$margen, fila, marg)
    fila <- fila + nrow(tabs[[i]]$margen) + 2
  }
  openxlsx::setColWidths(wb, "Margen de error", 1:12, 18)

  openxlsx::addWorksheet(wb, "Ficha")
  openxlsx::writeData(wb, "Ficha", ficha, colNames = FALSE)
  openxlsx::addStyle(wb, "Ficha", neg, rows = seq_len(nrow(ficha)), cols = 1)
  openxlsx::addStyle(wb, "Ficha", openxlsx::createStyle(wrapText = TRUE, valign = "top"), rows = seq_len(nrow(ficha)), cols = 2)
  openxlsx::setColWidths(wb, "Ficha", 1:3, c(22, 80, 12))
  if (length(colores)) {
    f0 <- nrow(ficha) + 2
    openxlsx::writeData(wb, "Ficha", data.frame(`Serie` = names(colores), `Color` = unname(colores)), startRow = f0, headerStyle = neg)
    for (i in seq_along(colores))
      openxlsx::addStyle(wb, "Ficha", openxlsx::createStyle(fgFill = colores[[i]]), rows = f0 + i, cols = 3)
  }
  openxlsx::addWorksheet(wb, "Detalle")
  openxlsx::writeData(wb, "Detalle", detalle, headerStyle = cab)
  openxlsx::setColWidths(wb, "Detalle", seq_along(detalle), "auto")
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
}
