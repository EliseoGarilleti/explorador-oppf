# =============================================================================
# Datos del explorador: copia ligera de la ponderación, solo con las columnas
# que usa la app, dentro de su carpeta (datos/oppf_explorador.rds).
#
# Ejecutar desde la raíz del proyecto cada vez que cambie la ponderación
# (data/output longitudinal/oppf_AAAA_AAAA.RDS):
#   Rscript explorador_v2/actualizar_datos.R
# =============================================================================
local({
  app <- "explorador_v2"
  if (!dir.exists(app)) stop("Ejecuta el script desde la raíz del proyecto.", call. = FALSE)
  source(file.path(app, "R", "1_datos.R"), local = TRUE, encoding = "UTF-8")

  origen <- origen_datos(".")
  if (!length(origen))
    stop("No encuentro data/output longitudinal/oppf_AAAA_AAAA.RDS: ejecuta antes code/OPPF_ponderacion.R", call. = FALSE)
  d <- readRDS(origen)

  cols  <- columnas_app()
  falta <- setdiff(cols, names(d))
  if (length(falta)) stop("Faltan columnas en ", basename(origen), ": ", paste(falta, collapse = ", "), call. = FALSE)
  d <- d[, cols]

  dir.create(file.path(app, "datos"), showWarnings = FALSE)
  f <- file.path(app, "datos", "oppf_explorador.rds")
  saveRDS(d, f, compress = "xz")
  message(sprintf("Guardado %s: %d filas, %d columnas, %.1f MB (de %s)",
                  f, nrow(d), ncol(d), file.size(f) / 1e6, basename(origen)))
})
