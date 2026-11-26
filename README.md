# Explorador OPPF

Explorador interactivo de la encuesta anual **Opinión Pública y Política Fiscal** del Centro de Investigaciones Sociológicas (CIS), de 2010 a 2026: qué piensan los españoles de los impuestos, el gasto público, el funcionamiento de los servicios públicos y el fraude fiscal, y cómo ha cambiado.

**[Abrir el explorador](https://01a217d3-1525-4c15-c658-39842a4462cb.share.connect.posit.cloud/)** (si nadie lo ha usado en un rato, tarda unos segundos en arrancar).

- **Evolución o una sola ola** de cada pregunta, en líneas, barras o barras apiladas.
- **Cruces** por sexo, edad, ideología, recuerdo de voto, estudios, situación económica, hábitat, situación laboral, ingresos y religiosidad.
- **Respuestas agrupables**, y N.S./N.C. que se pueden ocultar o sacar de la base.
- **Descargas:** el gráfico (PNG), la tabla (CSV) o un Excel con la forma que pide la hoja de datos de un gráfico de PowerPoint.

## Datos y método

- **Microdatos:** Fichero Integrado de Datos del CIS y estudios anuales, de 2010 a 2026.
- **Ponderación propia, igual en todas las olas.** El peso del CIS cambia a lo largo de la serie (no pondera hasta 2020, añade los estudios en 2024) y no corrige el recuerdo de voto. Aquí cada ola se calibra (*raking*) por sexo × edad, comunidad autónoma, tamaño de municipio, estudios × edad y recuerdo de voto. Los objetivos salen del INE (población y Padrón), la EPA y los resultados electorales oficiales. El explorador permite comparar con el peso del CIS y con el propio sin recuerdo de voto.
- **Cambio de modo en 2020:** la encuesta pasa de presencial a telefónica. Los gráficos de evolución marcan el cambio.
- **Precisión:** los intervalos de confianza al 95 % tienen en cuenta los pesos, y se señalan las celdas con menos de 100 casos efectivos.

## Ejecutarlo en local

Con R 4.5 o posterior, desde la carpeta del repositorio:

```r
install.packages(c("shiny", "bslib", "ggplot2", "ggiraph", "ggrepel", "scales",
                   "dplyr", "systemfonts", "ragg", "openxlsx", "DT"))
shiny::runApp()
```

`manifest.json` recoge las versiones exactas de los paquetes con las que se publica.

## Estructura

| Ruta | Contenido |
|---|---|
| `app.R` | Interfaz y servidor |
| `R/1_datos.R` | Catálogo de preguntas, etiquetas y variables de cruce |
| `R/2_calculo.R` | Porcentajes y medias ponderados, con su error |
| `R/3_grafico.R` | Gráficos (ggplot2 + ggiraph) |
| `R/4_excel.R` | Exportación a Excel |
| `datos/oppf_explorador.rds` | Microdatos ponderados (solo las variables que usa la app) |
| `fuentes/` | Roboto (Apache 2.0) |
| `actualizar_datos.R` | Regenera `datos/` desde el proyecto de ponderación (no incluido aquí) |

## Origen de los datos

Origen de los datos: Centro de Investigaciones Sociológicas (CIS), estudios «Opinión Pública y Política Fiscal», 2010-2026. Reutilizados según sus [condiciones de reutilización](https://www.cis.es/es/condiciones-reutilizaci%C3%B3n-datos-del-cis).

Los datos de `datos/` son una selección de variables con pesos de elaboración propia. El CIS no participa en este proyecto ni lo respalda.

## Autoría

Eliseo Garilleti.
