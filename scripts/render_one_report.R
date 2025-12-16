regiones <- excel_sheets("data/data_one.xlsx")[-1]

render_reporte <- function(hoja) {
  rmarkdown::render(
    input = "reportes/repote_one/template.Rmd",
    output_file = paste0(hoja, ".docx"),
    params = list(
      sheet = hoja
    )
  )
}

purrr::walk(
  regiones[1:5],
  render_reporte
)
