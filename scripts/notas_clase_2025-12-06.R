nombres <- c("Johan", "Luis", "Natanael")
edad <- c(33, 35, 39)

# Función con un argumento y pasos intermedio
say_hello <- function(nombre) {
  upper_name <- toupper(nombre)
  paste("Hola", upper_name)
}

# Función sin argumentos
dime_la_hora <- function() {
  paste("La hora exacta es:", Sys.time())
}

# Calcula el promedio
my_mean <- function(v) {
  sum(v) / length(v)
}

my_mean(edad)
mean(edad)

dime_la_hora()
# Mayúsculas y minúsculas
toupper(nombres)
tolower(nombres)

say_hello("JOhan")
say_hello(nombres)

calcular_imc <- function(peso_kg, altura_m) {
  peso_kg / (altura_m ^ 2)
}

# Con categorización
clasificar_imc <- function(peso_kg, altura_m) {
  imc <- calcular_imc(peso_kg, altura_m)
  
  categoria <- dplyr::case_when(
    imc < 18.5 ~ "Bajo peso",
    imc < 25   ~ "Normal",
    imc < 30   ~ "Sobrepeso",
    TRUE       ~ "Obesidad"
  )
  
  return(list(imc = round(imc, 2), categoria = paste("categoria:", categoria)))
}

clasificar_imc(65, 1.35)
clasificar_imc(90, 1.35)
clasificar_imc(45, 1.35)
clasificar_imc(36, 1.35)
# $imc
# [1] 22.86
# 
# $categoria
# [1] "Normal"


# Función para descargar el tipo de cambio  -------------------------------

download_tipo_cambio_bcrd <- function(dest_folder) {
  date <- Sys.Date()
  url <- "https://cdn.bancentral.gov.do/documents/estadisticas/mercado-cambiario/documents/TASA_DOLAR_REFERENCIA_MC.xlsx"
  download.file(
    url, 
    destfile = paste0(dest_folder, "/tc_bcrd_", date, ".xlsx")
  )
}

download_tipo_cambio("data")

df <- tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5),
  d = rnorm(5),
  e = rnorm(5)
)

df |> mutate(
  a = (a - min(a, na.rm = TRUE)) / 
    (max(a, na.rm = TRUE) - min(a, na.rm = TRUE)),

  b = (b - min(a, na.rm = TRUE)) / 
    (max(b, na.rm = TRUE) - min(b, na.rm = TRUE)),

  c = (c - min(c, na.rm = TRUE)) / 
    (max(c, na.rm = TRUE) - min(c, na.rm = TRUE)),

  d = (d - min(d, na.rm = TRUE)) / 
    (max(d, na.rm = TRUE) - min(d, na.rm = TRUE)),
  
  e = (e - min(e, na.rm = TRUE)) / 
    (max(e, na.rm = TRUE) - min(e, na.rm = TRUE)),
)

min_max <- function(vect) {
  (vect - min(vect, na.rm = TRUE)) / 
    (max(vect, na.rm = TRUE) - min(vect, na.rm = TRUE)) ^ 2
}

df |>
  mutate(
    a = min_max(a),
    b = min_max(b)
  )


# Los operadores son funciones --------------------------------------------

2 + 2
`+`(3, 4)
`*`(3, 4)
`<-`(x, 56)


# Scope de una función (rango) --------------------------------------------
rm("url") # Remover un objeto del ambiente global
rm(list = ls()) #Remueve todos los objetos definidos en el ambiente global

nombre <- "Johan"

say_hello <- function(nombre = "Julio") {
  nombre_upper <- toupper(nombre)
  paste("Hola", nombre_upper)
}


say_hello() # Las funciones con valore por defecto, se pueden llamar sin darle parametros
say_hello("Manuel")

## TODO: Definir un <<- en otro archivo y hacer binding flexible
source("scripts/utils.R")
my_mean(1:4)

f1 <- function(x = 1, y = 50, z = a + b) {
  a <- 10
  b <- 100
  
  c(x, y, z)
}

# Errores y validación
calcular_imc <- function(peso_kg, altura_m) {
  if (class(peso_kg) != "numeric") stop("El peso debe ser un número")
  if (class(altura_m) != "numeric") stop("La altura debe ser un número")

  peso_kg / (altura_m ^ 2)
}

calcular_imc("johan", 1.5)
calcular_imc(50, "Hola")

# Crear un archivo temporal
f <- tempfile(fileext = ".xlsx")
on.exit(unlink(f))

## Las funciones guardan y comparten el ambiente en el que fueron creadas
rm(list = ls())
create_counter <- function() {
  cuenta <- 0

  aumentar = function() cuenta <<- cuenta + 1
  disminuir = function() cuenta <<- cuenta - 1

  list(
    aumentar = aumentar,
    disminuir = disminuir
  )
}

contador <- create_counter()
(contador$aumentar())
(contador$disminuir())

