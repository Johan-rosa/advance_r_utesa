# Creamos vectores simples
nombres <- c("Johan", "Luis", "Natanael")
edad <- c(33, 35, 39)

# -------------------------------------------------------------------------
# Función con un argumento y pasos intermedios
# -------------------------------------------------------------------------
say_hello <- function(nombre) {
  upper_name <- toupper(nombre)   # Convierte el nombre a MAYÚSCULAS
  paste("Hola", upper_name)       # Construye un saludo
}

# Función sin argumentos
dime_la_hora <- function() {
  paste("La hora exacta es:", Sys.time())   # Sys.time() devuelve la hora actual
}

# -------------------------------------------------------------------------
# Función que calcula el promedio
# -------------------------------------------------------------------------
my_mean <- function(v) {
  sum(v) / length(v)   # fórmula del promedio
}

my_mean(edad)   # Uso de tu función
mean(edad)      # Comparación con la función base de R

dime_la_hora()

# Transformaciones de texto
toupper(nombres)  # Convierte a mayúsculas
tolower(nombres)  # Convierte a minúsculas

say_hello("JOhan")
say_hello(nombres)   # R vectoriza automáticamente

# -------------------------------------------------------------------------
# Función para calcular IMC
# -------------------------------------------------------------------------
calcular_imc <- function(peso_kg, altura_m) {
  peso_kg / (altura_m ^ 2)
}

# -------------------------------------------------------------------------
# Función con categorización usando dplyr::case_when
# -------------------------------------------------------------------------
clasificar_imc <- function(peso_kg, altura_m) {
  imc <- calcular_imc(peso_kg, altura_m)   # Reutiliza la función anterior
  
  categoria <- dplyr::case_when(
    imc < 18.5 ~ "Bajo peso",
    imc < 25   ~ "Normal",
    imc < 30   ~ "Sobrepeso",
    TRUE       ~ "Obesidad"
  )
  
  return(list(
    imc = round(imc, 2),
    categoria = paste("categoria:", categoria)
  ))
}

clasificar_imc(65, 1.35)
clasificar_imc(90, 1.35)
clasificar_imc(45, 1.35)
clasificar_imc(36, 1.35)

# -------------------------------------------------------------------------
# Función para descargar el tipo de cambio desde el BCRD
# -------------------------------------------------------------------------
download_tipo_cambio_bcrd <- function(dest_folder) {
  date <- Sys.Date()   # fecha actual
  url <- "https://cdn.bancentral.gov.do/.../TASA_DOLAR_REFERENCIA_MC.xlsx"
  
  # Guarda el archivo en la carpeta indicada
  download.file(
    url,
    destfile = paste0(dest_folder, "/tc_bcrd_", date, ".xlsx")
  )
}

download_tipo_cambio_bcrd("data") 

# -------------------------------------------------------------------------
# Tibble y normalización manual
# -------------------------------------------------------------------------
df <- tibble(
  a = rnorm(5),
  b = rnorm(5),
  c = rnorm(5),
  d = rnorm(5),
  e = rnorm(5)
)

# Normalización min-max por columnas
df |> mutate(
  a = (a - min(a, na.rm = TRUE)) / (max(a) - min(a)),
  b = (b - min(b)) / (max(b) - min(b)),
  c = (c - min(c)) / (max(c) - min(c)),
  d = (d - min(d)) / (max(d) - min(d)),
  e = (e - min(e)) / (max(e) - min(e)),
)

# Función min-max generalizada
min_max <- function(vect) {
  (vect - min(vect, na.rm = TRUE)) /
    (max(vect, na.rm = TRUE) - min(vect, na.rm = TRUE)) ^ 2
}

df |>
  mutate(
    a = min_max(a),
    b = min_max(b)
  )

# -------------------------------------------------------------------------
# Los operadores también son funciones
# -------------------------------------------------------------------------
2 + 2
`+`(3, 4)     # mismo que 3 + 4
`*`(3, 4)     # mismo que 3 * 4
`<-`(x, 56)   # asignación con la función `<-`

# -------------------------------------------------------------------------
# Scope (rango) de los objetos en R
# -------------------------------------------------------------------------
rm("url")          # Elimina un objeto del entorno global
rm(list = ls())    # Limpia todo el entorno global

nombre <- "Johan"

# Función con argumento por defecto
say_hello <- function(nombre = "Julio") {
  nombre_upper <- toupper(nombre)
  paste("Hola", nombre_upper)
}

say_hello()         # usa "Julio"
say_hello("Manuel")

source("scripts/utils.R")   # Cargar funciones externas
my_mean(1:4)

# Variables dentro de funciones
f1 <- function(x = 1, y = 50, z = a + b) {
  a <- 10
  b <- 100
  c(x, y, z)
}

# -------------------------------------------------------------------------
# Validación de argumentos y manejo de errores
# -------------------------------------------------------------------------
calcular_imc <- function(peso_kg, altura_m) {
  if (class(peso_kg) != "numeric") stop("El peso debe ser un número")
  if (class(altura_m) != "numeric") stop("La altura debe ser un número")
  
  peso_kg / (altura_m ^ 2)
}

calcular_imc("johan", 1.5)   # error
calcular_imc(50, "Hola")     # error

# -------------------------------------------------------------------------
# Crear un archivo temporal
# on.exit() asegura borrar el archivo al terminar la función
# -------------------------------------------------------------------------
f <- tempfile(fileext = ".xlsx")
on.exit(unlink(f))   # se ejecutará al salir del script o función

# -------------------------------------------------------------------------
# Closures: funciones que recuerdan su entorno
# -------------------------------------------------------------------------
rm(list = ls())

create_counter <- function() {
  cuenta <- 0   # variable que solo vive dentro del closure
  
  aumentar  <- function() cuenta <<- cuenta + 1   # <<- modifica el entorno superior
  disminuir <- function() cuenta <<- cuenta - 1
  
  list(
    aumentar  = aumentar,
    disminuir = disminuir
  )
}

contador <- create_counter()

(contador$aumentar())   # 1
(contador$disminuir())  # 0
