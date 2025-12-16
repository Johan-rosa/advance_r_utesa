
# Paquetes ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(stringr)

# install.packages("readxl")


# Ejemplo para una provincia ----------------------------------------------

# Cuáles son las hojas de ese excel?
hojas <- excel_sheets("data/data_one.xlsx")[-1]

poblacion_sexo_edad_azua <- read_excel(
  "data/data_one.xlsx", 
  sheet = "Provincia Azua",
  skip = 6
) |>
  rename(edad = 1) |>
  mutate(
    sexo = ifelse(
      str_detect(edad, "Ambos|Hombres|Mujeres"), 
      edad, 
      NA
    )
  ) |>
  fill(sexo) |> 
  filter(
    !str_detect(edad, "Fuente"),
    str_detect(edad, "\\d"),
    sexo != "Ambos sexos"
  ) 

poblacion_sexo_edad_pueblo_viejo <- read_excel(
  "data/data_one.xlsx", 
  sheet = "Pueblo viejo",
  skip = 6
) |>
  rename(edad = 1) |>
  mutate(
    sexo = ifelse(
      str_detect(edad, "Ambos|Hombres|Mujeres"), 
      edad, 
      NA
    )
  ) |>
  fill(sexo) |> 
  filter(
    !str_detect(edad, "Fuente"),
    str_detect(edad, "\\d"),
    sexo != "Ambos sexos"
  ) 


### NOTA importante: No repetir el código muchas veces, NOO!, crea una función!!!

leer_data_one <- function(ruta, hoja) {
  read_excel(
    ruta, 
    sheet = hoja,
    skip = 6
  ) |>
    rename(edad = 1) |>
    mutate(
      sexo = ifelse(
        str_detect(edad, "Ambos|Hombres|Mujeres"), 
        edad, 
        NA
      )
    ) |>
    fill(sexo) |> 
    filter(
      !str_detect(edad, "Fuente"),
      str_detect(edad, "\\d"),
      sexo != "Ambos sexos"
    )  |>
    suppressMessages()
}

leer_data_one("data/data_one.xlsx", "Total País")
leer_data_one("data/data_one.xlsx", "Provincia Azua")
leer_data_one("data/data_one.xlsx", "Azua")

data_desagregada_list <- map(
  hojas, 
  \(hoja) leer_data_one("data/data_one.xlsx", hoja),
  .progress = TRUE
) 

data_desagregada_list <- data_desagregada_list |>
  set_names(hojas)

data_desagregada <- data_desagregada_list |>
  bind_rows(.id = "provincia_municipio") |>
  pivot_longer(matches("\\d"), names_to = "year", values_to = "poblacion")



## Cómo funciona `bind_rows`
a <- data.frame(
  x = c(1, 2),
  y = c(10, 20)
)

b <- data.frame(
  x = c(4, 3),
  y = c(10, 50)
)

bind_rows(list(df_a = a, df_b = b), .id = "source")
# source x  y
# 1   df_a 1 10
# 2   df_a 2 20
# 3   df_b 4 10
# 4   df_b 3 50


# Entendiendo pivot_longer ------------------------------------------------


data_desagregada_list[["Distrito Nacional"]] |>
  pivot_longer(
    -c(edad, sexo),
    names_to = "year",
    values_to = "poblacion"
  ) |>
  mutate(year = as.numeric(year))
