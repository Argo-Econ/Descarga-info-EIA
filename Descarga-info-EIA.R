#------------------------------------------------------------------------------#
#                       Extracción Data de sector energia EIA
#------------------------------------------------------------------------------#

# Elaborado por: Arturo Gonzalez


# Carga de paquetes ----
# ---------------------------------------------------------------------------- #

pacman::p_load(tidyverse, glue, janitor, tictoc, lubridate, httr, jsonlite
               ,dplyr, xts, TSstudio, ggplot2, ggridges)

# Define tu clave de API de la EIA
# fuente: https://www.eia.gov/opendata/index.php
api_key <- "API Key"

# Define el endpoint de la API y los parámetros de solicitud
url <- "https://api.eia.gov/v2/steo/data/?frequency=monthly&data[0]=value&facets[seriesId][]=BREPUUS&facets[seriesId][]=WTIPUUS&start=1990-01&end=2025-12&sort[0][column]=period&sort[0][direction]=asc&offset=0&length=5000"

# Realiza la solicitud
response <- GET(url, query = list(api_key = api_key))
response


# Comprueba el estado de la respuesta
if (status_code(response) == 200) {
  
  # Analiza la respuesta JSON
  data_cruda <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  # Extrae la información de interés
  Datos_extraidos <- data_cruda$response$data

  # Imprime el data frame
  print(tail(Datos_extraidos))
  print(class(Datos_extraidos))
  print(dim(Datos_extraidos))

  } else {
  # En caso de error imprime el estado
  print(paste("Error:", status_code(response)))
}

# Implementación ---------------------------------------------------------------
## Filtrado y ajuste ------------------------------------------------------------

tail(Datos_extraidos)
Datos_ent <- Datos_extraidos %>% dplyr::select(period, seriesId, value) %>% 
             mutate(fecha = as.Date(paste0(period,"-01"))) %>% 
             pivot_wider(data = .,names_from = seriesId, values_from = value) %>% 
             select(-period)
Datos_ent
tail(Datos_ent)


# Series de tiempo --------------------------------------------------------
str(Datos_ent)
Datos_alt <- apply(Datos_ent,2,function(x) if ( is.character(x) ) {
             as.numeric(x)})

Datos_ent_xts <- Datos_ent[,-1] %>% mutate(dplyr::across(where(is.character),as.numeric)) %>% 
                 xts(.,order.by = Datos_ent$fecha)

ts_plot(Datos_ent_xts
        ,line.mode = "lines"
        ,width = 1)

# Histograma por años -----
names(Datos_ent)
Datos_alt <- Datos_ent %>% mutate(dplyr::across(where(is.character),as.numeric)) %>% 
             pivot_longer(.,cols = c("BREPUUS", "PATC_WORLD", "PAPR_WORLD")
                                        ,names_to = "Ticker"
                                        ,values_to = "valor") %>% 
             separate(fecha,sep = "-",into = c("anho","mes","dia")) %>% 
             filter(Ticker=="BREPUUS" & anho>=2000)


# -----------------------------------------------------------------------------/
# Fin de programa
# -----------------------------------------------------------------------------/



