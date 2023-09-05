pacman::p_load(tidyverse, readxl)

Sys.setlocale("LC_ALL", "C")

archivos <- list.files(path = "./pistacho/datos_horarios",
                       pattern = "(txt$)",
                       full.names = T)


data_frame_resultado <- read.table(textConnection(tibble_resultado), 
                                   widths = ancho_columnas, skip = 2, 
                                   col.names = c("FECHA", "HORA", "TEMP", "HUM", "PNM", "DD", "FF", "NOMBRE"))


col.names(lista_completa) <- c("FECHA", "HORA", "TEMP", "HUM", "PNM", "DD", "FF", "NOMBRE")

archivos
list.data<-list()

for(i in seq_along(archivos)){
  stringr::str_conv(archivos[i], "UTF-8")
  ancho_columnas <- c(8, 7, 6, 6, 7, 5, 6, 50)
  list.data[[i]] <- read.fwf(archivos[i], widths = ancho_columnas, skip = 2)
}  

lista_completa <- do.call(rbind.data.frame, list.data) %>% 
  na.omit()

lista_completa %>% 
  rename(Fecha = V1, 
         Hora = V2, 
         Tm = V3, 
         HR = V4, 
         PNM = V5, 
         DD = V6, 
         FF = V7, 
         Est = V8) %>% 
  View()

