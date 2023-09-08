pacman::p_load(tidyverse, readxl, lubridate, 
               magick, Cairo, rsvg)

Sys.setlocale("LC_ALL", "C")

archivos <- list.files(path = "./pistacho/datos_horarios",
                       pattern = "(txt$)",
                       full.names = T)

list.data<-list()
for(i in seq_along(archivos)){
  stringr::str_conv(archivos[i], "UTF-8")
  ancho_columnas <- c(8, 7, 6, 6, 7, 5, 6, 50)
  list.data[[i]] <- read.fwf(archivos[i], widths = ancho_columnas, skip = 2)
}  

list.data

lista_completa <- do.call(rbind.data.frame, list.data) %>% 
  na.omit()

View(lista_completa)

lista_temp <- lista_completa %>% 
  rename(Fecha = V1, 
         Hora = V2, 
         Temp = V3, 
         HR = V4, 
         PNM = V5, 
         DD = V6, 
         FF = V7, 
         Est = V8) %>% 
  mutate(Fecha = lubridate::dmy(Fecha), 
         Hora = lubridate::hms(paste(Hora, "00", "00")), 
         Hora = sprintf("%02d:%02d:%02d", hour(Hora), minute(Hora), second(Hora)),
         Temp = as.numeric(Temp), 
         HR = as.numeric(HR), 
         PNM = as.numeric(PNM), 
         DD = as.numeric(DD), 
         FF = as.numeric(FF), 
         Est = as.factor(Est), 
         Hora_Fecha = paste(Fecha, Hora)) %>%
  select(Hora_Fecha, Temp, Est)


asd <- lista_temp %>% 
  mutate(HF = ifelse(Temp <= 7, 1, 0)) 

asd2 <- asd %>% group_by(Est) %>% 
  summarise(suma = sum(HF))

jpeg("pistacho/graficos_pistacho/t1.jpeg", width = 6000, height = 4000, units = "px", res = 800)
tibble(asd2[0:50,]) %>% ggplot(aes(x = as.factor(Est), y = suma)) +
  geom_col() + 
  ylab(label = "Suma de Horas Frio") +
  xlab(label = "Estacion") +
  ylim(0, 4000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 5, hjust = 0))
dev.off()

jpeg("pistacho/graficos_pistacho/t2.jpeg", width = 6000, height = 4000, units = "px", res = 800)
tibble(asd2[51:100,]) %>% ggplot(aes(x = as.factor(Est), y = suma)) +
  geom_col() + 
  ylab(label = "Suma de Horas Frio") +
  xlab(label = "Estacion") +
  ylim(0, 4000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 5, hjust = 0))
dev.off()

jpeg("pistacho/graficos_pistacho/t3.jpeg", width = 6000, height = 4000, units = "px", res = 800)
tibble(asd2[101:123,]) %>% ggplot(aes(x = as.factor(Est), y = suma)) +
  geom_col() + 
  ylab(label = "Suma de Horas Frio") +
  xlab(label = "Estacion") +
  ylim(0, 4000) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 5, hjust = 0))
dev.off()

imagen1 <- image_read("pistacho/graficos_pistacho/t1.jpeg")
imagen2 <- image_read("pistacho/graficos_pistacho/t2.jpeg")
imagen3 <- image_read("pistacho/graficos_pistacho/t3.jpeg")
lista <- c(imagen1, imagen2, imagen3)
combinacion <- image_append(lista, stack=T)
image_write(combinacion, path = 'pistacho/graficos_pistacho/combinado.PDF', format = 'PDF',
            density = 800)

image_write()
