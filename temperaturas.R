library(readxl)
library(tidyverse)

angulos <- read_excel("datos/temperatura/temperatura_angulos.xlsx")

tangulos <- angulos %>% 
  mutate(Fecha = format(`Fecha Hora`, "%d-%m-%Y")) %>% 
  group_by(Fecha) %>% 
  summarise(tmed_angulos = ((min(Tangulos)+max(Tangulos))/2))
write.csv2(tangulos, "datos/temperatura/tangulos.csv")

tilimuqui <- read_excel("datos/temperatura/temperatura_tilimuqui.xlsx")
ttilimuqui <- tilimuqui %>%
  mutate(`Fecha / Hora` = as.POSIXct(`Fecha / Hora`, format = "%Y-%m-%d %H:%M:%S"),
    Fecha = format(`Fecha / Hora`, "%d-%m-%Y"), 
    tmed_tilimuqui = Tmed_tilimuqui) %>% 
  dplyr::select(Fecha, tmed_tilimuqui)
write.csv2(ttilimuqui, "datos/temperatura/ttilimuqui.csv")

tupungato <- read_excel("datos/temperatura/temperatura_tupungato.xlsx")
ttupungato <- tupungato %>%
  mutate(Fecha1 = as.POSIXct(Fecha, format = "%Y-%m-%d %H:%M:%S"),
         Fecha = format(Fecha1, "%d-%m-%Y")) %>% 
  dplyr::select(Fecha, tmed_tupungato)
write.csv2(ttupungato, "datos/temperatura/ttupungato.csv")

tunuyan <- read_excel("datos/temperatura/temperatura_tunuyan.xlsx")
ttunuyan <- tunuyan %>%
  mutate(Fecha1 = as.POSIXct(Fecha, format = "%Y-%m-%d %H:%M:%S"),
         Fecha = format(Fecha1, "%d-%m-%Y")) %>% 
  dplyr::select(Fecha, tmed_tunuyan)
write.csv2(ttunuyan, "datos/temperatura/ttunuyan.csv")

t1 <- inner_join(tangulos, ttilimuqui) 
t2 <- inner_join(t1, ttupungato)
t3 <- inner_join(t2, ttunuyan)

write.csv2(t3, "datos/temperatura/tabla_completa.csv")
