library(readxl)
library(tidyverse)
library(chillR)

#agua amarga
data_agua_amarga <- readxl::read_excel("temperatura/2023/2023_agua_amarga.xlsx")

d_agua_amarga <- data_agua_amarga %>% 
  mutate(Fecha = lubridate::as_date(Fecha), 
         Year = lubridate::year(Fecha), 
         Month = lubridate::month(Fecha),
         Day = lubridate::day(Fecha), 
         JDay = lubridate::yday(Fecha),
         Tmin = as.numeric(Tmin),
         Tmax = as.numeric(Tmax),
         YYMMDD = Year * 10000 + Month * 
           100 + Day) %>% 
  select(YYMMDD, Year, Month, Day, JDay, Tmin, Tmax) %>% 
  na.omit()

d_agua_amarga <- stack_hourly_temps(d_agua_amarga, latitude = 33.5)

dm_mza_agua_amarga_as <- d_agua_amarga$hourtemps %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=90 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(MZA_Agua_Amarga = mean(DM))

dm_mza_agua_amarga_ms <- d_agua_amarga$hourtemps %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=120 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(MZA_Agua_Amarga = mean(DM))

#angulos
data_angulos <- readxl::read_excel("temperatura/2023/2023_angulos.xlsx")

d_angulos <- data_angulos %>% 
  mutate(Year = lubridate::year(Fecha), 
         Month = lubridate::month(Fecha),
         Day = lubridate::day(Fecha), 
         JDay = lubridate::yday(Fecha), 
         Hour = as.integer(Hora), 
         Temp = as.numeric(Temp), 
         YYMMDD = Year * 10000 + Month * 
           100 + Day) %>% 
  select(YYMMDD, Year, Month, Day, JDay, Hour, Temp) %>% 
  na.omit()

dm_lr_angulos_as <- d_angulos %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=90 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(LR_Angulos = mean(DM))

dm_lr_angulos_ms <- d_angulos %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=120 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(LR_Angulos = mean(DM))

#catamarca
data_catamarca <- readxl::read_excel("temperatura/2023/2023_catamarca.xlsx")

data_catamarca

d_catamarca <- data_catamarca %>% 
  mutate(Year = lubridate::year(Fecha), 
         Month = lubridate::month(Fecha),
         Day = lubridate::day(Fecha), 
         JDay = lubridate::yday(Fecha), 
         Hour = as.integer(Hora), 
         Temp = as.numeric(Temperatura), 
         YYMMDD = Year * 10000 + Month * 
           100 + Day) %>% 
  select(YYMMDD, Year, Month, Day, JDay, Hour, Temp) %>% 
  na.omit()

dm_catamarca_as <- d_catamarca %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=90 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(CAT_La_Rinconada = mean(DM))

dm_catamarca_ms <- d_catamarca %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=120 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(CAT_La_Rinconada = mean(DM))

#la consulta
data_la_consulta <- readxl::read_excel("temperatura/2023/2023_la_consulta.xlsx")

d_la_consulta <- data_la_consulta %>% 
  mutate(Fecha = lubridate::as_date(Fecha), 
         Year = lubridate::year(Fecha), 
         Month = lubridate::month(Fecha),
         Day = lubridate::day(Fecha), 
         JDay = lubridate::yday(Fecha),
         Tmin = as.numeric(Tmin),
         Tmax = as.numeric(Tmax),
         YYMMDD = Year * 10000 + Month * 
           100 + Day) %>% 
  select(YYMMDD, Year, Month, Day, JDay, Tmin, Tmax) %>% 
  na.omit()

d_la_consulta <- stack_hourly_temps(d_la_consulta, latitude = 33.7)

dm_mza_la_consulta_as <- d_la_consulta$hourtemps %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=90 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(MZA_La_Consulta = mean(DM))

dm_mza_la_consulta_ms <- d_la_consulta$hourtemps %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=120 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(MZA_La_Consulta = mean(DM))

#los_sauces
data_los_sauces <- readxl::read_excel("temperatura/2023/2023_los_sauces.xlsx")

d_los_sauces <- data_los_sauces %>% 
  mutate(Fecha = lubridate::as_date(Fecha), 
         Year = lubridate::year(Fecha), 
         Month = lubridate::month(Fecha),
         Day = lubridate::day(Fecha), 
         JDay = lubridate::yday(Fecha),
         Tmin = as.numeric(Tmin),
         Tmax = as.numeric(Tmax),
         YYMMDD = Year * 10000 + Month * 
           100 + Day) %>% 
  select(YYMMDD, Year, Month, Day, JDay, Tmin, Tmax) %>% 
  na.omit()

d_los_sauces <- stack_hourly_temps(d_los_sauces, latitude = 33.7)

dm_mza_los_sauces_as <- d_los_sauces$hourtemps %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=90 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(MZA_Los_Sauces = mean(DM))

dm_mza_los_sauces_ms <- d_los_sauces$hourtemps %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=120 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(MZA_Los_Sauces = mean(DM))

#miranda
data_miranda <- readxl::read_excel("temperatura/2023/2023_miranda.xlsx")

d_miranda <- data_miranda %>% 
  mutate(Year = lubridate::year(Fecha), 
         Month = lubridate::month(Fecha),
         Day = lubridate::day(Fecha), 
         JDay = lubridate::yday(Fecha), 
         Hour = as.integer(Hora), 
         Temp = as.numeric(Tmiranda), 
         YYMMDD = Year * 10000 + Month * 
           100 + Day) %>% 
  select(YYMMDD, Year, Month, Day, JDay, Hour, Temp) %>% 
  na.omit()

dm_lr_miranda_as <- d_miranda %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=90 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(LR_Miranda = mean(DM))

dm_lr_miranda_ms <- d_miranda %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=120 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(LR_Miranda = mean(DM))

#tilimuqui
data_tilimuqui <- readxl::read_excel("temperatura/2023/2023_tilimuqui.xlsx")

d_tilimuqui <- data_tilimuqui %>% 
  mutate(Year = lubridate::year(Fecha), 
         Month = lubridate::month(Fecha),
         Day = lubridate::day(Fecha), 
         JDay = lubridate::yday(Fecha), 
         Hour = as.integer(Hora), 
         Temp = as.numeric(Ttilimuqui), 
         YYMMDD = Year * 10000 + Month * 
           100 + Day) %>% 
  select(YYMMDD, Year, Month, Day, JDay, Hour, Temp) %>% 
  na.omit()

dm_lr_tilimuqui_as <- d_tilimuqui %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=90 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(LR_Tilimuqui = mean(DM))

dm_lr_tilimuqui_ms <- d_tilimuqui %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=120 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(LR_Tilimuqui = mean(DM))

#san_martin
data_san_martin <- readxl::read_excel("temperatura/2023/2023_san_martin.xlsx")

d_san_martin <- data_san_martin %>% 
  mutate(Fecha = lubridate::as_date(Fecha), 
         Year = lubridate::year(Fecha), 
         Month = lubridate::month(Fecha),
         Day = lubridate::day(Fecha), 
         JDay = lubridate::yday(Fecha),
         Temp = as.numeric(Temp),
         YYMMDD = Year * 10000 + Month * 
           100 + Day) %>% 
  select(YYMMDD, Year, Month, Day, JDay, Temp) %>% 
  na.omit()

dm_mza_san_martin_as <- d_san_martin %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=90 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(MZA_San_Martin = mean(DM))

dm_mza_san_martin_ms <- d_san_martin %>% 
  group_by(Year) %>% 
  arrange(Year) %>% 
  filter(JDay>=120 & JDay<=250) %>% 
  mutate(DM = Dynamic_Model(Temp)) %>% 
  select(Year, Month, JDay, DM) %>% 
  group_by(JDay) %>% 
  arrange(JDay) %>% 
  summarise(MZA_San_Martin = mean(DM))

## compilacion
tabla_abril_sep <- cbind(dm_catamarca_as, dm_lr_angulos_as[2], dm_lr_miranda_as[2],
           dm_lr_tilimuqui_as[2], dm_mza_agua_amarga_as[2], dm_mza_la_consulta_as[2], 
           dm_mza_los_sauces_as[2], dm_mza_san_martin_as[2])

write.csv2(tabla_abril_sep, "temperatura/2023/tabla_abril_sep.csv")

tabla_mayo_sep <- cbind(dm_catamarca_ms, dm_lr_angulos_ms[2], dm_lr_miranda_ms[2],
                        dm_lr_tilimuqui_ms[2], dm_mza_agua_amarga_ms[2], dm_mza_la_consulta_ms[2], 
                        dm_mza_los_sauces_ms[2], dm_mza_san_martin_ms[2])

write.csv2(tabla_mayo_sep, "temperatura/2023/tabla_mayo_sep.csv")

b <- pivot_longer(tabla_mayo_sep, cols = c("CAT_La_Rinconada", "LR_Angulos", 
                                            "LR_Miranda", "LR_Tilimuqui", 
                                            "MZA_Agua_Amarga", "MZA_La_Consulta", 
                                            "MZA_Los_Sauces", "MZA_San_Martin"),
                  names_to = "Sitio", values_to = "PF")

jpeg("graficos/PF.jpeg", width = 6000, height = 3800, 
     units = "px", res = 850) 
b %>% arrange(Sitio) %>% 
  mutate(Sitio = as.factor(Sitio)) %>% 
  ggplot(aes(x = as.Date(JDay, origin="2023-01-01"), 
             y = PF)) +
  geom_smooth(aes(color = Sitio), size = 1, se = F) +
  scale_colour_discrete(
    labels = c("CAT_La_Rinconada" = "CAT - La Rinconada",
               "LR_Angulos" = "LR - Angulos",
               "LR_Miranda" = "LR - Miranda", 
               "LR_Tilimuqui" = "LR - Tilimuqui", 
               "MZA_Augua_Amarga" = "MZA - Agua Amarga", 
               "MZA_La_Consulta" = "MZA - La Consulta", 
               "MZA_Los_Sauces" = "MZA - Los Sauces", 
               "MZA_San_Martin" = "MZA - San Martín")) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  labs(color = element_blank(), x = "") +
  ylab("Porciones de frío acumuladas (2023)") +
  theme_bw() +
  theme(aspect.ratio = 3/4,
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16, face = "bold"), 
        title = element_text(size = 18, face = "bold"), 
        text = element_text(family = "serif"))
dev.off()

a %>% arrange(Sitio) %>% 
  mutate(Sitio = as.factor(Sitio)) %>% 
  ggplot(aes(x = as.Date(JDay, origin="2023-01-01"), 
             y = PF)) +
  geom_smooth(aes(color = Sitio), size = 1, se = F) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10)) +
  labs(color = element_blank(), x = "") +
  ylab("Porciones de frío acumuladas (2023)") +
  theme_bw() +
  theme(aspect.ratio = 3/4,
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16, face = "bold"), 
        title = element_text(size = 18, face = "bold"))
