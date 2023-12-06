#grafico y ajuste lineal de floración
#primer etapa armo base de datos común a todos los sitios
#segunda etapa manipulo datos para obtener proporcion de estadio
#tercera etapa grafico y ajuste lineal
pacman::p_load(tidyverse, readxl, lubridate, ggpmisc)

#1: armo base de datos
v_archivos <- list.files(path = "./datos/fenologia",
                         pattern = "(xlsx$|csv$)",
                         full.names = T)



for (i in seq_along(v_archivos)) {
  print(v_archivos[i])
  
  hojas_excel <- excel_sheets(v_archivos[i]) 
  
  for (j in seq_along(hojas_excel)) {
  
  #obtener nombre de archivo para etiqueta sitio
  c_sitio <- hojas_excel[j] %>% 
    str_split(pattern = "[\\s]") %>% 
    unlist() %>% 
    magrittr::extract(1) %>% 
    str_replace_all("_", " ") %>% 
    str_to_title()
    
  #leo hojas excel en tablas individuales
  tb_aux_excel <- v_archivos[i] %>% 
    read_xlsx(sheet = hojas_excel[j]) %>% 
    mutate(sitio = c_sitio, 
           .before = 1,
           Df = as.numeric(Df), 
           brote = as.character(brote))
  
  #concateno hojas excel en una misma tabla
  if(j == 1) {
    
    tb_aux <- tb_aux_excel
    
  } else {
    
    tb_aux <- 
      tb_aux %>%  bind_rows(tb_aux_excel)
    }
  }
  
  #combinar las diferentes tablas
  exists("tb_final") -> existencia
  if(existencia == F) {
    
    tb_final <- tb_aux 
    
  } else {
    
    tb_final <- 
      tb_final %>% 
      bind_rows(tb_aux)
  }
}

#2: manipulo para obtener proporciones

data <- 
  tb_final %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(sitio = as.factor(sitio),
         fecha = lubridate::ymd(fecha),
         dia_juliano = lubridate::yday(fecha), .after = fecha) %>% 
  mutate(yf = (Cf+Cf2+Df+Df2+Ef+Ff+Ff1+Ff2+Ff3+Gf),
         yf_d = (Ef+Ff+Ff1+Ff2+Ff3+Gf),
         ym = (Amg+Bm+Cm+Dm+Dm2+Fm+Fm2+Gm+Hm), 
         ym_d = (Cm+Dm+Dm2+Fm+Fm2+Gm))

#acumular estadio final - fenología femenina
d_sum <- data %>% 
  filter(yf_d > 0) %>% 
  group_by(sitio, dia_juliano) %>% 
  summarise(sCf = sum(Cf),
            sCf2 = sum(Cf2), 
            sDf = sum(Df), 
            sDf2 = sum(Df2),
            sEf = sum(Ef),
            sFf = sum (Ff),
            sFf1 = sum(Ff1),
            sFf2 = sum(Ff2), 
            sFf3 = sum(Ff3), 
            sGf = sum(Gf),
            syf = sum(yf),
            syf_d = sum(yf_d))

yf_d_t <- d_sum %>% 
  group_by(sitio) %>% 
  filter(syf_d == max(syf_d))

d_prop <- d_sum %>% 
  filter(syf_d > 0) %>%
  group_by(sitio) %>% 
  mutate(Cf = (sCf/max(syf_d)), 
         Cf2 = (sCf2/max(syf_d)), 
         Df = (sDf/max(syf_d)), 
         Df2 = (sDf2/max(syf_d)),
         Ef = (sEf/max(syf_d)),
         Ff = (sFf/max(syf_d)),
         Ff1 = (sFf1/max(syf_d)),
         Ff2 = (sFf2/max(syf_d)), 
         Ff3 = (sFf3/max(syf_d)), 
         Gf = (sGf/max(syf_d)),
         Estado_f = Ff1+Ff2+Ff3+Gf) %>% 
  select(sitio, dia_juliano, "Cf":"Estado_f")


#3: grafico
interceptos <- d_prop %>% 
  mutate(iasd = (0.8 - coef(lm(Estado_f ~ dia_juliano))[[1]]) / coef(lm(Estado_f ~ dia_juliano))[[2]]) %>% 
  group_by(sitio) %>% 
  summarise(inter = mean(iasd))
d_prop <- left_join(d_prop, interceptos, by = "sitio")

jpeg("graficos/estaciones.jpeg", width = 6000, height = 4000, 
     units = "px", res = 600) 

d_prop %>%
  ggplot(aes(x = dia_juliano, y = Estado_f, group = sitio)) +
  geom_point() +
  geom_smooth(aes(color = sitio), method = "lm", se = FALSE) +
  stat_poly_eq(aes(label = paste("atop(", ..eq.label.., ",", ..rr.label.., ")", sep = "")),
               parse = TRUE, size = 3, label.y = 0.99) +
  geom_vline(aes(xintercept = inter), color = "red", linetype = "dashed") +
  scale_color_manual(values = c("purple", "blue", "darkgreen", "purple", "purple", "purple", "purple", "blue", "blue")) +
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed") +
  
  ylim(0, 1) +
  xlim(280, 340) +
  labs(x = "Día Juliano",
       y = "Proporción") +
  facet_wrap(~str_to_title(sitio)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")  

dev.off()
  
# floracion masculina ####
names(data)
dm_sum <- data %>% 
  filter(ym_d > 0) %>% 
  group_by(sitio, dia_juliano) %>% 
  summarise(sAmg = sum(Amg),
            sBm = sum(Bm), 
            sCm = sum(Cm), 
            sDm = sum(Dm),
            sDm2 = sum(Dm2),
            sEm = sum (Em),
            sFm = sum(Fm),
            sFm2 = sum(Fm2), 
            sGm = sum(Gm),
            sym = sum(ym),
            sym_d = sum(yf_d))

ym_d_t <- dm_sum %>% 
  group_by(sitio) %>% 
  filter(sym_d == max(sym_d))

dm_prop <- dm_sum %>% 
  filter(sym_d > 0) %>%
  group_by(sitio) %>% 
  mutate(Amg = (sAmg/max(sym_d)), 
         Bm = (sBm/max(sym_d)), 
         Cm = (sCm/max(sym_d)), 
         Dm = (sDm/max(sym_d)),
         Dm2 = (sDm2/max(sym_d)),
         Em = (sEm/max(sym_d)),
         Fm = (sFm/max(sym_d)),
         Fm2 = (sFm2/max(sym_d)), 
         Gm = (sGm/max(sym_d)), 
         Estado_f = Fm+Fm2+Gm) %>% 
  select(sitio, dia_juliano, "Cm":"Estado_f") %>% 
  summarise(jdaymin = min(dia_juliano), 
            jdaymax = max(dia_juliano))
dm_prop
dm_prop <- left_join(d_prop, dm_prop, by = "sitio")

jpeg("graficos/flormf.jpeg", width = 6000, height = 4000, 
     units = "px", res = 600) 
dm_prop %>%
  ggplot(aes(x = dia_juliano, y = Estado_f, group = sitio)) +
  geom_rect(aes(xmin = jdaymin, xmax = jdaymax, ymin = -Inf, ymax = Inf), 
            fill = "#f6edc3") +
  geom_point() +
  geom_smooth(aes(color = sitio), method = "lm", se = FALSE) +
  scale_color_manual(values = c("purple", "blue", "darkgreen", 
                                "purple", "purple", "purple", 
                                "purple", "blue", "blue")) +
  stat_poly_eq(aes(label = paste("atop(", ..eq.label.., ",", ..rr.label.., ")", sep = "")),
               parse = TRUE, size = 3, label.y = 0.1, label.x = 0.95) +
  geom_vline(aes(xintercept = inter), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = jdaymin), color = "darkgreen") +
  geom_vline(aes(xintercept = jdaymax), color = "darkgreen") +
  
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed") +
  xlim(280, 340) +
  labs(x = "Día Juliano",
       y = "Proporción") +
  facet_wrap(~str_to_title(sitio)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")  
dev.off()
