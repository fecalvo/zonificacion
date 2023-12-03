library(tidyverse)
data <- read.csv2("datos/feno_LR.csv")

data <- data %>% 
  mutate(Sitio = as.factor(Sitio), 
    Fecha = lubridate::dmy(Fecha),
    Df = as.integer(Df)) %>% 
  mutate(yf = (Cf+Cf2+Df+Df2+Ff+Ff1+Ff2+Ff3+Gf), 
         ym = (Cm+Dm2+Fm+Fm2+Gm+Hm+yf))

d1 <- data %>% 
  filter(yf > 0) %>% 
  group_by(Sitio, Fecha) %>% 
  summarise(sCf = sum(Cf), 
            sCf2 = sum(Cf2), 
            sDf = sum(Df), 
            sDf2 = sum(Df2), 
            sFf = sum (Ff), 
            sFf2 = sum(Ff2), 
            sFf3 = sum(Ff3), 
            sGf = sum(Gf)) %>% 
  mutate(syf = (sCf+sCf2+sDf+sDf2+sFf+sFf2+sFf3+sGf))

d2 <- d1 %>% 
  mutate(Cf = (sCf/syf), 
         Cf2 = (sCf2/syf), 
         Df = (sDf/syf), 
         Df2 = (sDf2/syf), 
         Ff = (sFf/syf), 
         Ff2 = (sFf2/syf), 
         Ff3 = (sFf3/syf), 
         Gf = (sGf/syf)) %>% 
  select(Sitio, Fecha, Cf, Cf2, Df, Df2, Ff, Ff2, Ff3, Gf)

d3 <- pivot_longer(d2, cols = 3:10, names_to = "estado", values_to = "cantidad")

sano <- d3 %>% 
  filter(Sitio == "Sanogasta") %>% 
  ggplot(aes(x = Fecha, 
               y = cantidad)) +
  geom_smooth(aes(color = as.factor(estado)), se = FALSE) +
  scale_x_date(limits = as.Date(c('2023-09-15','2023-11-15'))) +
  ylim(0,1) +
  ylab("Proporcion") +
  labs(title = "Sañogasta") +
  theme_bw() +
  theme(legend.position="none", 
        panel.grid.minor = element_blank()) 

angu <- d3 %>% 
  filter(Sitio == "Angulos") %>% 
  ggplot(aes(x = Fecha, 
             y = cantidad)) +
  geom_smooth(aes(color = as.factor(estado)), se = FALSE) +
  scale_x_date(limits = as.Date(c('2023-09-15','2023-11-15'))) +
  ylim(0,1) +
  ylab("Proporcion") +
  labs(title = "Angulos", col = "Estado \nFenológico") +
  theme_bw() +
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank())

tili <- d3 %>% 
  filter(Sitio == "Tilimuqui") %>% 
  ggplot(aes(x = Fecha, 
             y = cantidad)) +
  geom_smooth(aes(color = as.factor(estado)), se = FALSE) +
  scale_x_date(limits = as.Date(c('2023-09-15','2023-11-15'))) +
  ylim(0,1) +
  ylab("Proporcion") +
  labs(title = "Tilimuqui") + 
  theme_bw() +
  theme(legend.position="none", 
        panel.grid.minor = element_blank()) 
  
ggpubr::ggarrange(sano, tili, angu)


d3 %>% 
  ggplot(aes(x = Fecha, 
             y = cantidad)) +
  geom_smooth(aes(color = as.factor(estado)), se = F) +
  facet_wrap(~Sitio, ncol = 2) +
  ylim(0, 1) +
  scale_x_date(limits = as.Date(c('2023-09-15','2023-11-15'))) +
  ylab("Proporcion") +
  labs(col = "Estado \nFenológico") +
  theme_bw()

# Sañogasta

data_sano <- data %>% 
  filter(Sitio == "Sanogasta") %>% 
  mutate(Fecha = lubridate::yday(Fecha),
         Cf = as.integer(Cf), 
         Df = as.integer(Df))

d1_sano <- data_sano %>% 
  filter(yf > 0) %>% 
  group_by(Sitio, Fecha) %>% 
  summarise(sCf = sum(Cf), 
            sCf2 = sum(Cf2), 
            sDf = sum(Df), 
            sDf2 = sum(Df2), 
            sFf = sum (Ff), 
            sFf2 = sum(Ff2), 
            sFf3 = sum(Ff3), 
            sGf = sum(Gf)) %>% 
  mutate(syf = (sCf+sCf2+sDf+sDf2+sFf+sFf2+sFf3+sGf))

d2_sano <- d1_sano %>% 
  mutate(Cf = (sCf/29), 
         Cf2 = (sCf2/29), 
         Df = (sDf/29), 
         Df2 = (sDf2/29), 
         Ff = (sFf/29), 
         Ff2 = (sFf2/29), 
         Ff3 = (sFf3/29), 
         Gf = (sGf/29)) %>% 
  select(Sitio, Fecha, Cf, Cf2, Df, Df2, Ff, Ff2, Ff3, Gf)

d3_sano <- pivot_longer(d2_sano, cols = 3:10, names_to = "estado", values_to = "cantidad")
d3_sano

d3_sano %>% 
  ggplot(aes(x = Fecha, 
             y = cantidad)) +
  geom_smooth(aes(color = as.factor(estado)), se = FALSE) +
  ylim(0,1) +
  xlim(260, 320) +
  ylab("Proporcion") +
  labs(title = "Sañogasta") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) 

library(ggpmisc)

jpeg("graficos/feno_LR.jpeg", width = 4000, height = 3000, res = 800)
d2_sano %>% 
  mutate(estado_f = ((Ff+Ff2+Ff3)/(Cf+Cf2+Df+Df2))) %>% 
  ggplot(aes(x = Fecha, 
             y = estado_f)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept = 0.1, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 281.5, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0.8, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 308.2, color = "red", linetype = "dashed") +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  xlim(270, 320) +
  ylim(0, 1) + 
  labs(title = "Sañogasta | Estado F", 
       x = "Día Juliano", 
       y = "Proporción") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

dev.off()
d2_sano %>% 
  ggplot(aes(x = Fecha, 
             y = Ff3+Gf)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept = 0.705, color = "red") +
  geom_vline(xintercept = 311, color = "red") +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  ylim(0, 1) +
  xlim(280, 320) +
  labs(title = "Sañogasta | Estado Ff3 + Gf", 
       x = "Día Juliano", 
       y = "Proporción") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Tilimuqui

data_tili <- data %>% 
  filter(Sitio == "Tilimuqui") %>% 
  mutate(Fecha = lubridate::yday(Fecha),
         Cf = as.integer(Cf), 
         Df = as.integer(Df))

d1_tili <- data_tili %>% 
  filter(yf > 0) %>% 
  group_by(Sitio, Fecha) %>% 
  summarise(sCf = sum(Cf), 
            sCf2 = sum(Cf2), 
            sDf = sum(Df), 
            sDf2 = sum(Df2), 
            sFf = sum (Ff), 
            sFf2 = sum(Ff2), 
            sFf3 = sum(Ff3), 
            sGf = sum(Gf)) %>% 
  mutate(syf = (sCf+sCf2+sDf+sDf2+sFf+sFf2+sFf3+sGf))

d2_tili <- d1_tili %>% 
  mutate(Cf = (sCf/14), 
         Cf2 = (sCf2/14), 
         Df = (sDf/14), 
         Df2 = (sDf2/14), 
         Ff = (sFf/14), 
         Ff2 = (sFf2/14), 
         Ff3 = (sFf3/14), 
         Gf = (sGf/14)) %>% 
  select(Sitio, Fecha, Cf, Cf2, Df, Df2, Ff, Ff2, Ff3, Gf)

d3_tili <- pivot_longer(d2_tili, cols = 3:10, names_to = "estado", values_to = "cantidad")

d3_tili %>% 
  ggplot(aes(x = Fecha, 
             y = cantidad)) +
  geom_smooth(aes(color = as.factor(estado)), se = FALSE) +
  ylim(0,1) +
  xlim(260, 320) +
  ylab("Proporcion") +
  labs(title = "Tilimuqui") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) 

library(ggpmisc)
d2_tili
d2_sano
d2_tili %>% 
  mutate(estado_f = ((Ff+Ff2+Ff3)/(Ff+Ff2+Ff3+Cf+Cf2+Df+Df2))) %>% 
  ggplot(aes(x = Fecha, 
             y = estado_f)) +
  geom_point() 
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept = 0.1, color = "red") +
  geom_vline(xintercept = 281.5, color = "red") +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  xlim(270, 320) +
  labs(title = "Sañogasta | Estado F", 
       x = "Día Juliano", 
       y = "Proporción") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

d2_sano %>% 
  ggplot(aes(x = Fecha, 
             y = Ff3+Gf)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  geom_hline(yintercept = 0.705, color = "red") +
  geom_vline(xintercept = 311, color = "red") +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  ylim(0, 1) +
  xlim(280, 320) +
  labs(title = "Sañogasta | Estado Ff3 + Gf", 
       x = "Día Juliano", 
       y = "Proporción") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())