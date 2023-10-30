library(tidyverse)
data <- read.csv2("datos/feno_LR.csv")
str(data)
data <- data %>% 
  mutate(Sitio = as.factor(Sitio), 
    Fecha = lubridate::dmy(Fecha),
    Df = as.integer(Df)) %>% 
  mutate(yf = (Cf+Cf2+Df+Df2+Ff+Ff1+Ff2+Ff3+Gf), 
         ym = (Cm+Dm2+Fm+Fm2+Gm+Hm+yf))

data %>% 
  mutate(Cf = Cf/yf)

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

d3 <- pivot_longer(d2, cols = 3:8, names_to = "estado", values_to = "cantidad")

sano <- d3 %>% 
  filter(Sitio == "Sanogasta") %>% 
  ggplot(aes(x = Fecha, 
               y = cantidad)) +
  geom_smooth(aes(color = as.factor(estado)), , se = FALSE) +
  scale_x_date(limits = as.Date(c('2023-09-15','2023-11-1'))) +
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
  geom_smooth(aes(color = as.factor(estado)), , se = FALSE) +
  scale_x_date(limits = as.Date(c('2023-09-15','2023-11-1'))) +
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
  scale_x_date(limits = as.Date(c('2023-09-15','2023-11-1'))) +
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
  scale_x_date(limits = as.Date(c('2023-09-15','2023-11-1'))) +
  ylab("Proporcion") +
  labs(col = "Estado \nFenológico") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())
