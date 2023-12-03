pacman::p_load(tidyverse)

temp_lr <- read.csv2("temperatura/Libro1.csv", sep = ";")
temp_qnuts <- read.csv2("temperatura/qnuts.csv", sep = ";")

temp2 <- temp_lr %>% 
  mutate(Fecha = lubridate::dmy(Fecha), 
         Angulos = as.numeric(Angulos), 
         Tilimuqui = as.numeric(Tilimuqui), 
         Miranda = as.numeric(Miranda), 
         Sañogasta = as.numeric(Sañogasta)) 

# media diaria
temp3 <- temp2 %>% 
  mutate(dia = format(as.Date(Fecha), "%m-%d")) %>% 
  arrange(dia) %>% 
  na.omit() %>% 
  group_by(dia) %>% 
  summarise(Angulos = mean(Angulos), 
            Tilimuqui = mean(Tilimuqui), 
            Sañogasta = mean(Sañogasta))
temp3
tempq2 <- temp_qnuts %>% 
  mutate(Fecha = lubridate::dmy(Fecha),
         Temp = as.numeric(Temp)) %>% 
  rename(QNUTS = Temp)

tempq3 <- tempq2 %>% 
  mutate(dia = format(as.Date(Fecha), "%m-%d")) %>% 
  na.omit() %>% 
  group_by(dia) %>% 
  summarise(QNUTS = mean(QNUTS))

diarios <- inner_join(temp3, tempq3)

diarios_long <- pivot_longer(data = diarios, cols = c(Angulos, Tilimuqui, Sañogasta, QNUTS), 
                      names_to = "Estación", values_to = "Temp")
diarios_long <- diarios_long %>% 
  mutate(dia = as.Date(dia, format = "%m-%d"))

jpeg("temperatura/tempdia.jpeg", width = 3000, height = 2000, units = "px", res = 300)
ggplot(data = diarios_long, aes(x = dia, group = Estación)) + 
  geom_line(aes(y = Temp, color = Estación), size = 1) + 
  xlab("") + 
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b") +
  ylab("Tempereratura media diaria (°C)") +
  theme_bw() +
  theme(aspect.ratio = 3/4, 
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), 
        panel.grid.minor = element_blank()) 
dev.off()

# media mensual 
temp3 <- temp2 %>% 
  mutate(Mes = format(as.Date(Fecha), "%m-%y")) %>% 
  arrange(Mes) %>% 
  na.omit() %>% 
  group_by(Mes) %>% 
  summarise(Angulos = mean(Angulos), 
            Tilimuqui = mean(Tilimuqui), 
            Sañogasta = mean(Sañogasta))


min3 <- temp2 %>% 
  mutate(Mes = format(as.Date(Fecha), "%m-%y")) %>% 
  arrange(Mes) %>% 
  na.omit() %>% 
  group_by(Mes) %>% 
  summarise(Angulos = min(Angulos), 
            Tilimuqui = min(Tilimuqui), 
            Sañogasta = min(Sañogasta))

max3 <- temp2 %>% 
  mutate(Mes = format(as.Date(Fecha), "%m-%y")) %>% 
  arrange(Mes) %>% 
  na.omit() %>% 
  group_by(Mes) %>% 
  summarise(Angulos = max(Angulos), 
            Tilimuqui = max(Tilimuqui), 
            Sañogasta = max(Sañogasta))


temp4 <- pivot_longer(data = temp3, cols = c(Angulos, Tilimuqui, Sañogasta), 
             names_to = "Estación", values_to = "Temp")

min4 <- pivot_longer(data = min3, cols = c(Angulos, Tilimuqui, Sañogasta), 
                    names_to = "Estación", values_to = "min")

max4 <- pivot_longer(data = max3, cols = c(Angulos, Tilimuqui, Sañogasta), 
                    names_to = "Estación", values_to = "max")

df_join <- temp4 %>% 
  left_join(min4) %>% 
  left_join(max4)

df_join

jpeg("LR/temp.jpeg", width = 1500, height = 1000)
temp4 %>% 
  mutate(Estación = as.factor(Estación)) %>% 
  ggplot(aes(x = Mes, 
             y = Temp, 
             group = Estación)) + 
  geom_line(aes(color = Estación),
            alpha = 0.8, 
            size = 1) +
  ylab("Temperatura (°C)") +
  xlab("") +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5,),
        aspect.ratio = 3/4) +
  theme_bw()
dev.off()

jpeg("LR/temp2.jpeg", width = 3000, height = 2000, units = "px", res = 300)
ggplot(data = df_join, aes(x = Mes, group = Estación)) + 
  geom_line(aes(y = Temp, color = Estación), size = 1) + 
  geom_ribbon(aes(y = Temp, ymin = min, ymax = max, fill = Estación), alpha = .1) +
  xlab("") + 
  ylab("Temperatura media (°C)") +
  theme(aspect.ratio = 3/4) +
  theme_bw() 
dev.off()
