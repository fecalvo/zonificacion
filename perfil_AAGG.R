pacman::p_load(tidyverse, FactoMineR, nlme, emmeans, multcomp, multcompView, 
               magrittr, factoextra, corrplot, lubridate, magick, ggpubr, ggrepel)

datos <- read.csv2("datos/perfil.csv", sep = ";")

datos %<>% mutate(Temporada = as.factor(Temporada), 
                  Departamento = as.factor(Departamento), 
                  c18.1 = (c18.1.c+c18.1.t)) %>% 
  dplyr::select(Temporada, Departamento, mg, c16, c18, c18.1, c18.2, c18.3)

for (i in colnames(datos[c(3:length(datos))])) {
  print(cat(paste("---------------------------------------", "\n",
                  "Variable  ---> ", i, "\n", 
                  "---------------------------------------", "\n")))
  variable = datos[[i]]
  lme_cer <- lme(variable ~ factor(Temporada), random = ~ 1 | Departamento, 
                 data = datos)
  print(anova(lme_cer))
  mult <- emmeans(lme_cer, pairwise ~ Temporada)
  print(cld(mult, alpha = 0.05, Letters = letters))
}

for (i in colnames(datos[c(3:length(datos))])) {
  print(cat(paste("---------------------------------------", "\n",
                  "Variable  ---> ", i, "\n", 
                  "---------------------------------------", "\n")))
  variable = datos[[i]]
  lme_cer <- lme(variable ~ factor(Departamento), random = ~ 1 | Temporada, 
                 data = datos)
  print(anova(lme_cer))
  mult <- emmeans(lme_cer, pairwise ~ Departamento)
  print(cld(mult, alpha = 0.05, Letters = letters))
}

for (i in colnames(datos[c(3:length(datos))])) {
  cat("---------------------------------------\n",
      "Variable  ---> ", i, "\n", 
      "---------------------------------------\n")
  variable = datos[[i]]
    modelo <- lm(variable ~ factor(Temporada) * factor(Departamento), data = datos)
  print(anova(modelo))
  mult <- emmeans(modelo, pairwise ~ Temporada*Departamento)
  print(cld(mult, alpha = 0.05, Letters = letters))
}






perfil_2 <- datos %>% 
  filter(Temporada == 2)

for (i in colnames(perfil_2[c(4:8)])) {
  print(cat(paste("---------------------------------------", "\n",
                  "Temporada 2 | AAGG  ---> ", i, "\n", 
                  "---------------------------------------", "\n")))
  variable = perfil_1[,i]
  lme_cer <- lme(variable ~ factor(Tratamiento), random = ~ 1 | Bloque, 
                 data = perfil_2)
  print(anova(lme_cer))
  mult <- emmeans(lme_cer, pairwise ~ Tratamiento)
  print(cld(mult, alpha = 0.05, Letters = letters))
}

# Temporadas unicamente 

for (i in colnames(datos[c(4:8)])) {
  print(cat(paste("---------------------------------------", "\n",
                  "AAGG  ---> ", i, "\n", 
                  "---------------------------------------", "\n")))
  variable = datos[,i]
  lme_cer <- lme(variable ~ factor(Temporada), random = ~ 1 | Bloque/Tratamiento, 
                 data = datos)
  print(anova(lme_cer))
  mult <- emmeans(lme_cer, pairwise ~ Temporada)
  print(cld(mult, alpha = 0.05, Letters = letters))
}

# relacion mufa/pufa ####
mufa_pufa <- datos %>% 
  mutate(mp = (C18.1/(C18.2+C18.3)))

mufa_pufa %>% 
  dplyr::select(-Bloque) %>% 
  group_by(Temporada, Tratamiento) %>% 
  summarise(media = mean(mp), 
            de = round(sd(mp), digits = 2))

# mufa_pufa por temporada y tratamiento
mufa_pufa_T1 <- mufa_pufa %>% 
  filter(Temporada == 1) %>% 
  lme(mp ~ 1 + Tratamiento,
      random = ~ 1 | Bloque,
      data = ,.)
anova(mufa_pufa_T1)

mufa_pufa_T2 <- mufa_pufa %>% 
  filter(Temporada == 2) %>% 
  lme(mp ~ 1 + Tratamiento,
      random = ~ 1 | Bloque,
      data = ,.)
anova(mufa_pufa_T2)

#mufa_pufa por temporada 
mufa_pufa %>% 
  dplyr::select(-Bloque) %>% 
  group_by(Temporada) %>% 
  summarise(media = round(mean(mp), digits = 2), 
            de = round(sd(mp), digits = 2))

mp_temp <- mufa_pufa %>% 
  lme(mp ~ 1 + Temporada,
      random = ~ 1 | Bloque/Tratamiento,
      data = ,.)
anova(mp_temp)

# relacion UFA/USA #### 
ufa_sfa <- datos %>% 
  mutate(uu = ((C18.1+C18.2+C18.3)/(C16+C18)))

ufa_sfa %>% 
  dplyr::select(-Bloque) %>% 
  group_by(Temporada, Tratamiento) %>% 
  summarise(media = as.character(round(mean(uu), digits = 2)) , 
            de = round(sd(uu), digits = 3))

# ufa_sfa por temporada y tratamiento
ufa_sfa_T1 <- ufa_sfa %>% 
  filter(Temporada == 1) %>% 
  lme(uu ~ 1 + Tratamiento,
      random = ~ 1 | Bloque,
      data = ,.)
anova(ufa_sfa_T1)

ufa_sfa_T2 <- ufa_sfa %>% 
  filter(Temporada == 2) %>% 
  lme(uu ~ 1 + Tratamiento,
      random = ~ 1 | Bloque,
      data = ,.)
anova(ufa_sfa_T2)

#ufa_sfa por temporada 
ufa_sfa %>% 
  dplyr::select(-Bloque) %>% 
  group_by(Temporada) %>% 
  summarise(media = as.character(round(mean(uu), digits = 2)), 
            de = round(sd(uu), digits = 3))

ufa_sfa_temp <- ufa_usa %>% 
  lme(uu ~ 1 + Temporada,
      random = ~ 1 | Bloque/Tratamiento,
      data = ,.)
anova(ufa_sfa_temp)

# relacion omega-6/omega-3 ####
# omega-6/omega-3 por temporada y tratamiento 
datos %>%
  mutate(o6o3 = C18.2/C18.3) %>% 
  group_by(Temporada, Tratamiento) %>% 
  summarise(mean(o6o3), 
            sd(o6o3))

mlm_o6o3_T1 <- datos %>%
  mutate(o6o3 = C18.2/C18.3) %>% 
  filter(Temporada == 1) %>% 
  lme(o6o3 ~ 1 + Tratamiento,
      random = ~ 1 | Bloque,
      data = ,.)
anova(mlm_o6o3_T1)

mlm_o6o3_T2 <- datos %>%
  mutate(o6o3 = C18.2/C18.3) %>% 
  filter(Temporada == 2) %>% 
  lme(o6o3 ~ 1 + Tratamiento,
      random = ~ 1 | Bloque,
      data = ,.)
anova(mlm_o6o3_T2)

# omega-6/omega-3 tratamiento
datos %>%
  mutate(o6o3 = C18.2/C18.3) %>% 
  group_by(Tratamiento) %>% 
  summarise(mean(o6o3), 
            sd(o6o3))

mlm_o6o3_Trat <- datos %>%
  mutate(o6o3 = C18.2/C18.3) %>% 
  lme(o6o3 ~ 1 + Tratamiento,
      random = ~ 1 | Bloque,
      data = ,.)
anova(mlm_o6o3_Trat)

# conc aceite temporada 
datos %>%
  mutate(o6o3 = C18.2/C18.3) %>% 
  group_by(Temporada) %>% 
  summarise(mean(o6o3), 
            sd(o6o3))

mlm_o6o3_Temp <- datos %>%
  mutate(o6o3 = C18.2/C18.3) %>% 
  lme(o6o3 ~ 1 + Temporada,
      random = ~ 1 | Tratamiento,
      data = ,.)
anova(mlm_o6o3_Temp)

# Multivariado perfil AAGG ####

datos_temp <- datos %>% 
  dplyr::select(C16, C18, C18.1, C18.2, C18.3)

datos_temp$Season[datos$Temporada == 1] <- "2018-19"
datos_temp$Season[datos$Temporada == 2] <- "2019-20"
# datos_temp$Treatment[datos$Tratamiento == 50] <- "50"
# datos_temp$Treatment[datos$Tratamiento == 75] <- "75"
# datos_temp$Treatment[datos$Tratamiento == 100] <- "100"
# datos_temp$Treatment[datos$Tratamiento == 125] <- "125"

pca_temp <- PCA(datos_temp, ncp = 5, quali.sup = 6)
g.pca.temp <- plot.PCA(pca_temp, axes=c(1, 2), habillage=6, label = "none", 
                       choix = "ind", graph.type = "ggplot", 
                       invisible = "quali")
cja <- HCPC(pca_temp, nb.clust = 2, method = )

svg("graficos/pca.svg")

fviz_pca_biplot(pca_temp, habillage = 6, label = "var", 
                invisible = "quali", pointshape = 19,
                repel = 1, col.var = "black", pointsize = 2,
                addEllipses = F, legend.title = "Season", 
                arrowsize = 0.7, title = "") +
  xlim(-5,5) +
  ylim(-5,5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"), 
        aspect.ratio = 3/4)

# es fundamental dar la orden dev.off() para guardar el documento
dev.off()

plot3d(pca_temp$ind$coord[, 1:3 ])

# CLUSTERING perfil AAGG ####

nombres <- as_tibble(cja$data.clust)
datos_temp$cluster[nombres$clust == 2] <- "A"
datos_temp$cluster[nombres$clust == 1] <- "B"

jpeg("graficos/hcpc.jpeg", width = 6000, height = 4000, units = "px", res = 800)

fviz_pca_biplot(pca_temp, habillage=as.factor(datos_temp$cluster), label = F,
                invisible = "quali", col.var = "black", pointsize = -1, 
                pointshape = 19, addEllipses = T, ellipse.type = "norm", repel = TRUE, 
                ellipse.alpha = 0, legend.title = "Cluster",  
                arrowsize = 0.7, title = "", palette = c("#3e30d9", "#d9306b"))+  
  geom_point(aes(shape = factor(datos_temp$Season), 
                 fill = datos_temp$cluster, 
                 color = datos_temp$cluster)) +
  # geom_point(aes(shape = factor(datos_temp$Season)), alpha = 0.5) +
  scale_shape_manual(values = c(21, 24)) +
  guides(shape = guide_legend(title = "Season")) +
  geom_text_repel(aes(label = datos$Tratamiento), alpha = 0.5, size = 3, 
                  nudge_y = 0.2) +
  xlim(-5,5) +
  ylim(-5,5) +
  theme_bw() +
  annotate(geom="label", x=-2.2, y=-0.7, label="C16", size = 4, fill="white") +
  annotate(geom="label", x=2.9, y=1, label="C18", size = 4, fill="white") +
  annotate(geom="label", x=0.5, y=3.2, label="C18:1", size = 4, fill="white") +
  annotate(geom="label", x=3.2, y=-1.2, label="C18:2", size = 4, fill="white") +
  annotate(geom="label", x=-3.5, y=0.3, label="C18:3", size = 4, fill="white") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12, hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"), 
        aspect.ratio = 3/4)

dev.off()

fviz_eig(pca_temp, addlabels = TRUE, ylim = c(0, 50))
cont_1 <- fviz_contrib(pca_temp, choice = "var", axes = 1, top = 10)
cont_2 <- fviz_contrib(pca_temp, choice = "var", axes = 2, top = 10)
ggarrange(cont_1, cont_2, nrow = 2)
variables <- get_pca_var(pca_temp)

# En el CP1 están bien proyectados C16 (mas o menos), C18, C18.2 y C18.3
# En CP2 están bien representada solo la variable C18.1

corrplot(pca_temp$var$cos2, is.corr=FALSE, method = "color", 
         col=colorRampPalette(c("#f8766d", "white", "#00bfc4"))(200))

# correlacion perfil AAGG ####
# no lo pondré en el articulo
correlacion <- cor(datos_temp[1:5])

svg("graficos/corr.svg")

corrplot(correlacion, method = "color", tl.col="black", 
         tl.srt=45, addCoef.col = "black", win.asp = 1, type = "lower", 
         cl.cex = 1.2, tl.cex = 1.5, number.cex = 1.5, cl.ratio = 0.3,
         cl.length = 3, 
         col=colorRampPalette(c("#f8766d", "white", "#00bfc4"))(200))

dev.off()

# Relacion perfil vs. potencial ####

pot <- read.csv("tablas/potencial_medio.csv", sep = ";") 
names(pot)[1] <- "Temporada"
pot %<>% mutate(Temporada = as.factor(Temporada), 
                Tratamiento = as.factor(Tratamiento), 
                Bloque = as.factor(Bloque))

per_pot <- inner_join(datos, pot)

corr_pot_pag <- as.data.frame(cor(per_pot[4:9]))
rownames(corr_pot_pag) <- (c("C16", "C18", "C18.1", "C18.2", "C18.3", "Potencial"))
corr_pot_pag 

cor.test(per_pot$C16, per_pot$Potencial)
cor.test(per_pot$C18, per_pot$Potencial)
cor.test(per_pot$C18.1, per_pot$Potencial)
cor.test(per_pot$C18.2, per_pot$Potencial)
cor.test(per_pot$C18.3, per_pot$Potencial)

asd <- per_pot[4:9] %>% 
  mutate(Temporada = datos$Temporada)

svg("graficos/perfil_potencial.svg")

asd %>% 
  pivot_longer(c(-Potencial, -Temporada)) %>% 
  ggplot(aes(x = Potencial, y = value)) +
  geom_point(aes(color = name, shape = Temporada), size = 2) +
  theme_bw() +
  xlim(-1,-0.5) +
  ylab("Relativa abundance (%)") +
  scale_color_discrete(name = "") +
  scale_shape(name = "Season") +
  xlab("Seasonal mean midday stem water potential (MPa)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_text(size=12, hjust = 0.5),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"), 
        aspect.ratio = 3/4)

dev.off()
