#*++++++++++++++++++++++
#* estadistica de variables de fruto
#*++++++++++++++++++++++
#* 1. carga base de datos 
#* 2. funcion para analisis estadistico (normalidad, mlm, medias, ajuste)
#* 3. explorar resultados de mlm
#* 4. correlaciones entre variables

#librerias
pacman::p_load(tidyverse, readxl, lmerTest, lme4, 
               emmeans, multcomp, fitdistrplus, GGally)

tb_final <- read.csv2("resultados/tb_final.csv")

#creo tabla con muestreo de cosecha
tb_cosecha <- tb_final %>% 
  mutate(sem_fr = (ps_sem/ps_fr)*100) %>% 
  group_by(sitio) %>% 
  filter(muestreo == max(muestreo))

#tabla alternativa si hay que eliminar datos
tb_cosecha_c <- tb_cosecha %>% 
  filter(sitio %in% c("los_sauces", "chacon", "la_consulta", 
                      "agua_amarga", "trinidad"))

#tb_cosecha[-c(654),] eliminar para d1
#tb_cosecha[-c(351, 837, 906, 948),] eliminar para peso seco semilla

#creo funcion para analisis estadistico de variable respuesta
#* 2.1 chequea normalidad de variable
#* 2.2 modelo lineal mixto, sitio efecto fijo y planta efecto aleatorio
#* 2.3 comparacion de medias
#* 2.4 chequea ajuste de modelo

mlm <- function(vr, tb){
  
  par(mfrow=c(1,3))
  
  #testeo normalidad de variable
  vr_1 <- tb %>% pull(vr) %>% unique() %>% na.omit() %>% as.vector()
  fit.norm=fitdist(vr_1,"norm")
  qqcomp(list(fit.norm), main = "Variable normal?")
  
  #modelo lineal mixto, con planta como EA y sitio como EF
  m1 <- lmer(paste(vr, " ~ sitio - 1 + (1|id_planta)"), data = tb, na.action = na.omit)
  m1.2 <- lmer(paste(vr, " ~  1 + (1|id_planta)"), data = tb, na.action = na.omit)
  res1 <- (anova(m1, m1.2))
  res2 <- (summary(m1))
  
  #comparacion de medias
  res3 <- cld(emmeans(m1, "sitio", decreasing = TRUE), 
              details=FALSE, sort=TRUE, Letters=c("ABCDE"))
  
  #análisis de ajuste del modelo
  resid=resid(m1, type="pearson")
  plot(fitted(m1), resid, abline(h=0), main = "Varianza homogénea?")
  qqnorm(resid, main = "Residuos normales?")
  qqline(resid, col="red3", lwd=2)
  
  #resultados de la funcion
  resultados <- list(anova = res1, summary_modelo = res2,
                     comp_medias = res3)
  return(resultados)
  
  par(mfrow=c(1,1))
  
}

#explorar resultados de funcion
resultado <- mlm("long", tb_cosecha)
resultado$summary_modelo$coefficients[,1:2]
resultado$comp_medias
resultado$anova

#explorar datos a eliminar
which(tb_cosecha$d1 < 10)

#correlaciones entre variables
tb_corr <- tb_cosecha[-654,c(6:7, 9, 11, 14:15)]
ggpairs(tb_corr)

#fin------------------
