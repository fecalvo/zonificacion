##*++++++++++++++++++++++
#* estadistica descriptiva
#*++++++++++++++++++++++
#* 1. pivotear tabla y guardarla
#* 2. estadistica descriptiva y exportar tabla
#* 3. funcion para gaficos descriptivos
#* 3.1. puntos y lineas para evolucion en el tiempo
#* 3.2. barras para valores de cosecha
#* 
#* library----
pacman::p_load(tidyverse, readxl, ggpubr)

#llamo a script de lectura y limpieza de tabla
source("lectura_limpieza.R")

colnames(tb_final) 

tb_final <- tb_final %>% 
  select(sitio, id_planta:d1, long:p_36h_sem)

#pivoteo tabla para tener una columna con id_variable y otra con el valor 
tb_pivot <- tb_final %>% 
            pivot_longer(cols = c(6:14),
                         names_to = "id_variable",
                         values_to = "valor")

#estadistica descriptiva
#genero talba con medidas resumen (media, desvio, n)
tb_resumen <- tb_pivot %>% 
  group_by(sitio, muestreo, fecha, id_variable) %>% 
  summarise(media = mean(valor, na.rm = T),
            desvio = sd(valor, na.rm = T),
            conteo = n()) %>% 
  arrange(id_variable, muestreo) %>% 
  ungroup()

#guardo tabla de medidas resumen
# F: por si quieres conservar la tabla la mandé a la nueva ubicación
# llamada "resultados"
# tb_resumen_descriptiva <- 
write_csv(tb_resumen, "./resultados/tb_resumen_descriptiva.csv")

#graficos exploratorios
#defino mis preferencias gráficas
my_theme <- theme_bw() + 
  theme(panel.grid = element_blank())

v_colores <- c("cyan2", "cadetblue4", "dodgerblue", "dodgerblue4",
               "green3", "palegreen2", "brown2") 

#creo funcion para graficar evolucion (puntos y lineas) en el tiempo
#permite ajustar escala eje y a limites de variable

graf_evolucion <- function(tb, x, y, sd, fill, variable){
  
  #selecciono variable a graficar
  tb <- tb %>% 
    filter(id_variable == variable)
  
  #creo vector con limite max y min de y para escala grafico
  y_1 <- dplyr::enquo(y)
  sd_y <- dplyr::enquo(sd)
  v_escala <- 
    tb %>% 
    summarise(
      min_y = min(!!y_1-!!sd_y, na.rm = T),
      max_y = max(!!y_1+!!sd_y, na.rm = T)) %>% 
    unlist()
  
  #grafico
  ggplot(tb, aes(x = {{x}}, y = {{y}}, colour = {{fill}})) +
    geom_point(size=3) +
    geom_line() +
    geom_errorbar(aes(ymin = {{y}}-{{sd}}, ymax = {{y}}+{{sd}}), width = 0.2) +
    scale_y_continuous(breaks = round(seq
                                      (from=min(v_escala), 
                                        to=max(v_escala),
                                        length.out = 5), 1)) +
    scale_color_manual(values=v_colores) +
    ggtitle({{str_to_title(variable)}}) +
    my_theme
}

#ejemplo de grafico evolucion
g1 <- graf_evolucion(tb_resumen, fecha, media, desvio, sitio, "long")  
g2 <- graf_evolucion(tb_resumen, fecha, media, desvio, sitio, "ps_sem")

# ggarrange me parece algo mejor para unir gráficos que el propio ggplot
ggpubr::ggarrange(g1,g2)

#para explorar otras opciones de variables a graficar reemplar último argumento
#de la función (entre comillas)
tb_resumen %>% pull(id_variable) %>% unique()

#graficos de barras para muestreo en cosecha

graf_cosecha <- function(tb, y, sd, variable){
  
  #selecciono variable a graficar del ultimo muestreo de cada sitio (cosecha)
  tb_cosecha <- tb %>% 
    group_by(sitio) %>% 
    filter(muestreo == max(muestreo) & id_variable == variable)
  
  #grafico de barras
  ggplot(tb_cosecha, aes(x = sitio, y = {{y}}, fill = sitio)) + 
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = {{y}}-{{sd}}, ymax = {{y}}+{{sd}}), width = 0.2) +
    scale_fill_manual(values = c("agua_amarga" = "cyan2", 
                                 "chacon" =  "cadetblue4",
                                 "la_consulta" = "dodgerblue",
                                 "los_sauces" = "dodgerblue4",
                                 "santa_florentina" = "green3", 
                                 "tilimuqui" = "palegreen2", 
                                 "trinidad" = "brown2")) +
    ggtitle({{str_to_title(variable)}}) +
    my_theme + 
    theme(legend.position = "none")
}

#ejemplo grafico cosecha
#idem funcion anterior para graficar otras variables
graf_cosecha(tb_resumen, media, desvio, "ps_sem")  

#fin---------------------

