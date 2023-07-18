#*++++++++++++++++++++++
#* leer y exportar tabla
#*++++++++++++++++++++++
#* 1. levanta las tablas (provienen de 3 csv y un excel con 4 hojas)
#* 2. une todo en una: identifica nombre de archivo para usar como etiqueta de sitio
#* 3. exporto en .csv
#* 
#* library----


#* Agus, voy a dejar algunos comentarios, para ello me parece mas facil usar 
#* una F al inicio de cada comentario. 
#* 
#* F: en lugar de llamar a cada librería por separado es
#* mas facil usar la libería pacman (de package manager), es interesante 
#* cuando se utilizan muchas liberías

# library(tidyverse)
# library(readxl)

pacman::p_load(tidyverse, readxl)

#* lectura y orden----
v_archivos <- list.files(path = "./datos",
                         pattern = "(xlsx$|csv$)",
                         full.names = T)

for(i in seq_along(v_archivos)){
  print(v_archivos[i])
  
  #T o F para seleccionar csv  
  ext_arch <- str_detect(string = v_archivos[i], 
                         pattern = "csv$")
  
  # lectura de archivo
  if(ext_arch == T){
    # es CSV
    
    #obtener nombre de archivo para etiqueta sitio
    c_sitio <-  v_archivos[i] %>% 
      str_split("/") %>% 
      magrittr::extract2(1) %>% 
      tail(n = 1) %>% 
      str_remove(pattern = ".csv") %>% 
      str_to_lower()
    
    #lectura de archivos csv
    tb_aux <- v_archivos[i] %>% 
      read_csv() %>% 
      mutate(sitio = c_sitio, 
             .before = 1,
             fecha = dmy(fecha))
    
  }else{
    # es EXCEL
    
    hojas_excel <- excel_sheets(v_archivos[i])
    for(j in seq_along(hojas_excel)) {
      
      #obtener nombre de archivo para etiqueta sitio
      c_sitio <- hojas_excel[j] %>% 
        str_split(pattern = "[\\s]") %>% 
        unlist() %>% 
        magrittr::extract(1) %>% 
        str_to_lower()
      
      #leo hojas excel en tablas individuales
      tb_aux_excel <- v_archivos[i] %>%
        read_xlsx(sheet = hojas_excel[j]) %>%
        mutate(sitio = c_sitio,
               .before = 1)
      
      #concateno hojas excel en una misma tabla
      if(j == 1){
        
        tb_aux <- tb_aux_excel
        
      }  else {
        
        tb_aux <- 
          tb_aux %>% bind_rows(tb_aux_excel)
        
      }
    }
  }
  
  #combinar las diferentes tablas
  exists("tb_final") -> existencia
  if(existencia == F){
    
    tb_final <- tb_aux
    
  } else{
    
    tb_final <- 
      tb_final %>% 
      bind_rows(tb_aux)
  }
}

#*guardar----

# tb_final_csv <- write_csv(tb_final, "./R_nuez/tb_resultados/tb_final_csv.csv")
# F: la mandé a la nueva ubicación "resultados" dado que por como has escrito 
# el código si va a una carpeta común con los datos crudos generará nuevas 
# variables
write.csv2(tb_final, "resultados/tb_final.csv")
