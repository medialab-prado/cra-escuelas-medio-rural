library(sp)
library(rgeos)
library(maptools)
library(ggplot2)
library(dplyr)
library(stringr)

#https://www.educacion.gob.es/educabase/tabla.do?path=/Educacion/Alumnado/Resultadosacad/2014-2015/RegGen/l0/&file=RARG02.px&type=pcaxis&L=0
escolar <- read.csv('../data/pcaxis-26832846.csv', header = F)
escolar_limpio <- escolar[-c(1:5, 66),]
escolar_limpio <- escolar_limpio[,-11]
names(escolar_limpio) <- c('comunidad', 
                           'primer_ciclo_todos_centros', 'primer_ciclo_centros_publicos', 'primer_ciclo_centros_privados', 
                           'segundo_ciclo_todos_centros', 'segundo_ciclo_centros_publicos', 'segundo_ciclo_centros_privados',
                           'tercer_ciclo_todos_centros', 'tercer_ciclo_centros_publicos', 'tercer_ciclo_centros_privados')

escolar_limpio_ambos_sexos <- escolar_limpio[2:20,]
names(escolar_limpio_ambos_sexos) <- names(escolar_limpio_ambos_sexos) %>% 
  str_c(., '_HYM')

escolar_limpio_hombres <- escolar_limpio[22:40,]
names(escolar_limpio_hombres) <- names(escolar_limpio_hombres) %>%
  str_c(., '_HOM')

escolar_limpio_mujeres <- escolar_limpio[42:60,]
names(escolar_limpio_mujeres) <- names(escolar_limpio_mujeres) %>%
  str_c(., '_MUJ')

escolar_comunidades <- cbind(escolar_limpio_ambos_sexos, escolar_limpio_hombres, escolar_limpio_mujeres) %>%
  select(-comunidad_HOM, -comunidad_MUJ) %>%
  rename(id = comunidad_HYM) %>%
  mutate(id = id %>%
           str_to_title() %>%
           str_replace('\\s\\s*', '')) %>%
  mutate_if(is.factor, funs(. %>% as.character() %>% as.numeric()))
  
  
escolar_comunidades$id[c(3,4,5,7, 10, 13, 14, 15, 17, 18, 19)] = c('Principado de Asturias', 'Islas Baleares', 'Islas Canarias',
                                                                   spain_shape$NAME_1[5], 'Comunidad Valenciana', 'Comunidad de Madrid',
                                                                   spain_shape$NAME_1[18], 'Comunidad Foral de Navarra', 'La Rioja', 
                                                                   'Ceuta y Melilla', 'Ceuta y Melilla')

cym <- escolar_comunidades %>% 
  filter(id == 'Ceuta y Melilla') %>%
  mutate_if(is.numeric, funs(sum)) %>%
  .[1,]
  
escolar_comunidades <- escolar_comunidades %>%
  filter(id != 'Ceuta y Melilla') %>%
  rbind(cym)

readr::write_csv(escolar_comunidades, '../data/alumnado_que_promociona_primaria.csv')

#http://www.gadm.org/
spain_shape <- readRDS('../data/ESP_adm1.rds')
plot(spain_shape)
spain_fort <- fortify(spain_shape, region = 'NAME_1')

escolar_comunidades_shape <- merge(spain_fort, escolar_comunidades, by = 'id')

ggplot(escolar_comunidades_shape, aes(long, lat, group = group, fill = tercer_ciclo_todos_centros_HYM)) +
  geom_polygon(color = 'black', size = .25) +
  coord_map()
