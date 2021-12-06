#####
source('src/librerias.R')
# source('src/data_indicadores_estrategicos.R')
load('data/interim/indicadores.RData')



#####
# total
t1_0 <- indicadores %>% filter(mes_crisis_1 == 0) %>% pull(v243_tasa_de_participacion) %>% as.double()
t2_0 <- indicadores %>% filter(mes_crisis_2 == 0) %>% pull(v243_tasa_de_participacion) %>% as.double()

indicadores <- indicadores %>% 
  mutate(v243_tasa_de_participacion = as.double(v243_tasa_de_participacion)) %>% 
  mutate(cambio_en_tasa_1 = (v243_tasa_de_participacion / t1_0 - 1)*100,
         cambio_en_tasa_2 = (v243_tasa_de_participacion / t2_0 - 1)*100)

# hombre
t1_0 <- hombre %>% filter(mes_crisis_1 == 0) %>% pull(v243_tasa_de_participacion) %>% as.double()
t2_0 <- hombre %>% filter(mes_crisis_2 == 0) %>% pull(v243_tasa_de_participacion) %>% as.double()

hombre <- hombre %>% 
  mutate(v243_tasa_de_participacion = as.double(v243_tasa_de_participacion)) %>% 
  mutate(cambio_en_tasa_1 = (v243_tasa_de_participacion / t1_0 - 1)*100,
         cambio_en_tasa_2 = (v243_tasa_de_participacion / t2_0 - 1)*100)

# mujer
t1_0 <- mujer %>% filter(mes_crisis_1 == 0) %>% pull(v243_tasa_de_participacion) %>% as.double()
t2_0 <- mujer %>% filter(mes_crisis_2 == 0) %>% pull(v243_tasa_de_participacion) %>% as.double()

mujer <- mujer %>% 
  mutate(v243_tasa_de_participacion = as.double(v243_tasa_de_participacion)) %>% 
  mutate(cambio_en_tasa_1 = (v243_tasa_de_participacion / t1_0 - 1)*100,
         cambio_en_tasa_2 = (v243_tasa_de_participacion / t2_0 - 1)*100)


#####

## Grafiquirri
# Ventana de meses antes y después de cada crisis que queremos graficar
ventana <- c(0, 18)
# Gráfica
g1 <- ggplot() + 
  geom_line(data = hombre %>% filter(between(mes_crisis_1, ventana[1], ventana[2])),
            aes(x = mes_crisis_1, y = cambio_en_tasa_1, colour = 'Hombre'),
            linetype = 2, size = 1.25) + 
  geom_line(data = mujer %>% filter(between(mes_crisis_1, ventana[1], ventana[2])),
            aes(x = mes_crisis_1, y = cambio_en_tasa_1, colour = 'Mujer'),
            linetype = 3, size = 1.25) + 
  labs(title = 'Gráfica 1. Cambio porcentual en tasa de participación',
       subtitle = '2009-2010', 
       caption = 'Cifras de la ENOE y ETOE (INEGI)',
       colour = '') + 
  xlab('Mes de desarrollo del ajuste de mercado') + 
  ylab('Puntos porcentuales') +
  theme_light() + 
  theme(legend.position = "bottom") + 
  scale_color_lancet()

g2 <- ggplot() + 
  geom_line(data = hombre %>% filter(between(mes_crisis_2, ventana[1], ventana[2])),
            aes(x = mes_crisis_2, y = cambio_en_tasa_2, colour = 'Hombre'),
            linetype = 2, size = 1.25) + 
  geom_line(data = mujer %>% filter(between(mes_crisis_2, ventana[1], ventana[2])),
            aes(x = mes_crisis_2, y = cambio_en_tasa_2, colour = 'Mujer'),
            linetype = 3, size = 1.25) + 
  labs(title = 'Gráfica 2. Cambio porcentual en tasa de participación',
       subtitle = '2020-2021',
       caption = 'Cifras de la ENOE y ETOE (INEGI)',
       colour = '') + 
  xlab('Mes de desarrollo del ajuste de mercado') + 
  ylab('Puntos porcentuales') +
  theme_light() + 
  theme(legend.position = "bottom") + 
  scale_color_lancet()


#####


# indicadores <- indicadores %>% 
#   mutate(emp_2_pop_ratio = as.numeric(v6_ocupada) / as.numeric(v4_2_poblacion_de_15_anos_y_mas),
#          diff_e2p_ratio_1 = emp_2_pop_ratio / (indicadores %>% filter() %>% select()) - 1,
#   )
# hombre <- hombre %>% 
#   mutate(emp_2_pop_ratio = as.numeric(v6_ocupada) / as.numeric(v4_2_poblacion_de_15_anos_y_mas))
# mujer <- mujer %>% 
#   mutate(emp_2_pop_ratio = as.numeric(v6_ocupada) / as.numeric(v4_2_poblacion_de_15_anos_y_mas))
# 
# indicadores %>% filter(mes_crisis_2 >= 0) %>% select(emp_2_pop_ratio)
# hombre %>% filter(mes_crisis_2 >= 0) %>% select(emp_2_pop_ratio)
# mujer %>% filter(mes_crisis_2 >= 0) %>% select(emp_2_pop_ratio)
# 
# indicadores %>% 
#   filter(fecha > '2020-01-01') %>% 
#   select(fecha, v6_ocupada)
# 
# hombre %>% 
#   filter(fecha > '2020-01-01') %>% 
#   select(fecha, v6_ocupada)
# 
# mujer %>% 
#   filter(fecha > '2020-01-01') %>% 
#   select(fecha, v6_ocupada)
# 
# 
# 
