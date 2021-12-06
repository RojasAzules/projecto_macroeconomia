
male_vs_female <- function(interes){
  # Metadatos de la gráfica  
  iVar <- which(nomenclatura$variable == interes)
  titulo <- nomenclatura[iVar, 'descripcion']
  if (!is.na(nomenclatura[iVar, 'titulo'])) {
    titulo <- nomenclatura[iVar, 'titulo']
  }
  subtitulo <- 'Hombre vs mujer'
  # if (!is.na(nomenclatura[iVar, 'subtitulo'])) {
  #   subtitulo <- nomenclatura[iVar, 'subtitulo']
  # }
  etiqueta <- 'Puntos porcentuales'
  # if (!is.na(nomenclatura[iVar, 'ylabel'])) {
  #   etiqueta <- nomenclatura[iVar, 'ylabel']
  # }
  factor <- 1
  if (!is.na(nomenclatura[iVar, 'factor'])) {
    factor <- as.numeric(nomenclatura[iVar, 'factor'])
  }
  # La variable de interés se escala por un factor para que quede, por ejemplo,
  # en millones de personas en vez de personas. El factor depende de la variable.
  # Para algunas variables el factor es 1.
  hombre$interes <- as.numeric(hombre[, interes])*factor
  mujer$interes <- as.numeric(mujer[, interes])*factor
  # Ventana de meses antes y después de cada crisis que queremos graficar
  ventana <- c(-3, 24)
  # Gráfica
  g <- ggplot() + 
    geom_line(data = hombre %>% filter(between(mes_crisis_2, ventana[1], ventana[2])),
              aes(x = mes_crisis_2, y = interes, colour = '2020')) + 
    geom_line(data = mujer %>% filter(between(mes_crisis_2, ventana[1], ventana[2])),
              aes(x = mes_crisis_2, y = interes, colour = '2020')) + 
    labs(title = titulo,
         subtitle = subtitulo, 
         caption = 'Cifras de la ENOE y ETOE (INEGI).',
         colour = 'Año') + 
    xlab('Mes de desarrollo del ajuste de mercado') + 
    ylab(etiqueta) +
    theme_light() + 
    theme(legend.position = "bottom") + 
    scale_colour_simpsons()
  # scale_color_lancet()
  
  ggsave(filename = paste(interes, '.png', sep = ''), 
         path = 'img/male_vs_female/',
         width = 6, height = 4)
  
  g
}

interes <- 'v243_tasa_de_participacion'; male_vs_female(interes)

v220_horas_trabajadas_a_la_semana_por_la_poblacion_ocupada
v221_promedio
v222_mediana
v223_ingreso_pesos_por_hora_trabajada_de_la_poblacion_ocupada10
v224_promedio
v225_mediana
v243_tasa_de_participacion
v245_tasa_de_desocupacion
v246_tasa_de_ocupacion_parcial_y_desocupacion_1_topd1
v249_tasa_de_trabajo_asalariado
v250_tasa_de_subocupacion


hombre %>% filter(between(mes_crisis_2, -3, 12)) %>% select(v243_tasa_de_participacion)


