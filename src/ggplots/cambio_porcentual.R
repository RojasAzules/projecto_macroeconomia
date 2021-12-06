cambio_porcentual <- function(interes){
  # Agregamos variable de interés y su cambio porcentual
  indicadores$interes <- as.numeric(indicadores[, interes])
  indicadores <- indicadores %>% mutate(diff_anual = (interes / lag(interes, 12) - 1)*100)
  # Título de la gráfica
  iVar <- which(nomenclatura$variable == interes)
  titulo <- nomenclatura[iVar, 'descripcion']
  if (!is.na(nomenclatura[iVar, 'titulo'])) {
    titulo <- nomenclatura[iVar, 'titulo']
  }
  # Ventana de meses antes y después de cada crisis que queremos graficar
  ventana <- c(-3, 24)
  # Gráfica
  g <- ggplot() + 
    geom_line(data = indicadores %>% filter(between(mes_crisis_1, ventana[1], ventana[2])),
              aes(x = mes_crisis_1, y = diff_anual, color = '2008')) + 
    geom_line(data = indicadores %>% filter(between(mes_crisis_2, ventana[1], ventana[2])),
              aes(x = mes_crisis_2, y = diff_anual, color = '2020')) + 
    labs(title = titulo,
         subtitle = 'Cambio anual', 
         caption = 'Cifras de la ENOE y ETOE (INEGI).',
         colour = 'Año') + 
    xlab('Mes de desarrollo del ajuste de mercado') + 
    ylab('Puntos porcentuales') +
    theme_light() + 
    theme(legend.position = "bottom") + 
    scale_colour_simpsons()
  # scale_color_lancet()
  
  ggsave(filename = paste(interes, '.png', sep = ''), 
         path = 'img/variacion_porcentual/',
         width = 6, height = 4)
  
  g
}
