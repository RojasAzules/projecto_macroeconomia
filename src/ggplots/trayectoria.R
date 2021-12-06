trayectoria <- function(interes){
  # Metadatos de la gráfica  
  iVar <- which(nomenclatura$variable == interes)
  titulo <- nomenclatura[iVar, 'descripcion']
  if (!is.na(nomenclatura[iVar, 'titulo'])) {
    titulo <- nomenclatura[iVar, 'titulo']
  }
  subtitulo <- ''
  if (!is.na(nomenclatura[iVar, 'subtitulo'])) {
    subtitulo <- nomenclatura[iVar, 'subtitulo']
  }
  etiqueta <- ''
  if (!is.na(nomenclatura[iVar, 'ylabel'])) {
    etiqueta <- nomenclatura[iVar, 'ylabel']
  }
  factor <- 1
  if (!is.na(nomenclatura[iVar, 'factor'])) {
    factor <- as.numeric(nomenclatura[iVar, 'factor'])
  }
  # La variable de interés se escala por un factor para que quede, por ejemplo,
  # en millones de personas en vez de personas. El factor depende de la variable.
  # Para algunas variables el factor es 1.
  indicadores$interes <- as.numeric(indicadores[, interes])*factor
  # Ventana de meses antes y después de cada crisis que queremos graficar
  ventana <- c(-3, 24)
  # Gráfica
  g <- ggplot() + 
    geom_line(data = indicadores %>% filter(between(mes_crisis_1, ventana[1], ventana[2])),
              aes(x = mes_crisis_1, y = interes, colour = '2008')) + 
    geom_line(data = indicadores %>% filter(between(mes_crisis_2, ventana[1], ventana[2])),
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
         path = 'img/trayectoria/',
         width = 6, height = 4)
  
  g
}

# interes <- 'v6_ocupada'; trayectoria(interes)
