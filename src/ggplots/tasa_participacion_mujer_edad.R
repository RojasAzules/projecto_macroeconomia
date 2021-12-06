load('data/interim/datirri.RData')

g3 <- ggplot() + 
  geom_line(
    data = datirri %>% filter(sex == 2, gpo_edad == 1),
    aes(x = date, y = tasa_part, color = hay_menores)
  ) + 
  labs(title = 'Gráfica 3. Tasa de participación de mujeres 1',
       subtitle = '15-24 años',
       caption = 'Cifras de la ENOE y ETOE (INEGI)',
       color = 'Infantes en el hogar') + 
  xlab('Fecha') + 
  ylab('') +
  theme_light() + 
  theme(legend.position = "bottom") + 
  scale_color_lancet()

g4 <- ggplot() + 
  geom_line(
    data = datirri %>% filter(sex == 2, gpo_edad == 2),
    aes(x = date, y = tasa_part, color = hay_menores)
  ) + 
  labs(title = 'Gráfica 4. Tasa de participación de mujeres 2',
       subtitle = '25-44 años',
       caption = 'Cifras de la ENOE y ETOE (INEGI)',
       color = 'Infantes en el hogar') + 
  xlab('Fecha') + 
  ylab('') +
  theme_light() + 
  theme(legend.position = "bottom") + 
  scale_color_lancet()

g5 <- ggplot() + 
  geom_line(
    data = datirri %>% filter(sex == 2, gpo_edad == 3),
    aes(x = date, y = tasa_part, color = hay_menores)
  ) + 
  labs(title = 'Gráfica 5. Tasa de participación de mujeres 3',
       subtitle = '45-64 años',
       caption = 'Cifras de la ENOE y ETOE (INEGI)',
       color = 'Infantes en el hogar') + 
  xlab('Fecha') + 
  ylab('') +
  theme_light() + 
  theme(legend.position = "bottom") + 
  scale_color_lancet()

g6 <- ggplot() + 
  geom_line(
    data = datirri %>% filter(sex == 2, gpo_edad == 4),
    aes(x = date, y = tasa_part, color = hay_menores)
  ) + 
  labs(title = 'Gráfica 6. Tasa de participación de mujeres 4',
       subtitle = '65 años o más',
       caption = 'Cifras de la ENOE y ETOE (INEGI)',
       color = 'Infantes en el hogar') + 
  xlab('Fecha') + 
  ylab('') +
  theme_light() + 
  theme(legend.position = "bottom") + 
  scale_color_lancet()



