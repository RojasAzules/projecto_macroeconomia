source('src/data_indicadores_estrategicos.R')
source('src/ggplots/cambio_porcentual.R')
source('src/ggplots/trayectoria.Rctoria.R')

# interes <- 'v6_ocupada'; trayectoria(interes)

sapply(indicadores %>% select(starts_with('v')) %>% colnames(), 
       cambio_porcentual)

sapply(indicadores %>% select(starts_with('v')) %>% colnames(), 
       trayectoria)

