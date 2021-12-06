#####
# Base de relativos contiene datos de hombres y mujeres
# Cargar datos, crear columna concepto y quedarse con columnas relevantes
relativos <- read_excel('data/raw/enoe_indicadores_estrategicos_2005_2020_mensual.xlsx', 
                          sheet = 3,
                          col_names = FALSE) %>% # Totales absolutos
  mutate(...1 = replace_na(...1, ''),
         ...2 = replace_na(...2, ''),
         ...3 = replace_na(...3, ''),
         ...4 = replace_na(...4, ''),
         ...5 = replace_na(...5, '')) %>% 
  mutate(concepto = paste(...1, ...2, ...3, ...4, ...5, sep = '')) %>% 
  relocate(concepto) %>% 
  slice(5:(n()-18)) %>% 
  select(concepto, ...6:...188)
# Conceptos que serán nombres de la tabla
nombres <- relativos %>% select(concepto)
nombres$concepto[3] <- 'Mes'
# Transponemos datos y renombramos columnas
relativos <- relativos %>% select(...6:...188) %>% t() %>% as.data.frame()
# Nos quedamos con columnas con datos
relativos <- relativos[ , nombres$concepto != '']
nombres <- nombres$concepto[nombres$concepto != '']
nombres <- paste( paste("v",as.character(seq(1, length(nombres))), sep = ''), nombres, sep = '_')
colnames(relativos) <- nombres

# Columna año
relativos <- relativos %>% 
  mutate(Anho = na.locf(v1_Indicador),
         Mes = recode(v2_Mes, 'Ene' =  1, 'Feb' =  2, 'Mar' =  3,
                              'Abr' =  4, 'May' =  5, 'Jun' =  6,
                              'Jul' =  7, 'Ago' =  8, 'Sep' =  9,
                              'Oct' = 10, 'Nov' = 11, 'Dic' = 12)) %>% 
  relocate(Anho, Mes) %>% 
  select(-v1_Indicador, -v2_Mes)

# 2020/04, 2020/05 y 2020/06
extraccion_etoe <- function(archivo, agno){
  mensual <- read_excel(archivo, 
                        sheet = 2, col_names = FALSE) %>% # Totales relativ
    mutate(...1 = replace_na(...1, ''),
           ...2 = replace_na(...2, ''),
           ...3 = replace_na(...3, ''),
           ...4 = replace_na(...4, ''),
           ...5 = replace_na(...5, '')) %>% 
    mutate(concepto = paste(...1, ...2, ...3, ...4, ...5, sep = '')) %>% 
    relocate(concepto) %>% 
    slice(5:265) #%>% 
  nombres <- mensual %>% select(concepto)
  nombres$concepto[2] <- 'Mes'
  # Transponemos datos y renombramos columnas
  mensual <- mensual %>% select(...13) %>% t() %>% as.data.frame()
  # Agregamos número de mes a partir del nombre del archivo
  nombre_archivo <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(archivo))
  mensual$V2[1] <- as.integer(substr(nombre_archivo, nchar(nombre_archivo)-1, nchar(nombre_archivo)))
  # Nos quedamos con columnas con datos
  mensual <- mensual[ , nombres$concepto != '']
  nombres <- nombres$concepto[nombres$concepto != '']
  nombres <- paste(paste("v", as.character(seq(1, length(nombres))), sep = ''), nombres, sep = '_')
  # Ajustar dos nombres para que los pies de página no hagan ruido
  nombres[9] <- substr(nombres[9], 1, 13)
  nombres[181] <- substr(nombres[181], 1, 91)
  colnames(mensual) <- nombres
  # Agregar variables de Año y Mes
  mensual <- mensual %>% 
    mutate(Anho = agno,
           Mes = as.integer(v2_Mes)) %>% 
    relocate(Anho, Mes) %>% 
    select(-v1_INDICADOR, -v2_Mes)  
}

etoe <- rbind (
  extraccion_etoe('data/raw/etoe_indicadores_estrategicos_2004.xlsx', 2020),
  extraccion_etoe('data/raw/etoe_indicadores_estrategicos_2005.xlsx', 2020),
  extraccion_etoe('data/raw/etoe_indicadores_estrategicos_2006.xlsx', 2020))
# Ajustamos nombre de v102_No especificado de etoe porque v88_No especificado de eteo
# corresponde a v102_No especificado de enoe y produce un sobremapeo
names(etoe)[names(etoe) == 'v102_No especificado'] <- 'v102_No especificado_'
# Ajustamos nombres de etoe para reflejar la nomenclatura de la enoe
nomenclatura <- read_excel('data/raw/vars_indicadores.xlsx') %>% janitor::clean_names()
for (x in seq(1, nrow(nomenclatura))) {
  if (!is.na(nomenclatura$etoe[x])) {
    names(etoe)[names(etoe) == nomenclatura$etoe[x]] <- nomenclatura$enoe[x]
  }
}

extraccion_mensual <- function(archivo, agno){
  mensual <- read_excel(archivo, 
                        sheet = 2, col_names = FALSE) %>% # Totales absolutos
    mutate(...1 = replace_na(...1, ''),
           ...2 = replace_na(...2, ''),
           ...3 = replace_na(...3, ''),
           ...4 = replace_na(...4, '')) %>% 
    mutate(concepto = paste(...1, ...2, ...3, ...4, sep = '')) %>% 
    relocate(concepto) %>% 
    slice(5:294)
  nombres <- mensual %>% select(concepto)
  nombres$concepto[1] <- 'Anho'
  nombres$concepto[2] <- 'Mes'
  # Transponemos datos y renombramos columnas
  mensual <- mensual %>% select(...17) %>% t() %>% as.data.frame()
  mensual$V1[1] <- agno
  nombre_archivo <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(archivo))
  mensual$V2[1] <- as.integer(substr(nombre_archivo, nchar(nombre_archivo)-1, nchar(nombre_archivo)))
  # Nos quedamos con columnas con datos
  mensual <- mensual[ , nombres$concepto != '']
  nombres <- nombres$concepto[nombres$concepto != '']
  nombres <- paste(paste("v", as.character(seq(1, length(nombres))), sep = ''), nombres, sep = '_')
  colnames(mensual) <- nombres
  # Agregar variables de Año y Mes
  mensual <- mensual %>% 
    mutate(Anho = as.integer(v1_Anho),
           Mes = as.integer(v2_Mes)) %>% 
    relocate(Anho, Mes) %>% 
    select(-v1_Anho, -v2_Mes)  
}

# extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2007.xlsx')
mensual <- extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2007.xlsx', 2020) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2008.xlsx', 2020)) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2009.xlsx', 2020)) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2010.xlsx', 2020)) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2011.xlsx', 2020)) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2012.xlsx', 2020)) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2101.xlsx', 2021)) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2102.xlsx', 2021)) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2103.xlsx', 2021)) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2104.xlsx', 2021)) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2105.xlsx', 2021)) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2106.xlsx', 2021)) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2107.xlsx', 2021)) %>% 
  rbind(extraccion_mensual('data/raw/enoe_n_indicadores_estrategicos_2108.xlsx', 2021))
# Nomenclatura de los relativos mensuales desde 2005
colnames(mensual) <- colnames(relativos)
# Unir todas las bases
relativos <- bind_rows(relativos %>% mutate(Anho = as.integer(Anho)), etoe,
                         mensual %>% mutate(Anho = as.integer(Anho))) %>% 
  janitor::clean_names()

# Agregamos las siguientes variables
#   1) fecha, en formato aaaa/mm/dd, último día del mes correspondiente
#   2) mes_crisis_1, meses transcurridos desde el inicio de la crisis 2008
#   3) mes_crisis_2, meses transcurridos desde el inicio de la crisis 2020
i2008 <- which(relativos$anho == 2009 & relativos$mes == 3)
i2020 <- which(relativos$anho == 2020 & relativos$mes == 3)

relativos <- relativos %>% 
  mutate(
    fecha = ceiling_date( # último día del mes
      as.Date(as.yearmon(paste(anho, mes, sep = '-'))),
      'month') - days(1),
    mes_crisis_1 = seq(1-i2008, nrow(relativos)-i2008),
    mes_crisis_2 = seq(1-i2020, nrow(relativos)-i2020)) %>% 
  relocate(fecha, mes_crisis_1, mes_crisis_2) %>% 
  select(-anho, -mes)


# Nos quedamos sólo con base de relativos estratégicos
rm(etoe, mensual, nombres, x, extraccion_etoe, extraccion_mensual, i2020, i2008)

