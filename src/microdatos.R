#####
source('src/librerias.R')

#####
# Funciones para importar
importarSocio <- function(ruta_archivo, bandera = 0) {
  if (bandera == 0) { # Sirve de 320 hasta 221
    salida <- read.csv(ruta_archivo) %>% 
      janitor::clean_names() %>% 
      select(cd_a, ent, con, v_sel, n_hog, h_mud, tipo, mes_cal, ca, per, n_ren, fac_tri,
             eda, sex, e_con, clase1, clase2, clase3, pos_ocu, rama, ing7c, dur9c,
             rama_est1, rama_est2, dur_est, niv_ins, eda5c)
  } else if (bandera == 1) {  # Sirve para 120 y anterior
    salida <- read.csv(ruta_archivo) %>% 
      janitor::clean_names() %>% 
      select(cd_a, ent, con, v_sel, n_hog, h_mud, per, n_ren, fac, 
             eda, sex, e_con, clase1, clase2, clase3, pos_ocu, rama, ing7c, dur9c,
             rama_est1, rama_est2, dur_est, niv_ins, eda5c) 
  } else if (bandera == 2) { # Sirve para 321
    salida <- read.csv(ruta_archivo) %>% 
      janitor::clean_names() %>% 
      select(cd_a, ent, con, v_sel, n_hog, h_mud, tipo, mes_cal, per, n_ren, fac_tri,
             eda, sex, e_con, clase1, clase2, clase3, pos_ocu, rama, ing7c, dur9c,
             rama_est1, rama_est2, dur_est, niv_ins, eda5c)
  } else {
    salida <- c()
  }
  
  if (any(colnames(salida) == 'tipo')){
    salida <- salida %>% mutate(tipo = replace_na(as.integer(tipo), 0))
  }
  if (any(colnames(salida) == 'mes_cal')){
    salida <- salida %>% mutate(mes_cal = replace_na(as.integer(mes_cal), 0))
  }
  if (any(colnames(salida) == 'ca')){
    salida <- salida %>% mutate(ca = replace_na(as.integer(ca), 0))
  }  
  if (any(colnames(salida) == 'fac')){
    salida <- salida %>% mutate(fac = replace_na(as.integer(fac), 0))
  }
  if (any(colnames(salida) == 'fac_tri')){
    salida <- salida %>% mutate(fac_tri = replace_na(as.integer(fac_tri), 0))
  }  
  
  salida %>% 
    mutate(per     = replace_na(as.integer(per), 0),
           cd_a    = replace_na(as.integer(cd_a), 0),
           ent     = replace_na(as.integer(ent), 0),
           v_sel   = replace_na(as.integer(v_sel), 0),
           n_hog   = replace_na(as.integer(n_hog), 0),
           h_mud   = replace_na(as.integer(h_mud), 0),
           con     = replace_na(con, 0))
  
}

importarQuestionario <- function(ruta_archivo, bandera = 0) {
  if (bandera == 0) {
    salida <- read.csv(ruta_archivo) %>% 
      janitor::clean_names() %>% 
      select(cd_a, ent, con, v_sel, n_hog, h_mud, tipo, mes_cal, ca, per, n_ren, 
             fac_tri, eda, 
             p1, p3h, p4)
  } else if (bandera == 1) { 
    salida <- read.csv(ruta_archivo) %>% 
      janitor::clean_names() %>% 
      select(cd_a, ent, con, v_sel, n_hog, h_mud, per, n_ren, 
             fac, eda, 
             p1, p3h, p4)
  } else if (bandera == 2) {
    salida <- read.csv(ruta_archivo) %>% 
      janitor::clean_names() %>% 
      select(cd_a, ent, con, v_sel, n_hog, h_mud, tipo, mes_cal, per, n_ren, 
             fac_tri, eda,  
             p1, p3h, p4)
  } else {
    salida <- c()
  }
  
  if (any(colnames(salida) == 'tipo')){
    salida <- salida %>% mutate(tipo = replace_na(as.integer(tipo), 0))
  }
  if (any(colnames(salida) == 'mes_cal')){
    salida <- salida %>% mutate(mes_cal = replace_na(as.integer(mes_cal), 0))
  }
  if (any(colnames(salida) == 'ca')){
    salida <- salida %>% mutate(ca = replace_na(as.integer(ca), 0))
  }  
  if (any(colnames(salida) == 'fac')){
    salida <- salida %>% mutate(fac = replace_na(as.integer(fac), 0))
  }
  if (any(colnames(salida) == 'fac_tri')){
    salida <- salida %>% mutate(fac_tri = replace_na(as.integer(fac_tri), 0))
  }  
  
  salida %>% 
    mutate(per     = replace_na(as.integer(per), 0),
           cd_a    = replace_na(as.integer(cd_a), 0),
           ent     = replace_na(as.integer(ent), 0),
           v_sel   = replace_na(as.integer(v_sel), 0),
           n_hog   = replace_na(as.integer(n_hog), 0),
           h_mud   = replace_na(as.integer(h_mud), 0),
           con     = replace_na(con, 0),
           eda     = as.integer(eda))
}

#####
# sociodemogràfico y cuestionario

# --- SOCIODEMO ---
sociodemo <- bind_rows(importarSocio('data/raw/micro/2019trim4_csv/SDEMT419.CSV', 1),
                       importarSocio('data/raw/micro/2020trim1_csv/sdemt120.csv', 1),
                       importarSocio('data/raw/micro/enoe_n_2020_trim3_csv/ENOEN_SDEMT320.csv'),
                       importarSocio('data/raw/micro/enoe_n_2020_trim4_csv/ENOEN_sdemt420.csv'),
                       importarSocio('data/raw/micro/enoe_n_2021_trim1_csv/ENOEN_SDEMT121.csv'),
                       importarSocio('data/raw/micro/enoe_n_2021_trim2_csv/ENOEN_SDEMT221.csv'),
                       importarSocio('data/raw/micro/enoe_n_2021_trim3_csv/ENOEN_SDEMT321.csv', 2))

# --- QUESTIONARIO 1 ---
questionario_1 <- bind_rows(importarQuestionario('data/raw/micro/2019trim4_csv/COE1T419.CSV', 1),
                            importarQuestionario('data/raw/micro/2020trim1_csv/coe1t120.csv', 1),
                            importarQuestionario('data/raw/micro/enoe_n_2020_trim3_csv/ENOEN_COE1T320.csv'),
                            importarQuestionario('data/raw/micro/enoe_n_2020_trim4_csv/ENOEN_coe1t420.csv'),
                            importarQuestionario('data/raw/micro/enoe_n_2021_trim1_csv/ENOEN_COE1T121.csv'),
                            importarQuestionario('data/raw/micro/enoe_n_2021_trim2_csv/ENOEN_COE1T221.csv'),
                            importarQuestionario('data/raw/micro/enoe_n_2021_trim3_csv/ENOEN_COE1T321.csv', 2))

# Identificando de manera única
# vivienda %>% distinct(cd_a, ent, con, v_sel, tipo, mes_cal, ca, per) %>% nrow()
# hogar %>% distinct(cd_a, ent, con, v_sel, n_hog, h_mud, tipo, mes_cal, ca, per) %>% nrow()
# sociodemo %>% distinct(cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, tipo, mes_cal, ca, per) %>% nrow()
# questionario_1 %>% distinct(cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, tipo, mes_cal, ca, per) %>% nrow()
# Aunque en sociodemo h_mud es redundante
# sociodemo %>% distinct(cd_a, ent, con, v_sel, n_hog, n_ren, tipo, mes_cal, ca, per) %>% nrow()

#####

# --- VIVIENDA ---
# vivienda <- bind_rows(read.csv('data/raw/micro/2019trim4_csv/VIVT419.CSV'),
#  read.csv('data/raw/micro/2020trim1_csv/vivt120.csv'),
#  read.csv('data/raw/micro/enoe_n_2020_trim3_csv/ENOEN_VIVT320.csv'),
#  read.csv('data/raw/micro/enoe_n_2020_trim4_csv/ENOEN_vivt420.csv'),
#  read.csv('data/raw/micro/enoe_n_2021_trim1_csv/ENOEN_VIVT121.csv'),
#  read.csv('data/raw/micro/enoe_n_2021_trim2_csv/ENOEN_VIVT221.csv'),
#  read.csv('data/raw/micro/enoe_n_2021_trim3_csv/ENOEN_VIVT321.csv') %>% 
# janitor::clean_names())

# --- HOGAR ---
hogar <- bind_rows(read.csv('data/raw/micro/2019trim4_csv/HOGT419.CSV'),
                   read.csv('data/raw/micro/2020trim1_csv/hogt120.csv'),
                   read.csv('data/raw/micro/enoe_n_2020_trim3_csv/ENOEN_HOGT320.csv'),
                   read.csv('data/raw/micro/enoe_n_2020_trim4_csv/ENOEN_hogt420.csv'),
                   read.csv('data/raw/micro/enoe_n_2021_trim1_csv/ENOEN_HOGT121.csv'),
                   read.csv('data/raw/micro/enoe_n_2021_trim2_csv/ENOEN_HOGT221.csv'),
                   read.csv('data/raw/micro/enoe_n_2021_trim3_csv/ENOEN_HOGT321.csv') %>% 
                     janitor::clean_names())


#####
sociodemo %>% select(per) %>% is.na() %>% sum()

# Agregamos "número de menores/mayores en el hogar" a la base del cuestionario
temp <- sociodemo %>% 
  filter(!is.na(eda)) %>% 
  group_by(per, cd_a, ent, con, v_sel, tipo, mes_cal, ca, n_hog, h_mud) %>% 
  summarise(n_menores = sum(eda<12, na.rm = TRUE),
            n_mayores = sum(eda>75, na.rm = TRUE))

sociodemo <- sociodemo %>% 
  left_join(temp, by = c("cd_a", "ent", "con", "v_sel", "n_hog", "h_mud", "tipo", "mes_cal", "ca", "per"))

# And join demo variables
questionario_1 <- questionario_1 %>% 
  left_join(sociodemo %>% select(cd_a, ent, con, v_sel, n_hog, h_mud, tipo, mes_cal, ca, per, n_ren, 
                                 sex, e_con, clase1, clase2, clase3, pos_ocu, rama, ing7c, dur9c,
                                 rama_est1, rama_est2, dur_est, niv_ins, eda5c, n_menores, n_mayores))

save(questionario_1, file = 'data/interim/questionario_1.RData')
save(hogar, file = 'data/interim/hogar.RData')


q <- questionario_1 %>% distinct(cd_a, ent, con, v_sel, n_hog, h_mud, tipo, mes_cal, ca, per)
s <- sociodemo %>% distinct(cd_a, ent, con, v_sel, n_hog, h_mud, tipo, mes_cal, ca, per)
khe_1 <- setdiff(q, s)
khe_2 <- setdiff(s, q)

