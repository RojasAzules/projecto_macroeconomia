#####
source('src/microdatos.R')

# Agregamos "número de menores/mayores en el hogar" a la base del cuestionario
test <- sociodemo %>% 
  group_by(cd_a, ent, con, v_sel, n_hog, h_mud, tipo, mes_cal, ca, per) %>% 
  summarise(n_menores = sum(eda<12, na.rm = TRUE),
            n_mayores = sum(eda>75, na.rm = TRUE),
            n_reciben_cuidados = n_menores + n_mayores)

questionario_1 <- questionario_1 %>% 
  left_join(test, by = c("cd_a", "ent", "con", "v_sel", "n_hog", "h_mud", "tipo", "mes_cal", "ca", "per"))

# And join demo variables
questionario_1 <- questionario_1 %>% 
  left_join(sociodemo %>% select(cd_a, ent, con, v_sel, n_hog, h_mud, tipo, mes_cal, ca, per, n_ren, 
                                 sex, e_con, clase1, clase2, clase3, pos_ocu, rama, ing7c, dur9c,
                                 rama_est1, rama_est2, dur_est, niv_ins, eda7c))

save(questionario_1, file = 'data/interim/questionario_1.RData')

#####
# Conteo de personas en trabajo doméstico remunerado por trimestre

questionario_1 %>% 
  filter(per == 419,
         p1 == 1,
         p4 == 3, 
         p3h == 1,
         !is.na(fac)) %>% 
  pull(fac) %>% 
  sum()

questionario_1 %>% 
  filter(per == 120,
         p1 == 1,
         p4 == 3, 
         p3h == 1,
         !is.na(fac)) %>% 
  pull(fac) %>% 
  sum()

questionario_1 %>% 
  filter(per == 320,
         p1 == 1,
         p4 == 3, 
         p3h == 1,
         !is.na(fac_tri)) %>% 
  pull(fac_tri) %>% 
  sum()

questionario_1 %>% 
  filter(per == 420,
         p1 == 1,
         p4 == 3, 
         p3h == 1,
         !is.na(fac_tri)) %>% 
  pull(fac_tri) %>% 
  sum()




#####
# Por número de hijos

questionario_1 %>% 
  filter(per == 419,
         p1 == 1,
         p4 == 3, 
         p3h == 1,
         !is.na(fac)) %>% 
  group_by(n_menores) %>% 
  summarise(personas = sum(fac))

questionario_1 %>% 
  filter(per == 420,
         p1 == 1,
         p4 == 3, 
         p3h == 1,
         !is.na(fac_tri)) %>% 
  group_by(n_menores) %>% 
  summarise(personas = sum(fac_tri))


#####
# Pocenjate de mujeres en el trabajo doméstico remunerado
(questionario_1 %>% 
  filter(per == 419, !is.na(fac), 
         #-----------------------#
         sex == 2,
         p1 == 1,
         p4 == 3, 
         p3h == 1,
         #-----------------------#
  ) %>% 
  pull(fac) %>% 
  sum()
/
questionario_1 %>% 
  filter(per == 419, !is.na(fac), 
         #-----------------------#
         p1 == 1,
         p4 == 3, 
         p3h == 1,
         #-----------------------#
  ) %>% 
  pull(fac) %>% 
  sum())





