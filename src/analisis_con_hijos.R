#####
source('src/librerias.R')
load('data/interim/questionario_1.RData')

#####
# Dejamos los factores de expansión en la variable fac, a partir de la ENOE(N)
# se usa la variable fac_tri que supersede a fac

questionario_1 <- questionario_1 %>% 
  mutate(fac = replace_na(fac,0) + replace_na(fac_tri,0),
         hay_menores = ifelse(n_menores > 0, 'Sí', 'No')) %>% 
  mutate(hay_menores = as.factor(hay_menores))



nPeriodos <- length(unique(questionario_1$per))
nGruposDeEdad <- length(unique(questionario_1$eda5c))

datirri <- data.frame(
  per         = rep(unique(questionario_1$per), each = 2*nGruposDeEdad*2),
  sex         = rep(rep(1:2, nPeriodos), each = nGruposDeEdad*2),
  gpo_edad    = rep(rep(sort(unique(questionario_1$eda5c)), nPeriodos*2), each = 2),
  hay_menores = rep(as.factor(c('No', 'Sí')), nPeriodos*2*nGruposDeEdad),
  ponderacion = 0
)

datirri$tasa_part <- mapply(function (p, s, e, c) {
  (questionario_1 %>% 
    filter(per == p,
           sex == s,
           eda5c == e,
           hay_menores == c,
           clase1 == 1) %>% 
     pull(fac) %>% sum() 
   / 
  questionario_1 %>% 
    filter(per == p,
           sex == s,
           eda5c == e,
           hay_menores == c) %>% 
     pull(fac) %>% sum())*100     }, 
  datirri$per, datirri$sex, datirri$gpo_edad, datirri$hay_menores)

fecha <- data.frame(
  per = c(419, 120, 220, 320, 420, 121, 221, 321, 421),
  date = c(ymd('20191231'), ymd('20200331'), ymd('20200630'), ymd('20200930'), ymd('20201231'), 
           ymd('20210331'), ymd('20210630'), ymd('20210930'), ymd('20211231')))

datirri <- left_join(datirri, fecha)

save(datirri, file = 'data/interim/datirri.RData')
