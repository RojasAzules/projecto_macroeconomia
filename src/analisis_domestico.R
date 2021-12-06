#####
source('src/librerias.R')
load('data/interim/hogar.RData')



#####
hogar <- hogar %>% 
  mutate(fac = replace_na(fac,0) + replace_na(fac_tri,0))

(
  hogar %>% 
    filter(per == 419,
           p4_1 == 4) %>% 
    pull(fac) %>% 
    sum() /
  hogar %>% 
    filter(per == 419,
           p4_1 != 4) %>% 
    pull(fac) %>% 
    sum()
)

(
  hogar %>% 
    filter(per == 420,
           p4_1 == 4) %>% 
    pull(fac) %>% 
    sum() /
    hogar %>% 
    filter(per == 420,
           p4_1 != 4) %>% 
    pull(fac) %>% 
    sum()
)

(
  hogar %>% 
    filter(per == 321,
           p4_1 == 4) %>% 
    pull(fac) %>% 
    sum() /
    hogar %>% 
    filter(per == 321,
           p4_1 != 4) %>% 
    pull(fac) %>% 
    sum()
)
