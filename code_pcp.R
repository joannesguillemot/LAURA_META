#Paper tree regeneration under monocultures

library(stringr)
library(dplyr)
library(ggplot2)

tab = read.csv2('Table.csv',sep=',')
tab = tab[1:620, 1:36]

yop = as.data.frame(table(tab$gen_mono))


gen_cat = rep('Other',620)
tab = cbind(tab, gen_cat)
tab = tab %>%
  mutate(gen_cat=replace(gen_cat, gen_mono=='Eucalyptus', 'Eucalyptus')) %>%
  mutate(gen_cat=replace(gen_cat, gen_mono=='Corymbia', 'Eucalyptus')) %>%
  mutate(gen_cat=replace(gen_cat, gen_mono=='Pinus', 'Pinus')) %>%
  mutate(gen_cat=replace(gen_cat, gen_mono=='Grevillea', 'Grevillea')) %>%
  mutate(gen_cat=replace(gen_cat, gen_mono=='Acacia', 'Acacia')) %>%
  mutate(gen_cat=replace(gen_cat, gen_mono=='Araucaria', 'Araucaria')) %>%
  mutate(gen_cat=replace(gen_cat, gen_mono=='Swietenia', 'Swietenia')) %>%
  mutate(gen_cat=replace(gen_cat, gen_mono=='Vochysia', 'Vochysia')) %>%
  mutate(gen_cat=replace(gen_cat, gen_mono=='Cupressus', 'Cupressus')) %>%
  mutate(gen_cat=replace(gen_cat, gen_mono=='Tectona', 'Tectona')) 




yop2 = as.data.frame(table(tab$gen_cat))

#convert to numeric et add 1 to 0 (log)
tab$area_rich = as.numeric(tab$area_rich)
tab$rich = as.numeric(tab$rich)
tab[tab$rich==0 & is.na(tab$rich)==F,'rich']=1


#GRPAHS
tab %>% ggplot(aes( log(as.numeric(area_rich)),  log(as.numeric(rich)), color=gen_cat)) +
  geom_point(size=2) 

tab %>% filter(gen_cat == "Eucalyptus" | gen_cat == "Pinus") %>% ggplot(aes( log(as.numeric(area_rich)),  log(as.numeric(rich)), color=gen_cat)) +
  geom_point(size=2) 

tab %>% ggplot(aes( log(as.numeric(area_rich)),  log(as.numeric(rich)),  color = dist_frag)) +
  geom_point(size=2) 

tab %>% ggplot(aes( log(as.numeric(area_rich)),  log(as.numeric(rich)), color=or_mono)) +
  geom_point(size=2) 

tab %>% ggplot(aes( log(as.numeric(area_rich)),  log(as.numeric(rich)), color=as.numeric(age_mono))) +
  geom_point(size=2) 

#TESTS
lm1 = lm(log(as.numeric(area_rich)) ~ log(as.numeric(rich)), data = tab[tab$gen_cat=="Eucalyptus" | tab$gen_cat=="Pinus",])
summary(lm1)


