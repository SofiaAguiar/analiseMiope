# Programa para treinar analise descritiva e manipulação de dados

if (!require(pacman)) install.packages('pacman')
library(pacman)
pacman::p_load(tidyverse, gt, aplore3, RcolorBrewer, hrbrthemes)

cores1 =  cores1 = c("#FC8D62","#66C2A5",  "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")

data('myopia')
View(myopia)

dados = myopia

dados$maemy <- as.character(dados$mommy)
dados$maemy[dados$maemy == 'Yes'] <- 1
dados$maemy[dados$maemy == 'No'] <- 0
dados$maemy <- as.numeric(dados$maemy)

dados$paimy <- as.character(dados$dadmy)
dados$paimy[dados$paimy == 'Yes'] <- 1
dados$paimy[dados$paimy == 'No'] <- 0
dados$paimy <- as.numeric(dados$paimy)

dados = dados %>% 
  mutate(totalmy = maemy + paimy, .keep='unused')

pie(table(dados$myopic), main = 'Proporção de míopes dentro do estudo', 
    border = 'white', col = cores1, labels = c('Não - 86,9%', 'Sim - 13,1%'))

dados %>% 
  ggplot(aes(x=readhr, group=myopic, fill=myopic))+
  geom_density() +
  theme(legend.position="none") +
  ggtitle("Horas por semana lendo para não-míopes e míopes") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 11),
    plot.title = element_text(size=16)
  ) +
  facet_wrap(~myopic)

dados %>% 
  ggplot(aes(x=tvhr, group=myopic, fill=myopic))+
  geom_density() +
  theme(legend.position="none") +
  ggtitle("Horas por semana vendo TV para não-míopes e míopes") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 10),
    plot.title = element_text(size=16)
  ) +
  facet_wrap(~myopic)

dados %>% 
  ggplot(aes(x=diopterhr, group=myopic, fill=myopic))+
  geom_density() +
  theme(legend.position="none") +
  ggtitle("Horas por semana de atividades similares a trabalho para não-míopes e míopes") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 10),
    plot.title = element_text(size=11)
  ) +
  facet_wrap(~myopic)


paismyTbl = tibble(dados) 

paismyTbl = paismyTbl %>% 
  select(myopic, totalmy)

colnames(paismyTbl)[1] = 'Filho Míope'




