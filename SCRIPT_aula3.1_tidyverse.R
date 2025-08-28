### CONTINUAÇÃO AULA 3
# RESUMINDO DADOS COM summarize 


library(tidyverse)
library(gapminder)

year_2007 <- gapminder %>% 
  filter(year == 2007)
year_2007 #OBJETO ATRIBUIDO

str(gapminder)
summary(gapminder) ## ESTATÍSTICAS BÁSICAS DO OBJETO, BÁSICO DO R

# FUNÇÃO summarize(), dplyr/tidyverse, CALCULA RESUMOS PERSONALIZADOS DE COLUNAS

summarise(gapminder) ## NÃO RETORNO VALORES PQ NÃO AGRUPEI A VARIÁVEL DE INTERESSE

## SE EU QUERO A MÉDIA DA EXPECTATIVA DE VIDA DA BASE DE DADOS COMPLETA:
gapminder %>% 
  summarise(media_expcVida = mean(lifeExp))

## SE EU QUERO A MÉDIA DA EXPECTATIVA DE VIDA PARA O ANO DE 2007
gapminder %>% 
  filter(year==2007) %>% 
  summarise(media_expcVida = mean(lifeExp))

year_2007 %>% 
  summarise(media_expcVida = mean(lifeExp))

## ADICIONANDO O TOTAL DA POP
year_2007 %>% 
  summarise(media_expcVida = mean(lifeExp),
            totaPOP = sum(pop))

## CRIANDO UMA ÚNICA LINHA PARA CADA CONTINENTE:
year_2007 %>% 
  group_by(continent) %>% 
  summarise(media_expcVIDA = mean(lifeExp)) ## FAZ A MÉDIA POR CONTINENTE

## ADICIONANDO O TOTAL DA POP POR CONTINENTE
year_2007 %>% 
  group_by(continent) %>% 
  summarise(media_expcVIDA = mean(lifeExp),
            totalPOP = sum(pop))

### ATIVIDADE 1
# RESUMA OS DADOS CRIANDO UMA COLUNA CHAMADA totalPop SOMANDO A POPULAÇÃO E CRIANDO OUTRA COLUNA CHAMADA
# meanlifeExp COM A MÉDIA DE lifeExp DOS DADOS DO PACOTE gapminder AGRUPANDO POR ANO E CONTINENTE

gapminder ## VER O PACOTE ORIGINAL

gapminder %>% 
  group_by(year,continent) %>% 
  summarise(
    totalPop=sum(pop),
    meanlifeExp=mean(lifeExp))

### CRIANDO UM OBJETO CHAMADO by_year COM DADOS AGRUPADOS POR ANO,
# POP SOMADA E MÉDIA DA EXPCT. VIDA

by_year <- gapminder %>% 
  group_by(year) %>% 
  summarise(somaPOP=sum(pop),
            mediaEXPCVIDA=mean(lifeExp))
by_year

