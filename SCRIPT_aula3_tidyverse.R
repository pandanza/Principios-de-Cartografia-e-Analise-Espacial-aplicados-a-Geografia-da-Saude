### PACOTE Tidyverse, CONTÉM OUTROS PACOTES,
# TAIS COMO: ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, forcats, ...

# PACOTE DE DADOS gapminder (nativo do R), CONTÉM AS SEGUINTES VARIÁVEIS:
# country, continent, year, lifeExp, pop, gdpPercap.

install.packages("tidyverse")
install.packages("gapminder")

library(tidyverse)
library(gapminder)

### ENCADEANDO SEQUÊNCIAS DE COMANDOS: %>% (ALT + CTRL + M)
str(gapminder) #dataframe

##$ filtgapminder##$ filter()
year_2007 <- gapminder %>% 
  filter(year == 2007)
year_2007 #OBJETO ATRIBUIDO

gapminder %>% 
  filter(country=="Brazil") #FILTRO FEITO APENAS, SEM OBJETO

gapminder %>% 
  filter(country=="Brazil", year==2007)

### ATIVIDADE 1
# FILTRE OS PAÍSES COM POP MAIOR DO QUE 100 MILHÕES DE HABITANTES EM 2007.

gapminder %>% 
  filter(pop>100000000, year==2007)

### arrange()
# ORGANIZANDO PIB PER CAPITA
gapminder %>% 
  arrange(gdpPercap) # PADRÃO É PRDEM CRESCENTE

gapminder %>% 
  arrange(desc(gdpPercap))

## ATIVIDADE 2
# FILTRE OS PAÍSES COM POP MAIOR DO QUE 100 MILHÕES DE HAB. EM 2007
# EM SEGUIDA, ORGANIZE EM ORDEM DECRESCENTE DE VALORES DE ‘lifeExp’

gapminder %>% 
  filter(pop>100000000, year==2007) %>% 
  arrange(desc(lifeExp))

### mutate()
# PERMITE ALTERAR VARIÁVEIS OU ADICIONÁ-LAS
# PARA TRABALHAR COM POP POR 1 MILHÃO DE HAB. DIVIDE-SE POP/1 MILHÃO

gapminder %>% 
  mutate(pop=pop / 1000000) ## ALTERA A COLUNA

# CRIANDO UMA NOVA COLUNA CHAMADA gdp, RESULTANDO DA MULTIPLICAÇÃO ENTRE gdpPercap E pop
gapminder %>% 
  mutate(gdp = gdpPercap*pop)

### ATIVIDADE 3:
# CONSIDERANDO O BANCO DE DADOS gapminder, 
# CRIE UMA COLUNA CHAMADA gdp COMO RESULTADO DA MULT. DE gdpPercap E pop.; 
# FILTRE PARA O ANO DE 2007 E ORGANIZE EM ORDEM DECRESCENTE DE gdp.

gapminder %>% 
  mutate(gdp = gdpPercap*pop) %>% 
  filter(year==2007) %>%
  arrange(desc(gdp))

### PACOTE ggplot2: GRÁFICOS
# VARIÁVEIS: ‘gdpPercap’ E ‘lifeExp

ggplot(year_2007, aes(x = gdpPercap, y = lifeExp))+
  geom_point()

# ESTÉTICA ADICIONAL
ggplot(year_2007, aes(x = gdpPercap, y = lifeExp, color = continent))+
  geom_point()

# DESMEMBRANDO GRÁFICOS DIVERSOS
ggplot(year_2007, aes(x = gdpPercap, y = lifeExp))+
  geom_point() +
  facet_wrap(~continent)

### ATIVIDADE 1
# ELABORE UMA SÉRIE DE GRÁFICOS SEGUNDO CONTINENTE, ADICIONANDO O TAMANHO DA POP.

ggplot(year_2007, aes(x = gdpPercap, y = lifeExp, size = pop, colour = continent))+
  geom_point() +
  facet_wrap(~continent)