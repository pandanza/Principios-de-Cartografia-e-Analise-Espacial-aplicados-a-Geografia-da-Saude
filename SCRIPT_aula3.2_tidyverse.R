### CONTINUAÇÃO AULA 3
# GRÁFICOS
# PARA FAZER OS GRÁFICOS, É PRECISO AGRUPÁ-LOS.

library(tidyverse)
library(gapminder)

gapminder

### GRÁFICO DE LINHAS, EXPECTATIVA DE VIDA AO LONGO DOS ANOS POR CONTINENTE:
year_continent <- gapminder |> 
  group_by(year, continent) |> 
  summarise(TOTALPOP=sum(pop),
            MEDIAEXPECVIDA=mean(lifeExp))

ggplot(year_continent, aes(x = year, y = MEDIAEXPECVIDA, color = continent)) +
  geom_line() +
  expand_limits(y = 0)

### GRÁFICO DE BARRAS, EXPECTATIVA DE VIDA POR CONTINENTE, ANO 2007
by_continente <- gapminder |> 
  filter(year == 2007) |> 
  group_by(continent) |> 
  summarise(MEDIAEXPECVIDA = mean(lifeExp))

ggplot(by_continente, aes(x = continent, y = MEDIAEXPECVIDA))+
  geom_col()

### HISTOGRAMA, DISTRIBUIÇÃO DA FREQUÊNCIA DE UMA VARIÁVEL:
# EXPECTATIVA DE VIDA
ggplot(gapminder, aes(x=lifeExp))+
  geom_histogram()

# GDP PER CAPITA
ggplot(gapminder, aes(x=gdpPercap))+
  geom_histogram()

### BOXPLOT
# EXPECTATIVA DE VIDA
ggplot(gapminder, aes(x = continent, y = lifeExp))+
  geom_boxplot()

# GDP PER CAPITA
ggplot(gapminder, aes(x = continent, y = gdpPercap))+
  geom_boxplot()
