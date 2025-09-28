### MAPA COROPLÉTICO
## ÍNDICE GeoSES DO MUNICÍPIO DE SP
## ÍNDICE SOCIOECONÔMICO PARA ESTUDOS DE SAÚDE NO BRASIL

setwd("C:\\Users\\themo\\Documents\\PROFA_LIGIA\\MAPA_COROPLETICO")
getwd()
library(devtools)
library(spdep)
library(rgeoda)
library(pacman)
library(ggplot2)
pacman::p_load(
  tidyverse,
  sf,
  colorspace,
  ggplot2,
  ggspatial,
  gridExtra,
  classInt,
  sp)

geoses <- st_read("MSP_AREAP_GEOSES.shp")
ggplot()+
  geom_sf(data = geoses, colour="black", fill="gray70")

## MAPAS COROPLÉTICOS:
# Nº DE CLASSES
# COMO FAZER A DISCRETIZAÇÃO (DIVIDIR OS INTERVALOS DE CLASSES)
# QUAL A PALETA DE CORUTILIZAR
# TUDO É INFORMAÇÃO EM COARTOGRAFIA TEMÁTICA

## FÓRMULA DE STURGES PARA ORIENTAR A QUANTIDADE DE CLASSES (K)
# k = 1+3,3*log(n) // n = Nº DE UNIDADES GEOGRÁFICAS

summary(geoses) # 310 UNIDADES DE PONDERAÇÃO IBGE

1+3.3*log10(310) ## BASE 10
1+3.3*log(310)

## QUANDO OS DADOS APRESENTAM UMA DISTRIBUIÇÃO SIMÉTRICA, A FORMA DE DISCRETIZAÇÃO AFETA POUCO O PADRÃO ESPACIAL
# NO ENTANTO, QNDO HÁ ASSIMETRIA, É PRECISO TOMAR MAIS CUIDADO PARA QUE A REPRESENTAÇÃO CARTOGRÁFICA COMUNIQUE
# O PADRÃO ESPACIAL DO FENÔMENO DE FORMA MAIS ADEQUADA

hist_geoses <- ggplot(geoses, aes(x=GeoSES))+ 
  geom_histogram(bins=12, aes(y = after_stat(density)), 
                 colour = "blue", 
                 fill="lightblue", 
                 na.rm = T)+
  geom_vline(xintercept = mean(geoses$GeoSES, na.rm = T), 
             linewidth = 1, 
             colour = "orange", 
             linetype = "dashed")+
  stat_function(fun = dnorm, 
                colour = "red", 
                linewidth = 1,
                args = list(mean = mean(geoses$GeoSES, na.rm = T),
                            sd = sd(geoses$GeoSES, na.rm = T)))
hist_geoses # VER O HISTOGRAMA EM Plots

## MAPA SEM CLASSES
mapa_sem_classes <- ggplot()+
  geom_sf(data = geoses, 
          mapping = aes(fill = GeoSES)) +
  scale_fill_continuous_diverging("Red-Green") +
  ggspatial::annotation_scale(location = "br", width_hint = 0.2, 
                              line_width = 0.5, height = unit(0.1,"cm"))
mapa_sem_classes

# PACOTE 'classInt' INSTALADO PELO MENU Tools
# DISPÕE DE FUNÇÃO PARA AVALIAR A ACURÁCIA DOS INTERVALOS PARA CADA TIPO DE DISCRETIZAÇÃO
# EXISTEM 2 ÍNDICES: índice de Jenks E O Tabular Accuracy Index (TAI)
library(classInt)

## CRIANDO INTERVALOS PARA A DISCRETIZAÇÃO DO ÍNDICE GeoSES
geoses_fixed <- classIntervals(geoses$GeoSES, n=7, style = "fixed",
                               fixedBreaks=c(-1, -0.66, -0.33, 0.0, 0.33, 0.66, 1))
geoses_fixed

geoses_sd <- classIntervals(geoses$GeoSES, n=7, style="sd")
geoses_equal <- classIntervals(geoses$GeoSES, n=7, style="equal")
geoses_quant <- classIntervals(geoses$GeoSES, n=7, style="quant")
geoses_kmeans <- classIntervals(geoses$GeoSES, n=7, style="kmeans")
geoses_pretty <- classIntervals(geoses$GeoSES, n=7, style="pretty")
geoses_fisher <- classIntervals(geoses$GeoSES, n=7, style="fisher")
geoses_jenks <- classIntervals(geoses$GeoSES, n=7, style="jenks")

## RODANDO O TESTE PARA CADA DISCRETIZAÇÃO
print(jenks.tests(geoses_fixed))
print(jenks.tests(geoses_sd))
print(jenks.tests(geoses_equal))
print(jenks.tests(geoses_quant))
print(jenks.tests(geoses_kmeans))
print(jenks.tests(geoses_pretty))
print(jenks.tests(geoses_fisher))
print(jenks.tests(geoses_jenks))

## MELHOR RESULTADO: print(jenks.tests(geoses_sd))
# classes  Goodness of fit Tabular accuracy 
# 10.0000000        0.9802571        0.8518538 

## INTERVALOS DE geoses_sd
geoses_sd$brks

## CRIANDO UMA COLUNA CHAMADA 'geoses_class' COM OS VALORES DAS QUEBRAS DE 'sd'
library(tidyverse)
geoses <- geoses |> 
  mutate(geoses_classes=cut(GeoSES, geoses_sd$brks, include.lowest=TRUE))

legend_values <- c("-1.00 a -0.79", "-0.79 a -0.58", "-0.58 a -0.37", "-0.37 a -0.15", "-0.15 a 0.05",  "0.05 a 0.27", "0.27 a 0.48", "0.48 a 0.70", "0.70 a 0.91", "0.91 a 1.00")

## ELABORANDO MAPA COM AS CLASSES:
library(colorspace) ## PACOTE 'colorspace' INSTALADO PELO MENU Tools
library(ggplot2)
mapa <- ggplot()+
  geom_sf(data = geoses, 
          aes(fill = geoses_classes)) +
  scale_fill_discrete_diverging("Red-Green", 
                                labels = legend_values) +
  labs(fill = "GeoSES") +
  ggspatial::annotation_scale(location = "br", width_hint = 0.2, 
                              line_width = 0.5, height = unit(0.1,"cm"))

mapa

## COMPARANDO OS 2 MAPAS: SEM CLASSE E COM CLASSES
comparing <- grid.arrange(mapa_sem_classes,
                          mapa,
                          ncol=2)
library(gridExtra) ## PARA COMPARAR OS MAPAS // INSTALADO PELO MENU Tools

## SALVANDO COMO .jpg
ggsave("comparing.jpg",
       comparing,
       device = "jpeg",
       width = 8,
       height = 3,
       dpi = 300)
