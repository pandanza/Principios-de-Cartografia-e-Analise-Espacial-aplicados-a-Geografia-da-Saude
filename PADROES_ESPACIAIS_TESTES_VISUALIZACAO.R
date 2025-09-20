### PADRÕES ESPACIAIS: TESTES E VISUALIZAÇÕES

library(sf)
library(devtools)
library(spdep)
library(rgeoda)
library(sf)
library(ggplot2)
library(paletteer)
library(pacman)

pacman::p_load(
  foreign,
  readxl,
  janitor,
  lubridate,
  tidyverse,
  dplyr,
  ggplot2,
  spdep,
  sf)

setwd("C:\\Users\\themo\\Documents\\PROFA_LIGIA\\BASE_DADOS_PADRAO_ESPACIAL")
getwd()

## AJUSTE DA LINGUA E CODIFICAÇÃO:
Sys.setlocale(category = "LC_ALL", locale = "pt_BR.UTF-8")

## ABRINDO O .shp em SIRGAS 2000 / UTM zone 23S
shp <- st_read("C:\\Users\\themo\\Documents\\PROFA_LIGIA\\BASE_DADOS_PADRAO_ESPACIAL\\MortInfPadr_Distr.shp", crs=31983)

## VISUALIZAR O DADO
shp$SMR_quantil <- cut_number(shp$SMR, n=5, labs=FALSE)

ggplot(data = shp) +
  geom_sf(aes(fill = factor(SMR_quantil)), color = "gray30", size = 0.2) +
  scale_fill_brewer(
    name = "Taxa mortalidade infantil padronizada",
    palette = "Oranges"
  ) +
  theme_minimal() +
  labs(title = "Mapa de mortalidade infantil (SMR)",
       subtitle = "Classificação por quantis",
       caption = "Fontes: DataSUS, IBGE / FLG5153") +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) ## 'ABRE O MAPA NA ABA PLOTS'

## GRÁFICOS: 
# histograma:
hist(shp$SMR, main=NULL)

# boxplot:
boxplot(shp$SMR, main=NULL) # vertical
boxplot(shp$SMR, main=NULL, horizontal = TRUE) 

#### COEFICIENTE I DE MORAN. PROPOSTA METODOLÓGICA: 
# <https://mgimond.github.io/es214_support_tutorials/moranI/Mapping_and_Morans.html>

# FUNÇÃO 'poly2nb', PACOTE {spdep}, Construct neighbours list from polygon list
vizinhos <- poly2nb(shp, queen = TRUE)

# FUNÇÃO 'nb2listw', PACOTE {spdep}, Spatial weights for neighbours lists
# style="W" INDICA QUE OS PESOS SERÃO PARONIZADOS POR LINHA
pesos <- nb2listw(vizinhos, style="W", zero.policy=TRUE)

matriz_pesos <- listw2mat(pesos)
print(matriz_pesos)

### DEFASAGEM ESPACIAL 'spatial lag'
# PARA CADA ÁREA i, O lag É A MÉDIA PONDERADA DOS VALORES DA VARIÁVEL x NOS VIZNHOS DE i, 
# COM OS PESOS DEFINIDOS PELA MATRIZ QUEEN
# FUNÇÃO 'lag.listw', PACOTE: {spdep}, Spatial lag of a numeric vector
spatial_lag <- lag.listw(pesos, shp$SMR)

plot(spatial_lag ~ shp$SMR, pch=16, asp=1)
abline(lm(spatial_lag~shp$SMR), col='red')

### COMPUTAR A ESTATÍSITCA DE MORAN
I <- moran(shp$SMR, pesos, length(vizinhos), Szero(pesos))[1]
I # 0.6238826

### TESTE DE HIPÓTESE
# H0 = OS VALORES DAS TAXAS SÃO DISTRIBUÍDOS DE FORMA ALEATÓRIA, SEGUINDO UM PROCESSO COMPLETAMENTE ALEATÓRIO
# HÁ 2 MÉTODOS PARA TESTAR A HIPÓTESE: UM ANALÍTICO E OUTRO POR MEIO DE SIMULAÇÃO DO TIPO MONTE CARLO.

# MÉTODO ANALÍTICO
# VANTAGEM: RÁPIDO. PORÉM, PODE SER SENSÍVEL A POLÍGONOS DE DISTRIBUIÇÃO IRREGIULAR
# FUNÇÃO 'moran.test', PACOTE {spdep}
moran.test(shp$SMR, pesos, alternative = 'greater') # 'alternative = greater' is default 
## p-value < 2.2e-16

# MÉTODO DE SIMULAÇÃO MONTE CARLO
# ABORDAGEM MAIS SEGURA, VISTO QUE SIMULA 'N' SIMULAÇÕES DEFINIDA PELO ARGUMENTO 'nsim'
# FUNÇÃO 'moran.mc'
# TAL FUNÇÃO TEM  OUTRO PARÂMETRO CHAMADO 'alternative =' COM 3 VALORES POSSÍVEIS:
# 'greater (the default), 'less' AND 'two.sided'. 
# A ESCOLHA É DITADA PELO LADO DA DISTRIBUÇÃO QUE SE QUER COMPUTAR PARA O P-VALOR
# REGRA GERAL: 'less' SE I FOR NEGATIVO; 'geater' SE I FOR POSITIVO

MC <- moran.mc(shp$SMR,pesos,nsim = 999,alternative = 'greater')
MC # p-value = 0.001 // NÃO É POSSÍVEL REJEITAR H0. PORANTANTO, H0 VERDADEIRA
plot(MC)

## DIAGRAMA DE AUTOCORRELAÇÃO DE MORAN
moran.plot(shp$SMR,pesos)

### CALCULANDO O LISA - "Local Indicator of Spatial Association"
#PACOTES: (rgeoda) E (sf)

# MATRIZ DE PESOS TIPO QUEEN
queen_m <- queen_weights(shp)
taxa_SMR = shp['SMR'] ## COLOCA-SE A TAXA COMO UM OBJETO NO AMBIENTE GLOBAL
lisa_taxa <- local_moran(queen_m,taxa_SMR)
lisa_cores <- lisa_colors(lisa_taxa)
lisa_rotulos <- lisa_labels(lisa_taxa)
lisa_agrupado <- lisa_clusters(lisa_taxa)

# MAPA LOCAL DE MORAN
plot(st_geometry(taxa_SMR), 
     col=sapply(lisa_agrupado, function(x){return(lisa_cores[[x+1]])}), 
     border = "#333333", lwd=0.2)
title(main = "Moran Local da taxa de mortalidade infantil padronizada")
legend('bottomleft', legend = lisa_rotulos, fill = lisa_cores, border = "#eeeeee")

# BASEMAP OPEN STREET MAP
shp$moran_agrupamento <- lisa_agrupado
unique(shp$moran_agrupamento) # 0 2 1
shp$moran_agrupamento <- as.character(shp$moran_agrupamento)
unique(shp$moran_agrupamento) #"0" "2" "1" AFTER as.character

cores_lisa <- c(
  "Não significativo." = "gray",
  "Alto-Alto" = "red",
  "Baixo-Baixo" = "blue"
)

# 9 TONS DE LARANA
library(ggthemes)
oranges <- paletteer_c("ggthemes::Orange-Gold", 9) 
# ℹ The package "ggthemes" is required.
# ✖ Would you like to install it?
# 1: Yes
# 2: No
1

#FACTOR COM PERSONALIZAÇÃO DOS NÍVEIS E RÓTULOS:
shp$moran_agrupamento <- factor(shp$moran_agrupamento, 
                                levels = c("0", "1", "2", "3", "4"),
                                labels = c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", "Alto-Baixo"))

# VISUALIZAR COM O BASEMAP
mapview::mapview(shp, zcol = "SMR",
                 legend = TRUE,
                 legend.style = "fill",
                 layer.name = "Taxa de mortalidade infantil padronizada",
                 col.regions = oranges)

# MAPA DE CLUSTER NO OpenStreetMap
mapview::mapview(shp, zcol = "moran_agrupamento",
                 legend = TRUE,
                 legend.style = "fill", 
                 layer.name = "Taxa de mortalidade infantil padronizada",
                 col.regions = cores_lisa)

# PLOTAR COM {ggplot2}
(mapa_lisa <- ggplot(data = shp) +
    geom_sf(aes(fill = moran_agrupamento), color = "lightgray", size = 0.2) +
    scale_fill_manual(
      name = "Clusters de Moran",
      values = cores_lisa,
      drop = FALSE
    ) +
    theme_minimal() +
    labs(title = "Clusters espaciais de mortalidade infantil",
         subtitle = "Baseado no índice de Moran Local",
         caption = "Fonte: IBGE, DataSUS / Elaborado por: Daniela") +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ))

# EXPORTANRO .jpg
ggsave("mapa_lisa1.jpg",
       plot = mapa_lisa,
       width = 15,
       height = 8,
       units = "cm",
       dpi = 300,
       bg = "white")

# EXPORTANDO COMO .shp
st_write(shp, "taxa_lisa.shp", append = FALSE )
