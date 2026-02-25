### ANÁLISES DE REGRESSÃO ESPACIAL

setwd("C:\\Users\\themo\\Documents\\PROFA_LIGIA\\REGRESSAO_ESPACIAL")
getwd()

library(pacman)
pacman::p_load(
  sp,
  dplyr,
  janitor,
  lattice,
  nlme,
  spdep,
  ggplot2,
  sf
)

shp <- st_read("C:\\Users\\themo\\Documents\\PROFA_LIGIA\\REGRESSAO_ESPACIAL\\msp_smr_geoses.shp")
# Projected CRS: SIRGAS 2000 / UTM zone 23S

library(spdep)
library(spatialreg)
library(sp)

shp <- shp %>% 
  janitor::clean_names()

## REGRESSÃO OLS - ORDINARY LEAST SQUARE ### SEMPRE COMEÇA DA MAIS SIMPLES
modelo_simples <- lm(smr~x_geo_ses, data = shp)
modelo_simples

## DOIS MODELOS GLOBAIS LARGAMENTE UTILIZADOS SÃO O *Spatial Error Model* E O *Spatial Lag Model*
# PARA DECIDIR QUAL MODELO USAR, UTILIZA-SE O Teste Multiplicador de Lagrange

# MATRIZ DE VIZINHANÇA
a <- poly2nb(shp, queen = TRUE)

# PESOS DOS VIZINHOS
b <- nb2listw(a,style = "W",zero.policy = TRUE)

# CONVERTER OBJETO sf EM sp
shp_sp <- as_Spatial(shp)

# CENTROIDES PARA MATRIZ BASEADA NA DISTÂNCIA
centroides <- coordinates(shp_sp)
plot(shp_sp)
points(centroides, col = "blue", pch = 16)

str(centroides) ## str ESTRUTURA DE UM OBJETO R ARBITRÁRIO

modelo_simples_lagrange <- lm.RStests(modelo_simples,
                                      listw = b,
                                      test = c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(modelo_simples_lagrange)
## Rao's score (a.k.a Lagrange multiplier) diagnostics for spatial dependence
# SIGNIFICATIVOS: RSerr, p-value = 0.001037 | RSlag, p-value = 0.0002464 | SARMA, p-value = 0.001003
# MENOR p-value = RSlag, p-value = 0.0002464 >> O MODELO A SER AJUSTADO

modelo_lag <- lagsarlm(smr~x_geo_ses, data = shp, listw = b)
summary(modelo_lag)


## INSTERPRETANDO OS RESULTADOS DO *Spatial Lag Model*
# O coeficiente espacial (Rho ou Lambda) indica o efeito da vizinhança espacial nas variáveis dependentes.
# Um Lambda positivo sugere autocorrelação espacial positiva, indicando que valores semelhantes estão agrupados.
# No nosso ‘modelo_lag’, o Lambda é Rho: 0.39598 (p-value: 0.00034129), portanto, significativo

# Os coeficientes associados às Variáveis Preditoras (X) funcionam de maneira semelhante a modelos de regressão tradicionais.
# Eles mostram como uma mudança em uma unidade nas variáveis preditoras afeta a variável dependente, considerando a influência espacial.
# No nosso ‘modelo_lag’, o coeficiente de x_geo_ses é -0.344472.

# Em um modelo de lag espacial, Pr(>|z|) refere-se ao valor p associado aos testes de hipóteses para os coeficientes estimados, representados pelos valores z. 
# x_geo_ses apresenta valor 5.259e-12, altamente significativo.
# Intercepto: Pr(>|z|) = 1.586e-09 também significativo

## ALGUNS PACOTES R FORNECEM A FUNÇÃO impacts, QUE CALCULA OS IMPACTOS ESPACIAIS DAS VARIÁVEIS,
# O QUE AJUDA A INTERPRETAR OS EFEITOS LIQUIDOS, LEVANDO EM CONTA A INFLUÊNCIA ESPACIAL

c_impacto <- impacts(modelo_lag,listw = b)
print(c_impacto)

## OS IMPACTOS FORNECEM UMA COMPREENSÃO ABRANGENTE DO IMAPACTO DA VARIÁVEL "x_geo_ses" NA VARIÁVEL RESPOSTA 
# Direct: Representa o efeito direto da variável x_geo_ses na variável resposta. Neste caso, para cada unidade de aumento em x_geo_ses, espera-se uma diminuição de aproximadamente 0.3581 unidades na variável de resposta, mantendo todas as outras variáveis constantes.
# Indirect: Representa o efeito indireto, considerando a influência espacial. Este impacto é mediado por outras unidades espaciais no conjunto de dados. No exemplo, um aumento de uma unidade em x_geo_ses está associado a uma diminuição de aproximadamente 0.2122 unidades na variável resposta, considerando a influência das unidades espaciais ao redor.
# Total: Representa o impacto total, incluindo tanto o efeito direto quanto o indireto. Neste caso, a soma dos efeitos direto e indireto resulta em uma diminuição total de aproximadamente 0.5703 unidades na variável de resposta para cada unidade de aumento em x_geo_ses.


## MAPAS RESIDUAIS: IDENTIFICA PADRÕES  NÃO CAPTURADOS PELO MODELO, O QUE AJUDA A AVALIAR SE HÁ AUTOCORRELAÇÃO ESPACIAL
plot(modelo_lag$residuals, col = "red", main = "Gráfico de Resíduos Espaciais")


library(sp)
library(lattice)
library(ggplot2)
library(colorspace)

## ADICIONAR OS RESÍDUOS E OS VALIRES AJUSTADOS AO BANCO DE DADOS
shp$residuos <- modelo_lag$residuals
shp$fitted <- modelo_lag$fitted.values

# GRAFICO
ggplot()+
  geom_sf(data = shp, 
          aes(fill = residuos)) +
  scale_fill_continuous_diverging(palette = "Blue-Red 3", 
                                  rev = TRUE,
                                  na.value = "grey95") ### SEM PADRÃO ESPACIAL

###########
#######
###
## MODELO DE REGRESSÃO ESPCIAL LOCAL *Geographically Weighted Regression* - GWR
library(lctools)
library(shapefiles)

## NO CONTEXTO DA GWR, UMA 'BANDA' SE REFERE À DISTÂNCIA OU Nº DE VIZINHOS USADOS PARA CADA EQUAÇÃO DE REGRESSÃO LOCAL.
# DEFINE A EXTENSÃO ESPACIAL AO REDOR DE CADA PONTO ONDE A REGRESSÃO É AJUSTADA, OU SEJA,
# É A BANDA QUE DELIMITA A ÁREA GEOGRÁFICA NA QUAL AS RELAÇÕES ENTRE VARIÁVEIS SÃO MODELADAS LOCALMENTE

# O TAMANHO DA BANDA INFLUENCIA DIRETAMENTE A SENSIBILIDADE DO MODELO ÀS VARIÁVEIS ESPACIAIS, SENDO QUE UMA BANDA MAIOR 
# SUAVIZA AS VARIAÇÕES LOCAIS, ENQUANTO UMA BANDA MENOR PERMITE CAPTURAR VARIAÇÕES MAIS PRECISAS EM ÁREAS ESPECÍFICAS.
# SUA DEFINIÇÃO GERALMENTE ENVOLVE A ESCOLHA DE UMA FUNÇÃO *kernel* E A ESPECIFICAÇÃO DA LARGURA DA BANDA

## FUNÇÃO *gwr.bw*, {lctools - Local Correlation, Spatial Inequalities, Spatial Regression and Other Tools}


### DEFININDO A LARGURA DA BANDA: "função Gaussiana (default). É preciso indicar as coordenadas dos centroides dos Distritos"

shp_centr <- st_geometry(st_centroid(shp))
class(shp_centr) # PONTO
shp_coord <- st_coordinates(shp_centr)
class(shp_coord)

banda_shp <- gwr.bw(smr~x_geo_ses, shp, shp_coord, kernel = "adaptive", algorithm = "exhaustive",
                    optim.method = "Nelder-Mead", b.min = 5, b.max = NULL, step = NULL)

glimpse(banda_shp) # PARA VER A MENOR LARGURA DE BANDA
str(banda_shp) # PARA VER A MENOR LARGURA DE BANDA
## MENOR LARGURA DE BANDA (CV)
# CV: 1.459858 | Bandwidth:  37

### MODELO *GWR*
modelo_local <- gwr(smr~x_geo_ses, shp, 37, kernel = "adaptive", shp_coord)

## INTERPRETAÇÃO: "O coeficiente médio de ‘x_geo_ses’ é de -0.56, o que significa que na média, para o aumento de 1 unidade no GeoSES, diminui 56% a taxa de mortalidade infantil em São Paulo"

summary(modelo_local$LM_LEst) # RESUMO ESTATÍSTICO DOS COEFICIENTES DO *intercepto* E DO *x_geo_ses*
summary(modelo_local$LM_LPvalues) # RESUMO ESTATÍSTICO DO *p-value* DO INTERCEPTO E DO x_geo_ses
summary(modelo_local$LM_GofFit) # RESUMO ESTATÍSTICO DO VALOR DO *y original*, *do ajustado*, *dos resíduos*, *do AIC* E DA *variância*

## ADICIONANDO TAIS VALORES AO DADO ORIGINAL
shp$gwr_ajustado <- modelo_local$LM_GofFit$LM_yfit # smr ajustado
shp$coeficie_geoses <- modelo_local$LM_LEst$x_geo_ses # coeficiente do x_geo_ses
shp$coeficie_interc <- modelo_local$LM_LEst$X.Intercept. # coeficiente do intercepto
shp$residuos_gwr <- modelo_local$LM_GofFit$LM_Res # valor dos resíduos

## MAPAS
library(ggplot2)
library(colorspace)
mapa_fitted <- ggplot()+
  geom_sf(data = shp, 
          aes(fill = gwr_ajustado)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  )

mapa_fitted

####
mapa_coef_geoses <- ggplot()+
  geom_sf(data = shp, 
          aes(fill = coeficie_geoses)) +
  scale_fill_continuous_sequential(palette = "Oslo", 
                                   na.value = "grey95")

mapa_coef_geoses

####
mapa_coef_interc <- ggplot()+
  geom_sf(data = shp, 
          aes(fill = coeficie_interc)) +
  scale_fill_continuous_sequential(palette = "Oslo",
                                   rev = FALSE,
                                   na.value = "grey95")

mapa_coef_interc

####
mapa_resid_gwr <- ggplot()+
  geom_sf(data = shp, 
          aes(fill = residuos_gwr)) +
  scale_fill_continuous_diverging(palette = "Blue-Red 3", 
                                  rev = TRUE,
                                  na.value = "grey95")

mapa_resid_gwr

## COLOCANDO AS FIGURAS LADO A LADO
library(gridExtra)
plots <- gridExtra::arrangeGrob(mapa_fitted, 
                                mapa_coef_geoses, 
                                mapa_coef_interc, 
                                mapa_resid_gwr, ncol=2)
plot(plots)

## SALVAR A IMAGEM
ggsave(file="mapa_resultados_gwr.jpg", plots,
       width = 8, 
       height = 8, 
       dpi = 300)