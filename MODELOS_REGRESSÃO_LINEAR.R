# ANÁLISE DE REGRESSÃO LINEAR BIVARIADA \\ CONT. ANÁLISE EXPLORATÓRIA DE DADOS
# VARIÁVEL DEPENDENTE Y: TAXA DE MORTALIDADE INFANTIL PADRONIZADA: 'smr'
# VARIÁVEL INDEPENDENTE X: CONTEXTO SOCIOECONÔMICO: 'GeoSES'

setwd("C:\\Users\\themo\\Documents\\PROFA_LIGIA\\MODELOS_REGRESSAO_DZ")
getwd()

library(pacman)
pacman::p_load(
  sp,
  dplyr,
  janitor,
  nlme,
  spdep,
  ggplot2,
  sf
)


shp <- st_read("C:\\Users\\themo\\Documents\\PROFA_LIGIA\\MODELOS_REGRESSAO_DZ\\msp_smr_geoses.shp")

shp <- shp |> 
  janitor::clean_names()
glimpse(shp)

# MODELO DO TIPO OLS: "Ordinary Least Squares" - MÉTODO: Mínimos Quadrados Ordinários
modelo_simples <- lm(smr~x_geo_ses, shp)
modelo_simples
summary(modelo_simples)
str(modelo_simples)
shp$residuos <- modelo_simples$residuals
glimpse(shp)
shp$fitted <- modelo_simples$fitted.values # VALORES AJUSTADOS DO MODELO, SEM OS ERROS
glimpse(shp)

# ANÁLISE DOS RESÍDUOS
library(rcompanion)
plotNormalHistogram(shp$residuos)
qqnorm(shp$residuos,
       ylab = "Resíduos")
qqline(shp$residuos, col="red")

# TESTE (KS) DE NORMALIDADE
# H0: A AMOSTRA SEGUE UMA DISTRIBUIÇÃO NORMAL, SE:
# P-VALOR > 0.05
ks.test(shp$residuos, "pnorm", mean=mean(shp$residuos), sd=sqrt(var(shp$residuos))) # p-value = 0.8638

# HETEROSCEDASTICIDADE (VARIÃNCIA CONSTANTE, OU NÃO)
plot(x = shp$x_geo_ses, y = shp$residuos, main = "Gráfico de Dispersão dos Resíduos",
     xlab = "GeoSES", ylab = "Resíduos")
abline(h = 0, col = "red")

# PACOTE 'lmtest' FAZ TESTES ESTATÍSTICOS PARA PRESSUPOSTOS DE MODELOS DE REGRESSÃO LINEAR 
library(lmtest)
bptest(modelo_simples) # p-value = 0.8502
gqtest(modelo_simples, 19) # p-value = 0.7539

# DEPENDÊNCIA ESPACIAL \\ 'lm.morantest' {spdep}
# 'Moran's I test for spatial autocorrelation in residuals from an estimated linear model (lm())'

library(spdep)
matriz <- poly2nb(shp, queen = TRUE)
pesos <- nb2listw(matriz, style = 'W', zero.policy = TRUE) # IF 'zero.policy' FALSE, ASSIGN 'NA' IN ZONES WITHOUT NEIGHBOURS
modelo_simples_moran <- lm.morantest(modelo_simples, pesos)
print(modelo_simples_moran) # p-value = 0.0001136 SIGNIFICATIVO
modelo_simples_moran2 <- lm.morantest(modelo_simples, pesos, alternative = "two.sided")
print(modelo_simples_moran2) # p-value = 0.0002272 SIGNIFICATIVO

