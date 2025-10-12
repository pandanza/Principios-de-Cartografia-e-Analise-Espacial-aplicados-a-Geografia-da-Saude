## ANÁLISE EXPLORATÓRIA DE DADOS
## MODELOS DE REGRESSÃO - Fonte: Zuur et al. (2007, 2010)

setwd("C:\\Users\\themo\\Documents\\PROFA_LIGIA\\MODELOS_REGRESSAO_DZ")
getwd()

# SUBIR O SHP
shp <- st_read("C:\\Users\\themo\\Documents\\PROFA_LIGIA\\MODELOS_REGRESSAO_DZ\\msp_smr_geoses.shp")

library(pacman)
pacman::p_load(
  dplyr,
  janitor,
  nlme,
  spdep,
  ggplot2,
  sf
)

# FUNÇÃO DESENVOLVIDA POR Zuur et al. (2010)
# O SCRIPT DA FUNÇÃO TEM QUE ESTAR NO MESMO DIRETÓRIO DOS DADOS
source("HighstatLivV9.R")

glimpse(shp)

shp <- shp |> 
  janitor::clean_names()

boxplot(shp$smr)
dotchart(shp$smr)
str(shp) # 'str' DE STRUCTURE OF ANY R OBJECT

shp_data.frame <- as.data.frame(shp)
glimpse(shp_data.frame)
names(shp_data.frame)

variaveis <- c("x_geo_ses", "x_educac", "x_mobilidad", "x_pobreza", "x_priv_mat", "x_riqueza", "x_renda", "x_segreg")

library(lattice)
Mydotplot(shp_data.frame[,variaveis]) # OUTLIERS DE CADA VARIÁVEL
# A dimensão privação material do GeoSES (‘x_priv_mat’) 
# CORRESPONDE À VARIÁVEL PORCENTAGEM DE DOMICILIOS ADEQUADOS POR DISTRITOR
boxplot(shp_data.frame$x_priv_mat)

library(rcompanion)
plotNormalHistogram(shp_data.frame$x_priv_mat)
qqnorm(shp_data.frame$x_priv_mat,
       ylab="Privação Material")
qqline(shp_data.frame$x_priv_mat, col="red")

shp_data.frame$x_priv_mat <- (shp_data.frame$x_priv_mat)^2
qqnorm(shp_data.frame$x_priv_mat,
       ylab="Privação material elevada ao quadrado")
qqline(shp_data.frame$x_priv_mat, col="red")
plotNormalHistogram(shp_data.frame$x_priv_mat)

library(bestNormalize)
modelo_normal=bestNormalize(shp_data.frame$x_priv_mat)
modelo_normal
plotNormalHistogram(modelo_normal$x.t)
qqnorm(modelo_normal$x.t,
       ylab="Privação material BestNormalize")
qqline(modelo_normal$x.t, col="red")

# VERIFICAR 'zeros" NA VARIÁVEL DEPENDNETE "smr'
sum(shp_data.frame$smr==0)/nrow(shp_data.frame)*100

str(shp_data.frame) # 'str' = 'structure'
pairs(shp_data.frame[,c(9:16)]) # GRÁFICO TIPO MATRIZ CORREL

correl <- cor(shp_data.frame[,9:16]) # OBJETO COM VALORES DE CORREL
correl

# EXPORTAT EM .txt AS CORRELAÇÕES
write.table(correl, file = "correl.txt", sep = ",", col.names = NA, qmethod = "double")

names(shp_data.frame)
MyVar <- c("x_geo_ses", "x_educac", "x_mobilidad", "x_pobreza", "x_priv_mat", "x_riqueza", "x_renda", "x_segreg")
Mypairs(shp_data.frame[,MyVar])

# FUNÇÃO 'corvif' PERMITE AVALIAR O VIF (Variance Inflation Factor) DE CADA VARIÁVEL
# DEVEM SER MENOR QUE 3

corvif(shp_data.frame[,MyVar])
MyVar <- c("x_educac", "x_mobilidad", "x_pobreza", "x_priv_mat", "x_riqueza", "x_renda", "x_segreg")
corvif(shp_data.frame[,MyVar])
MyVar <- c("x_educac", "x_mobilidad", "x_pobreza", "x_priv_mat", "x_riqueza", "x_renda")
corvif(shp_data.frame[,MyVar])
MyVar <- c("x_mobilidad", "x_pobreza", "x_riqueza", "x_renda")
corvif(shp_data.frame[,MyVar])
MyVar <- c("x_mobilidad", "x_pobreza", "x_riqueza", "x_renda")
corvif(shp_data.frame[,MyVar])
MyVar <- c("x_mobilidad", "x_pobreza", "x_renda")
corvif(shp_data.frame[,MyVar])
MyVar <- c("x_mobilidad", "x_renda")
corvif(shp_data.frame[,MyVar])

# VER A RELAÇÃO ENTRE AS VARIÁVEIS
glimpse(variaveis)
Myxyplot(shp_data.frame, variaveis, "smr", MyYlab="Taxa de Mortalidade Infantil")
