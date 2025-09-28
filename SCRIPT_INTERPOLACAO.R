### INTERPOLAÇÃO
### KRIGAGEM ORDINÁRIA

setwd("C:\\Users\\themo\\Documents\\PROFA_LIGIA\\INTERPOLACAO_DADOS")
getwd()

## AJUSTAR A LINGUA E ENCODING
Sys.setlocale(category = "LC_ALL", 
              locale = "pt_BR.UTF-8")

# PACOTES: 'rcompanion'; 'bestNormalize'; 'spatstat'; 'stars'; 'ggspatial', INSTALADOS VIA MENU Tools
library(rcompanion)
library(bestNormalize)
library(spatstat)
library(stars)
library(ggspatial) # ESCALA GRÁFICA
library(tidyverse)
library(pacman)
pacman::p_load(
  tidyverse,
  sf,
  exactextractr,
  ggplot2,
  gstat,
  terra,
  RColorBrewer,
  sp)

### INTERPOLAR SIGNIFICA ESTIMAR VALORES PARA LUGARES ONDE NÃO HÁ AMOSTRAS, A PARTIR DE PONTOS AMOSTRADOS
temp_media <- read.csv("./temp_media_postosCGE_2012_2022.csv")

postos_CGE <- read.csv("postos_cge_coords.csv")
# CGE = Centro de Gerenciamento de Emeregências climáticas (cidade SP)

# TRANFROMAR OS POSTOS EM OBJETO 'sf', ESPACIAL
postos_sf <- st_as_sf(postos_CGE,
                      coords = c("xcoord",
                                 "ycoord"))
head(postos_sf)
### CRS: NA
### PARA INTERPOLAÇÃO É NECESSÁRIO COORDENADA MÉTRICA
postos_sf <- st_set_crs(postos_sf, 
                        31983)
head(postos_sf)
### Projected CRS: SIRGAS 2000 / UTM zone 23S

### UNINDO AS TEMP. MÉDIAS AO OBJETO 'sf'
postos_temp_sf <- merge(postos_sf,
                        temp_media,
                        by="posto") # "posto" = CAMPO EM COMUM

## VISUALIANDO A INFORMAÇÃO
ggplot(data = postos_temp_sf) +
  geom_sf(aes(color = temp_media))+
  labs(title = "Estações climatológicas",
       color= "Temperatura Média")
## SUBINDO OS DISTRITOS DE SP
distritos <- st_read("./MSP_Distritos.shp",
                     crs=31983)
## VISUALIZAR OS DISTRITOS COM OS PONTOS
ggplot()+
  geom_sf(data = distritos)+
  geom_sf(data = postos_temp_sf,
          mapping = aes(col=temp_media))

## ABORDAGENS ESTOCÁSTICAS INCORPORAM O CONCEITO DE INCERTEZA; CONSIDERAM QUE AS RELAÇÕES ESPACIAIS SÃO ALEATÓRIAS E PODEM VARIAR.
# EXISTEM VÁRIOS MÉTODOS ESTOCÁSTICOS... AQUI, O UTILIZADO É A 'KRIGAGEM ORDINÁRIA'
# QUE É UM INTERPOLADOR QUE LEVA EM CONSIDERAÇÃO A INCERTEZA ASSOCIADA ÀS ESTIMATIVAS INTERPOLADAS
# OS DADOS PRECISAM ATENDER 3 PREMISSAS:
# 1. TER DISTRIBUIÇÃO NORMAL
# 2. PRECISAM SER ESTACIONÁRIOS
# 3. NÃO PODEM TER TENDÊNCIA

# VERIFICAR A DISTRIBUIÇÃO:
hist(temp_media$temp_media) # OBJETO 'temp_media'; VARIÁVEL 'temp_media'

# TESTE DE NORMALIDADE: 'Shapiro-Wilk', COM NÍVEL DE SIGINIFICÂNCIA MENOR QUE 0.05
# H0: OS DADOS SEGUEM UMA DISTRIBUIÇÃO NORMAL
# SE p-valor < 0.05, REJEITO A H0

objeto_teste_shapiro <- shapiro.test(temp_media$temp_media)
objeto_teste_shapiro # p-value = 0.2496 # MANTENHO A H0 - DADOS TENDEM À NORMALIDADE
## SE P-VALOR < 0.05 REJEITO H0


## QUANDO OS DADOS NÃO SEGUEM UMA DISTRIBUIÇÃO NORMAL, FAZ-SE UMA TRANSFORMAÇÃO PARA QUE TENHAM..
## DEPOIS DA ANÁLISE, RETORNA OS DADOS AO NORMAL DELES, COMO ERAM ANTES DA TRANSFORMAÇÃO

plotNormalHistogram(postos_temp_sf$temp_media) # PARA VER A LINHA DA DISTRIBUIÇÃO

qqnorm(postos_temp_sf$temp_media,
       ylab="Temperatura média") # PARA FAZER O GRÁFICO 
qqline(postos_temp_sf$temp_media, col="red") # PARA VER  LINHA

## NORMALIZANDO OS DADOS, PACOTE 'bestNormalize'
# SE NÃO FUNCIONAR, TROCAR A BASE DE DADOS
modelo_normal <- bestNormalize(postos_temp_sf$temp_media) # OBJETO: 'postos_temp_sf', VARIÁVEL: 'temp_media'
modelo_normal
plotNormalHistogram(modelo_normal$x.t) # OBJETO: 'modelo_normal',  VARIÁVEL: '$x.t'

## OBJETO PARA QUANDO FOR FAZER O MAPA FINAL
# DESNORMALIZAR OS DADOS.. RETORNAR PARA O NORMAL DE ANTES DA TRANSFORMAÇÃO
temp_inicial <- predict(modelo_normal,
                        newdata = modelo_normal$x.t,
                        inverse=TRUE)

## ESTACIONARIEDADE: VARIAÇÃO NÃO MUDA.. É CONSTANTE
## REMOÇÃO DE TENDÊNCIA E AJUSTE DO VARIOGRAMA: CALCULA-SE UMA EQUAÇÃO DE 1º GRAU PARA USAR NA INTERPOLAÇÃO POR KRIGAGEM
f.1 <- as.formula(modelo_normal$x.t~X+Y) # EQUAÇAO 1º GRAU

## ADD. AS COLUNAS 'X' & 'Y'
postos_temp_sf <- postos_temp_sf |> 
  mutate(X=unlist(map(postos_temp_sf$geometry,1)),
         Y=unlist(map(postos_temp_sf$geometry,2)))

## VARIOGRAMA SEM TENDÊNCIA, USANDO MODELO 'f.1'
var.smpl <- variogram(f.1, 
                      postos_temp_sf, 
                      cloud = FALSE, 
                      cutoff=50000, 
                      width=5000)
var.smpl # PARA VER OS PARÂMETROS

### MODELOS DE VARIOGRAMA
print(show.vgms())

## AJUSTAR OS MODELOS
todos_modelos <- vgm(model = c("Exp", "Gau"))
fit_auto <- fit.variogram(var.smpl, todos_modelos)
plot(var.smpl, fit_auto)
print(fit_auto)

## CRIAR O GRID DE INTERPOLAÇÃO
grid = st_as_stars(st_bbox(st_buffer(distritos,0001)))

## MODELO DE INTERPOLAÇÃO
kr <- gstat(formula = modelo_normal$x.t~1,
            data = postos_temp_sf,
            model = fit_auto)

## INTERPOLAÇÃO
z = predict(kr, grid) # [using ordinary kriging]

valores <- as.vector(z[[1]][[1]])
n_cores <- 10
cores_invertidas <- rev(hcl.colors(n_cores, "Orange"))
quebras <- seq(min(valores, na.rm = TRUE),
               max(valores, na.rm = TRUE),
               length.out = n_cores+1)
plot(z,
     col = cores_invertidas,
     reset = FALSE)
plot(st_geometry(postos_temp_sf),
     add = TRUE)
text(st_coordinates(postos_temp_sf),
     as.character(round(postos_temp_sf$temp_media),
                  2,
                  pos=3,
                  add=TRUE))

### PLOTAR AS VARIÂNCIAS
cores_invertidas <- rev(hcl(n_cores, "Orange"))
plot(z["var1.var"], 
     col=cores_invertidas, 
     reset=FALSE)

### RETOMAR OS VALORES ANTES DA TRANFORMAÇÃO
z$inicial <- predict(modelo_normal,
                     newdata=z$var1.pred,
                     inverse=TRUE)
plot(z["inicial"],
     col=cores_invertidas,
     reset=FALSE)
plot(st_geometry(distritos),
     add=TRUE)

### TRANSFORMANDO EM RASTER
temp_raster <- st_as_stars(z)
temp_distr <- st_crop(temp_raster,
                      distritos)
plot(temp_distr)
breaks_raster <- c(15.30, 16.00, 18.00, 20.00, 22.00, 23.34)


mapa_temp_media <- ggplot()+
  geom_stars(data=temp_distr,
             aes(fill=inicial))+
  scale_fill_gradientn(colors = c("#FBF2B5FF", "#FFC87AFF", "#F65620FF"),
                      breaks = breaks_raster,
                      labels = c(15.30, 16.00, 18.00, 20.00, 22.00, 23.34),
                      na.value = "white")+
  geom_point(data = postos_temp_sf,
             aes(x=X,
                 y=Y,
                 color="Estação"))+
  labs(
    fill="temp. Média (°C)",
    caption = "Fonte: CGE/SP (2023) \n Elaborado por: Daniela"
  )+
  scale_color_manual(
    values = "black",
    name= "Estações Climatológicas\n CGE/SP"
  )+
  geom_sf(data = distritos,
          colour="grey50",
          alpha=0)+
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Temperatura média anual (2012 a 2022)")+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ggspatial::annotation_scale(location="br",
                              width_hint=0.4,
                              line_width = 0.5,
                              height = unit(0.1,"cm"))
print((mapa_temp_media))
