### ANÁLISE DO PADRÃO ESPACIAL DO GeoSES
# ÍNDICE SOCIOECONÔMICO PARA ESTUDOS DE SAÚDE NO BRASIL

library(devtools)
library(spdep)
library(rgeoda)
library(ggplot2)

setwd("C:\\Users\\themo\\Documents\\PROFA_LIGIA\\MAPA_COROPLETICO")
getwd()

geoses <- st_read("MSP_AREAP_GEOSES.shp")

## MORAL GLOBAL - PACOTE 'spdep'
## MORAN LOCAL - PACOTE 'rgeoda'

matriz_queen <- poly2nb(geoses, queen = TRUE) ## PACOTE 'spdep'

peso_vizinhos <- nb2listw(matriz_queen, style = "W", zero.policy = TRUE) 
# STYLE "W" PESOS PADRONIZADOS POR LINHA

peso_vizinhos$weights[5]

defasagem <- lag.listw(peso_vizinhos, geoses$GeoSES)

plot(defasagem~geoses$GeoSES, pch=16, asp=1)
abline(lm(defasagem~geoses$GeoSES), col="red")

## MORAN GLOBAL - PACOTE 'spdep'
I <- moran(geoses$GeoSES, peso_vizinhos, length(matriz_queen), Szero(peso_vizinhos))[1]
I

## H0: OS VALORES DOS ÍNDICES SÃO DISTRIBUÍDOS ALEATORIAMENTE AO LONGO DO TERRITÓRIO,
# SEGUINDO UM PROCESSO COMPLETAMENTE ALEATÓRIO

# MÉTODO ANALÍTICO
moran.test(geoses$GeoSES, peso_vizinhos, alternative = "greater")

# MÉTODO DE SIMULAÇÃO DE MONTE CARLO
MC <- moran.mc(geoses$GeoSES, peso_vizinhos, nsim = 999, alternative = "greater")
MC
plot(MC)

# PARÂMETRO: 'nsim' = QUANTIDADE DE SIMULAÇÕES
# PARÂMETRO: 'alternative' COM 3 POSSIBILIDADES: 'greater'; 'less'; 'two.sided'
# SE O VALOR OBSERVADOESTIVER DO LADO DIREITO DA DISTRIBUIÇÃO ESPERADA, ADOTA-SE A OPÇÃO 'greater' QUE FOCARÁ NA CAUDA SUPERIOR DA DISTRIBUIÇÃO
# SE ESTIVER DO LADO ESQUERSO, ESCOLHER 'less' PARA FOCAR NA CAUDA INFERIOIR DA DISTRIBUIÇÃO 
# I = 0.85, POSITIVO, ESCOLHER 'greater' PARA O PARÂMETRO

moran.plot(geoses$GeoSES, peso_vizinhos)

## MORAN LOCAL - LISA “Local Indicator of Spatial Association”
# PACOTE 'rgeoda'

queen_w <- queen_weights(geoses) # MATRIZ DO LISA
geo = geoses["GeoSES"] # COLUNA DA TAXA SE CHAMA "GeoSES"
indice_lisa <- local_moran(queen_w,geo)
lisa_colors <- lisa_colors(indice_lisa)
lisa_labels <- lisa_labels(indice_lisa)
lisa_clusters <- lisa_clusters(indice_lisa)

## PLOTAR O MAPA  
plot(st_geometry(geo),
     col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}),
                border="#333333", lwd=0.2)
     title(main = "Moran Local do GeoSES")
     legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")

     
## VISUALIZAR O MESMO RESULTADO USANDO A BASE DO OpenStreetMap:
geoses$moran_cluster <- lisa_clusters ## ADD. OS clusters NO DADO geoses
geoses$moran_cluster <- as.character(geoses$moran_cluster)
cores_lisa <- c("white", "red", "blue", "lightblue", "lightpink") ## LINK PARA PALETA DE CORES EM R https://r-charts.com/color-palettes/#google_vignette
geoses$moran_cluster <- factor(geoses$moran_cluster,
                               levels = c("0","1","2","3","4"),
                               labels = c("Não significativo", "Alto-Alto", "Baixo-Baixo", "Baixo-Alto", "Alto-Baixo"))

## PARA VER O MAPA
mapview::mapview(geoses,
                 zcol="moran_cluster",
                 legend=TRUE,
                 legend.style="fill",
                 layer.name="Índice GeoSES",
                 col.regions=cores_lisa)

