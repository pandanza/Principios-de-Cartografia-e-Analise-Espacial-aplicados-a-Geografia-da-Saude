### UNINDO OS ARQUIVOS DAS MENINAS E MENINOS

(pacman::p_load(
  foreign,
  readxl,
  janitor,
  tidyverse,
  dplyr,
  tidyr))

names(uniao_fem)
names(uniao_masc)

uniao_fem <- uniao_fem |> 
  select(-sexo.x)

uniao_masc <- uniao_masc |> 
  select(-sexo.x)

uniao_fem <- uniao_fem |> 
  select(-total)
uniao_masc <- uniao_masc |> 
  select(-total)

names(uniao_fem)
names(uniao_masc)

## JUNTAR OS ARQUIVOS NUM SÓ

dados_combinados <- rbind(uniao_fem, uniao_masc)

## ORGANIZAR OS DADOS COMBINADOS

dados_combinados_order <- dados_combinados[order(
  dados_combinados$distrito_admin_residencia,
  dados_combinados$sexo.y,
  dados_combinados$`faixa etária`
),]

### AGORA SIM, CÁLCULO DA TAXA DE MORTALIDADE PADRONIZADA
## PACOTE: SpatialEpi INSTALADO PELO MENU TOOLs

library(SpatialEpi)

### PARA A PADRONIZAÇÃO, PRECISAREMOS TER O CÁLCULO DOS ÓBITOS ESPERADOS PARA CADA DISTRITO, OU SEJA, SABEMOS O Nº TOTAL DE BEBÊS QUE
# MORRERAM EM CADA IDADE E PARA CADA SEXO NO TOTAL DO MUNICÍPIO DE SP. NO ENTANTO, ESSE Nº VARIA EM CADA DISTRITO ADMINISTRATIVO. 
# QUANDO PADRONIZAMOS AS TAXAS, ELAS SE TORNAM COMPARÁVEIS, COMO SE EM TODOS OS DISTRITOS TIVESSEM O MESMO Nº DE NASCIDOS VIVOS PARA CADA SEXO.
# O Nº DE ÓBITOS ESPERADOS, PORTANTO, QUER DIZER EXATAMENTE ISSO: QUANTOS ÓBITOS SERIAM ESPERADOS PARA CADA DISTRITO SE TODOS TIVESSEM O MESMO NÚMERO DE NASCIDOS VIVOS.
# O PACOTE SpatialEpi FAZ O CÁLCULO, MAS O ARQUIVO DE ENTRADA DEVE ESTAR EXATAMENTE DA FORMA QUE DEIXAMOS NOS PASSOS ANTERIORIES. 

## OS CÓDIGOS A SEGUIR NÃO PODEM ALTERAR AS COLUNAS "E" (Expected), "population" (população, NASCIDOS VIVOS) e "cases" (casos, OS ÓBITOS). 

E <- expected(
  population = dados_combinados_order$pop,
  cases = dados_combinados_order$obitos,
  n.strata = 6
)


### A SEGUIR, É FEITO O AGRUPAMENTO DOS DADOS ORDENADOS POR DISTRITO,
# E SOMADO O Nº DE ÓBITOS POR DISTRITO

d_combinado <- group_by(dados_combinados_order, distrito_admin_residencia) |> 
  summarise(Y=sum(obitos))

## AGORA JUNTA O VALOR "E" AO DF "DISTRITOS COMBINADOS" (SÓ VEIO UMA COLUNA "E")

d_combinado$E <- E[match(d_combinado$distrito_admin_residencia,
                         unique(dados_combinados_order$distrito_admin_residencia))]

## AGORA PARA PREENCHER A COLUNA "E" COM O "SMR: 'Standardized Mortality Ratio'"

d_combinado$SMR <- d_combinado$Y / d_combinado$E

## PARA ABRIR O .shp DOS DISTRITOS DA CIDADE DE SP
library(sf)
shp <- st_read("C:/Users/themo/Documents/PROFA_LIGIA/BASE_DADOS_PADRONIZACAO_DADOS/Distr_Adm_Munic_SP.shp")
names(shp) ## "ds_codigo"   "ds_nome"     "X_NOME_tabn" "geometry"  
names(dados_combinados_order)

## PARA DEIXAR IGUAL AOS NOMES DA dados_combinados_order
shp$distrito_admin_residencia <- shp$X_NOME_tabn
names(shp)
names(dados_combinados_order)

## EXCLUIR A COLUNA DO NOME ANTIGO
shp <- shp |> 
  select(-X_NOME_tabn)

## UNIR A TABELA dados_combinados_order AO .shp
map <- merge(shp, d_combinado)

## PARA FAZER MAPA DA TAXA PADRONIZADA, PACOTE ggplot2
library(ggplot2)
library(ggspatial) ## TEVE QUE INSTALAR

## INDICANDO A CODIFICAÇÃO UTF-8:
Sys.setlocale("LC_ALL", "Portuguese_Brazil.utf8")

(mapa_1 <- ggplot(map) + 
    geom_sf(aes(fill = SMR)) +
    scale_fill_gradient2(
      midpoint = 1, low = "blue", mid = "grey", high = "red"
    ) +
    ggtitle("Taxa de Mortalidade Infantil Padronizada - \n de 2010 a 2019, São Paulo") +
    labs(caption = "Fonte: Sistema de Informações sobre Mortalidade\n – SIM/PRO-AIM – CEInfo –SMS-SP |\n Elaborado por: Daniela\n Disciplina: Princípios de Cartografia e Análise Espacial aplicados à Geografia da Saúde (2025)") +
    ggspatial::annotation_scale(location = "br", width_hint = 0.2, 
                                line_width = 0.5, height = unit(0.1,"cm")))

## SALVANDO O MAPA
ggsave("./mapa1_Taxa_Mortalidade_Infantil.jpg", dpi=300, width = 25, height = 15, units = "cm")

## SALVANDO COM .shp
# ATENÇÃO AO OBJETO, DEVE SER O sf NÃO O GRÁFICO
st_write(map, "./taxa.shp")


##### TAXA PADRONIZADA DA FAIXA ETÁRIA PERINATAL
# FILTRAR A FAIXA ETÁRIA
names(dados_combinados)
dados_combinados_order_peri <- dados_combinados_order |> 
  filter(`faixa etária` == "peri")

## CALCULANDO 'E', 'Y' E 'SMR' 
E_peri <- expected(
  population = dados_combinados_order_peri$pop,
  cases = dados_combinados_order_peri$obitos, n.strata = 2
)

## Y:
d_combinado_peri <- group_by(dados_combinados_order_peri, distrito_admin_residencia) |> 
  summarise(y=sum(obitos))

## JUNTAR O VALOR DE E_peri
d_combinado_peri$E_peri <- E_peri[match(d_combinado_peri$distrito_admin_residencia,
                         unique(dados_combinados_order$distrito_admin_residencia))]

## SMR (TAXA)
d_combinado_peri$SMR <- d_combinado_peri$y / d_combinado_peri$E_peri

## UNIR AO .shp A TAXA DA FAIXA ETÁRIA peri
mapa_peri <- merge(shp, d_combinado_peri)

## MAPA 2:
mapa_2 <- ggplot(mapa_peri) + 
  geom_sf(aes(fill = SMR)) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "grey", high = "red"
  ) +
  ggtitle("Taxa de Mortalidade Infantil Perinatal Padronizada - \n de 2010 a 2019, São Paulo") +
  labs(caption = "Fonte: Sistema de Informações sobre Mortalidade\n – SIM/PRO-AIM – CEInfo –SMS-SP |\n Elaborado por: Daniela. \n Disciplina: Princípios de Cartografia e Análise Espacial aplicados à Geografia da Saúde (2025)") +
  ggspatial::annotation_scale(location = "br", width_hint = 0.2, 
                              line_width = 0.5, height = unit(0.1,"cm"))
theme_bw()

mapa_2 ## PARA VER O MAPA

## EXPORTANDO EM .jpg E .shp
ggsave("./mapa_2_Taxa_Mortalidade_Infantil_Peri.jpg", dpi=300, width = 25, height = 15, units = "cm")

