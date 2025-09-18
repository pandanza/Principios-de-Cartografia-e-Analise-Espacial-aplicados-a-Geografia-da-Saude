### PADRONIZAÇÃO DE DADOS // MENINAS
setwd("C:/Users/themo/Documents/PROFA_LIGIA/BASE_DADOS_PADRONIZACAO_DADOS")
getwd()

### PADRONIZAÇÃO DE TAXA PELO MÉTODO INDIRETO // UNIDADE GEOGRÁFICA: DISTRITO
### PADRONIZAÇÃO DA MORTALIDADE INFANTIL EM SP POR DISTRITO ADMINISTRATIVO, CONSIDERANDO O PERÍODO DE 
# 2010 A 2019 E A POPULAÇÃO DE NASCIDOS VIVOS DE 2010 A 2019.

## PACOTES: (pacman INSTALADO PELO MENU TOOLS)
(pacman::p_load(
  foreign,
  readxl,
  janitor,
  tidyverse,
  dplyr,
  tidyr))

## ORGANIZANDO OS DADOS DOS MENINOS: COMEÇAR LIMPANDO OS DADOS, POIS NÃO DEVEM COMEÇAR EM Nºs NEM TER ESPAÇOS.
# FUNÇÃO clean_names() PACOTE **janitor**

obitos_fem_bruto <- read_xlsx("C:/Users/themo/Documents/PROFA_LIGIA/BASE_DADOS_PADRONIZACAO_DADOS/obitos_fem.xlsx")
limpo_obitos_fem_bruto <- obitos_fem_bruto |> 
  janitor::clean_names()

## MORTALIDADE DOS BEBÊS:
# peri - PERINATAL: PRIMEIROS 6 DIAS APÓS O NASCIMENTO 
# neo - NEONATAL: DE 7 A 27 DIAS APÓS O NASCIMENTO
# posneo - PÓS-NEONATAL DE 28 DIAS A 365 DIAS

names(limpo_obitos_fem_bruto) ## PARA VER OS NOMES DAS COLUNAS

limpo_obitos_fem_bruto <- limpo_obitos_fem_bruto |> 
  rename(peri=x7_dias) |> 
  rename(neo=x7_27_dias) |> 
  rename(posneo=x28d_1ano)

### PARA A PADRONIZAÇÃO, O ARQUIVO PRECIA CONTER OUTRAS INFORMAÇÕES: 
# DISTRITO ADMINISTRATIVO, FAIXA ETÁRIA, NÚMERO DE ÓBITOS E SEXO
# FUNÇÃO: pivot_longer PARA ORGANIZAR OS DADOS DESSA FORMA

limpo_obitos_fem_bruto <- limpo_obitos_fem_bruto |> 
  mutate(posneo=as.numeric(posneo))

class(limpo_obitos_fem_bruto$posneo)

limpo_obitos_fem_bruto_pivot <- limpo_obitos_fem_bruto |>
  pivot_longer(cols = c(peri,neo,posneo), names_to = "faixa etária", values_to = "obitos")

## ADMISSÃO DE QUE NA É ZERO
limpo_obitos_fem_bruto_pivot$obitos[is.na(limpo_obitos_fem_bruto_pivot$obitos)] <- 0


### ADICIONANDO O SEXO fem

limpo_obitos_fem_bruto_pivot <- limpo_obitos_fem_bruto_pivot |> 
  mutate(sexo="fem")

### PARA A PADRONIZAÇÃO, É PRECISA SABER QUANTOS BEBÊS NASCERAM NO PERÍODO. 
# DADOS DE NASCIDOS VIVOS, PRECISA LIMPAR OS DADOS, MESMO PROCEDIMENTO ... 

nv_fem_bruto <- read_xlsx("C:/Users/themo/Documents/PROFA_LIGIA/BASE_DADOS_PADRONIZACAO_DADOS/nv_fem.xlsx")
nv_fem_limpo <- nv_fem_bruto |> 
  janitor::clean_names()

### DO df nv_masc_limpo, SERÁ PRECISO APENAS AS COLUNAS DO DISTRITO ADM. E TOTAL

nv_fem_total <- nv_fem_limpo |> 
  select(distrito_administ_residencia, total)

### CADA FAIXA ETÁRIA PRECISA DE SEU TOTAL. ADICIONANDO COLUNAS
# FUNÇÃO pivot_longer

nv_fem_total$xperi <- nv_fem_total$total
nv_fem_total$xneo <- nv_fem_total$total
nv_fem_total$xposneo <- nv_fem_total$total

### NESSA PLANILHA DOS nv É PRECISO INDICAR O SEXO masc
# CRIANDO UMA NOVA COLUNA

nv_fem_total <- nv_fem_total |> 
  mutate(sexo = "fem")

### AGORA, ORGANIZA OS DADOS PARA A PADRONIZAÇÃO
# FUNÇÃO pivot_longer 

nv_fem_total <- nv_fem_total |> 
  pivot_longer(cols = starts_with("x"), names_to = "faixa etária",
               values_to = "pop")

nv_fem_total <- nv_fem_total |> 
  select(-total)

### JUNTAR OS DADOS DOS BBs nv E obitos MASCULINOS
# PARA ISSO, OS NOMES DAS COLUNAS DEVEM SER IGUAIS

names(limpo_obitos_fem_bruto_pivot)
names(nv_fem_total)

nv_fem_total <- nv_fem_total |> 
  rename(distrito_admin_residencia = distrito_administ_residencia) ## NOMES IGUAIS DOS 2 ARQUIVOS

### NO ENTANO, O ARQUIVO nv_masc_total AINDA ETÁ COM X NO NOME... ARRUMANDO ISSO...

nv_fem_total <- nv_fem_total |> 
  mutate(`faixa etária`= recode(`faixa etária`,
                                "xperi" = "peri",
                                "xneo" = "neo",
                                "xposneo" = "posneo"))

### UNINDO OS DOIS ARQUIVOS FUNÇAÕ merge

uniao_fem <- merge(nv_fem_total, limpo_obitos_fem_bruto_pivot,
                    by = c("distrito_admin_residencia","faixa etária"), all = TRUE)

### VERIFICANDO SE HÁ ALGUM VALOR FALTANTE

sum(is.na(uniao_fem$obitos)) ## ZERO






