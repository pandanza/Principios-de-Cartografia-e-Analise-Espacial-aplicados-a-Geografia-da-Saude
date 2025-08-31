### PADRONIZAÇÃO DE DADOS
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

obitos_masc_bruto <- read_xlsx("C:/Users/themo/Documents/PROFA_LIGIA/BASE_DADOS_PADRONIZACAO_DADOS/obitos_masc.xlsx")
limpo_obitos_masc_bruto <- obitos_masc_bruto |> 
  janitor::clean_names()

## MORTALIDADE DOS BEBÊS:
# peri - PERINATAL: PRIMEIROS 6 DIAS APÓS O NASCIMENTO 
# neo - NEONATAL: DE 7 A 27 DIAS APÓS O NASCIMENTO
# posneo - PÓS-NEONATAL DE 28 DIAS A 365 DIAS

names(limpo_obitos_masc_bruto) ## PARA VER OS NOMES DAS COLUNAS

limpo_obitos_masc_bruto <- limpo_obitos_masc_bruto |> 
  rename(peri=x7_dias) |> 
  rename(neo=x7_27_dias) |> 
  rename(posneo=x28d_1ano)

### PARA A PADRONIZAÇÃO, O ARQUIVO PRECIA CONTER OUTRAS INFORMAÇÕES: 
# DISTRITO ADMINISTRATIVO, FAIXA ETÁRIA, NÚMERO DE ÓBITOS E SEXO
# FUNÇÃO: pivot_longer PARA ORGANIZAR OS DADOS DESSA FORMA

limpo_obitos_masc_bruto_pivot <- limpo_obitos_masc_bruto |>
  pivot_longer(cols = c(peri,neo,posneo), names_to = "faixa etária", values_to = "obitos")

### ADICIONANDO O SEXO masc

limpo_obitos_masc_bruto_pivot <- limpo_obitos_masc_bruto_pivot |> 
  mutate(sexo="masc")

### PARA A PADRONIZAÇÃO, É PRECISA SABER QUANTOS BEBÊS NASCERAM NO PERÍODO. 
# DADOS DE NASCIDOS VIVOS, PRECISA LIMPAR OS DADOS, MESMO PROCEDIMENTO ... 

nv_masc_bruto <- read_xlsx("C:/Users/themo/Documents/PROFA_LIGIA/BASE_DADOS_PADRONIZACAO_DADOS/nv_masc.xlsx")
nv_masc_limpo <- nv_masc_bruto |> 
  janitor::clean_names()

### DO df nv_masc_limpo, SERÁ PRECISO APENAS AS COLUNAS DO DISTRITO ADM. E TOTAL

nv_masc_total <- nv_masc_limpo |> 
  select(distrito_administ_residencia, total)

### CADA FAIXA ETÁRIA PRECISA DE SEU TOTAL. ADICIONANDO COLUNAS
# FUNÇÃO pivot_longer

nv_masc_total$xperi <- nv_masc_total$total
nv_masc_total$xneo <- nv_masc_total$total
nv_masc_total$xposneo <- nv_masc_total$total

### NESSA PLANILHA DOS nv É PRECISO INDICAR O SEXO masc
# CRIANDO UMA NOVA COLUNA

nv_masc_total <- nv_masc_total |> 
  mutate(sexo = "masc")

### AGORA, ORGANIZA OS DADOS PARA A PADRONIZAÇÃO
# FUNÇÃO pivot_longer 

nv_masc_total <- nv_masc_total |> 
  pivot_longer(cols = starts_with("x"), names_to = "faixa etária",
               values_to = "pop")

nv_masc_total <- nv_masc_total |> 
  select(-total)

### JUNTAR OS DADOS DOS BBs nv E obitos MASCULINOS
# PARA ISSO, OS NOMES DAS COLUNAS DEVEM SER IGUAIS

names(limpo_obitos_masc_bruto_pivot)
names(nv_masc_total)

nv_masc_total <- nv_masc_total |> 
  rename(distrito_admin_residencia = distrito_administ_residencia) ## NOMES IGUAIS DOS 2 ARQUIVOS

### NO ENTANO, O ARQUIVO nv_masc_total AINDA ETÁ COM X NO NOME... ARRUMANDO ISSO...

nv_masc_total <- nv_masc_total |> 
  mutate(`faixa etária`= recode(`faixa etária`,
                                "xperi" = "peri",
                                "xneo" = "neo",
                                "xposneo" = "posneo"))

### UNINDO OS DOIS ARQUIVOS FUNÇAÕ merge

uniao_masc <- merge(nv_masc_total, limpo_obitos_masc_bruto_pivot,
                    by = c("distrito_admin_residencia","faixa etária"), all = TRUE)

### VERIFICANDO SE HÁ ALGUM VALOR FALTANTE

sum(is.na(uniao_masc$obitos)) ## ZERO






