# INTRODUÇÃO AO R - AULA 2.1

getwd()

### DATAFRAME
# DATAFRAME É UMA COLEÇÃO DE DADOS QUE CONTÉM VÁRIOS TIPOS DE DADOS,
# DIFERENTE DAS MATRIZES QUE SÓ ACEITAM UM TIPO DE DADO
# SÃO MUITO USADOS EM ANÁLISE DE DADOS.

### CONSTRUINDO DATAFRAMES:
# SEMPRE A APRTIR DE VETORES

# a = CAPITAIS DAS UNIDADES DA FEDERAÇÃO
# b = REGIÃO DO PAÍS
# c = POPULAÇÃO RESIDENTES EM CADA CAPITAL (VALORES HIPOTÉTICOS)
# d = CAPITAL FICA OU NÃO NO LITORAL

a <- c("Aracaju", "Belém", "Belo Horizonte", "Boa Vista", "Brasília", "Campo Grande", "Cuiabá", "Curitiba", "Florianópolis", "Fortaleza", "Goiânia", "João Pessoa", "Macapá", "Maceió", "Manaus", "Natal", "Palmas", "Porto Alegre", "Porto Velho", "Recife", "Rio Branco", "Rio de Janeiro", "Salvador", "São Luís", "São Paulo", "Teresina", "Vitória")
b <- c("Nordeste", "Norte", "Sudeste", "Norte", "Centro-Oeste", "Centro-Oeste", "Centro-Oeste", "Sul", "Sul", "Nordeste", "Centro-Oeste", "Nordeste", "Norte", "Nordeste", "Norte", "Nordeste", "Norte", "Sul", "Norte", "Nordeste", "Norte", "Sudeste", "Nordeste", "Nordeste", "Sudeste", "Nordeste", "Sudeste")
c <- c( 664908, 1499641, 2521564, 419652, 3055149, 906092, 618124, 1948626, 508826, 2686612, 1536097, 817511, 512902, 1025360, 2219580, 890480, 306296, 1488252, 539354, 1653461, 413418 , 6747815, 2886698, 1108975, 11253503, 868075, 365855)
d <-c (TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)

base_dados <- data.frame(a,b,c,d)
base_dados

str(b) # PARA EXAMINAR 'b' ou ...


### SELECIONANDO A CAPITAL DA LINHA 17, COLUNA 1
capital <- base_dados[17,1]
capital # Palmas

#### SELECIONAR TODAS AS INFORMAÇÃO DE SP
infos_sp <- base_dados[25,]
infos_sp

### FUNÇÃO subset(): SELECIONA APENAS A CONDIÇÃO DE INTERESSE
# POR EX. CAPITAIS QUE ESTÃO NO LITORAL
# subset = SUBGRUPO

capitais_litoral <- subset(base_dados,d == TRUE)
capitais_litoral

### FUNÇÃO order(): ORDENA A VARIÁVEL DE INTERESSE
# POR EX. ORDENAR A POP. DE FORMA CRESCENTE

pop_crescente <- order(base_dados$c) # O QUE QUERO ORDENAR NA 'BASE_DADOS'
pop_crescente

base_dados[pop_crescente,] # ORDENA NO CONSOLE A POP. DE FORMA CRESCENTE.
