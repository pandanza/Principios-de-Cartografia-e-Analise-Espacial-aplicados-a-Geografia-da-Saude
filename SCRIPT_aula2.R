# INTRODUÇÃO AO R - AULA 2

getwd()

## MATRIZES EM R SÃO UMA COLEÇÃO DE UM MESMO ELEMENTO, Nº OU CARACTERES OU...
# BIDIMENSIONAIS, POR SEREM COMPOSTAS POR LINHAS E COLUNAS. SEMPRE QUADRÁTICAS
# LINHAS: ROW
# COLUNAS: COLUMN

### CONSTRUINDO UMA MATRIZ 3x3 COM 9 ELEMENTOS:
matrix(9:17,
       byrow = TRUE, # byrow = true or false, ORGANIZA PORLINHA, SIM OU NÃO.
       nrow = 3)

# AS VECTOR: 
c(9,10,11,12,13,14,15,6,17)

# CRIANDO OBJETOS, MATRIZ E VETOR:
m1 <- matrix(9:17,
             byrow = TRUE,
             nrow = 3)

ma <- c(9,10,11,12,13,14,15,6,17)


## POPULAÇÃO DE SP CONFORME ASSENTAMENTO, IBGE:

# ASSENTAMENTOS REGULARES = a
# AGLOMERADOS SUBNORMAIS = b
# ASSENTAMENTOS PRECÁRIOS = c

## FAIXAS ETÁRIAS:
# 0 - 19 ANOS
# 20 - 59 ANOS
# 60 +

a <- c(2540608, 5794529, 1253256)
b <- c(539918, 777051, 65114)
c <- c(97712, 161663, 19225)

## A PARTIR DOS 3 VETORES, É POSSÍVEL CRIAR A MATRIZ COM A POPULAÇÃO:
SPmatriz <- matrix(c(a,b,c),
                   byrow = TRUE,
                   nrow = 3)
SPmatriz

## PARA NOMEAR O TIPO DE ASSENTAMENTO:
tipos <- c("ASSENT. REGULARES", "AGLO. SUBNORMAIS","ASSENT. PRECÁRIOS")
idades <- c("0 a 19", "20 a 59", "60 +")

colnames(SPmatriz) <- tipos # NOME COLUNA
rownames(SPmatriz) <- idades # NOME LINHAS

SPmatriz ## VER A MATRIZ NO CONSOLE

### CALCULO EM MATRIZES
# PARA SABER A POPULAÇÃO TOTAL POR FAIXA ETÁRIA, SOMA AS LINHAS.

total_faixa_etaria <- rowSums(SPmatriz)
total_faixa_etaria

# POP TOTAL POR TIPO DE ASSENTAMENTO, SOMA AS COLUNAS

total_assentamento <- colSums(SPmatriz)
total_assentamento

### PARA ADICIONAR COLUNAS OU LINHAS DOS TOTAIS, CRIA-SE NOVAS MATRIZ...

SPmatriz_total_faixa_etaria <- cbind(SPmatriz,total_faixa_etaria) # cbind ADD COLUNA

SPmatriz_total_assentamento <- rbind(SPmatriz,total_assentamento) # rbind ADD LINHA

SPmatriz_total_faixa_etaria
SPmatriz_total_assentamento

### MULTIPLICAR A MATRIZ SPmatriz POR 10%

SPmatriz_10 <- SPmatriz*0.10
SPmatriz_10

### VALORES HIPOTÉTICOS DE COBERTURA VACINAL 

cobertura_vacinal <- c(0.59, 0.88, 0.93, 0.8, 0.61, 0.79, 0.75, 0.66, 0.56)

matriz_cob_vacinal <- matrix(cobertura_vacinal,
                             byrow = TRUE,
                             nrow = 3)
matriz_cob_vacinal

### MULTIPLICAÇÃO DAS DUAS MATRIZES, SPmatriz e COBERTURA VACINAL
# PARA SABER QNTAS PESSOAS, POR CLASSE FORAM VACINADAS.

multiplicar_matriz <- SPmatriz * cobertura_vacinal
multiplicar_matriz


### PARA SELECIONAR UM ELEMNTO, É PRECISO SABER O LUGAR DESSE ELEMENTO..
# elemento_1 = faixa etária: 60+ em aglo. subnormais

elemento_1 <- SPmatriz[3,2]
elemento_1
