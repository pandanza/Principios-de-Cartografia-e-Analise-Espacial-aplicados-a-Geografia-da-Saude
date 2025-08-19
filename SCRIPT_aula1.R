getwd()

install.packages("rmarkdown")
library(rmarkdown)

## A partir dos vetores a seguir e das seleções segundo estação do ano, 
  ## forneça os totais e as médias de cada estação do ano para prec (b) e para temp (a).

# a = temperatura
a <- c(23.1, 23.5, 22.5, 21.2, 18.4, 17.5, 17.2, 18.1, 19.1, 20.5, 21.2, 22.6)

# b = preciptação
b <- c(292, 257, 229, 87, 66, 59, 48, 32, 83, 127, 143, 231)

meses <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")

# nomear os meses com os valores das variáveis
names(a) <- c(meses)
names(b) <- c(meses)
a #imprime os meses e os respectivos valores 


#primavera: setembro, outubro e novembro (9,10,11)
a_primavera <- a[c(9, 10, 11)]
b_primavera <- b[c(9, 10, 11)]
b_primavera

#verão: dezembro, janeiro, fevereiro (12,1,2)
a_verao <- a[c(12, 1, 2)]
b_verao <- b[c(12, 1, 2)]

#outono: março, abril e maio (3,4,5)
a_outono <- a[c(3, 4, 5)]
b_outono <- b[c(3, 4, 5)]

#inverno: junho, julho e agosto (6,7,8)
a_inverno <- a[c(6, 7, 8)]
b_inverno <- b[c(6, 7, 8)]


# (a) temp
total_temp_inverno <- sum(a_inverno)
media_temp_inverno <- mean(a_inverno)

total_temp_outono <-  sum(a_outono)
media_temp_outono <-  mean(a_outono)

total_temp_primavera <- sum(a_primavera)
media_temp_primavera <- mean(a_primavera)

total_temp_verao <-  sum(a_verao)
media_temp_verao <-  mean(a_verao)

#(b) prec
total_prec_inverno <- sum(b_inverno)
media_prec_inverno <- mean(b_inverno)

total_prec_outono <-  sum(b_outono)
media_prec_outono <-  mean(b_outono)

total_prec_primavera <- sum(b_primavera)
media_prec_primavera <- mean(b_primavera)

total_prec_verao <-  sum(b_verao)
media_prec_verao <-  mean(b_verao)

# valores:
total_temp_inverno
total_temp_outono
total_temp_primavera
total_temp_verao

media_temp_inverno
media_temp_outono
media_temp_primavera
media_temp_verao

total_prec_inverno
total_prec_outono
total_prec_primavera
total_prec_verao

media_prec_inverno
media_prec_outono
media_prec_primavera
media_prec_verao

# 2. Escreva um código para verificar se chove mais no verão ou no inverno em São Paulo.

total_prec_verao > total_prec_inverno


#  3. Verifique se a temperatura média no outono é diferente da do inverno

media_temp_outono != media_temp_inverno


# 4. Qual seria a precipitação em cada mês do ano se chovesse o triplo? 

b * 3


#  5. Qual a diferença de precipitação entre primavera e verão (nesta ordem)? Chame a nova variável de dif_prec. Imprima o resultado.

dif_prec <- b_primavera - b_verao
total_dif_prec <- sum(dif_prec)

total_dif_prec


# 6. Gere um vetor chamado prof com as seguintes profissões respondidas por 12 pessoas: geógrafo, biológo, engenheiro, professor, geógrafo, biólogo, professor, professor, professor, engenheiro, engenheiro, professor. Crie um fator chamado prof_fator a partir dos mesmos dados. Rode a função summary para prof e para prof_fator. O que você observa de diferente? Por que isso acontece?

prof <- c("geógrafo", "biólogo", "engenheiro", "professor", "geógrafo", "biólogo", "professor", "professor", "professor", "engenheiro", "engenheiro", "professor")

prof_factor <- factor(prof)

summary(prof)
summary(prof_factor)
