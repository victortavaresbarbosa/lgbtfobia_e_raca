##################################################################################
################### Analise de dados - trabalho final ############################
###################          Victor Tavares           ############################
##################################################################################

# --------------------------------------------------------------------------------

# Questao de pesquisa: Pessoas LGBTs negras foram as maiores vitimas de 
#                      violencia durante os contextos eleitoral e pos-eleitoral?


# H0: A quest√£a racial nao influenciou nos numeros de vitimas de violencia durante
#     os contextos eleitoral e pos-eleitoral.

#HA: Pessoas LGBTs negras foram as maiores vitimas de violencia durante os contextos
#   eleitoral e pos-eleitoral.

#HI: Renda media os efeitos de raca sobre vitimizacao.

# Variaveis de controle: genero, orientacao sexual, escolaridade, renda, 
#              raca

# VI: raca

# VD: frequencia de vitimizacao\vitimizacao


# ----------------------------------------------------------------------------------

# Importante bibliotecas uteis para o decorrer do trabalho

library(readxl)
library(stringr)
library(ggplot2)
library(car)
library(ggpubr)
if(!require(devtools)) install.packages("devtools")
install.packages("tidyselect")
# -----------------------------------------------------------------------------------

# Carregando a base de dados ####

base <- read_xlsx("C:/Users/DELL/Documents/2019.1/victor_tavares_ad_final/basededados_victor_tavares.xlsx", 1)

# Observando se a base foi importada corretamente

head(base) # primeiras 10 linhas
tail(base) # ultimas 10 linhas 

# --------------------------------------------------------------------------------------

# Selecionando as variaveis ####

colnames(base) # confirmando o nome das colunas 

# Criando novas variaveis 
#  criou-se uma 'nova' variavel e a adicionou a base, para encurtar seu nome por questoes organizacionais

base$orientacao <- base$`Qual sua orientaÁ„o sexual?` 

base$genero <- base$`Qual sua identidade de gÍnero?`

base$raca <- base$`Como vocÍ define sua raÁa/cor de pele?`

base$renda <- base$`Qual sua renda familiar?`

base$escolaridade <- base$`Qual sua escolaridade?`

base$frequencia_vitimizacao <- base$`P.10- Se sim, com qual frequÍncia ocorreu/ocorreram?`

base$vitimizacao <- base$`P.9- VocÍ sofreu algum tipo de violÍncia motivada por sua orientaÁ„o sexual e/ou identidade de gÍnero durante as eleiÁıes de 2018?`

# --------------------------------------------------------------------------------------------

# Observando com mais profundidade a VD frequencia de vitimizacao

str(base$frequencia_vitimizacao) #identificando o tipo da variavel 

table(base$frequencia_vitimizacao) # frequencia que os dados aparecem 

# --------------------------------------------------------------------------------------------

# Separando as variaveis que serao utilizadas no modelo em um objeto menor 

variaveis <- c('orientacao', 'genero', 'raca', 'renda', 'escolaridade', 
               'frequencia_vitimizacao', 'vitimizacao')

# criando uma nova base apenas com as variaveis que usaremos no modelo ################################

nova_base <- base[, variaveis] 


# ----------------------------------------------------------------------------------------------------

# Transformacao das variaveis ####

# Funcao ifelse utilizada como principal ferramenta 

# Transformando a variavel frequencia_vitimizacao #### ------------------------------------------------

table(nova_base$frequencia_vitimizacao) # visualizando a variavel 

nova_base$frequencia_vitimizacao <- ifelse(nova_base$frequencia_vitimizacao == "duas vezes", "2", nova_base$frequencia_vitimizacao)
nova_base$frequencia_vitimizacao <- ifelse(nova_base$frequencia_vitimizacao == "mais de tres vezes", "4", nova_base$frequencia_vitimizacao)
nova_base$frequencia_vitimizacao <- ifelse(nova_base$frequencia_vitimizacao == "NA", NA, nova_base$frequencia_vitimizacao)
nova_base$frequencia_vitimizacao <- ifelse(nova_base$frequencia_vitimizacao == "tres vezes", "3", nova_base$frequencia_vitimizacao)
nova_base$frequencia_vitimizacao <- ifelse(nova_base$frequencia_vitimizacao == "uma vez", "1", nova_base$frequencia_vitimizacao)

table(nova_base$frequencia_vitimizacao) # visualizando a variavel com os valores transformados

nova_base$frequencia_vitimizacao <- as.numeric(nova_base$frequencia_vitimizacao) # transformando de categorica para numerica

summary(nova_base$frequencia_vitimizacao) # verificando transformacao

# Corrigindo falsos NAs da variavel frequencia_vitimizacao com os dados da variavel vitimizacao


table(nova_base$vitimizacao) # visualizando a variavel

nova_base$frequencia_vitimizacao <- ifelse(nova_base$vitimizacao %in% c("nao", "nao sabe/nao respondeu"), 0, nova_base$frequencia_vitimizacao) # transformando em dummy

summary(nova_base$frequencia_vitimizacao) # confirmando

# Transformando a variavel vitimizacao em dummy #### ---------------------------------------------------------

nova_base$vitimizacao <- ifelse(nova_base$vitimizacao == "sim", 1, 0)

table(nova_base$vitimizacao)

# Transformando a variavel escolariade #### ----------------------------------------------------

table(nova_base$escolaridade) # visualisando a variavel

nova_base$escolaridade <- ifelse(grepl("fundamental", nova_base$escolaridade), "1", nova_base$escolaridade)
nova_base$escolaridade <- ifelse(nova_base$escolaridade == "ensino medio incompleto", 2, nova_base$escolaridade)
nova_base$escolaridade <- ifelse(nova_base$escolaridade %in% c("ensino medio completo", "ensino mÈdio completo"), '3', nova_base$escolaridade) # funcao %in% usada para testar se os valores da variavel pertencem a um grupo de valores ou nao
nova_base$escolaridade <- ifelse(nova_base$escolaridade == "ensino superior incompleto", 4, nova_base$escolaridade)
nova_base$escolaridade <- ifelse(nova_base$escolaridade == "ensino superior completo", 5, nova_base$escolaridade)
nova_base$escolaridade <- ifelse(nova_base$escolaridade == "pos-graduacao incompleto", 6, nova_base$escolaridade)
nova_base$escolaridade <- ifelse(nova_base$escolaridade == "pos-graduacao completo", 7, nova_base$escolaridade)

table(nova_base$escolaridade) # confirmando 

nova_base$escolaridade <- as.numeric(nova_base$escolaridade) #  transformando de categorica para numerica

summary(nova_base$escolaridade) # confirmando transformando do tipo

# Transformando a varivavel raca em dummy #### -------------------------------------------------------------

table(nova_base$raca) # visualizando 

nova_base$raca <- ifelse(nova_base$raca %in% c("preta", "parda"), 1, 0)

table(nova_base$raca) # confirmando


# Transformando a variavel orientacao ####  -------------------------------------------------------------

table(nova_base$orientacao) # visualizando a variavel
class(nova_base$orientacao) # confirmando seu tipo

# "heterosexual" e "pansexual" foram alocamos juntamente a "outra" devido a quantidade reduzida de observacoes

nova_base$orientacao <- ifelse(nova_base$orientacao == "heterossexual", "outra", nova_base$orientacao)
nova_base$orientacao <- ifelse(nova_base$orientacao == "pansexual", "outra", nova_base$orientacao)



# Transformando a variavel renda #### -------------------------------------------------------------------

table(nova_base$renda) # visualizando a variavel

nova_base$renda <- ifelse(nova_base$renda == 'ate 1 S.M', '1', nova_base$renda)
nova_base$renda <- ifelse(nova_base$renda == '1 a 3 S.M', '2', nova_base$renda)
nova_base$renda <- ifelse(nova_base$renda == '3 a 5 S.M', '3', nova_base$renda)
nova_base$renda <- ifelse(nova_base$renda == '5 a 7 S.M', '4', nova_base$renda)
nova_base$renda <- ifelse(nova_base$renda == '7 a 10 S.M', '5', nova_base$renda)
nova_base$renda <- ifelse(nova_base$renda == 'mais de 10 S.M', '6', nova_base$renda)

table(nova_base$renda) # confirmando

nova_base$renda <- as.numeric(nova_base$renda) # transformando de categorica para numerica

# Transformando a variavel genero #### --------------------------------------------------------------------

table(nova_base$genero)  # visualizando

class(nova_base$genero) # conferindo o tipo

# assim como ocorreu na variavel orientacao, ha poucas observacoes na base das indentidades de genero abaixo,
# por isso foram alocadas no objeto "outra"

nova_base$genero <- ifelse(nova_base$genero == 'homem trans', 'outra', nova_base$genero)
nova_base$genero <- ifelse(nova_base$genero == 'mulher trans', 'outra', nova_base$genero)
nova_base$genero <- ifelse(nova_base$genero == 'pessoa nao binaria', 'outra', nova_base$genero)
nova_base$genero <- ifelse(nova_base$genero == 'travesti', 'outra', nova_base$genero)

table(nova_base$genero) # confirmando a transformacao


class(nova_base$genero) # confirmando a transformacao de tipo

# --------------------------------------------------------------------------------------------------
# Graficos #########################################################################################

# Grafico de cada vari·vel 

# Grafico 1 para a variavel raca mostrando a quantidade de cada elemento -----------------------

table(base$raca) # observando a tabela no console para preencher o data.frame

teste.raca <- data.frame(rotulos = c("amarela", "branca", "indigena", "outra", "parda", "preta"),
                         frequencia = c(9, 172, 11, 10, 89, 108))


ggplot(teste.raca, aes(y = frequencia, x = rotulos)) + # componenetes basicos do grafico
 geom_histogram(colour = "black", fill="black", width=.8, stat="identity") +
               xlab("RaÁa/cor da pele") + ylab("FrequÍncia") +
               ggtitle("Gr·fico 1: raÁa/cor da pele")


# Grafico 2 de barras para a varivael dummy raca -----------------------------------------------------

table(nova_base$raca) #visualizando

# construindo um dataframe separado


g.raca <- data.frame(raÁa = c("N„o-negros", "negros"), # rotulos das barras
                     frequencia = c(203, 197))             # frequencia das barras

ggplot(g.raca, aes(y = frequencia, x = raÁa)) + # componenetes basicos do grafico
  geom_bar(stat = "identity") +   #  stat = "identity" por padrao
  ggtitle("Gr·fico 2: negros e n„o-negros")

# Graficos 13 de barras empilhadas (vitimizacao vs raca) associaÁ„o #### -------------------------------------------------------

t.raca.vit <- table(nova_base$raca,nova_base$vitimizacao) # salvando tabela de frequencia conjunta das variaveis

# nome das linhas
row.names(t.raca.vit) <- c("N„o-negro", "Negro") # inserindo rotulos de cor

# nome das colunas
colnames(t.raca.vit) <- c("N„o-vÌtima", "VÌtima") # inserindo rotulos de vitimizacao

# Transformando par porcentagem

t.raca.vit["N„o-negro",] <- t.raca.vit["N„o-negro",] / sum(t.raca.vit["N„o-negro",])
t.raca.vit["Negro",] <- t.raca.vit["Negro",] / sum(t.raca.vit["Negro",])

t.raca.vit # consultando tabela

g.raca.vit <- data.frame(cor =       c("N„o-negro", "Negro",    "N„o-negro","Negro"),
                         vitima =    c("N„o-vÌtima", "N„o-vÌtima","VÌtima",    "VÌtima"),
                         frequencia= c(0.4941860 , 0.484581, 0.5058140, 0.5154185 ))



ggplot(g.raca.vit, aes(y = frequencia, # frequencia das barras
                       x = cor,    # nome das barras 
                       fill = vitima)) + # nome da divisoria das barras
  geom_bar(stat = 'identity') + 
  labs(y = "FrequÍncia", x = "", fill = "VitimizaÁ„o") + #alterando os rotulos
  ggtitle("Gr·fico 13: VitimizaÁ„o associada ‡ raÁa") +
  geom_hline(yintercept = 0.5)
  


# grafico 11 de variavel numerica (frequencia de vitimizacao) ---------------------------------------------------------------

# construindo um dataframe separado
t.vitimizacao <-table(nova_base$frequencia_vitimizacao)

g.vitimizacao <- data.frame(rotulos = names(t.vitimizacao),
                            frequencia = c(t.vitimizacao),
                            ordem = 1:length(t.vitimizacao))

g.vitimizacao$rotulos <- reorder(g.vitimizacao$rotulos, g.vitimizacao$ordem)

ggplot(g.vitimizacao, aes(y = frequencia, x = rotulos)) + 
  geom_histogram(colour = "black", fill="red4", width=.8, stat="identity") +
  xlab("FrequÍncia de vitimizaÁ„o") + ylab("Quantidade") +
  ggtitle("Gr·fico 11: FreqÍncia de vitimizaÁ„o")

# Grafico 10 para vitimizacao ##### ---------------------------------------------------------------------------

t.vit <- table(nova_base$vitimizacao)

g.vit <- data.frame(rotulos = names(t.vit),
                            frequencia = c(t.vit),
                            ordem = 1:length(t.vit))

g.vit$rotulos <- reorder(g.vit$rotulos, g.vit$ordem)

ggplot(g.vit, aes(y = frequencia, x = rotulos)) + 
  geom_histogram(colour = "black", fill="blue", width=.8, stat="identity") +
  ggtitle("Gr·fico 10: VitimizaÁ„o")

# Grafico 7 da variavel numerica (renda) ----------------------------------------------------------------

t.renda <- table(base$renda)  # foi usada os dados da "base" e n„o "nova_base" uma vez que em "base"
                              # ainda nao havia sido feita a transformacao dos dados pro modelo

g.renda <- data.frame(rotulos = names(t.renda),
                      renda = c(t.renda),
                      ordem = 1:length(t.renda))

g.renda$rotulos <- reorder(g.renda$rotulos, g.renda$ordem)

ggplot(g.renda, aes(y = renda, x = rotulos)) +
  geom_histogram(stat = "identity") +
  ggtitle("Gr·fico 7: DistribuiÁ„o de renda")

# Grafico 4 da variavel genero --------------------------------------------------------------------

t.genero <- table(base$genero) # foi usada os dados da "base" e n„o "nova_base" uma vez que em "base"
                               # ainda nao havia sido feita a transformacao dos dados pro modelo

g.genero <- data.frame(rotulos = names(t.genero),
                       quantidade = c(t.genero),
                       ordem = 1:length(t.genero))

ggplot(g.genero, aes(y = quantidade, x = rotulos)) +
  geom_bar(colour = "black", fill="#E69F00", width=.8, stat="identity") +
  xlab("GÍneros") + ylab("Quantidade") +
  ggtitle("Gr·fico 4: Identidades de gÍnero")

# Grafico 3 da variavel orientacao ----------------------------------------------------------------

t.oriencao <- table(base$orientacao) # foi usada os dados da "base" e n„o "nova_base" uma vez que em "base"
                                     # ainda nao havia sido feita a transformacao dos dados pro modelo

g.orientacao <- data.frame(rotulos = names(t.oriencao),
                       frequencia = c(t.oriencao),
                       ordem = 1:length(t.oriencao))

ggplot(g.orientacao, aes(y = frequencia, x = rotulos)) +
  geom_bar(colour = "black", fill="#E69F00", width=.8, stat="identity") + 
  guides(fill=FALSE) +
  xlab("OrientaÁıes") + ylab("Quantidade") +
  ggtitle("Gr·fico 3: OrientaÁıes sexuais")


# Grafico 5 - variavel escolaridade #### ---------------------------------------------------------------------

t.escolaridade <- table(nova_base$escolaridade)

gg.escolaridade <- data.frame(rotulos = names(t.escolaridade),
                             frequencia = c(t.escolaridade),
                             ordem = 1:length(t.escolaridade))

ggplot(gg.escolaridade, aes(y = frequencia, x = rotulos)) +
  geom_bar(colour = "black", fill="#E69F00", width=.8, stat="identity") + 
  guides(fill=FALSE) +
  xlab("NÌvel de estudos") + ylab("FrequÍncia") +
  ggtitle("Gr·fico 5: Escolaridade")


# Grafico de orientacao associado a vitimizacao -----------------------------------------------------

nova_base$orientacao_g <- base$orientacao # foi usada os dados da "base" e n„o "nova_base" uma vez que em "base"
                                          # ainda nao havia sido feita a transformacao dos dados pro modelo

t.oriencao.vit <- table(nova_base$orientacao_g, nova_base$vitimizacao) # salvando tabela de frequencia de todas as variaveis do grafico 

# transformando os valores do objeto t.orientacao.vit em decimais 

t.oriencao.vit['bissexual',] <- t.oriencao.vit['bissexual',] / sum(t.oriencao.vit['bissexual',])
t.oriencao.vit['gay',] <- t.oriencao.vit['gay',] / sum(t.oriencao.vit['gay',])
t.oriencao.vit['heterossexual',] <- t.oriencao.vit['heterossexual',] / sum(t.oriencao.vit['heterossexual',])
t.oriencao.vit['lesbica',] <- t.oriencao.vit['lesbica',] / sum(t.oriencao.vit['lesbica',])
t.oriencao.vit['pansexual',] <- t.oriencao.vit['pansexual',] / sum(t.oriencao.vit['pansexual',])

t.oriencao.vit # para visualizar a tabela no console e preencher o data.frame para a criacao do grafico

g.orientacao.vit <- data.frame(orientacao = c('bissexual', 'bissexual', 'gay', 'gay', 'lesbica', 'lesbica'), # foi usado apenas bissexual, gay e lesbica devido a quantidade reduzida de observacoes das outras orientacoes
                               vitima = c("N„o-vÌtima", "vÌtima", "N„o-vÌtima", "vÌtima", "N„o-vÌtima", "vÌtima"),
                               frequencia = c(0.5545455, 0.4454545, 0.5063291, 0.4936709,0.4315789,0.5684211))


ggplot(g.orientacao.vit, aes(y =frequencia, x = orientacao, fill = vitima)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 0.5) +
  ggtitle("Gr·fico 16: VitimizaÁ„o associada ‡ orientaÁ„o")

# grafico 15 de renda associado a vitimizacao ----------------------------------------------------------------------------------------

nova_base$renda_g <- base$renda # foi usada os dados da "base" e n„o "nova_base" uma vez que em "base"
                                # ainda nao havia sido feita a transformacao dos dados pro modelo

t.renda.vit <- table(nova_base$renda_g, nova_base$vitimizacao) # salvando tabela de frequencia de todas as variaveis do grafico 

table(nova_base$renda_g)


# transformando os valores do objeto t.renda.vit em decimais 

t.renda.vit['1 a 3 S.M',] <- t.renda.vit['1 a 3 S.M',] / sum(t.renda.vit['1 a 3 S.M',])
t.renda.vit['3 a 5 S.M',] <- t.renda.vit['3 a 5 S.M',] / sum(t.renda.vit['3 a 5 S.M',])
t.renda.vit['5 a 7 S.M',] <- t.renda.vit['5 a 7 S.M',] / sum(t.renda.vit['5 a 7 S.M',])
t.renda.vit['7 a 10 S.M',] <- t.renda.vit['7 a 10 S.M',] / sum(t.renda.vit['7 a 10 S.M',])
t.renda.vit['ate 1 S.M',] <- t.renda.vit['ate 1 S.M',] / sum(t.renda.vit['ate 1 S.M',])
t.renda.vit['mais de 10 S.M',] <- t.renda.vit['mais de 10 S.M',] / sum(t.renda.vit['mais de 10 S.M',])

t.renda.vit # para visualizar a tabela no console e preencher o data.frame para a criacao do grafico

g.renda.vit <- data.frame(renda = c('1 a 3 S.M', "1 a 3 S.M", '3 a 5 S.M', '3 a 5 S.M', '5 a 7 S.M', '5 a 7 S.M', '7 a 10 S.M', '7 a 10 S.M', 'ate 1 S.M', 'ate 1 S.M', 'mais de 10 S.M', 'mais de 10 S.M' ),
                               vitima = c("N„o-vÌtima", "vÌtima", "N„o-vÌtima", "vÌtima", "N„o-vÌtima", "vÌtima", "N„o-vÌtima", "vÌtima", "N„o-vÌtima", "vÌtima", "N„o-vÌtima", "vÌtima"),
                               frequencia = c(0.4596273, 0.5403727, 0.4711538, 0.5288462, 0.4722222, 0.5277778, 0.6666667, 0.3333333, 0.4166667, 0.5833333, 0.7333333, 0.2666667))

ggplot(g.renda.vit, aes(y =frequencia, x = renda, fill = vitima)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 0.5) +
  ggtitle("Gr·fico 15: VitimizaÁ„o associada ‡ renda")

# grafico 9 de renda associado a raca #### -------------------------------------------------------------------

nova_base$renda_g <- base$renda # foi usada os dados da "base" e n„o "nova_base" uma vez que em "base"
# ainda nao havia sido feita a transformacao dos dados pro modelo

t.raca.renda <- table(nova_base$renda_g, nova_base$raca)#  salvando tabela de frequencia de todas as variaveis do grafico 

# transformando os valores do objeto t.raca.renda em decimais 

t.raca.renda['1 a 3 S.M',] <- t.raca.renda['1 a 3 S.M',] / sum(t.raca.renda['1 a 3 S.M',])
t.raca.renda['3 a 5 S.M',] <- t.raca.renda['3 a 5 S.M',] / sum(t.raca.renda['3 a 5 S.M',])
t.raca.renda['5 a 7 S.M',] <- t.raca.renda['5 a 7 S.M',] / sum(t.raca.renda['5 a 7 S.M',])
t.raca.renda['7 a 10 S.M',] <- t.raca.renda['7 a 10 S.M',] / sum(t.raca.renda['7 a 10 S.M',])
t.raca.renda['ate 1 S.M',] <- t.raca.renda['ate 1 S.M',] / sum(t.raca.renda['ate 1 S.M',])
t.raca.renda['mais de 10 S.M',] <- t.raca.renda['mais de 10 S.M',] / sum(t.raca.renda['mais de 10 S.M',])

t.raca.renda # para visualizar a tabela no console e preencher o data.frame para a criacao do grafico

g.raca.renda <- data.frame(renda = c('1 a 3 S.M', "1 a 3 S.M", '3 a 5 S.M', '3 a 5 S.M', '5 a 7 S.M', '5 a 7 S.M', '7 a 10 S.M', '7 a 10 S.M', 'ate 1 S.M', 'ate 1 S.M', 'mais de 10 S.M', 'mais de 10 S.M' ),
                           raÁa = c("N„o-negro", "Negro","N„o-negro", "Negro","N„o-negro", "Negro","N„o-negro", "Negro","N„o-negro", "Negro","N„o-negro", "Negro"),
                           frequencia = c(0.4409938, 0.5590062, 0.5288462, 0.4711538, 0.5277778, 0.4722222, 0.5714286, 0.4285714, 0.5208333, 0.4791667, 0.7000000, 0.3000000))


ggplot(g.raca.renda, aes(y =frequencia, x = renda, fill = raÁa)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 0.5) +
  ggtitle("Gr·fico 9: Renda associada ‡ raÁa")

# grafico 6 boxplot com a variavel escolaridade ---------------------------------------------

g.escolaridade <- data.frame(variavel = nova_base$escolaridade)

g.escolaridade = na.omit(g.escolaridade) # omitir os NAS

ggplot(g.escolaridade, aes(y = variavel)) +
  geom_boxplot() +
  ggtitle("Gr·fico 6: Boxplot de escolaridade")

# grafico 9 boxplot com a variavel renda  -----------------------------------------------------------

g.box.renda <- data.frame(variavel = nova_base$renda)

ggplot(g.box.renda, aes(y = variavel)) +
  geom_boxplot() +
  ggtitle("Gr·fico 8: Boxplot de renda")


## grafico 12 boxplot com frequencia de vitimizacao -------------------------------------------------

g.frequencia <- data.frame((variavel = nova_base$frequencia_vitimizacao))

ggplot(g.frequencia, aes(y= variavel)) +
  geom_boxplot(fill = '#4271AE', line = "#1F3552") +
  ggtitle("Gr·fico 12: Boxplot da FrequÍncia de vitimizaÁ„o")

# Grafico 14 boxplot bivariado com frequencia de vitimizacao associedado a raca -----------------------------------------------

g.frequencia.biv <- data.frame(frequencia = nova_base$frequencia_vitimizacao,
                               raca = nova_base$raca) # criando objeto para o grafico

table(nova_base$raca) #observando mais uma vez a variavel raca

g.frequencia.biv$raca <- ifelse(g.frequencia.biv$raca == 1, "Negro", "N„o-negro") # criando um novo objeto agora adicionado rotulos a dummy

g.frequencia.biv <- na.omit(g.frequencia.biv) # omitindo os NAs

ggplot(g.frequencia.biv, aes(y = frequencia, x= raca)) +
  geom_boxplot() +
  ggtitle("Gr·fico 14: FrequÍncia de vitimizaÁ„o associada ‡ raÁa")

# Modelo de regressao ###########################################################################

reg <- lm(data = nova_base, frequencia_vitimizacao ~ raca  + 
            renda + escolaridade + factor(genero) + factor(orientacao)) # construindo o modelo da regressao 


summary(reg) # resultado da regressao

confint(reg) #intervalo de confianÁa 

# Pressupostos do modelo -----------------------------------------------------------------------------------

# heterosedasticidade e homocedasticidade (distruibuicao dos erros ao longo dos valores preditos) ------------------------------

res.prev <- data.frame(residuos = residuals(reg),
                       previstos = predict(reg))

ggplot(res.prev, aes(y = residuos, x = previstos)) +
  geom_point() +
  geom_abline(slope=0, intercept=0)


plot(reg, which = 1)

# nos valores menores esta errando pra mais, no meio errando relativamente menos e nos valores maiores ta errando pra menos

# histogramama (normalidade ----------------------------------------------------------------------------------------7

hist(residuals(reg))


shapiro.test(reg$residuals)


# media = 0------------------------------------------------------------------------------------------------------

mean(residuals(reg))

#  multicolinearidade -------------------------------------------------------------------------------------

vif(reg)

# genero \ orientacao problematica (se for maior que dois È problematico)















  
  
  
