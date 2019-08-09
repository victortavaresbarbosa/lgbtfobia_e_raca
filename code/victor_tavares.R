##################################################################################
################### Analise de dados - trabalho final ############################
###################          Victor Tavares           ############################
###################            2019.1                 ############################
##################################################################################

# --------------------------------------------------------------------------------

# Questao de pesquisa: Pessoas LGBTs negras foram as maiores vitimas de 
#                      violencia durante os contextos eleitoral e pos-eleitoral?


# H0: A questao racial nao influenciou nos numeros de vitimas de violencia durante
#     os contextos eleitoral e pos-eleitoral.

#HA: Pessoas LGBTs negras foram as maiores vitimas de violencia durante os contextos
#   eleitoral e pos-eleitoral.

#HI: Renda media os efeitos de raca sobre vitimizacao.

# Variaveis de controle: genero, orientacao sexual, escolaridade, renda, 
#              raca

# VI: raca

# VD: frequencia de vitimizacao\vitimizacao


# ----------------------------------------------------------------------------------

# Instalando bibliotecas

install.packages("sandwich")
install.packages("lmtest") 
install.packages(c('sjPlot','sjmisc'))

# Importante bibliotecas uteis para o decorrer do trabalho

library(readxl)
library(stringr)
library(ggplot2)
library(car)
library(stargazer)
library(sandwich)
library(lmtest)
library(sjPlot)
library(sjmisc)


# -----------------------------------------------------------------------------------

# Carregando a base de dados ####

base <- read_xlsx("C:/Users/DELL/Documents/2019.1/victor_tavares_ad_final/Dados_Abertos_Violencia_LGBT+nas Eleicoes.xlsx", 1)

# Observando se a base foi importada corretamente

head(base) # primeiras 10 linhas
tail(base) # ultimas 10 linhas 

# --------------------------------------------------------------------------------------

# Selecionando as variaveis ####

colnames(base) # confirmando o nome das colunas 

# Criando novas variaveis 
#  criou-se uma 'nova' variavel e a adicionou a base, para encurtar seu nome por questoes organizacionais

base$orientacao <- base$`Qual sua orientação sexual?` 

base$genero <- base$`Qual sua identidade de gênero?`

base$raca <- base$`Como você define sua raça/cor de pele?`

base$renda <- base$`Qual sua renda familiar?`

base$escolaridade <- base$`Qual sua escolaridade?`

base$frequencia_vitimizacao <- base$`P.10- Se sim, com qual frequência ocorreu/ocorreram?`

base$vitimizacao <- base$`P.9- Você sofreu algum tipo de violência motivada por sua orientação sexual e/ou identidade de gênero durante as eleições de 2018?`

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
nova_base$escolaridade <- ifelse(nova_base$escolaridade %in% c("ensino medio completo", "ensino médio completo"), '3', nova_base$escolaridade) # funcao %in% usada para testar se os valores da variavel pertencem a um grupo de valores ou nao
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

class(nova_base$raca)


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

# Grafico de cada variável 

# (figura) Grafico  para a variavel raca mostrando a quantidade de cada elemento -----------------------

table(base$raca) # observando a tabela no console para preencher o data.frame

teste.raca <- data.frame(rotulos = c("amarela", "branca", "indigena", "outra", "parda", "preta"),
                         frequencia = c(9, 172, 11, 10, 89, 108))


ggplot(teste.raca, aes(y = frequencia, x = rotulos)) + # componenetes basicos do grafico
 geom_histogram(colour = "black", fill="black", width=.8, stat="identity") +
               xlab("Raça/cor da pele") + ylab("Frequência") +
               ggtitle("Figura 11: raça/cor da pele")


# (figura) Gráfico de barras para a varivael dummy raca -----------------------------------------------------

table(nova_base$raca) #visualizando

# construindo um dataframe separado


g.raca <- data.frame(raça = c("Não-negros", "negros"), # rotulos das barras
                     frequencia = c(203, 197))             # frequencia das barras

ggplot(g.raca, aes(y = frequencia, x = raça)) + # componenetes basicos do grafico
  geom_bar(stat = "identity") +   #  stat = "identity" por padrao
  ggtitle("Figura 12: negros e não-negros")

# (Figura 4) Graficos de barras empilhadas (vitimizacao vs raca) associação #### -------------------------------------------------------

t.raca.vit <- table(nova_base$raca,nova_base$vitimizacao) # salvando tabela de frequencia conjunta das variaveis

# nome das linhas
row.names(t.raca.vit) <- c("Não-negro", "Negro") # inserindo rotulos de cor

# nome das colunas
colnames(t.raca.vit) <- c("Não-vítima", "Vítima") # inserindo rotulos de vitimizacao

# Transformando par porcentagem

t.raca.vit["Não-negro",] <- t.raca.vit["Não-negro",] / sum(t.raca.vit["Não-negro",])
t.raca.vit["Negro",] <- t.raca.vit["Negro",] / sum(t.raca.vit["Negro",])

t.raca.vit # consultando tabela

g.raca.vit <- data.frame(cor =       c("Não-negro", "Negro",    "Não-negro","Negro"),
                         vitima =    c("Não-vítima", "Não-vítima","Vítima",    "Vítima"),
                         frequencia= c(0.4941860 , 0.484581, 0.5058140, 0.5154185 ))



ggplot(g.raca.vit, aes(y = frequencia, # frequencia das barras
                       x = cor,    # nome das barras 
                       fill = vitima)) + # nome da divisoria das barras
  geom_bar(stat = 'identity') + 
  labs(y = "Frequência", x = "", fill = "Vitimização") + #alterando os rotulos
  ggtitle("Figura 4: Vitimização associada à raça") +
  geom_hline(yintercept = 0.5)
  


# (figura 2) Grafico de variavel numerica frequencia de vitimizacao ---------------------------------------------------------------

# construindo um dataframe separado
t.vitimizacao <-table(nova_base$frequencia_vitimizacao)

g.vitimizacao <- data.frame(rotulos = names(t.vitimizacao),
                            frequencia = c(t.vitimizacao),
                            ordem = 1:length(t.vitimizacao))

g.vitimizacao$rotulos <- reorder(g.vitimizacao$rotulos, g.vitimizacao$ordem)

ggplot(g.vitimizacao, aes(y = frequencia, x = rotulos)) + 
  geom_histogram(colour = "black", fill="red4", width=.8, stat="identity") +
  xlab("Frequência de vitimização") + ylab("Quantidade") +
  ggtitle("Figura 2: Freqência de vitimização")

# (figura 1) Grafico  para vitimizacao ##### ---------------------------------------------------------------------------

t.vit <- table(nova_base$vitimizacao)

g.vit <- data.frame(rotulos = names(t.vit),
                            frequencia = c(t.vit),
                            ordem = 1:length(t.vit))

g.vit$rotulos <- reorder(g.vit$rotulos, g.vit$ordem)

ggplot(g.vit, aes(y = frequencia, x = rotulos)) + 
  geom_histogram(colour = "black", fill="blue", width=.8, stat="identity") +
  ggtitle("Figura 1: Vitimização")

# (figura 8) Grafico  da variavel numerica renda ----------------------------------------------------------------

t.renda <- table(base$renda)  # foi usada os dados da "base" e não "nova_base" uma vez que em "base"
                              # ainda nao havia sido feita a transformacao dos dados pro modelo

g.renda <- data.frame(rotulos = names(t.renda),
                      renda = c(t.renda),
                      ordem = 1:length(t.renda))

g.renda$rotulos <- reorder(g.renda$rotulos, g.renda$ordem)

ggplot(g.renda, aes(y = renda, x = rotulos)) +
  geom_histogram(stat = "identity") +
  ggtitle("Figura 8: Distribuição de renda")

# (figura 14) Graficoda variavel genero --------------------------------------------------------------------

t.genero <- table(base$genero) # foi usada os dados da "base" e não "nova_base" uma vez que em "base"
                               # ainda nao havia sido feita a transformacao dos dados pro modelo

g.genero <- data.frame(rotulos = names(t.genero),
                       quantidade = c(t.genero),
                       ordem = 1:length(t.genero))

ggplot(g.genero, aes(y = quantidade, x = rotulos)) +
  geom_bar(colour = "black", fill="#E69F00", width=.8, stat="identity") +
  xlab("Gêneros") + ylab("Quantidade") +
  ggtitle("Figura 14: Identidades de gênero")

# (Figura 13) Grafico da variavel orientacao ----------------------------------------------------------------

t.oriencao <- table(base$orientacao) # foi usada os dados da "base" e não "nova_base" uma vez que em "base"
                                     # ainda nao havia sido feita a transformacao dos dados pro modelo

g.orientacao <- data.frame(rotulos = names(t.oriencao),
                       frequencia = c(t.oriencao),
                       ordem = 1:length(t.oriencao))

ggplot(g.orientacao, aes(y = frequencia, x = rotulos)) +
  geom_bar(colour = "black", fill="#E69F00", width=.8, stat="identity") + 
  guides(fill=FALSE) +
  xlab("Orientações") + ylab("Quantidade") +
  ggtitle("Figura 13: Orientações sexuais")


# (figura 15) Grafico variavel escolaridade #### ---------------------------------------------------------------------

t.escolaridade <- table(nova_base$escolaridade)

gg.escolaridade <- data.frame(rotulos = names(t.escolaridade),
                             frequencia = c(t.escolaridade),
                             ordem = 1:length(t.escolaridade))

ggplot(gg.escolaridade, aes(y = frequencia, x = rotulos)) +
  geom_bar(colour = "black", fill="#E69F00", width=.8, stat="identity") + 
  guides(fill=FALSE) +
  xlab("Nível de estudos") + ylab("Frequência") +
  ggtitle("Figura 15: Escolaridade")


# (figura 17) Grafico de orientacao associado a vitimizacao -----------------------------------------------------

nova_base$orientacao_g <- base$orientacao # foi usada os dados da "base" e não "nova_base" uma vez que em "base"
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
                               vitima = c("Não-vítima", "vítima", "Não-vítima", "vítima", "Não-vítima", "vítima"),
                               frequencia = c(0.5545455, 0.4454545, 0.5063291, 0.4936709,0.4315789,0.5684211))


ggplot(g.orientacao.vit, aes(y =frequencia, x = orientacao, fill = vitima)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 0.5) +
  ggtitle("Figura 17: Vitimização associada à orientação")

# (ficura 6) grafico de renda associado a vitimizacao ----------------------------------------------------------------------------------------

nova_base$renda_g <- base$renda # foi usada os dados da "base" e não "nova_base" uma vez que em "base"
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
                               vitima = c("Não-vítima", "vítima", "Não-vítima", "vítima", "Não-vítima", "vítima", "Não-vítima", "vítima", "Não-vítima", "vítima", "Não-vítima", "vítima"),
                               frequencia = c(0.4596273, 0.5403727, 0.4711538, 0.5288462, 0.4722222, 0.5277778, 0.6666667, 0.3333333, 0.4166667, 0.5833333, 0.7333333, 0.2666667))

ggplot(g.renda.vit, aes(y =frequencia, x = renda, fill = vitima)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 0.5) +
  ggtitle("Figura 6: Vitimização associada à renda")

# (figura 10) grafico de renda associado a raca #### -------------------------------------------------------------------

nova_base$renda_g <- base$renda # foi usada os dados da "base" e não "nova_base" uma vez que em "base"
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
                           raça = c("Não-negro", "Negro","Não-negro", "Negro","Não-negro", "Negro","Não-negro", "Negro","Não-negro", "Negro","Não-negro", "Negro"),
                           frequencia = c(0.4409938, 0.5590062, 0.5288462, 0.4711538, 0.5277778, 0.4722222, 0.5714286, 0.4285714, 0.5208333, 0.4791667, 0.7000000, 0.3000000))


ggplot(g.raca.renda, aes(y =frequencia, x = renda, fill = raça)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = 0.5) +
  ggtitle("Figura 10: Renda associada à raça")

# (figura 16) graficoboxplot com a variavel escolaridade ---------------------------------------------

g.escolaridade <- data.frame(variavel = nova_base$escolaridade)

g.escolaridade = na.omit(g.escolaridade) # omitir os NAS

ggplot(g.escolaridade, aes(y = variavel)) +
  geom_boxplot() +
  ggtitle("Figura 16: Boxplot de escolaridade")

# (figura 9) grafico boxplot com a variavel renda  -----------------------------------------------------------

g.box.renda <- data.frame(variavel = nova_base$renda)

ggplot(g.box.renda, aes(y = variavel)) +
  geom_boxplot() +
  ggtitle("Figura 9: Boxplot de renda")


## (figura 3)  grafico boxplot com frequencia de vitimizacao -------------------------------------------------

g.frequencia <- data.frame((variavel = nova_base$frequencia_vitimizacao))

ggplot(g.frequencia, aes(y= variavel)) +
  geom_boxplot(fill = '#4271AE', line = "#1F3552") +
  ggtitle("Figura 3: Boxplot - Frequência de vitimização")

# (figura 5) grafico boxplot bivariado com frequencia de vitimizacao associedado a raca -----------------------------------------------

g.frequencia.biv <- data.frame(frequencia = nova_base$frequencia_vitimizacao,
                               raca = nova_base$raca) # criando objeto para o grafico

table(nova_base$raca) #observando mais uma vez a variavel raca

g.frequencia.biv$raca <- ifelse(g.frequencia.biv$raca == 1, "Negro", "Não-negro") # criando um novo objeto agora adicionado rotulos a dummy

g.frequencia.biv <- na.omit(g.frequencia.biv) # omitindo os NAs

ggplot(g.frequencia.biv, aes(y = frequencia, x= raca)) +
  geom_boxplot() +
  ggtitle("Figura 5: Frequência de vitimização associada à raça")

# Modelo de regressao ###########################################################################

# O ajuste sera amarzenado no objeto reg

reg <- lm(data = nova_base, frequencia_vitimizacao ~ raca * renda + 
            escolaridade + factor(genero)+ factor(orientacao)) # construindo o modelo da regressao 
                                                                # as variaveis categorias sao inseridas como fatores (factor)

reg$coefficients # estimativas do parametro

summary(reg) # resultado da regressao

confint(reg) #intervalo de confiança 

stargazer(reg,    # alternativa para visualizar os resultados da regressao
          type = "text",
          header = FALSE,
          title = "Tabela de resultados",
          style = "ajps",
          p.auto = FALSE)

# (figura 7)Grafico de interacao -------------------------------------------------------------------------------

plot_model(reg, type = "pred", terms = c("renda", "raca")) # grafico da reg




# Pressupostos do modelo -----------------------------------------------------------------------------------

# heterosedasticidade e homocedasticidade (distruibuicao dos erros ao longo dos valores preditos) ------------------------------
# Análise visual para homogeneidade dos resíduos (visualmente eles devem se distribuir igualmente #abaixo e acima da linha



res.prev <- data.frame(residuos = residuals(reg), #criando data frame
                       previstos = predict(reg))

ggplot(res.prev, aes(y = residuos, x = previstos)) + # grafico de residuos vs preditos
  geom_point() +
  geom_abline(slope=0, intercept=0)


plot(reg, which = 1) #outra opcao de visualizao do grafico


# nos valores menores esta errando pra mais, no meio errando relativamente menos e nos valores maiores ta errando pra menos

# Aplicando função sobre o modelo de regressão para diminuir a heterosedasticidade

coeftest(reg,vcov. = vcovHC)

# histogramama (normalidade) ----------------------------------------------------------------------------------------7

hist(residuals(reg)) # histograma

plot(reg, which = 2) # grafico de normal q-q


shapiro.test(reg$residuals) # (valores de p > 0,05 indicam dados normais)
                            # no entanto, o resultado é 2.2e-16, demostrando a falta de normalidade
                            # a hipótese de distribuicao normal foi rejeitada 

# media = 0------------------------------------------------------------------------------------------------------

mean(residuals(reg))

#  multicolinearidade -------------------------------------------------------------------------------------

vif(reg)  #VIF (Variance Inflation Factor)
          # VIF mede a correlação da variável com todas as outras do modelo
# genero \ orientacao problematica (se for maior que dois é problematico)



save.image("myWorkSpace.RData") # facilita a criacao do rmd














  
  
  
