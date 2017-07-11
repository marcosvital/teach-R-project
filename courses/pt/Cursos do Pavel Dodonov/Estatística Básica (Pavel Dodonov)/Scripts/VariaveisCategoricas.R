#############################################
##### Curso de Estatística Básica ###########
#### Trabalhando com variáveis categóricas###
#### Pavel Dodonov ##########################
#### pdodonov@gmail.com #####################
#############################################

### Hoje trabalharemos com variáveis-resposta categóricas.
### A situação mais simples: variáveis binárias.
#### 1 representa ausência, 0 representa ausência.

setwd("/home/pavel/Profissional/Ensino/Disciplinas/EstatisticaBasica_UESC_PPGECB_2017/Planilhas")
dados.sapos <- read.table("Sapos.txt", header=T, sep=",", dec=".")

# Dados cedidos por Amanda Anjos.
# Cada linha é uma espécie de anfíbio (anuros)

str(dados.sapos)
head(dados.sapos)

# Status: Se a espécie está ameaçada (1) ou não (0)
# Desenvolvimento, PeriodoDeAtividade: são variáveis categóricas.
# Desova, AltitudeMaxima: são variáveis contínuas.

# Vamos transformar as contínuas em fatores:
dados.sapos$Desenvolvimento <- as.factor(dados.sapos$Desenvolvimento)
dados.sapos$PeriodoDeAtividade <- as.factor(dados.sapos$PeriodoDeAtividade)
str(dados.sapos)

####### Variáveis explanatórias categóricas

# Gráficos para explorar relações com as variáveis categóricas
par(mfrow=c(2,1))
plot(as.factor(Status)~Desenvolvimento+PeriodoDeAtividade, data=dados.sapos)

# Isso é o que se chama mosaic plot (gráfico em mosaico)
# Traduzindo em números
# Usamos a função table, que trabalha com dataframes ou com matrizes (ou vetores).
tab.Desenvolvimento <- table(dados.sapos[c("Status","Desenvolvimento")])
tab.Desenvolvimento

# Teste de Qui-quadrado
chisq.test(tab.Desenvolvimento)

# Teste exato de Fisher
fisher.test(tab.Desenvolvimento)

# Ou seja, não há associação entre o tipo de desenvolvimento e o grau de ameaça.

# Exercício: Façam isso para o PeriodoDeAtividade.

########## Variável explanatória contínua

plot(Status ~ Desova, data=dados.sapos)
# Se preferirem:
plot(jitter(Status) ~ Desova, data=dados.sapos)

# Sapos com pouca desova parecem mais prováveis de serem ameaçados.

mod.Desova <- glm(Status ~ Desova, data=dados.sapos, family=binomial)
par(mfrow=c(2,2))
plot(mod.Desova)
# Estamos trabalhando com GLM binomial. A interpretação destes gráficos é mais difícil.
dev.off()

summary(mod.Desova)
# Parece não significativo
# Outra forma de avaliar:
mod.null <- glm(Status ~ 1, data=dados.sapos, family=binomial)
AIC(mod.null, mod.Desova)
# Aviso! Número de observações difere (NAs pra Desova)
# As comparações não são válidas!
mod.null <- glm(Status ~ 1, data=subset(dados.sapos, !is.na(Desova)), family=binomial)
AIC(mod.null, mod.Desova)
# AIC muito parecidos.

### Para vermos graficamente, é mais complicadinho mas pode ser feito.
### 1) Criamos um vetor para valores preditos
x.novo <- seq(min(dados.sapos$Desova, na.rm=T), max(dados.sapos$Desova, na.rm=T), length.out=200)
# na.rm=T para o comando remover valores ausentes.
# Criamos uma sequência de 200 números que vai do valor mínimo de desova pro valor máximo.
### 2) Calculamos valores de Y para cada valor de X
y.novo <- predict(mod.Desova, newdata=list(Desova=x.novo), type="response")
# newdata significa que os valores serão preditos para um novo vetor, não para os dados originais.
# list(Desova=x.novo) diz que a variável correspondente à Desova (usada no modelo) será o x.novo
# type="response" é necessário por ser um GLM (ele faz uma transformação pra ajustar o modelo.
### 3) Criamos o gráfico e adicionamos a linha.
plot(Status ~ Desova, data=dados.sapos)
lines(y.novo ~ x.novo)
### A linha mostra a probabilidade de uma espécie com desova X estar ameaçada.

######## Exercício: Verifiquem se o status de ameaça está relacionado com a AltitudeMaxima.

#### Podemos também usar seleção de modelos, mas como temos poucas espécies ameaçadas, é provável que não funcione.
#### Originalmente avaliamos as variáveis individualmente.






