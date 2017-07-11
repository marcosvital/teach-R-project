#################### Curso de Estatística Básica #############
########### Pavel Dodonov ####################################
########### pdodonov@gmail.com ###############################
##### Ajuste e seleção de modelos em R #######################
##############################################################

####### Parte 1 - Múltiplas variáveis explanatórias #########

### Vamos ajustar modelos lineares e anova bifatorial.

setwd("/home/pavel/Profissional/Ensino/Disciplinas/EstatisticaBasica_UESC_PPGECB_2017/Planilhas")

dados.pter <- read.table("Pteridium.txt", header=T, sep="\t", dec=".")
str(dados.pter)
head(dados.pter)

# Área e coleta são variáveis categóricas.
# Mas estão como numéricas
# Vamos dar um jeito nisso.

dados.pter$Area <- as.factor(dados.pter$Area)
dados.pter$Coleta <- as.factor(dados.pter$Coleta)
str(dados.pter)

# Área: dois níveis - 1 e 2 (queimada e não queimada) (ou ao contrário, não lembro)

# Trabalhamos com altura

# Sugestão de gráfico exploratório: jitter plot, com diferentes cores pras áreas
plot(Altura ~ jitter(as.numeric(Coleta)), col=as.numeric(Area), data=dados.pter)

# Vamos rodar uma análise de variância bifatorial
anova.pter <- aov(Altura ~ Area + Coleta + Area:Coleta, data=dados.pter)
anova.pter
### Gráficos de validação
par(mfrow=c(2,2))
plot(anova.pter)
dev.off()
# Temos heterogeneidade de variâncias e desvios de normalidade.
# Gráficos de resíduos em funções de cada variável explanatória:
resid.pter <- resid(anova.pter)
plot(resid.pter ~ jitter(as.numeric(dados.pter$Area)))
# Santa heteroscedasticidade, Batman!
plot(resid.pter ~ jitter(as.numeric(dados.pter$Coleta)))

### Exercício: Façam isso aí usando o logaritmo da altura e vejam se melhora.
### A solução que encontrei foi usar Monte Carlo.

# Para ver significância:
summary(anova.pter)
# Tudo significativo.
# Para saber mais:
TukeyHSD(anova.pter)

### Lição de casa opcional: Interpretem isso aí :-)

### Outra forma: modelo linear

lm.pter <- lm(Altura ~ Area + Coleta + Area:Coleta, data=dados.pter)

par(mfrow=c(2,2))
plot(lm.pter)
dev.off()

# Parece similar...

summary(lm.pter)
summary(anova.pter)
resid(lm.pter) == resid(anova.pter)
# É o mesmo modelo. Só mudam os testes.

summary(anova.pter)
anova(lm.pter)

# summary e anova são diferentes formas de examinar o modelo (calcular a significância, etc).

###### Parte 2 - Regressão múltipla ######################

dados.serap <- read.table("Serapilheira.txt", header=T, sep="\t", dec=".")
str(dados.serap)
head(dados.serap)

# Variáveis-resposta: LeafLitter, Grass Litter
# Variáveis explanatórias: Distancia, Height, Canopy, GramineasNativas, GramineasExoticas

# Fazer um gráfico...
par(mfrow = c(3,2) )
plot(LeafLitter ~ Distancia, data=dados.serap)
plot(LeafLitter ~ Height, data=dados.serap)
plot(LeafLitter ~ Canopy, data=dados.serap)
plot(LeafLitter ~ GramineasNativas, data=dados.serap)
plot(LeafLitter ~ GramineasExoticas, data=dados.serap)

### Testar colinearidade...
pairs(dados.serap[,c(3,6:9)])
cor(dados.serap[,c(3,6:9)])
cor(dados.serap[,c(3,6:9)])^2
# Por que elevei ao quadrado?

nrow(dados.serap)

# temos 75 linhas... Mas há valores ausentes
is.na(dados.serap$LeafLitter)
sum(is.na(dados.serap$LeafLitter))
# Este sum conta quantos TRUE temos. TRUE = 1.

### Vamos excluir os valores ausentes...
linhas.excluir <- which(is.na(dados.serap$LeafLitter))
dados.serap.backup <- dados.serap
# Caso algo dê errado, temos o backup :-)

dados.serap <- dados.serap[-linhas.excluir,]
# Todas as colunas, e todas as linhas exceto ( - ) as com NA
str(dados.serap)

### 71 observações e 5 variáveis explanatórias.
### Podemos incluir todas e quiçá algumas interações...

### Vamos fazer uns gráficos exploratórios do nosso Y também...
par(mfrow=c(3,1))
plot(dados.serap$LeafLitter)
hist(dados.serap$LeafLitter)
qqnorm(dados.serap$LeafLitter)
qqline(dados.serap$LeafLitter)

dev.off()

mod.completo <- lm(LeafLitter ~ Distancia + Height + Canopy + GramineasNativas+ GramineasExoticas + Distancia:Height + Distancia:Canopy + Distancia:GramineasNativas + Distancia:GramineasExoticas, data=dados.serap)
par(mfrow=c(2,2))
plot(mod.completo)

# Heterogeneidade de variâncias, mas de resto parece sussa...

dev.off()


# Inclui as interações das variáveis com distância de borda.
summary(mod.completo)
# Singularities - falta valores únicos... Ou independência entre variáveis.

teste <- lm(GramineasNativas ~ Distancia+Height+Canopy+GramineasExoticas, data=dados.serap)
summary(teste)


# Então vamos testar sem esta variável...
mod.semNativas <- lm(LeafLitter ~ Distancia + Height + Canopy + GramineasExoticas + Distancia:Height + Distancia:Canopy + Distancia:GramineasExoticas, data=dados.serap)
summary(mod.semNativas)
summary(mod.completo)

par(mfrow=c(2,2))
plot(mod.semNativas)
# Parece razoavelmente bom...

# Excluir esta variável não mudou em nada o modelo - ela era irrelevante.
### Uma das formas de prosseguir é excluir a variável menos significativa:
mod.3 <- lm(LeafLitter ~ Distancia + Height + Canopy + GramineasExoticas + Distancia:Height + Distancia:Canopy, data=dados.serap)
summary(mod.3)
# Prosseguimos...
mod.4 <- lm(LeafLitter ~ Distancia + Height + Canopy + GramineasExoticas + Distancia:Canopy, data=dados.serap)
summary(mod.4)
# Mais um pouco...
# Distancia e Distancia:Canopy são as menos significativa. Vamos tirar a interação primeiro.
mod.5 <- lm(LeafLitter ~ Distancia + Height + Canopy + GramineasExoticas, data=dados.serap)
summary(mod.5)
# Agora tirar a distância...
mod.6 <- lm(LeafLitter ~ Height + Canopy + GramineasExoticas, data=dados.serap)
summary(mod.6)
# Um p de 0.053 não me parece suficiente para exlucir as GramineasExoticas.
par(mfrow=c(2,2))
plot(mod.6)

par(mfrow=c(3,1))
plot(LeafLitter ~ Height, data=dados.serap)
plot(LeafLitter ~ Canopy, data=dados.serap)
plot(LeafLitter ~ GramineasExoticas, data=dados.serap)


### Exercício: Repitam isso para a GrassLitter



dados.test <- read.table("teste.txt", header=T, sep="\t")
dados.IS <- subset(dados.test, Site=="IS")

Nat.IS <- subset(dados.test, Variable=="Grasses-Native", select=Value.NaN)[,1]
Exo.IS <- subset(dados.test, Variable=="Grasses-Exotic", select=Value.NaN)[,1]
Hei.IS <- subset(dados.test, Variable=="Height", select=Value.NaN)[,1]
Cov.IS <- subset(dados.test, Variable=="Cover", select=Value.NaN)[,1]
Dist.IS <- subset(dados.test, Variable=="Cover", select=Dist)[,1]

length(Nat.IS)
length(Exo.IS)
length(Hei.IS)
length(Cov.IS)
length(Dist.IS)


mod.test <- lm(Nat.IS ~ Exo.IS + Hei.IS + Cov.IS + Dist.IS)
summary(mod.test)



##### Outra forma de fazer isso - seleção de modelos
### Usando * no lugar o +, as interações são adicionadas automaticamente.

mod.completo.inter <- lm(LeafLitter ~ Distancia + Height + Canopy + GramineasNativas+ GramineasExoticas + Distancia:Height + Distancia:Canopy + Distancia:GramineasNativas + Distancia:GramineasExoticas, data=dados.serap)
mod.completo <- lm(LeafLitter ~ Distancia + Height + Canopy, data=dados.serap)
mod.DistanciaHeightExoticas <- lm(LeafLitter ~ Distancia + Height + GramineasExoticas, data=dados.serap)
mod.DistanciaCanopyExoticas <- lm(LeafLitter ~ Distancia + Canopy + GramineasExoticas, data=dados.serap)
mod.DistanciaHeightCanopy <- lm(LeafLitter ~ Distancia + Height + Canopy, data=dados.serap)
mod.HeightCanopyExoticas <- lm(LeafLitter ~ Height + Canopy + GramineasExoticas, data=dados.serap)
mod.DistHeight <- lm(LeafLitter ~ Distancia + Height, data=dados.serap)
mod.DistHeight.inter <- lm(LeafLitter ~ Distancia * Height, data=dados.serap)
mod.DistCanopy <- lm(LeafLitter ~ Distancia + Canopy, data=dados.serap)
mod.DistCanopy.inter <- lm(LeafLitter ~ Distancia * Canopy, data=dados.serap)
mod.DistExoticas <- lm(LeafLitter ~ Distancia + GramineasExoticas, data=dados.serap)
mod.DistExoticas.inter <- lm(LeafLitter ~ Distancia * GramineasExoticas, data=dados.serap)
mod.HeightCanopy <- lm(LeafLitter ~ Height + Canopy, data=dados.serap)
mod.HeightCanopy.inter <- lm(LeafLitter ~ Height * Canopy, data=dados.serap)
mod.HeightExoticas <- lm(LeafLitter ~ Height + GramineasExoticas, data=dados.serap)
mod.HeightExoticas.inter <- lm(LeafLitter ~ Height * GramineasExoticas, data=dados.serap)
mod.CanopyExoticas <- lm(LeafLitter ~ Canopy + GramineasExoticas, data=dados.serap)
mod.CanopyExoticas.inter <- lm(LeafLitter ~ Canopy * GramineasExoticas, data=dados.serap)
mod.Dist <- lm(LeafLitter ~ Distancia, data=dados.serap)
mod.Height <- lm(LeafLitter ~ Height, data=dados.serap)
mod.Canopy <- lm(LeafLitter ~ Canopy, data=dados.serap)
mod.Exoticas <- lm(LeafLitter ~ GramineasExoticas, data=dados.serap)
mod.null <- lm(LeafLitter ~ 1, data=dados.serap)

AIC.serap <- AIC(mod.completo.inter, mod.completo, mod.DistanciaHeightExoticas, mod.DistanciaCanopyExoticas, mod.DistanciaHeightCanopy, mod.HeightCanopyExoticas, mod.DistHeight, mod.DistHeight.inter, mod.DistCanopy, mod.DistCanopy.inter, mod.DistExoticas, mod.DistExoticas.inter, mod.HeightCanopy, mod.HeightCanopy.inter, mod.HeightExoticas, mod.HeightExoticas.inter, mod.CanopyExoticas, mod.CanopyExoticas.inter, mod.Dist, mod.Height, mod.Canopy, mod.Exoticas, mod.null)
str(AIC.serap)

AIC.serap[order(AIC.serap$AIC),]

library(bbmle)

AICtab(mod.completo.inter, mod.completo, mod.DistanciaHeightExoticas, mod.DistanciaCanopyExoticas, mod.DistanciaHeightCanopy, mod.HeightCanopyExoticas, mod.DistHeight, mod.DistHeight.inter, mod.DistCanopy, mod.DistCanopy.inter, mod.DistExoticas, mod.DistExoticas.inter, mod.HeightCanopy, mod.HeightCanopy.inter, mod.HeightExoticas, mod.HeightExoticas.inter, mod.CanopyExoticas, mod.CanopyExoticas.inter, mod.Dist, mod.Height, mod.Canopy, mod.Exoticas, mod.null, weights=T)



#####################################################
###### Curso de Estatíst Básica######################
###### Pavel Dodonov ################################
###### pdodonov@gmail.com ###########################
###### Modelos aditivos - efeitos não-lineares ######
#####################################################

library(mgcv)
library(bbmle)

dados.subbosque <- read.table("Subbosque.txt", header=T, sep="\t", dec=".")
str(dados.subbosque)

### Nossa variável X é a distância. Temos 11 variáveis explanatórias.
### As variáveis explanatórias são os índices de diversidade fornecidos pelo Past :-)

# Vamos fazer um gráfico pra todas elas...

# Para não fazermos um por vez, faremos um loop.

# Primeiro, definimos quem é a nossa variável x:
x <- dados.subbosque$Distance
# Também criamos um nome com os nomes das variáveis:
nomes <- names(dados.subbosque)

# Criamos um dispositivo com espaço para 11 gráficos:
par(mfrow=c(4,3), mar=c(4,4,2,2) ) # Definimos margens pequenas pra caber tudo.

for (i in 1:11) { # Isso diz que repetiremos um procedimento 11 vezes
	# As primeiras 4 colunas não corresponde a valores de Y.
	y <- dados.subbosque[ ,i+4]
	# Assim ele vai pegar, em cada iteração do loop, diferentes colunas, começando pela 5a e indo até a 15a.
	nome <- nomes[i+4] # Mesma coisa pros nomes das variáveis
	plot(y ~ x, main=nome)
}

dev.off()

### Agora, ajustar um modelo aditivo.
str(dados.subbosque)
# Taxa_S é o número de espécies.
# Como tal, deve seguir distribuição de Poisson.

mod.gam.Taxa_S <- gam(Taxa_S ~ Distance, data=dados.subbosque, family=poisson)
summary(mod.gam.Taxa_S)
# Deu um efeito significativo... Mas isso ainda não foi o modelo aditivo.
# Falta especificar a função (smoothing function).
mod.gam.Taxa_S <- gam(Taxa_S ~ s(Distance, fx=F, k=-1), data=dados.subbosque, family=poisson)
# s() diz que é uma função aditiva.
# fx=F diz que não especificamos o grau de suavização.
# k=-1 diz que o grau máximo de suavização é infinito.

# Deu erro - temos poucos valores de X pra um modelo altamente não linear.
mod.gam.Taxa_S <- gam(Taxa_S ~ s(Distance, fx=F, k=6), data=dados.subbosque, family=poisson)
# Agora foi.
summary(mod.gam.Taxa_S)
# O modelo mais parcimonioso foi o linear.
plot(mod.gam.Taxa_S)
# Mostra a função de suavização
gam.check(mod.gam.Taxa_S)
# Gráficos de validação.

# Para fazer a figura, é mais complicadinho.
# 1: Fazemos um vetor de valores de X para prever.
x.novo <- seq(min(dados.subbosque$Distance), max(dados.subbosque$Distance), by=1)
# 2: Calculamos valores correspondentes de Y:
y.novo <- predict(mod.gam.Taxa_S, newdata=list(Distance=x.novo), type="response")
# newdata diz que a predição será feita para novos dados, não os originais.
# list(Distance=x.novo) diz que a variável usada no lugar da Distance será o x.novo.

# 3: Fazemos o gráfico e adicionamos a linha
plot(Taxa_S ~ Distance, data=dados.subbosque)
lines(y.novo ~ x.novo)

### Para uma outra variável:

mod.gam.Menhinick <- gam(Menhinick ~ s(Distance, fx=F, k=6), data=dados.subbosque)
# Não usei family=Poisson porque não são mais dados de contagem.
summary(mod.gam.Menhinick)
# Deu uma relação não-linear, e também não significativa.
plot(mod.gam.Menhinick)
gam.check(mod.gam.Menhinick)

y.novo <- predict(mod.gam.Menhinick, newdata=list(Distance=x.novo), type="response")

plot(Menhinick ~ Distance, data=dados.subbosque)
lines(y.novo ~ x.novo)

# Calculamos os resíduos:
resid.Menhinick <- resid(mod.gam.Menhinick)
plot(resid.Menhinick ~ dados.subbosque$Distance)

# Me parece suficientemente bom.
# Podemos também comparar, por exemplo, este modelo com um linear e um nulo:
mod.lin.Menhinick <- lm(Menhinick ~ Distance, data=dados.subbosque)
mod.null.Menhinick <- lm(Menhinick ~ 1, data=dados.subbosque)

AICctab(mod.gam.Menhinick, mod.lin.Menhinick, mod.null.Menhinick)

# Usamos AICctab porque o AICc é melhor para tamanos amostrais pequenos.
# O GAM é melhor - mas o nulo não é de todo ruim também.



