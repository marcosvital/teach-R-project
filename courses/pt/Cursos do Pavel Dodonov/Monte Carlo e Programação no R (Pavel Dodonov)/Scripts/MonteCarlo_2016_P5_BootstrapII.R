### Fundamentos de estatística Monte Carlo e de programação em R para Ecologia
### Pavel Dodonov - pdodonov@gmail.com
### Aula prática 5 - Bootstrap e seleção de modelos

library(bbmle) #vamos usar este pacote

setwd("F:/Profissional/Ensino/Disciplinas/MonteCarlo_UESC_PPGECB_2016/Planilhas")
getwd()
dados.full <- read.table("PredacaoDeNinhos.txt", header=T, sep="\t")
str(dados.full)
###São dados relacionando predação de ninhos com distância de borda e outros fatores
### Dodonov et al. submetido
### Vamos trabalhar com o dia de retirada, uma estimativa do tempo de sobrevivência
#### do ninho
### Podemos pensar em diferentes modelos; vamos dar a eles números e nomes
mod0.null <- glm(TempoSobrevivencia ~ 1, data=dados.full, family=poisson)
### Este é o modelo nulo; gosto de incluir ele nos modelos avaliados.
mod1.local <- glm(TempoSobrevivencia~ Local, data=dados.full, family=poisson)
mod2.altura <- glm(TempoSobrevivencia ~ Altura, data=dados.full, family=poisson)
mod3.dens <- glm(TempoSobrevivencia ~ Densidade, data=dados.full, family=poisson)
mod4.loc_alt <- glm(TempoSobrevivencia ~ Local+Altura, data=dados.full, family=poisson)
mod5.loc_alt.int <- glm(TempoSobrevivencia ~ Local+Altura+Local:Altura, 
	data=dados.full, family=poisson)
### Vamos armazenar estes modelos com outros nomes também
mod0.null.obs <- mod0.null
mod1.local.obs <- mod1.local
mod2.altura.obs <- mod2.altura
mod3.dens.obs <- mod3.dens
mod4.loc_alt.obs <- mod4.loc_alt
mod5.loc_alt.int.obs <- mod5.loc_alt.int
### Temos seis modelos; incluí modelos com borda e altura e com borda e altura
#### mais a interação porque fazem sentido ecológico pra mim.
### Para examinar cada modelo:
summary(mod0.null)
summary(mod1.local)
summary(mod2.altura)
summary(mod3.dens)
### Etc
### Mas em uma abordagem de seleção de modelos, não olhamos simplesmente a significância.
### Olhamos a verossimilhança e o AIC (Akaike Information Criterion) que dela deriva.
AICctab(mod0.null, mod1.local, mod2.altura, mod3.dens, mod4.loc_alt, mod5.loc_alt.int)
### O modelo com local, altura e interação foi o selecionado.
### Usamos AICc porque o tamanho amostral é relativamente pequeno
#### (em relação ao número de modelos comparados)
### Mas e se os dados fossem outros?
### Simulamos com bootstrap!
### Vamos ver se conseguimos, antes de tudo, salvar o modelo extraído em um objeto...
AICc_obs <- AICctab(mod0.null, mod1.local, 
	mod2.altura, mod3.dens, mod4.loc_alt, mod5.loc_alt.int)
str(AICc_obs)
### Reparem que o melhor modelo aparece em primeiro
### E reparem que existe o atributo row.names
rownames(AICc_obs)
### Não funcionou...
### O comando attr retorna os atributos do objeto
attr(AICc_obs)
### Falta um argumento
?attr
### Olhem o que o R nos diz:
#### which: a non-empty character string specifying which attribute is to be accessed.
### string: mesma coisa que texto. Ou seja, entre aspas.
attr(AICc_obs, "row.names")
### Mostra os modelos!
Vamos guardar o modelo selecionado em um objeto...
modSel_obs <- attr(AICc_obs, "row.names")[1]
modSel_obs
### O que queremos saber é, em cada aleatorização, qual modelo seria selecionado?
### Mas como aleatorizar? (Com reposição)
sample(dados.full, replace=T)
### O R aleatorizou colunas, precisamos de linhas.
### Vamos então falar quais linhas nos queremos!
nrow(dados.full)
### Nossos dados têm 89 linhas, números 1 a 89
linhas <- 1:89
### Ou melhor:
linhas <- 1:nrow(dados.full)
linhas
linhas.boot <- sample(linhas, replace=T)
linhas.boot
sort(linhas.boot)
### E agora fazemos um novo objeto:
dados.boot <- dados.full[linhas.boot,]
dados.boot
### Agora temos valores que se repetem, como um bom bootstrap deve ser.
### Agora em um loop!
### Vamos precisar guardar apenas os modelos selecionados
Nboot <- 1000
modSel_boot <- character(Nboot)
### Pois será um vetor de texto!
modSel_boot[1] <- modSel_obs
for(i in 2:Nboot) {
	linhas.boot <- sample(linhas, replace=T)
	dados.boot <- dados.full[linhas.boot,]
	mod0.null <- glm(TempoSobrevivencia ~ 1, data=dados.boot, family=poisson)
	mod1.local <- glm(TempoSobrevivencia~ Local, data=dados.boot, family=poisson)
	mod2.altura <- glm(TempoSobrevivencia ~ Altura, data=dados.boot, family=poisson)
	mod3.dens <- glm(TempoSobrevivencia ~ Densidade, data=dados.boot, family=poisson)
	mod4.loc_alt <- glm(TempoSobrevivencia ~ Local+Altura, data=dados.boot, family=poisson)
	mod5.loc_alt.int <- glm(TempoSobrevivencia ~ Local+Altura+Local:Altura, 
		data=dados.boot, family=poisson)
	AICc_boot <- AICctab(mod0.null, mod1.local, 
		mod2.altura, mod3.dens, mod4.loc_alt, mod5.loc_alt.int)
	modSel_boot[i] <- attr(AICc_boot, "row.names")[1]
	print(i)
}
modSel_boot
table(modSel_boot)
modSel_result <- table(modSel_boot)/Nboot
as.matrix(modSel_result)
### Neste caso todos os modelos apareceram, mas poderia não acontecer...
### Exercício: façam seleção de modelos com bootstrap para a planilha
#### A variável-resposta é a última coluna.
#### Vocês terão que decidir quais modelos ajustar por conta própria
##### Divirtam-se! :-)