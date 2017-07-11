### Fundamentos de estatística Monte Carlo e de programação em R para ecologia
### Pavel Dodonov - pdodonov@gmail.com
### PPGECB - UESC - Ilhéus-BA

### Aula prática P6
### Simulando dados

### Esta aula será diferente [vem com a gente]
###### [Alguém entendeu a referência?]
### Não iremos trabalhar com dados reais, e sim simular nossos dados
### Isso pode ser usado e.g. para testar se sua análise funciona.
### Existem algumas distribuições facilmente simuladas pelo R
sim.unif <- runif(50)
sim.unif
hist(sim.unif)
### Esta é a distribuição uniforme 0-1.
#### Cada valor tem a mesma probabilidade de ser gerado.
sim.unif <- runif(50, 0, 100)
hist(sim.unif)
### Mesma coisa, mas agora varia de 0 a 100
### Argumentos: número de elementos, valor mínimo e máximo
sim.norm <- rnorm(50)
sim.norm
hist(sim.norm)
### Isto é uma distribuição normal
sim.norm <- rnorm(50, mean=5, sd=2)
hist(sim.norm)
### Podemos definir a média e o desvio padrão
sim.pois <- rpois (50)
### Distribuição de Poisson, mas precisamos definir o parâmetro lambda
sim.pois <- rpois(50, lambda=5)
hist(sim.pois)
mean(sim.pois)
var(sim.pois)
### O lambda é como se fosse a média e a variância da distribuição
sim.binom <- rbinom(50)
### Distribuição binomial; faltam argumentos
?rbinom
sim.binom <- rbinom(50, size=10, prob=0.2)
hist(sim.binom)
### Número de sucessos em 10 tentativas, sendo que a
#### probabilidade de sucesso em cada é de 20%
### Também há distribuição exponencial, gamma etc...
#### Perguntem ao Google se haver necessidade!
### Podemos fazer isso para testar análises
### Então vamos ver quão bem funciona nosso teste de regressão
### E vamos definir uma semente (seed) para repetir o procedimento
set.seed(42)
### Vamos gerar uma variável explanatória aleatória:
x.sim <- runif(40, 0, 100)
### Varia de 0 a 100 - poderia ser cobertura florestal.
### Vamos arredondar ela para uma casa decimal (mais realista)
x.sim <- round(x.sim, 1)
x.sim
### E vamos simular uma espécie cuja abundância depende da cobertura
slope <- 0.1 # para cada 10% de floresta abundância aumenta em 1
y.sim <- x.sim * slope
plot(y.sim ~ x.sim)
### Mas vamos também adicionar um ruído (erro)
### Abundância - erro pode seguir distribuição de Poisson
y.sim <- x.sim * slope + rpois(40, 1)
plot(y.sim~x.sim)
y.sim <- x.sim * slope + rpois(40, 3)
plot(y.sim~x.sim)
### Parece realista, né?
y.sim
### Temos valores decimais; vamos arredondar.
y.sim <- round(y.sim)
y.sim
plot(y.sim~x.sim)
### Vamos ver se o teste funciona...
lm.sim <- lm(y.sim~x.sim)
lm.sim
### Estou deliberadamente usando um modelo linear sendo que um GLM é apropriado
incl.sim <- lm.sim$coefficients[2]
Nrand <- 1000
### Vamos fazer o teste...
incl.sim.rand <- numeric(Nrand)
incl.sim.rand[1] <- incl.sim
for (i in 2:Nrand) {
	y.sim.rand <- sample(y.sim)
	lm.sim.rand <-lm(y.sim.rand~x.sim)
	incl.sim.rand[i] <- lm.sim.rand$coefficients[2]
	print(i)
}
hist(incl.sim.rand)
abline(v=incl.sim)
signif <- sum(abs(incl.sim.rand) >= abs(incl.sim))/Nrand
signif
### Mas e fizéssemos outras simulações?...
### Vamos fazer um loop dentro do loop!
Nsim <- 100
Nrand <- 100 #para demorar menos
set.seed(42)
signif <- numeric(Nsim) #o que nos interessa é a significância
for (m in 1:Nsim) {
	x.sim <- round(runif(40,0,100),1)
	y.sim <- x.sim * slope + rpois(40, 3)
	y.sim <- round(y.sim)
	lm.sim <- lm(y.sim~x.sim)
	incl.sim <- lm.sim$coefficients[2]
	incl.sim.rand <- numeric(Nrand)
	incl.sim.rand[1] <- incl.sim
	for(i in 2:Nrand) {
		y.sim.rand <- sample(y.sim)
		lm.sim.rand <- lm(y.sim.rand~x.sim)
		incl.sim.rand[i] <- lm.sim.rand$coefficients[2]
		print(c(m,i))
	}
	signif[m] <- sum(abs(incl.sim.rand) >= abs(incl.sim))/Nrand
}
hist(signif)
### Todos os valores foram muito baixos - alto poder, mas talvez alto demais
### Mas será que este teste detectaria uma diferença nula que não existe?
### Exercício: Repitam este procedimento, mas criando uma função para a
#### análise de regressão por aleatorizações 
#### e usando ela no lugar do segundo loop
### Exercício 2: verifiquem se esta análise por regressão detectaria uma
#### diferença significativa se a hipótese nula for verdadeira
#### (ou seja, simulem dados não correlacionados e repitam o procedimento
##### Fazendo isso, estimamos a probabilidade de erro do tipo 1
##### (rejeitar uma hipótese nula verdadeira).