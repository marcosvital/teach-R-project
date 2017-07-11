### Fundamentos de estatística Monte Carlo e de programação em R para Ecologia
### Pavel Dodonov - pdodonov@gmail.com
### Aula prática 4 - Bootstrap

setwd("F:/Profissional/Ensino/Disciplinas/MonteCarlo_PPGECB_UESC_2016/Planilhas")
getwd()

dados.orig <- read.table("Seedbank.txt", header=T, sep="\t")
### O sep="\t" é necessário porque existe um espaço nos nomes de coluna
str(dados.orig)
### No nome da coluna ele foi substituído por um ponto.

### Vamos trabalhar com apenas um nível de área, data, ano e tratamento

dados.teste <- subset(dados.orig, area=="CNQ", data=="EC",
	ano==2009, tratamento=="P1")
### Deu erro... Porque o teste lógico estava errado
dados.teste <- subset(dados.orig, area=="CQ" & data=="EC" &
	ano==2009 & tratamento=="P1")
str(dados.teste)
### Agora sim - as 30 observações correspondentes a essa combinação
#### de fatores.
### Vamos pegar apenas os valores de abundância para simplificar
dados.teste <- dados.teste$M..albicans
str(dados.teste)
### Um vetor com 30 números - parece que está tudo certo...
hist(dados.teste)
qqnorm(dados.teste)
qqline(dados.teste)
###Isso não é normal nem aqui nem em Mordor...
##### Calculando diferentes intervalos de confiança
###Intervalo de confiança paramétrico
media.real <- mean(dados.teste)
N <- length(dados.teste) 
#tamanho amostral
SE.real <- sd(dados.teste)/sqrt(N)  
#standard error, ou erro padrão (da média)
#sqrt: raiz quadrada
IC.parametrico <- c(media.real-1.96*SE.real, media.real+1.96*SE.real)
IC.parametrico
###Intervalo de confiança por bootsrap padrão
### Vamos fazer um objeto com o número de boots
Nboot=5000
#### Fica mais fácil para modificarmos depois
medias.boot <- numeric(Nperm)
medias.boot[1] <- media.real
for (i in 2:5000) {
	dados.boot=sample(dados.teste, replace=T)	
	#replace=T diz que é bootstrap (com reposição)
	medias.boot[i]=mean(dados.boot)	
	}
hist(medias.boot)
### Reparem que a distribuição é assimétrica...
SE.boot=sd(medias.boot)	
c(SE.real, SE.boot)
### Reparem que os dois erros foram parecidos... mas diferentes!
IC.boot.padrao=c(media.real-1.96*SE.boot, media.real+1.96*SE.boot)
IC.parametrico
IC.boot.padrao
### Mais estreito que o paramétrico
IC.boot.padrao - media.real
### Mas é simétrico ao redor da média...
#### Intervalo de confiança de Efron:
### Simplesmente pegar os 95% centrais da distribuição
IC.boot.Efron<-quantile(medias.boot, c(0.025, 0.975))
### Nem precisam fazer a reamostragem de novo!
IC.boot.Efron
IC.boot.Efron-media.real
### Não é simétrico, e sim maior para a direita
#### Intervalo de confiança de Hall:
### Calcular os erros
### Calcular os limites superiores e inferiores dos erros
### Aplicar a fórmula: media.real - lim.sup, media.real - lim.inf
erros.boot <- medias.boot - media.real
hist(erros.boot)
IC.boot.erros <- quantile(erros.boot, c(0.025, 0.975))
IC.boot.erros
### Para a fórmula, é mais fácil inverter:
IC.boot.erros <- quantile(erros.boot, c(0.975, 0.025))
IC.boot.erros
IC.boot.Hall <- media.real - IC.boot.erros
IC.boot.Hall
### Façamos um gráfico...
hist(dados.teste, breaks=20)
abline(v=IC.parametrico, col="red", lwd=3)
abline(v=IC.boot.padrao, col="blue", lwd=3)
abline(v=IC.boot.Efron, col="green", lwd=3)
abline(v=IC.boot.Hall, col="orange", lwd=3)

### Tá, mas e pra fazer pras outras combinações?

### Podemos transformar isso em funções!

bootstrap <- function(x, Nboot=5000, type="Efron") {
	### A primeira linha diz que faremos uma função chamada bootstrap
	### Esta função terá os argumentos: x (o vetor de dados),
	### Nboot (o número de boots) e type (o tipo de IC)
	media.real <- mean(x)
	medias.boot <- numeric(Nboot)
	medias.boot[1] <- media.real
	for (i in 2:Nboot) {
		### Reparem que aqui já é indentação dupla!
		medias.boot[i] <- mean(sample(x, replace=T))
		### Fiz tudo em uma única linha
	}
	if (type=="standard") {
		SE.boot <- sd(medias.boot)	
		IC <- c(media.real-1.96*SE.boot, media.real+1.96*SE.boot)
	}
	if (type=="Efron") {
		IC <- quantile(medias.boot, c(0.025, 0.975))
	}
	if (type=="Hall") {
		erros.boot <- medias.boot - media.real
		IC.erros <- quantile(erros.boot, c(0.975, 0.025))
		IC <- media.real - IC.erros
	}
	return(IC)
}
bootstrap(dados.teste)
### Se repetirmos várias vezes, vai dar pequenas diferenças
### Se quisermos pra cada ano: função aggregate
aggregate(M..albicans ~ ano, data=dados.orig, FUN=bootstrap)
aggregate(M..albicans ~ ano, data=dados.orig, FUN=bootstrap, type="Hall")
### Para dois fatores
aggregate(M..albicans ~ ano+area, data=dados.orig, FUN=bootstrap, type="Hall")
### Para mais fatores
aggregate(M..albicans ~ ano+data+area, data=dados.orig, FUN=bootstrap, type="Hall")
### Lindo, não? :-)
### A função bootstrapT, que enviei, calcula um outro tipo de bootstrap
### Colem ela no console para ela ficar guardada
aggregate(M..albicans ~ ano, data=dados.orig, FUN=bootstrapT)
### Exercício: criem uma função para calcular o intervalo de confiança paramétrico (sem bootstrap)
### Exercício 2: comentem a função bootstrapT (explicando o que cada as diferentes partes dela fazem)



	




