#######Estatística Monte Carlo e fundamentos de programação em R para ecologia
###### Pavel Dodonov - pdodonov@gmail.com
##### Laboratório de Ecologia Aplicada à Conservação - UESC / Ilhéus-BA e Laboratório de Ecologia Espacial e Conservação - Unesp / Rio Claro - SP

#### Aula 1 - Introdução ao R e a algoritmos

### O R é uma linguagem de programação orientada a objetos. Sendo assim, sempre trabalhamos com objetos.
### Objetos armazenam informações, como números, texto etc
### Podemos inserir objetos a partir de arquivos de dados - vamos abrir a partir de uma planilha de texto
### Primeiro, estabelecemos um diretório inicial
setwd("F:/Profissional/Ensino/Disciplinas/MonteCarlo_PPGECB_UESC_2016/Planilhas/")
### Para rodar uma linha: apertem CTRL R no Windows ou CMD ENTER no Mac
### Linhas que começam com # não são executadas (usadas para comentários)
### setwd: Set Working Directory
### Coloquem o caminho para a pasta (diretório) onde vocês salvaram os arquivos.
### Cuidado! Usem sempre a barra / e não a barra \
### Ou usem \\
setwd("F:\\Profissional\\Ensino\\Disciplinas\\MonteCarlo_PPGECB_UESC_2016\\Planilhas\\")
### Para verificar se deu certo, usamos o comando getwd()
getwd()
### getwd: Get Working Directory
### Agora vamos abrir um arquivo
dados.bruto <- read.table("Pteridium_01.txt", header=T, sep="\t")
### header=T significa que os dados contêm uma coluna de cabeçalho
### sep="\t" significa que as colunas são separadas por tabulações
### Vamos ver como é a estrutura destes dados:
str(dados.bruto)
### Este comando mostra como é a nossa planilha.
### Mostra que temos variáveis de dois tipos:
##### int: números inteiros
##### num: números inteiros ou decimais
### Podemos também examinar ele inteiro (já que são apenas 40 valores)
dados.bruto
### Reparem que temos duas áreas, ou seja, coletas feitas em duas áreas distintas
### O nosso interesse aqui está em comparar entre essas duas áreas
### Vamos agora examinar uma única coluna - Altura
dados.bruto$Altura
### E vamos criar um objeto para ela
altura <- dados.bruto$Altura
altura
### Examinando a estrutura dele:
str(altura)
### Este objeto é um vetor numérico. Um vetor é uma sequência de números (ou texto).
### Vamos agora separar ele pelas duas áreas diferentes
area <- dados.bruto$Area
which(area==1)
### Isso nos mostra quais as linhas correspondentes à área 1
linhas.area1 <- which(area==1)
linhas.area2 <- which(area==2)
linhas.area1
linhas.area2
altura.area1 <- altura[linhas.area1]
altura.area2 <- altura[linhas.area2]
### Os colchetes [ ] são usados para indexação
### Ou seja, para escolher apenas uma parte de um dado objeto.
str(altura.area1)
str(altura.area2)
### Reparem que cada objeto agora tem 20 observações.
### Serão as médias das duas observações diferentes?
mean(altura.area1)
mean(altura.area2)
### Parece que sim! Mas vamos olhar a dispersão dos dados...
### Vamos fazer isso em um jitter plot...
plot(altura ~ area)
### Difícil de observar por causa da valores sobrepostos.
area.jitter <- jitter(area)
plot(altura~area.jitter)
### A função jitter adiciona ruído aleatório a uma variável.
### Parece que as alturas na área 1 são de fato maiores que na área 2.
#### Se quiserem, podemos salvar as figuras usando o comando png
png(filename="P1_Fig1_Altura.png", width=20, height=20, unit="cm", res=300)
plot(altura~area.jitter)
dev.off()
### Na função png(), definimos o nome do arquivo (filename),
#### a altura (height) e largura (width) da imagem, as unidades
#### em que a altura e a largura são medidas (unit="cm"), e a
#### resolução da imagem em DPI (res=300).

###### Exercício 1#######
#### Façam os mesmos gráficos para as outras variáveis


####### Aula 2 - Permutações simples #########
setwd("F:/Profissional/Ensino/Disciplinas/MonteCarlo_PPGECB_UESC_2016/Planilhas/")
getwd()
dados.orig <- read.table("Pteridium_01.txt", header=T, sep="\t")
altura <- dados.orig$Altura
areas <- dados.orig$Area
altura.area1 <- altura[areas==1]
altura.area2 <- altura[areas==2]
### A parte sep="\t" mostra que as colunas são separadas por tabulações
### Para verificar se deu certo, usamos o comando getwd()
#### Tendo feito os outros gráficos, vamos ver se de fato há diferença na média
### O teste paramétrico seria um teste t de Student:
t.test(altura.area1, altura.area2)
### Um P-valor altamente significativo
### Será que os dados são normais?
hist(altura.area1, breaks=10)
### Vixxxx....
hist(altura.area2, breaks=10)
### Tá, quem sabe...
### Um outro teste gráfico é o grafico quantil-quantil
### Compara os quantis dos dados com uma distribuição normal
qqnorm(altura.area1)
qqline(altura.area1)
### Não está na linha, ou seja, provavelmente os dados não vêm de uma distribuição normal.
#### Então como seria um teste por permutações?
### Nosso interesse: diferença entre as médias.
### Qual a diferença real?
altura.med.area1 <- mean(altura.area1)
altura.med.area2 <- mean(altura.area2)
altura.dif <- altura.med.area1 - altura.med.area2
altura.dif
### O que seria esperado sob o modelo nulo?
### Hipótese nula - as amostras vêm da mesma população
altura.juntas <- c(altura.area1, altura.area2)
str(altura.juntas)
### Um vetor com 40 números
### Na hipótese nula, tiramos 20 indivíduos aleatoriamente para cada área
### O jeito mais prática de fazer isso: reordenar os valores.
altura.juntas.rand <- sample(altura.juntas)
### O comando sample aleatoriza valores
### Uso o termo rand para valores aleatórios ("random")
### Agora - vamos pegar 20 primeiros para área 1 e os outros para área 2
altura.area1.rand <- altura.juntas.rand[1:20]
altura.area2.rand <- altura.juntas.rand[21:40]
### Calcular a diferença
altura.med.area1.rand <- mean(altura.area1.rand)
altura.med.area2.rand <- mean(altura.area2.rand)
altura.dif.rand <- altura.med.area1.rand - altura.med.area2.rand
altura.dif.rand
### Provavelmente será bem menor do que o valor real observado...
### Vamos repetir isso algumas vezes e anotar os valores...
### Basta selecionar as linhas e rodar de novo
### A partir de qual linha?
### Da linha altura.juntas.rand, que tem o comando sample
### Vamos fazer um vetor em R que armazene estes valores
altura.dif.rand <- c() #Preencher com valores obtidos
### Mas não queremos fazer isso manualmente, então vamos automatizar!
altura.dif.rand <- numeric()
### Aqui criamos um vetor vazio para armazenar os valores
### numeric porque é um vetor que será preenchido com números
### E vamos fazer um loop para o R rodar isso de uma vez
for (i in 1:5000) {
	altura.juntas.rand <- sample(altura.juntas)
	altura.area1.rand <- altura.juntas.rand[1:20]
	altura.area2.rand <- altura.juntas.rand[21:40]
	altura.med.area1.rand <- mean(altura.area1.rand)
	altura.med.area2.rand <- mean(altura.area2.rand)
	altura.dif.rand[i] <- altura.med.area1.rand - altura.med.area2.rand
}
### Basicamente, colocamos dentro do loop o código que deve ser repetido
### E colocamos o índice [i] para armazenar um valor por vez no vetor
#### Para ver o seu funcionamento - definar i <- 1, i <- 2 por vez
### e rodem o conteúdo do loop (sem os colchetes.
str(altura.dif.rand)
### Agora temos um vetor com 5000 valores de altura sob a hipótese nula
hist(altura.dif.rand)
abline(v=altura.dif, col="red")
### O valor real está tão longe que nem aparece no gráfico
### Vamos fazer um histograma incluindo o valor real...
hist(c(altura.dif.rand, altura.dif))
abline(v=altura.dif, col="red")
### Um diferença altamente significativa, ao que parece!
### Agora para calcular significância
### Com que frequência valores maiores ou iguais ao observado são simulados?
teste.uni <- altura.dif.rand >= altura.dif
str(teste.uni)
sum(teste.uni)
signif.uni <- sum(teste.uni) / 5000
signif.uni
teste.bi <- abs(altura.dif.rand) >= abs(altura.dif)
str(teste.bi)
signif.bi <- sum(teste.bi) / 5000
signif.bi
### Mas... isso tá estranho
### Não faz sentido falar que o resultado observado tem uma probabilidade zero de ocorrer
### Afinal, ele foi observado!
### Portanto, incluimos o resultado observado nas permutações:
altura.dif.rand <- numeric()
altura.dif.rand[1] <- altura.dif #O primeiro valor é o valor observado
for (i in 2:5000) { # Simulamos a partir do segundo valor. Ou seja, 4999 permutações.
	altura.juntas.rand <- sample(altura.juntas)
	altura.area1.rand <- altura.juntas.rand[1:20]
	altura.area2.rand <- altura.juntas.rand[21:40]
	altura.med.area1.rand <- mean(altura.area1.rand)
	altura.med.area2.rand <- mean(altura.area2.rand)
	altura.dif.rand[i] <- altura.med.area1.rand - altura.med.area2.rand
}
hist(altura.dif.rand)
abline(v=altura.dif, col="red")
### Com que frequência valores maiores ou iguais ao observado são simulados?
teste.uni <- altura.dif.rand >= altura.dif
str(teste.uni)
sum(teste.uni)
signif.uni <- sum(teste.uni) / 5000
signif.uni
teste.bi <- abs(altura.dif.rand) >= abs(altura.dif)
str(teste.bi)
signif.bi <- sum(teste.bi) / 5000
signif.bi
### Significância de 0.0002, que é a menor possível com 5000 permutações

### Exercício 2: façam esta análise para as outras variáveis :-)

### E se os dados fossem pareados?
### Eles não são, mas vamos então adicionar uma estrutura de dependência para eles
length(altura.area1)
altura.par.area1 <- altura.area1 + 1:length(altura.area1)
altura.par.area2 <- altura.area2 + 1:length(altura.area2)
### Adicionamos o mesmo número às alturas de ambos os grupos
alturas.par.juntas <- c(altura.par.area1, altura.par.area2)
areas.jitter <- jitter(areas)
plot(alturas.par.juntas ~ areas.jitter)
### Mas adicionando linhas...
segments(x0=areas.jitter[1:20], x1=areas.jitter[21:40], y0=altura.par.area1, y1=altura.par.area2)
t.test(altura.par.area1, altura.par.area2) # Sem diferença significativa
t.test(altura.par.area1, altura.par.area2, paired=T) # Com diferença significativa
### A hipótese nula: as diferenças podem ser positivas ou negativas
altura.par.difs <- altura.par.area1 - altura.par.area2
altura.par.difs
altura.par.difs.med <- mean(altura.par.difs)
altura.par.difs.med
### Vamos criar um vetor aleatório de diferenças positivas ou negativas
difs.rand <- sample(c(-1,1), 40, replace=T)
### Assim criamos 40 valores aleatórios, cada um dos quais pode ser igual a -1 ou +1
### Agora simulamos as diferenças na hipótese nula
altura.par.difs.rand <- altura.par.difs * difs.rand
### Cada diferença pode permanecer como está ou mudar de sinal
### Calculamos a média
altura.par.difs.med.rand <- mean(altura.par.difs.rand)
altura.par.difs.med.rand
### Uma diferença (provavelmente) bem menor do que esperado!

### Exercício 3: transformem isso em um loop para calcular significância!


### Aula P3 - Análise de regressão

###Vamos abrir outra planilha
setwd("F:/Profissional/Ensino/Disciplinas/MonteCarlo_PPGECB_UESC_2016/Planilhas/")
dados.regr <- read.table("Alometria.txt", header=T, sep="\t")
str(dados.regr)
### Temos plantas que foram queimadas (Fogo==1) e que não o foram (Fogo==0)
### H: altura (Height); DAS: diâmetro (Diâmetro à Altura do Solo)
### Vamos primeiro trabalhar com um único grupo
dados.regr.1 <- subset(dados.regr, Fogo==1)
str(dados.regr.1)
### Será que há relação entre diâmetro e altura?
dados.regr.1.H <- dados.regr.1$H
dados.regr.1.DAS <- dados.regr.1$DAS
### Façamos um gráfico
plot(dados.regr.1.H ~ dados.regr.1.DAS)
### E a regressão?
mod.lin.1 <- lm(dados.regr.1.H ~ dados.regr.1.DAS)
abline(mod.lin.1)
### No caso, o modelo linear não é o melhor mas vamos fingir que é
mod.lin.1
summary(mod.lin.1)
### O comando summary nos dá a significância da relação
### Mas vamos calculá-la por aleatorizações
### Para isso, precisamos da inclinação da reta
mod.lin.1
### Ela está no objeto... Só precisa ser extraída
str(mod.lin.1)
mod.lin.1$coefficients
### Está aqui!
str(mod.lin.1$coefficients)
### É um vetor com dois números
mod.lin.1$coefficients[2]
### É disso que precisamos!
mod.lin.1.incl <- mod.lin.1$coefficients[2]
mod.lin.1.incl
### Se quisermos manter apenas o valor:
mod.lin.1.incl <- as.numeric(mod.lin.1.incl)
mod.lin.1.incl
### Agora para fazer as aleatorizações...
incl.rand <- numeric(5000)
incl.rand[1] <- mod.lin.1.incl
### Hipótese nula: não há relação entre as duas variáveis
for(i in 2:5000) {
	mod.lin.1.rand <- lm(dados.regr.1.H ~ sample(dados.regr.1.DAS))
	incl.rand[i] <- mod.lin.1.rand$coefficients[2]
	print(i) # para mostrar quantas iterações já foram
}
hist(incl.rand, breaks=50)
abline(v=mod.lin.1.incl)
### Altamente significativa...
signif.bi <- sum(abs(incl.rand) >= abs(mod.lin.1.incl)) / 5000
signif.bi

### Exercício 4: Façam uma análise por permutação para testar se a inclinação é a mesma
#### independentemente do valor da variável Fogo (ou seja, se tem diferença quando Fogo==1
#### e quando Fogo==0).