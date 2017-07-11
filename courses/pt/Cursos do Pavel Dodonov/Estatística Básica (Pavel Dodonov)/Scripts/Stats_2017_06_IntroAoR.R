############################################################
##### Introdução ao R ######################################
##### Curso de Estatística Básica - PPGECB/UESC ############
###### 20 Março - 6 Abril 2017 #############################
###### Por Pavel Dodonov - pdodonov@gmail.com ##############
############################################################


# Antes de começar: Texto que começa com # são comentários
# Comentários não são entendidos pelo código.
# Podemos escrever:
# Oi! Eu sou um comentário!
# E nada acontece
# Mas se fizermos o seguinte:
Oi! Eu sou um comentário!
# Dá erro.
# Comentários são essenciais para um código bem-documentado que você possa entender depois.

########## Parte 1 - Operações matemáticas #############

5 + 2 # soma
5 * 2 # multiplação
5 / 2 # divisão
5 ^ 2 # potenciação
5 %% 2 # resto de divisão
5 %/% 2 # divisão inteira

# Criando uma sequência de números
1:5
1:5 + 2
1:5 / 2
1:5 + 2:6

# Juntando números em uma sequência

c(2.2, 3.1, 6.4, 4.2, 9.8)
c(2.2, 3.1, 6.4, 4.2, 9.8) + 2
c(2.2, 3.1, 6.4, 4.2, 9.8) / 2
c(2.2, 3.1, 6.4, 4.2, 9.8) * c(1, 2)

# Parabéns, você acaba de conhecer uma função!

######## Parte 2 - Funções e objetos #########

# Funções são, basicamente, coisas que fazem coisas (sim, amplo assim).

# A função c - vem de "concatenar" - agrupa números (ou caracteres) para trabalharmos com eles juntos.
c(2.2, 3.1, 6.4, 4.2, 9.8)
# E podemos armazenar esta nossa criação. Basta dar um nome a ela.
# A estrutura é simples: colocamos o nome, depois colocamos <- ou =, e colocamos o que será associado ao nome.
dados1 <- c(2.2, 3.1, 6.4, 4.2, 9.8)
# Ou
dados1 = c(2.2, 3.1, 6.4, 4.2, 9.8)
# Os códigos oficiais usam <-, mas tanto faz!

# Podemos então "chamar" o objeto:
dados1
# E ele aparece em toda a sua glória decimal.

# Podemos rodar operações matemáticas com ele:
dados1 + 2
dados1 / 2
# E criar novos objetos a partir dele
dados2 <- dados1+2

# E fazer operações diretamente com os objetos
dados2/dados1

# Algumas funções de estatística descritiva: mean, median, var, sd
mean(dados2)
median(dados2)
var(dados2)
sd(dados2)

# Para descobrir o que eles fazem, usamos o ?

?mean
?c

# Funções têm argumentos - explicarei melhor depois.

# Existem diferentes tipos de objetos em R
vetor1 <- 1:20
vetor1
# Um vetor é uma sequências de números.
matriz1 <- matrix(1:20, ncol=4)
matriz1
# Uma matriz é uma sequência de números organizada em linhas e colunas
lista1 <- list(vetor1, matriz1)
lista1
# Uma lista é algo que contem objetos (que podem ser de diferentes tipos).

vetor.logico <- c(TRUE, TRUE, T, T, FALSE, FALSE, F, F)
vetor.logico
### Um objeto lógico contem verdadeiros (TRUE ou T) e falsos (FALSE ou F)
vetor.caracteres <- c("Miconia", "Leandra", "Tibouchina", "Microlicia")
vetor.caracteres
# Caracters - character - representam texto (coisas não-numéricas)
vetor.fator <- c("Borda", "Borda", "Borda", "Interior", "Interior", "Interior")
vetor.fator <- as.factor(vetor.fator)
vetor.fator
# O comando as.factor transforma caracteres em fatores.
# Fator (factor) é uma variável categórica com diferentes níveis (no caso, Borda e Interior)
vetor.num <- c(1.1, 2.2, 3.1, 4.5, 1)
vetor.num
# Num: números que podem ser inteiros ou decimais
vetor.int <- 1:10
vetor.int
# Int: números inteiros apenas.

### Para ver a estrutura de um objeto, usamos o comando str:
str(vetor1)
str(matriz1)
str(lista1)
str(vetor.logico)
str(vetor.caracteres)
str(vetor.fator)
str(vetor.num)
str(vetor.int)


########### Parte 3 - Simulando dados ###############

# Simulações podem ajudar a entender a estatística e o programa.
# Para simular dados, precisamos definir de qual função de distribuição de probabilidades eles vêm.
# Vamos usar a distribuição normal e a uniforme.

vetor.normal <- rnorm(n=30, mean=0, sd=1)
vetor.normal <- rnorm(30, 0, 1)
# Os dois comandos acima são equivalentes. 
# Podemos especificar o nome dos argumentos na função;
# Ou podemos simplesmente colocar eles na ordem correta.
vetor.uniforme <- runif(n=30, min=0, max=1)
str(vetor.uniforme)
str(vetor.normal)

### Exercício: 
#### 1) Criem uma matriz com valores de uma distribuição normal (ao menos duas linhas e duas colunas)
#### 2) Mesma coisa com uma distribuição uniforme
#### 3) Somem as duas e armazenem o resultado como um novo objeto.



####### Parte 4 - Fazendo gráficos ####################

# O R faz gráficos lindos, mas tem que saber usar.
# Recomendo que aprendam a usar o ggplot2.
# Eu não sei usar o ggplot2, então vamos com o básico mesmo :-)

plot(vetor.normal) # A variável em função da ordem de observações
plot(vetor.uniforme)
plot(vetor.normal ~ vetor.uniforme) # Uma variável em função da outra

# Para deixar um gráfico abaixo do outro:
par(mfrow=c(2,1), mar=c(4,4,2,2))
hist(vetor.normal, breaks=12)
hist(vetor.uniforme, breaks=12)

par(mfrow=c(2,1), mar=c(4,4,2,2))
qqnorm(vetor.normal) # Faz o gráfico
qqline(vetor.normal) # Coloca a linha
qqnorm(vetor.uniforme)
qqline(vetor.uniforme)

# Testes de normalidade:
shapiro.test(vetor.normal)
shapiro.test(vetor.uniforme)

# mfrow: número de linhas, depois número de colunas
# mar: tamanho de cada margem (abaixo, esquerda, acima, direita)

# Vamos simular dados para uma regressão:

dados.sim.x <- runif(30, 0, 300)
dados.sim.y1 <- rnorm(30, dados.sim.x, 30)
plot(dados.sim.y1 ~ dados.sim.x)
# Simulei uma distribuição normal com média dada pela nossa distribuição anterior
# Acrescentarei um funil (heterogeneidade de variâncias)
dados.sim.y2 <- rnorm(30, dados.sim.x, dados.sim.x/5)
plot(dados.sim.y2 ~ dados.sim.x)
# Neste caso, a variância aumenta com os valores de x.

### Exercício: coloquem estes gráficos um do lado do outro usando par

############ Parte 5 - Testando os testes ##############

### Vamos aplicar regressão linear e teste t sobre dados simulados

mod.lin.sim1 <- lm(dados.sim.y1 ~ dados.sim.x)
mod.lin.sim2 <- lm(dados.sim.y2 ~ dados.sim.x)
# É a mesma sintaxe que usamos pros gráficos.

plot(mod.lin.sim1)
# Ele faz quatro gráficos, então vamos colocá-los juntos:
par(mfrow = c(2,2), mar=c(3,3,2,2))
plot(mod.lin.sim1)
par(mfrow = c(2,2), mar=c(3,3,2,2))
plot(mod.lin.sim2)
# Um fortíssimo padrão residual no segundo gráfico (como esperado).
### Para extrair as informações:
summary(mod.lin.sim1)
summary(mod.lin.sim2)


### Teste t - Vamos comparar duas amostras com a mesma média
dados.sim.mean10 <- rnorm(42, 10, 3)
dados.sim.mean10b <- rnorm(42, 10, 3)
t.test(dados.sim.mean10, dados.sim.mean10b)

# Há dois gŕaficos simples de fazer para isso:
## Boxplot e Jitter plot

# Para ambos os gráficos, precisamos:
## 1) Juntar as variáveis,
## 2) Criar uma variável que será o eixo X.

dados.sim.juntos <- c(dados.sim.mean10, dados.sim.mean10b)
dados.sim.grupo <- c(rep("A",42), rep("B",42))
# O comando rep repete o valor ("A") certo número de vezes (42)
# E precisamos definir que isso é um fator, e não um caracter
dados.sim.grupo <- as.factor(dados.sim.grupo)
plot(dados.sim.juntos ~ dados.sim.grupo)
# Neste caso, o R automaticamente faz o boxplot.

# Para fazer o jitter plot:
## Primeiro transformamos os grupos em número
dados.sim.grupo.num <- as.numeric(dados.sim.grupo)
# O R consegue transformar automaticamente em número.
# Então adicionamos um ruído
dados.sim.grupo.num.jitter <- jitter(dados.sim.grupo.num)
# E fazemos o gráfico
plot(dados.sim.juntos ~ dados.sim.grupo.num.jitter)

## Podemos inclusive fazer isso em um comando só:
plot(dados.sim.juntos ~ jitter(as.numeric(dados.sim.grupo)))

### E agora com médias diferentes
dados.sim.mean20 <- rnorm(42, 20, 3)
t.test(dados.sim.mean10, dados.sim.mean20)
dados.sim.juntos2 <- c(dados.sim.mean10, dados.sim.mean20)
plot(dados.sim.juntos2 ~ jitter(as.numeric(dados.sim.grupo)))

### Exercício: Testem diferentes combinações de média e desvio padrão
### Objetivo: ter uma noção de quão grandes devem ser as diferenças para serem detectáveis.

########### Parte 5 - Inserindo dados ################################

# Este é o momento pelo qual estavam esperando, né? :-)
# O ideal é ter os dados em arquivos .txt ou .csv.
# É possível copiar direto do clipboard (ctrl c), mas nem sempre funciona.
# Tendo os arquivos .txt, também fica mais fácil repetir as análises.
# Acreditem. Aprendi isso do jeito difícil. :-)

# Primeiro, precisamos falar pro R onde estão nossas arquivos.

setwd("/home/pavel/Profissional/Ensino/Disciplinas/EstatisticaBasica_UESC_PPGECB_2017/Planilhas")
getwd()
# Usamos este segundo comando para testar se estamos na pasta certa.
# wd ver de working directory, ou pasta (diretório) de trabalho.

# Podemos ver que arquivos temos nesta pasta
list.files()

# Vamos começar novamente com os dados de Assimetria Flutuante
read.table("Assimetria.txt")
# Ele abre o arquivo. Precisamos dar um nome a ele!
dados.assim <- read.table("Assimetria.txt")
dados.assim
# Muito longo. Vamos olhar só o cabeçalho.
head(dados.assim)
# Parece bom?
# Vamos olhar também a estrutura
str(dados.assim)
# Temos uma variável... categórica... com 972 níveis... Ein???
# Esquecemos do cabeçalho! (Nome da coluna)
dados.assim <- read.table("Assimetria.txt", header=TRUE)
head(dados.assim)
str(dados.assim)
# Agora sim - é uma variável numérico :-)

### Exercício: sobre estes dados, façam:
# 1) Histograma
# 2) Gráfico de probabilidade normal (com a linha)
# 3) Teste de Shapiro-Wilk
# 4) Calculem média, variância e desvio-padrão
# 4) Testes a hipótese nula de que a média é igual a zero
## Para o 4) - usem a função t.test. Usem o help - ?t.test


### E agora trabalhando com um outro conjunto de dados
### Dados de crescimento para teste t, em homenagem à 1E3lena (Milena)

dados.cresc <- read.table("Crescimento.txt", header=T, sep="\t", dec=".")
head(dados.cresc)
str(dados.cresc)
# Temos dois novos elevementos aqui. 
## sep="\t" diz que as colunas são separadas por tabulações (TAB).
## dec="." diz que o separador decimal é o ponto.
# Também podemos definir que a primeira coluna são nomes das linhas
dados.cresc <- read.table("Crescimento.txt", header=T, sep="\t", dec=".", row.names=1)
str(dados.cresc)
head(dados.cresc)

############ Parte 6 - Indexação ###################################

### Indexação é quando pegamos apenas uma parte de um objeto.

# Para pegar uma coluna:
dados.cresc[,1]
dados.cresc[,"Aug.11"]
dados.cresc$Aug.11
# Usando colchetes, colocamos o número das linhas e das colunas.
# Se não colocamos nada nas linhas ou nas colunas, todas as linhas ou colunas são usadas.
# O cifrão $ só funciona para data.frames.

### Exercício: Façam um jitter plot e um teste t com estes dados.

################ Parte 7 - Salvando figuras e dados ############

# O R permite também criar pastas e arquivos.
# Vamos criar uma pasta num nível acima

setwd("..")
# Isso sobe um nível
getwd()
dir.create("Resultados_R")
setwd("Resultados_R")
# Criamos uma nova pasta, chamada "Resultados_R", e entramos nela.


