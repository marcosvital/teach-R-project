############################################################
##### Trabalhando com dados em R ###########################
##### Curso de Estatística Básica - PPGECB/UESC ############
###### 20 Março - 6 Abril 2017 #############################
###### Por Pavel Dodonov - pdodonov@gmail.com ##############
############################################################

########### Parte 1 - Inserindo dados ################################

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
# Agora sim - é uma variável numérica :-)

### Exercício: sobre estes dados, façam:
# 1) Histograma
# 2) Gráfico de probabilidade normal (com a linha)
# 3) Teste de Shapiro-Wilk
# 4) Calculem média, variância e desvio-padrão

### Para testar a hipótese nula de que a média é igual a zero:
t.test(dados.assim)


### E agora trabalhando com um outro conjunto de dados
### Dados de crescimento para teste t, em homenagem à 1E3lena (Milena)

dados.cresc <- read.table("Crescimento.txt", header=T, sep="\t", dec=".")
# Temos dois novos elevementos aqui. 
## sep="\t" diz que as colunas são separadas por tabulações (TAB).
## dec="." diz que o separador decimal é o ponto.
# Também podemos definir que a primeira coluna são nomes das linhas
head(dados.cresc)
str(dados.cresc)
### É um data.frame - esse termo já apareceu antes.
## Um data.frame é uma planilha de dados
## Tem características de matrizes - mesmo número de observações em cada coluna;
## E de listas - podemos ter colunas de diferentes tipos (número, texto etc).
dados.cresc <- read.table("Crescimento.txt", header=T, sep="\t", dec=".", row.names=1)
str(dados.cresc)
head(dados.cresc)
# Para vermos nomes das colunas:
colnames(dados.cresc)
names(dados.cresc)

############ Parte 6 - Indexação ###################################

### Indexação é quando pegamos apenas uma parte de um objeto.

# Para pegar uma coluna:
dados.cresc[,1]
dados.cresc[,"Aug.11"]
dados.cresc$Aug.11
# Usando colchetes, colocamos o número das linhas e das colunas.
# Se não colocamos nada nas linhas ou nas colunas, todas as linhas ou colunas são usadas.
# O cifrão $ só funciona para data.frames.

# Fazer histogramas e gráficos de probilidade normal

par(mfrow=c(2,2), mar=c(4,4,2,2))
hist(dados.cresc[,1])
hist(dados.cresc[,2])
qqnorm(dados.cresc[,1])
qqline(dados.cresc[,1])
qqnorm(dados.cresc[,2])
qqline(dados.cresc[,2])


# Para fazer um teste t disso:

t.test(dados.cresc[,1], dados.cresc[,2])
t.test(dados.cresc[,1], dados.cresc[,2], paired=T)

### Exercício:
#### 1) Calculem a diferença entre as duas colunas
#### 2) Façam um histograma dessa diferença
#### 3) Apliquem o teste t de uma amostra sobre as diferenças

### Fazendo gráficos - boxplot e jitter plot:

# 1) Verifiquem quantas linhas temos no objeto original
nrow(dados.cresc)
# 2) Juntem as duas colunas em uma só
dados.cresc.juntos <- c(dados.cresc[,1], dados.cresc[,2])
# 3) Criem um vetor (factor) mostrando a que grupo cada observação pertence
dados.cresc.grupo <- c(rep("A",nrow(dados.cresc)),rep("B",nrow(dados.cresc)))
# 4) Transformem em fator
dados.cresc.grupo <- as.factor(dados.cresc.grupo)
# 5) Usem o comando plot:
plot(dados.cresc.juntos ~ dados.cresc.grupo)
# 6) Transformem isso em números:
dados.cresc.grupo.num <- as.numeric(dados.cresc.grupo)
# 7) Façam o gráfico adicionando um ruído aleatório:
plot(dados.cresc.juntos ~ jitter(dados.cresc.grupo.num))


################ Parte 7 - Salvando figuras ############

# O R permite também criar pastas e arquivos.
# Vamos criar uma pasta num nível acima

setwd("..")
# Isso sobe um nível
getwd()
dir.create("Resultados_R")
setwd("Resultados_R")
# Criamos uma nova pasta, chamada "Resultados_R", e entramos nela.
# Então preparamos o arquivo onde salvaremos a imagem

png(filename="Crescimento_JitterPlot.png", height=20, width=20, unit="cm", res=300)
plot(dados.cresc.juntos ~ jitter(dados.cresc.grupo.num))
dev.off()

# 1) Criamos uma figura de 20 x 20 cm, com resolução de 300 DPI.
# 2) Fazemos o gráfico
# 3) Fechamos a figura, para que o R a salve.

### Salvem os histogramas e gráficos de probabilidade normal da Assimetria e os gráficos do Crescimento.

#################### Parte 8 - Modelos lineares #######################

### Trabalharemos com os dados de Alometria
setwd("../Planilhas") # Para voltarmos à pasta da planilhas
getwd()
dados.alom <- read.table("Alometria.txt", header=T, sep="\t", dec=".")
str(dados.alom)
# Coluna 1: se são plantas queimadas ou não queimadas
# Coluna 2: H - altura
# Coluna 3: DAS - diâmetro

# O correto seria usar regressão RMA, mas isso requer instalar um pacote.
# Vamos usar regressão ordinária mesmo, para fins de prática.

# Os valores de plantas queimadas e não queimadas estão misturados
# Vamos separar.

dados.alom.Q <- subset(dados.alom, Status=="Q")
# O sinal == significa "igual a" - diferente do =, que especifica argumentos.
# Então criamos um objeto apenas com as linhas das plantas queimadas
str(dados.alom.Q)
unique(dados.alom.Q$Status)
# O fator tem dois niveis, mas apenas o nível Q está presente aqui.

### Exercício: 
#### 1) Façam gráficos da H em função do DAS, para plantas queimadas e não queimadas;
#### 2) Coloquem os dois gráficos lado a lado;
#### 3) Salvem eles em um arquivo PNG com 15 cm de largura e 15 cm de altura.

# Para ajustar uma regressão linear:

mod.alom.Q <- lm(H ~ DAS, data=dados.alom.Q)

# Começo o nome com "mod" porque é um modelo.
# O argumento data=dados.alom.Q diz que estaremos trabalhando com colunas deste objeto.
# Para ver o resultado:
mod.alom.Q
summary(mod.alom.Q)

# Para adicionar a linha à figura:
plot(H ~ DAS, data=dados.alom.Q)
abline(mod.alom.Q)

# Para extrair os resíduos:
resid.alom.Q <- resid(mod.alom.Q)
hist(resid.alom.Q, breaks=42)
qqnorm(resid.alom.Q)
qqline(resid.alom.Q)

# Mas o R sabe fazer gráficos bonitos assim por conta própria:
par(mfrow=c(2,2))
plot(mod.alom.Q)
# Estes são os gráficos de validação. Usem e abusem deles. :-)

#### Comparando a inclinação entre plantas queimadas e não-queimadas:

# Podemos incluir também o Status na análise.
mod.alom.tot <- lm(H ~ DAS + Status, data=dados.alom)
summary(mod.alom.tot)
# O Status e o DAS têm efeitos significativos.
par(mfrow=c(2,2))
plot(mod.alom.tot)
# Será que o Status afeta a inclinação?
# Testamos com interação:
mod.alom.tot2 <- lm(H ~ DAS + Status + Status:DAS, data=dados.alom)
summary(mod.alom.tot2)
# Esta análise é a ANCOVA - Analysis of Covariance
# Não há interação - a inclinação é a mesma, só muda o valor médio.
par(mfrow=c(2,2))
plot(mod.alom.tot2)
# Mas, santo desvio de normalidade, Batman!

plot(H ~ DAS, data=dados.alom, col=as.numeric(Status))
### Cores diferentes pra plantas queimadas e não queimadas :-)

############### Parte 4 - ANOVA ########################
dados.alelo <- read.table("Alelopatia.txt", header=T, sep="\t", dec=".")
str(dados.alelo)

### Para calcularmos as médias dos grupos, usamos a função apply:
apply(dados.alelo, 2, mean)
# Ele vai aplicar a função mean sobre as colunas (daí o 2) do objeto dados.alelos
### Para fazer o histograma:
par(mfrow=c(5,1), mar=c(2,2,2,2))
apply(dados.alelo, 2, hist)

### Para fazer ANOVA:
### Preparamos o objeto
dados.alelo.juntos <- c(dados.alelo[,1], dados.alelo[,2], dados.alelo[,3], dados.alelo[,4], dados.alelo[,5])
Nlinhas <- nrow(dados.alelo)
dados.alelo.grupo <- c(rep(1,Nlinhas),rep(2,Nlinhas),rep(3,Nlinhas),rep(4,Nlinhas),rep(5,Nlinhas))
dados.alelo.grupo <- as.factor(dados.alelo.grupo)
### Rodamos a ANOVA:
anova.alelo <- aov(dados.alelo.juntos ~ dados.alelo.grupo)
anova.alelo
summary(anova.alelo)
### Comparações par-a-par:
TukeyHSD(anova.alelo)
# Mostra as diferenças par-a-par.

# Para achar esta função, fiz assim:
??"Tukey"
# O R procura tudo que tenha "Tukey" no nome. No caso, o teste é o Tukey Honestly Significant Differences.

### Para ANOVA bifatorial, a sintaxe seria a mesma.

### Ou seja - regressão, ANCOVA, ANOVA e também GLMs usam a mesma sintaxe geral :-)