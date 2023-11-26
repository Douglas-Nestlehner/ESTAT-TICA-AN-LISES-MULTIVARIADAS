#leitura e organização dos dados
original = read.csv("BankChurners.csv", sep=",", header = TRUE,
                    stringsAsFactors = FALSE, encoding = "UTF-8")

dados = original[,-c(22,23)]
nomes=c("Identificador","Atividade","Idade","Sexo","Dependentes","Nível Educacional","Estado Civil",
        "Renda Anual", "Tipo do Cartão", "Período de Relacionamento", "Nº de Produtos Mantidos",
        "Meses inativos U.A","Nº de Contatos U.A","Limite de Crédito","Saldo Rotativo","Média de Crédito Aberto U.A",
        "Mudança no Valor Transacional","Valor Total da Transação U.A","Nº de Transações U.A","Mudança no Nº Transacional", "Taxa de Utilização Média")

library("data.table") 
setnames(dados, nomes)
dados

#-- Salvando essa base 
write.csv(dados, "dadoscartao.csv", row.names = F)

#verificando dados faltantes
any(is.na(dados))

#separando numérica de categórica
dadosnum=dados[,c(3,10,14,15,16,17,18,19,20,21)]
dadoscat=dados[,-c(3,10,14,15,16,17,18,19,20,21)]

#Organizando base Numérica
nomes2=c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10")



library("data.table") 
setnames(dadosnum, nomes2)

#Cabeçalho
str(dadosnum)
head(dadosnum)

#AlgumasEstatísticas 
options(scipen = 999)
summary(dadosnum)
var(dadosnum)


#analise descritiva
library(gridExtra)

tema = theme(axis.title.x = element_text(size = 14),axis.text.x = element_text(size = 12),axis.title.y = element_text(size = 14))

## X1
box_X1 = ggplot(dadosnum)+
  geom_boxplot(aes(x = X1), fill = "purple", col = "black")+
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Idade", y = "") + tema

his_X1 = ggplot(dadosnum)+
  geom_histogram(aes(x = X1), fill = "purple", col = "black", bins = 10)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Idade", y = "Frequência Absoluta") + tema

grid.arrange(box_X1 , his_X1, ncol=2, top= "Idade do cliente (em anos)")


## X2
box_X2 = ggplot(dadosnum)+
  geom_boxplot(aes(x = X2), fill = "purple", col = "black")+
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Período de Relacionamento", y = "") + tema

his_X2 = ggplot(dadosnum)+
  geom_histogram(aes(x = X2), fill = "purple", col = "black", bins = 10)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Período de Relacionamento", y = "Frequência Absoluta") + tema

grid.arrange(box_X2 , his_X2, ncol=2, top= "Período de relacionamento com o Banco (em meses)")



## X3
box_X3 = ggplot(dadosnum)+
  geom_boxplot(aes(x = X3), fill = "purple", col = "black")+
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Limite de crédito", y = "") + tema

his_X3 = ggplot(dadosnum)+
  geom_histogram(aes(x = X3), fill = "purple", col = "black", bins = 10)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Limite de crédito", y = "Frequência Absoluta") + tema

grid.arrange(box_X3 , his_X3, ncol=2, top= "Limite de crédito no cartão de crédito (U$)")


## X4
box_X4 = ggplot(dadosnum)+
  geom_boxplot(aes(x = X4), fill = "purple", col = "black")+
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Saldo rotativo total", y = "") + tema

his_X4 = ggplot(dadosnum)+
  geom_histogram(aes(x = X4), fill = "purple", col = "black", bins = 10)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Saldo rotativo total", y = "Frequência Absoluta") + tema

grid.arrange(box_X4 , his_X4, ncol=2, top= "Saldo rotativo total no cartão de crédito (U$)")



## X5
box_X5 = ggplot(dadosnum)+
  geom_boxplot(aes(x = X5), fill = "purple", col = "black")+
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Média de linhas abertas", y = "") + tema

his_X5 = ggplot(dadosnum)+
  geom_histogram(aes(x = X5), fill = "purple", col = "black", bins = 10)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Média de linhas abertas", y = "Frequência Absoluta") + tema

grid.arrange(box_X5 , his_X5, ncol=2, top= "Linha de crédito aberta para compra (média dos últimos 12 meses)")


## X6
box_X6 = ggplot(dadosnum)+
  geom_boxplot(aes(x = X6), fill = "purple", col = "black")+
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Mudanças no valor", y = "") + tema

his_X6 = ggplot(dadosnum)+
  geom_histogram(aes(x = X6), fill = "purple", col = "black", bins = 10)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Mudanças no valor", y = "Frequência Absoluta") + tema

grid.arrange(box_X6 , his_X6, ncol=2, top= "Mudança no valor da transação (Q4 sobre Q1)")


## X7
box_X7 = ggplot(dadosnum)+
  geom_boxplot(aes(x = X7), fill = "purple", col = "black")+
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Valor total", y = "") + tema

his_X7 = ggplot(dadosnum)+
  geom_histogram(aes(x = X7), fill = "purple", col = "black", bins = 10)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Valor total", y = "Frequência Absoluta") + tema

grid.arrange(box_X7, his_X7, ncol=2, top= "Valor total da transação (últimos 12 meses)")


## X8
box_X8 = ggplot(dadosnum)+
  geom_boxplot(aes(x = X8), fill = "purple", col = "black")+
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Contagem total", y = "") + tema

his_X8 = ggplot(dadosnum)+
  geom_histogram(aes(x = X8), fill = "purple", col = "black", bins = 10)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Contagem total", y = "Frequência Absoluta") + tema

grid.arrange(box_X8 , his_X8, ncol=2, top= "Contagem total de transações (nos últimos 12 meses)")


## X9
box_X9 = ggplot(dadosnum)+
  geom_boxplot(aes(x = X9), fill = "purple", col = "black")+
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Mudanças na contagem", y = "") + tema

his_X9 = ggplot(dadosnum)+
  geom_histogram(aes(x = X9), fill = "purple", col = "black", bins = 10)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = "Mudanças na contagem", y = "Frequência Absoluta") + tema

grid.arrange(box_X9 , his_X9, ncol=2, top= "Mudança na contagem de transações (Q4 sobre Q1)")


## X10
box_X10 = ggplot(dadosnum)+
  geom_boxplot(aes(x = X10), fill = "purple", col = "black")+
  theme_bw()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = " Taxa de utilização média", y = "") + tema

his_X10 = ggplot(dadosnum)+
  geom_histogram(aes(x = X10), fill = "purple", col = "black", bins = 10)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 18, family ="serif"))+
  labs(x = " Taxa de utilização média", y = "Frequência Absoluta") + tema

grid.arrange(box_X10 , his_X10, ncol=2, top= " Taxa de utilização média do cartão")



#Normalidade

library(MVN)
library(AID)
library(dgof)
require("MVA")
require(GGally)
require(CCA)


mvn(dadosnum, mvnTest= c("royston"), covariance=TRUE, scale=FALSE, desc=TRUE, 
    transform="none", R=1000, univariateTest = c("AD"), univariatePlot ="qq", multivariatePlot = "qq",
    multivariateOutlierMethod = "quan", bcType="optimal", showOutliers = TRUE, showNewData = FALSE)


library("nortest")

shapiro.test(dadosnum2$X1)
ad.test(dadosnum2$X1)
qqnorm(dadosnum2$X1)
qqline(dadosnum2$X1, col="red")
qqnorm(dadosnum$X2)
qqline(dadosnum$X2, col="red")
qqnorm(dadosnum$X3)
qqline(dadosnum$X3, col="red")
qqnorm(dadosnum$X4)
qqline(dadosnum$X4, col="red")
qqnorm(dadosnum$X5)
qqline(dadosnum$X5, col="red")
qqnorm(dadosnum$X6)
qqline(dadosnum$X6, col="red")
qqnorm(dadosnum$X7)
qqline(dadosnum$X7, col="red")
qqnorm(dadosnum$X8)
qqline(dadosnum$X8, col="red")
qqnorm(dadosnum$X9)
qqline(dadosnum$X9, col="red")
qqnorm(dadosnum$X10)
qqline(dadosnum$X10, col="red")
ks.test(dadosnum$X1,mean(dadosnum$X1),sd(dadosnum$X1))
ks.test(dadosnum$X2,mean(dadosnum$X2),sd(dadosnum$X2))
ks.test(dadosnum$X3,mean(dadosnum$X3),sd(dadosnum$X3))
ks.test(dadosnum$X4,mean(dadosnum$X4),sd(dadosnum$X4))
ks.test(dadosnum$X5,mean(dadosnum$X5),sd(dadosnum$X5))
ks.test(dadosnum$X10,mean(dadosnum$X10),sd(dadosnum$X10))

bestNormalize(dadosnum$X4)
bestNormalize(dadosnum$X10)

## Escolhendo 2 covariaveis 
df_ATV2 = data.frame(dadosnum$X1, dadosnum$X2)
nomes3=c("X1","X2")
setnames(df_ATV2, nomes3)
df_ATV2

#distancia estatistica
d3 = sqrt(mahalanobis(df_ATV2, 0, cov(df_ATV2)))
d3

generalizada$mahalanobis = d3

# Distancia do vetor de médias

# Obtendo o vetor X1 - X1_mean
novoX1 = 0
for(i in 1:nrow(df_ATV2)){
  novoX1[i] = df_ATV2$X1[i] - mean(df_ATV2$X1)
}

novoX1

# Obtendo o vetor X2 - X2_mean
novoX2 = 0
for(i in 1:nrow(df_ATV2)){
  novoX2[i] = df_ATV2$X2[i] - mean(df_ATV2$X2)
}

novoX2

novoX = data.frame(novoX1, novoX2)

# calculando a distancia 
C = data.matrix(novoX)
d3 = 0
for(i in 1:nrow(df_ATV2)){
  
  d3[i] = sqrt(t(C[i,])%*%solve(s)%*%C[i,])
  
}

options(max.print=100)
d3

d4 = sqrt(mahalanobis(df_ATV2, colMeans(df_ATV2), cov(df_ATV2)))
d4

generalizada$distancia_media = d3
generalizada$mahalanobis_media = d4
View(generalizada)

x = generalizada
x$distancia = NULL
x$Distancia_2 = NULL
x$mahalanobis = NULL
x$mahalanobis_media = NULL
x$ID = c(1:10127)
library(dplyr)
x%>% 
  arrange(desc(x$distancia_media))

View(x)


###Componentes principais
library(FactoMineR)
acp = PCA(dadosnum, scale.unit=TRUE, graph=TRUE)

#autovalores
acp$eig 

#autovetores
acp$var$coord

#escores fatoriais
acp$ind$coord
summary(acp$ind$coord)


# Scree Plot
library(factoextra)
fviz_eig(acp, addlabels=TRUE, xlab = "Autovalores",ylab = "Porcentagem de Explicação da Variância")


library ("corrplot")
#Qualidade de representação da variáveis 
corrplot(acp$var$coord, is.corr=FALSE,title = "
               Variável por componente")

# Contribuição das Variáveis para os CP
col3 = hcl.colors(10, "PuBu", rev = TRUE)
corrplot(acp$var$contrib, is.corr= FALSE,title = "
               Componente por variável",col = col3)


#graficos acp
dadosacp= data.frame(acp$ind$coord)

d1d2 =ggplot(dadosacp, aes(x=Dim.2, y=Dim.1)) + 
  xlab("Atividade em Transações")+ ylab("Score do Cartão de Crédito")+
  geom_point()

d3d4= ggplot(dadosacp, aes(x=Dim.4, y=Dim.3)) + 
  xlab("Mudanças nas operações")+ ylab("Maturidade do cliente")+
  geom_point()

grid.arrange(d1d2 , d3d4, ncol=2, top= "Dispersão dos Componentes")