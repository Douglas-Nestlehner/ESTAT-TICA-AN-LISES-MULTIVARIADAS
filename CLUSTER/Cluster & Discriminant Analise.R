library(tidyverse)
library(factoextra)
library(xtable)
library(reshape2)
library(factoextra)

setwd("")

set.seed(4)

# Carregando os Dados
df <- read.table("dados_atividade_5.txt", header = T)
colnames(df) <- c("Admin", "Educ", "Saude", "Urban", "Munic")
str(df)

xtable(df)
xtable(summary(df[,-5]))

# Boxplot
data_mod <- melt(df, 
                 id.vars = "Munic", 
                 measure.vars = c("Admin", "Educ", "Saude", "Urban")                 )

colorder <- c( "slateblue3", "sienna1", "olivedrab4", "tomato2")
ggplot(data_mod, aes(x=variable, y=value,fill=variable)) +
  geom_boxplot()+
  scale_fill_manual(values = colorder)+
  labs(title="",
       x = "", y="", fill = "Variável")+
  #scale_color_manual(values = c("green", "orange", "red", "blue"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 

# os dados estao na mesma escala, nao precisa padronizar


#########################################
# Analise de cluster - Linkagem Completa
df_clust <- df[,-5]
rownames(df_clust) <- df[,5]

distancia <- dist(df_clust, method="euclidean")
xtable(round(as.matrix(distancia)[1:5,1:5],3))

# Linkagem Completa
dist.munic <- hclust(d=distancia, method="complete")

alturas <- dist.munic$height
h <- length(alturas)
h.steps <- 1:h

# Corte Linkagem Completa
corte <- mean(alturas) + 1.25*sd(alturas)
corte

plot(h.steps, alturas, type="l", xlab = "junção")
points(h.steps, alturas, pch=19)
lines(c(0,h), c(corte, corte), lty = 2, col="red")

# Dendrograma
fviz_dend(dist.munic)+
  theme(plot.title = element_text(hjust = 0.5)) 

fviz_dend(dist.munic, cex=0.5, k = 3,
          color_labels_by_k = F, 
          rect = T)+
  theme(plot.title = element_text(hjust = 0.5)) 


#########################################
# Analise de cluster K-Means

## Indicar numero de grupos
fviz_nbclust(df_clust, kmeans, method = "wss")

fviz_nbclust(df_clust, kmeans, method = "wss")+
  geom_vline(xintercept = 3, linetype = 2)

# Agrupamento k-medias

km.clust <- kmeans(df_clust, 3, nstart=25)
print(km.clust)

aggregate(df_clust, by=list(cluster=km.clust$cluster), mean)

df2 <- cbind(df, cluster=km.clust$cluster)
head(df2)
rownames(df2) <- df2[,5]

km.clust$cluster

fviz_cluster(km.clust, data=df_clust,
             palette = c("green4", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal())


#######################################
#### ATIVIDADE 6 ######################
######################################
# Analise discriminante
library(MASS)
library(psych)
head(df2)
df2$cluster <- as.factor(df2$cluster)
df2

xtable(arrange(data.frame(df2$cluster, df2$Munic, df2$Admin, df2$Educ, df2$Saude, df2$Urban), df2$cluster))

# Analise descritiva
# Boxplot - Adimn
colorder2 <- c("slateblue3", "sienna1", "olivedrab4")

g1 <- ggplot(df2, aes(x = cluster, y = Admin, fill = cluster))+
  geom_boxplot()+
  scale_fill_manual(values = colorder2)+
  labs(title="Admin",
       x = "", y="", fill = "Grupo")+
  #scale_color_manual(values = c("green", "orange", "red", "blue"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 

# Boxplot - Educ
g2 <- ggplot(df2, aes(x = cluster, y = Educ, fill = cluster))+
  geom_boxplot()+
  scale_fill_manual(values = colorder2)+
  labs(title="Educ",
       x = "", y="", fill = "Grupo")+
  #scale_color_manual(values = c("green", "orange", "red", "blue"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 

# Boxplot - Saude
g3 <- ggplot(df2, aes(x = cluster, y = Saude, fill = cluster))+
  geom_boxplot()+
  scale_fill_manual(values = colorder2)+
  labs(title="Saude",
       x = "", y="", fill = "Grupo")+
  #scale_color_manual(values = c("green", "orange", "red", "blue"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 

# Boxplot - Urban
g4 <- ggplot(df2, aes(x = cluster, y = Urban, fill = cluster))+
  geom_boxplot()+
  scale_fill_manual(values = colorder2)+
  labs(title="Urban",
       x = "", y="", fill = "Grupo")+
  #scale_color_manual(values = c("green", "orange", "red", "blue"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 

library(gridExtra)
grid.arrange(g1,g2,g3,g4, ncol = 2)


# diagrama de dispercao, histograma e valores de correlação:
pairs.panels(df2[1:4],
             gap = 0,
             bg = c("red", "green", "blue")[df2$cluster],
             pch = 21)


### LDA
# Com toda a base
df3 <- df2[c(-1,-24),] # removendo Aracatuba e Votuporanga
model <- lda(cluster ~ Admin + Educ + Saude + Urban, data=df3)
model

p1 <- predict(model, df3)$class
tab <- table(Predicted = p1, Actual = df3$cluster)
tab
xtable(tab) # 100%

lda_plot <- cbind(df3, predict(model)$x)

ggplot(lda_plot, aes(LD1, LD2)) +
  geom_point(aes(color = cluster))

predict(model, df2[c(1,24),])

library(klaR)
partimat(cluster ~ Admin + Educ + Saude + Urban,
         data = df2, metodo = "lda")