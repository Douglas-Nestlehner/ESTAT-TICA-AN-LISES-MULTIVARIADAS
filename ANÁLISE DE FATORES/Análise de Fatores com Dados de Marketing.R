library(psych)
library(corrplot)
library(FactoMineR)
library(factoextra)

setwd("")

#########################
# Dados
df <- read.csv2("MarketingB.csv", sep = ",")
head(df)
str(df)
df$TempClien <- as.numeric(df$TempClien)



#########################
# Analise descritiva
summary(df) 

corr <- cor(df)
corrplot(corr,
         method = 'color', 
         type = 'lower',
         tl.col = '#424242',
         tl.srt = 45,
         addCoef.col = 'black',
         col = colorRampPalette(c('red', 'white', 'blue'))(200),
         tl.cex = 0.8,
         number.cex = 0.9,
         number.font = 1,
         cl.cex = 0.9,
         main = "Matriz de Correlação Grupo 1")


#########################
# Adequação amostral
KMO(df)

df = df[, KMO(df)$MSAi>0.50]
KMO(df) 

corr <- cor(df)

# teste de esfericidade de Bartlett.
cortest.bartlett(corr, n=nrow(df))


#########################
# Analise fatorial

#########
# componentes principais (com  a matriz de correlacao)

acpcor <- prcomp(df, scale = TRUE)
summary(acpcor)

plot(1:ncol(df), acpcor$sdev^2, type = "b", xlab = "Fator",
     ylab = "Variância", pch = 20, cex.axis = 1.3, cex.lab = 1.3)

## Cargas fatoriais 
k <- 3
carfat <- acpcor$rotation[, 1:k] %*% diag(acpcor$sdev[1:k])
colnames(carfat) <- paste("Fator", 1:k, sep = " ")
carfat
graf.cargas(carfat)

# F1 e F2
fviz_pca_var(acpcor, axes = c(1,2), col.var = "black")+ 
  labs(title = "Gráfico das Cargas Fatoriais", 
       x = "Fator 1", y = "Fator 2")+ 
  theme(plot.title = element_text(hjust = 0.5))

# F1 e F3
fviz_pca_var(acpcor, axes = c(1,3), col.var = "black")+ 
  labs(title = "Gráfico das Cargas Fatoriais", 
       x = "Fator 1", y = "Fator 3")+ 
  theme(plot.title = element_text(hjust = 0.5))


# F2 e F3
fviz_pca_var(acpcor, axes = c(2,3), col.var = "black")+ 
  labs(title = "Gráfico das Cargas Fatoriais", 
       x = "Fator 2", y = "Fator 3")+ 
  theme(plot.title = element_text(hjust = 0.5))


acp.mad = princomp(df, cor=T)

m <- 3
prp <- acp.mad$sdev[1:m]^2/sum(acp.mad$sdev^2)
prp.ac <- numeric(m)
prp.ac[1] <- prp[1]
for(i in 2:m){
  prp.ac[i] <- prp.ac[i-1]+prp[i]}

# Cargas Fatoriais
cargas.mad <- acp.mad$loadings[, 1:m] %*% diag(acp.mad$sdev[1:m])
colnames(cargas.mad) <- paste("Fator", 1:m, sep = " ")
cargas.mad

# Proporcao de variancia explicada
af.var <- rbind(acp.mad$sdev[1:m], prp, prp.ac)
dimnames(af.var)[[1]] <- c("Desv.padrao","prop.var","prop.acum")
af.var

# Comunalidades e variancias especificas
com.mad <- apply(cargas.mad^2,1,sum)
v.esp.mad <- diag(corr) - com.mad
AF.mad <- cbind(com.mad, v.esp.mad)
dimnames(AF.mad)[[2]] <- c("comunalidades","var.especificas")
AF.mad 

np<-ncol(df)

res.mad <- corr - (cargas.mad%*%t(cargas.mad) + diag(v.esp.mad))
round(res.mad,3)

sum(res.mad^2)

np-sum(cargas.mad[,1:2]^2)

sum(acp.mad$sdev[(m+1):np]^2)

## Rotação Varimax
##################
cargas.mad.rot <- varimax(cargas.mad)
cargas.mad.rot

comum.mad.rot <- apply(cargas.mad.rot$loadings^2,1,sum)
comum.mad.rot

v.esp.mad.rot <- diag(corr)-comum.mad.rot
v.esp.mad.rot

resid.mad.rot <- corr - (cargas.mad.rot$loadings%*%t(cargas.mad.rot$loadings ) +
                           diag(v.esp.mad.rot))
round(resid.mad.rot,4)

sum(resid.mad.rot^2)

np-sum(cargas.mad.rot$loadings[,1:2]^2)

sum(acp.mad$sdev[(m+1):np]^2)

par(mfrow = c(1,2))
graf.cargas(cargas.mad, correl=T)
graf.cargas(cargas.mad.rot$loadings, correl=T)
par(mfrow = c(1,1))

round(cbind(cargas.mad, cargas.mad.rot$loadings),3)