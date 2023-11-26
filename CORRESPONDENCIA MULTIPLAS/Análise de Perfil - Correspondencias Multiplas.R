library(tidyverse)
#library(ggpattern)
library(FactoMineR)
library(factoextra)
library(xtable)

setwd("")

#Dataset:  sexlierel.dat

#Source: R.R. Clayton (1971). "Religiosity and Premarital Sexual
#Permissiveness: Elaboration of the Relationship and Debate,"
#Sociological Analysis, Vol. 32, #2, pp81-96

#Description: Tabulation among 4 scales of premarital sexual
#permissiveness, religiosity, propensity to lie and gender.

#Variables/Columns
#Gender   8    /* 1=Female, 2=Male  */
#  Scale: 16  /* 1=Ritualistic, 2=Experiential, 3=Ideological, 4=Composite*/
#  Premarital sexual permissiveness  24 /* 1=Low, 2=High */ 
#  Propensity to Lie   32  /* 1=Lower, 2=Higher  */
#  Religiosity   40  /*  1=Low, 2=High   */
#  Count    46-48


df = read.table("Dados1.txt", skip = 13)
colnames(df) = c("Sexo", "Escala", "Perm", "Mentira", "Religião", "Casos")
head(df)

df$Sexo = as.factor(df$Sexo)
df$Escala = as.factor(df$Escala)
df$Perm = as.factor(df$Perm)
df$Mentira = as.factor(df$Mentira)
df$Religião = as.factor(df$Religião)

# Transformar os dados para o formato usual
df2 <- data.frame(lapply(df, rep, df$Casos))
df2$ID <- as.factor(seq.int(nrow(df2)))
df2$Casos = NULL
head(df2)

#write.csv(df2, "dados2.csv")


# DESCRITIVA


#VARIAVEL GENERO
# Calcular contagem de observações por categoria
df_count <- as.data.frame(table(df$Gênero))

# Renomear colunas
colnames(df_count) <- c("Gênero", "count")

# Calcular freqüência relativa
df_count$freq <- df_count$count / sum(df_count$count)

# Criando o gráfico
img1 = ggplot(df_count, aes(x = Gênero, y = count, fill = Gênero)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = paste0(round(freq * 100), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") + 
  labs(x = "Gênero", y = "Contagem", title = "", 
       fill = "Gênero") + 
  scale_fill_manual(values = c("#999999", "#E69F00")) + 
  theme_classic()



#VAR PERM
# Calcular contagem de observações por categoria
df_count <- as.data.frame(table(df$Perm))

# Renomear colunas
colnames(df_count) <- c("Perm", "count")

# Calcular freqüência relativa
df_count$freq <- df_count$count / sum(df_count$count)

# Criando o gráfico
img2 = ggplot(df_count, aes(x = Perm, y = count, fill = Perm)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = paste0(round(freq * 100), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") + 
  labs(x = "Permissividade de sexo antes do casamento", y = "Contagem",
       title = "", 
       fill = "Permissividade") + 
  scale_fill_manual(values = c("#999999", "#E69F00")) + 
  theme_classic()
img2




#MENTIRA
# Calcular contagem de observações por categoria
df_count <- as.data.frame(table(df$Mentira))

# Renomear colunas
colnames(df_count) <- c("Mentira", "count")

# Calcular freqüência relativa
df_count$freq <- df_count$count / sum(df_count$count)

# Criando o gráfico
img3 = ggplot(df_count, aes(x = Mentira, y = count, fill = Mentira)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = paste0(round(freq * 100), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") + 
  labs(x = "Propensão em Mentir", y = "Contagem", title = "", 
       fill = "Propensão em Mentir") + 
  scale_fill_manual(values = c("#999999", "#E69F00")) + 
  theme_classic()
img3


RELIGIOSIDADE
# Calcular contagem de observações por categoria
df_count <- as.data.frame(table(df$Religião))

# Renomear colunas
colnames(df_count) <- c("Religião", "count")

# Calcular freqüência relativa
df_count$freq <- df_count$count / sum(df_count$count)

# Criando o gráfico
img4 = ggplot(df_count, aes(x = Religião, y = count, fill = Religião)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = paste0(round(freq * 100), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white") + 
  labs(x = "Nível de Religiosidade", y = "Contagem", title = "", 
       fill = "Religiosidade") + 
  scale_fill_manual(values = c("#999999", "#E69F00")) + 
  theme_classic()
img4


# Juntando os quatro gráficos na mesma imagem
grid.arrange(img1, img2, img3, img4, nrow = 2, ncol = 2)




#graficos bivariados todos feitos apenas trocando as variaveis
df %>%
  count(Escala, Perm) %>%
  group_by(Escala) %>%
  mutate(pct = prop.table(n) * 100) %>%
  ggplot(aes(x = Escala, y = pct, fill = Perm)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = paste0(sprintf("%1.2f", pct),"%")),
            position = position_stack(vjust = 0.5)) +
  scale_y_continuous(labels = function(x) paste0(x,"%")) +
  labs(y = "Percentual")




# ANALISE DE CORRESPONDENCIA

# Estatistitica qui-quadrado
# existe uma dependecia significativa entre as linhas e colunas ?
# H0: assume que não a associação entre as variaveis;
# H1: assum que existe associação



# Tabela de Burt
df2$ID = NULL
library(GDAtools)
burt(df2)

xtable(burt(df2))

# Teste para verificar se existe rele??o entre os fatores 
chisq.test(burt(df2))



# Analise de correspondecias multiplas

# quantidade de fatores
cats = apply(df2, 2, function(x) nlevels(as.factor(x))) # numero de fatores 

# Ajuste do MCA
mca1 = MCA(df2, graph = FALSE)

mca1$eig # autovalores

mca1$var$coord # autovetores

round(mca1$var$cos2[,1:4],2)

mca1$var$v.test

xtable(mca1$eig)

# Graficos
fviz_screeplot(mca1, xlab = "Dimensão", main= "Scree Plot", 
               linecolor="red", addlabels= TRUE,
               ylab = "Variância explicada (%)")


head(mca1$ind$coord)

mca1_vars_df = data.frame(mca1$var$coord, 
                          Variable = rep(names(cats), cats))

mca1_obs_df = data.frame(mca1$ind$coord)

# plots dois a dois
# 1x2
ggplot(data = mca1_vars_df, aes(x = Dim.1, 
                                y = Dim.2, 
                                label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("An?lise de Correspond?ncias M?ltiplas")+ 
  theme(plot.title = element_text(hjust = 0.5))

# 1x3
ggplot(data = mca1_vars_df, aes(x = Dim.1, 
                                y = Dim.3, 
                                label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("An?lise de Correspond?ncias M?ltiplas")+ 
  theme(plot.title = element_text(hjust = 0.5))


# 1x4
ggplot(data = mca1_vars_df, aes(x = Dim.1, 
                                y = Dim.4, 
                                label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("An?lise de Correspond?ncias M?ltiplas")+ 
  theme(plot.title = element_text(hjust = 0.5))

# 2x3
ggplot(data = mca1_vars_df, aes(x = Dim.2, 
                                y = Dim.3, 
                                label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("An?lise de Correspond?ncias M?ltiplas")+ 
  theme(plot.title = element_text(hjust = 0.5))

# 2x4
ggplot(data = mca1_vars_df, aes(x = Dim.2, 
                                y = Dim.4, 
                                label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("An?lise de Correspond?ncias M?ltiplas")+ 
  theme(plot.title = element_text(hjust = 0.5))


# 3x4
ggplot(data = mca1_vars_df, aes(x = Dim.3, 
                                y = Dim.4, 
                                label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("An?lise de Correspond?ncias M?ltiplas")+ 
  theme(plot.title = element_text(hjust = 0.5))

