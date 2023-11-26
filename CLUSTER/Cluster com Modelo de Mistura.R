library(mclust)

dados = iris
head(dados)

#separando a vaiavel categorica dos dados

# variavel categorica
cat = dados[,5]

# variaveis continuas
dados = dados[,-5]

# Na chamada de função Mclust(), apenas a matriz de dados fornecida, 
# e o número de componentes de mistura e a parametrização de covariância só 
# selecionados usando o 
# Critério de Informação Bayesiano (BIC).

M1 = Mclust(dados)
summary(M1) # Numero de componentes de mistura indicado 
plot(M1, what = "BIC") # 
#plot(M1, what = "classification")
# Quanto maior o bic melhor;
# O maior BIC, está relacionado ao numero de componentes 2, porem o valor é 
#bem próximo ao numero de componentes 3;

# Ajuste considerando um total de 3 componentes
# O ajuste do modelo 3 já considera o numero de componentes, portanto, agora vai
#selecionar apenas a melhor,
#parametrização de covariancia, e mostra os resultados;
M2 = Mclust(dados, 3)
summary(M2, parameters = T)
plot(M2, what = "classification")

table(cat, M2\$classification)


# distriuições misturadas
mixmodBIC = mixmodCluster(dados, 3)
mixmodBIC
summary(mixmodBIC)

#plot(mixmodBIC)
hist(mixmodBIC)
#mostra quais distribuições foram misturadas