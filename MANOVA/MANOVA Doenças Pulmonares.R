library(tidyverse)
library(gridExtra)
library(dplyr)
library(rstatix)
library(MVN)

setwd("")

# Dados
df = read.csv2("Dados4_reord.csv", sep = ";")
colnames(df) = c("paciente", "insuficiencia_cardiaca", "DPOC_IC", 
                 "O2Hb_cerebral", "O2Hb_repiratorio", "O2Hb_perferico",
                 "HHb_cerebral", "HHb_repiratorio", "HHb_perferico",
                 "situacao")

df$insuficiencia_cardiaca = as.factor(df$insuficiencia_cardiaca)

df_tlim = df %>% 
  filter(df$situacao == "tlim")
df_tlim

df_repouso = df %>% 
  filter(df$situacao == "repouso")
head(df_repouso)

summary(df_tlim)
summary(df_repouso)


# "O2Hb_cerebral" (tipo de doença) - TLIM
b1_t = ggplot(df_tlim, aes(y = df_tlim$O2Hb_cerebral, 
                           x = df_tlim$insuficiencia_cardiaca, 
                           fill = df_tlim$insuficiencia_cardiaca)) +
  geom_boxplot()+
  #geom_jitter(color = "red")+
  labs(title="O2Hb cerebral (Tlim)", x = "", y="")+
  #coord_flip()+
  scale_fill_manual("Doença", 
                    values = c("#F8766D", "#619CFF"), 
                    labels=c("DPOC-IC", "Insuficiencia cardiaca"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


# "O2Hb_cerebral" (tipo de doença) - Repouso
b1_r = ggplot(df_repouso, aes(y = df_repouso$O2Hb_cerebral,
                              x = df_repouso$insuficiencia_cardiaca, 
                              fill = df_repouso$insuficiencia_cardiaca)) +
  geom_boxplot()+
  #geom_jitter(color = "red")+
  labs(title="O2Hb cerebral (Repouso)", x = "", y="")+
  #coord_flip()+
  scale_fill_manual("Doença", 
                    values = c("#F8766D", "#619CFF"), 
                    labels=c("DPOC-IC", "Insuficiencia cardiaca"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


# O2Hb_repiratorio (tipo de doença) - TLIM
b2_t = ggplot(df_tlim, aes(y = df_tlim$O2Hb_repiratorio, 
                           x = df_tlim$insuficiencia_cardiaca, 
                           fill = df_tlim$insuficiencia_cardiaca)) +
  geom_boxplot()+
  #geom_jitter(color = "red")+
  labs(title="O2Hb_repiratorio (Tlim)", x = "", y="")+
  #coord_flip()+
  scale_fill_manual("Doença", 
                    values = c("#F8766D", "#619CFF"), 
                    labels=c("DPOC-IC", "Insuficiencia cardiaca"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


# O2Hb_repiratorio (tipo de doença) - Repouso
b2_r = ggplot(df_repouso, aes(y = df_repouso$O2Hb_repiratorio, 
                              x = df_repouso$insuficiencia_cardiaca, 
                              fill = df_repouso$insuficiencia_cardiaca)) +
  geom_boxplot()+
  #geom_jitter(color = "red")+
  labs(title="O2Hb_repiratorio (Repouso)", x = "", y="")+
  #coord_flip()+
  scale_fill_manual("Doença", 
                    values = c("#F8766D", "#619CFF"), 
                    labels=c("DPOC-IC", "Insuficiencia cardiaca"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


# O2Hb_perferico (tipo de doença) - TLIM
b3_t = ggplot(df_tlim, aes(y = df_tlim$O2Hb_perferico, 
                           x = df_tlim$insuficiencia_cardiaca, 
                           fill = df_tlim$insuficiencia_cardiaca)) +
  geom_boxplot()+
  #geom_jitter(color = "red")+
  labs(title="O2Hb_perferico (Tlim)", x = "", y="")+
  #coord_flip()+
  scale_fill_manual("Doença", 
                    values = c("#F8766D", "#619CFF"), 
                    labels=c("DPOC-IC", "Insuficiencia cardiaca"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


# O2Hb_perferico (tipo de doença) - Repouso
b3_r = ggplot(df_repouso, aes(y = df_repouso$O2Hb_perferico, 
                              x = df_repouso$insuficiencia_cardiaca, 
                              fill = df_repouso$insuficiencia_cardiaca)) +
  geom_boxplot()+
  #geom_jitter(color = "red")+
  labs(title="O2Hb_perferico (Repouso)", x = "", y="")+
  #coord_flip()+
  scale_fill_manual("Doença", 
                    values = c("#F8766D", "#619CFF"), 
                    labels=c("DPOC-IC", "Insuficiencia cardiaca"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


# HHb_cerebral (tipo de doença) - TLIM
b4_t = ggplot(df_tlim, aes(y = df_tlim$HHb_cerebral, 
                           x = df_tlim$insuficiencia_cardiaca, 
                           fill = df_tlim$insuficiencia_cardiaca)) +
  geom_boxplot()+
  #geom_jitter(color = "red")+
  labs(title="HHb_cerebral (Tlim)", x = "", y="")+
  #coord_flip()+
  scale_fill_manual("Doença", 
                    values = c("#F8766D", "#619CFF"), 
                    labels=c("DPOC-IC", "Insuficiencia cardiaca"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


# HHb_cerebral (tipo de doença) - Repouso
b4_r = ggplot(df_repouso, aes(y = df_repouso$HHb_cerebral, 
                              x = df_repouso$insuficiencia_cardiaca, 
                              fill = df_repouso$insuficiencia_cardiaca)) +
  geom_boxplot()+
  #geom_jitter(color = "red")+
  labs(title="HHb_cerebral (Repouso)", x = "", y="")+
  #coord_flip()+
  scale_fill_manual("Doença", 
                    values = c("#F8766D", "#619CFF"), 
                    labels=c("DPOC-IC", "Insuficiencia cardiaca"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


# HHb_repiratorio (tipo de doença) - TLIM
b5_t = ggplot(df_tlim, aes(y = df_tlim$HHb_repiratorio, 
                           x = df_tlim$insuficiencia_cardiaca, 
                           fill = df_tlim$insuficiencia_cardiaca)) +
  geom_boxplot()+
  #geom_jitter(color = "red")+
  labs(title="HHb_repiratorio (Tlim)", x = "", y="")+
  #coord_flip()+
  scale_fill_manual("Doença", 
                    values = c("#F8766D", "#619CFF"), 
                    labels=c("DPOC-IC", "Insuficiencia cardiaca"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


# HHb_repiratorio (tipo de doença) - Repouso
b5_r = ggplot(df_repouso, aes(y = df_repouso$HHb_repiratorio, 
                              x = df_repouso$insuficiencia_cardiaca, 
                              fill = df_repouso$insuficiencia_cardiaca)) +
  geom_boxplot()+
  #geom_jitter(color = "red")+
  labs(title="HHb_repiratorio (Repouso)", x = "", y="")+
  #coord_flip()+
  scale_fill_manual("Doença", 
                    values = c("#F8766D", "#619CFF"), 
                    labels=c("DPOC-IC", "Insuficiencia cardiaca"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


# HHb_perferico (tipo de doença) - TLIM
b6_t = ggplot(df_tlim, aes(y = df_tlim$HHb_perferico, 
                           x = df_tlim$insuficiencia_cardiaca, 
                           fill = df_tlim$insuficiencia_cardiaca)) +
  geom_boxplot()+
  #geom_jitter(color = "red")+
  labs(title="HHb_perferico (Tlim)", x = "", y="")+
  #coord_flip()+
  scale_fill_manual("Doença", 
                    values = c("#F8766D", "#619CFF"), 
                    labels=c("DPOC-IC", "Insuficiencia cardiaca"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 


# HHb_perferico (tipo de doença) - Repouso
b6_r = ggplot(df_repouso, aes(y = df_repouso$HHb_perferico, 
                              x = df_repouso$insuficiencia_cardiaca, 
                              fill = df_repouso$insuficiencia_cardiaca)) +
  geom_boxplot()+
  #geom_jitter(color = "red")+
  labs(title="HHb_perferico (Repouso)", x = "", y="")+
  #coord_flip()+
  scale_fill_manual("Doença", 
                    values = c("#F8766D", "#619CFF"), 
                    labels=c("DPOC-IC", "Insuficiencia cardiaca"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

grid.arrange(b2_r, b2_t, b3_r, b3_t,  ncol = 2)
grid.arrange(b4_r, b4_t, b5_r, b5_t,  ncol = 2)

grid.arrange(b6_r, b6_t, ncol = 2)

#     Checando normaldidade multivariada

#Teste de Henze-Zirkler
mvn(data = df[, 4:10], subset = 'Grupo', mvnTest = 'hz')


#Verificando outliers multivariados
df %>% select(4:10) %>% group_by('Grupo') %>% doo(~mahalanobis_distance(.)) %>% 
  filter(is.outlier == TRUE)

#verificando homogeneidade das matrizez de covariancia e variancia
box_m(df[, 5:10], df$Grupo)