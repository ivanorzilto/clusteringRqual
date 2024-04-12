#script para o projeto de data science and analytcs
#clusterizacao de base de dados RQUAL-Cidades

#importando bibliotecas
pacotes <- c("caret","fmsb", "GGally","plot3D","kableExtra","factoextra", "lubridate","tidyr","pivottabler", "dplyr", "plotly", "cluster")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

#impportando os dados do arquivo csv
rqual <- read.csv("Tabela_CSV_Indicadores_RQUAL.csv", header = TRUE, sep = ";", dec = ",")

#filtrando os dados, removendo duplicadas e convertendo tipo de dados para numerico
rqualClusterCidSp01 <- subset(rqual, rqual$Prestadora == "CLARO" & rqual$Resultado != is.na(rqual$Resultado) & 
                                rqual$Serviço == "Telefonia Móvel" & rqual$Ano == 2023 & 
                                rqual$Mês  == 11 & rqual$Tipo == "Indicador IQS" & rqual$UF == "SP")
rqualClusterdSemDuplicadas <- unique(rqualClusterCidSp01) %>% mutate(Resultado = round(as.numeric(sub(",", ".", Resultado, fixed = TRUE)), digits=2))

#resumindo, transformando, pivotando os dados
rqualClusterResumida <- rqualClusterdSemDuplicadas %>%select(Município, Indicador, Resultado)
rqualClusterPivotada <- rqualClusterResumida%>%pivot_wider(names_from = "Indicador", values_from = "Resultado")

#primeiro ponto a ser analisado é a necessidade de normalização das variaveis quantitativas
#caso elas tenham uma amplitude grande, utilizar z-score.
#nesse caso, houve a utilização do zscore por conter grande variacao dos valores dos resultados no indicador de queda de chamadas

rqualClusterNormalizada <- scale(rqualClusterPivotada[,2:9])
rqualClusterNormalizadaFinal <- cbind(rqualClusterPivotada[,1], as.data.frame(rqualClusterNormalizada))

#tambem julguei por retirar 9 observacoes (cidades) com valores NA, pois não afetaria o conjunto de dados como um todo
summary(rqualClusterNormalizadaFinal)
rqualNA <- subset(rqualClusterNormalizadaFinal, is.na(rqualClusterNormalizadaFinal$IND6))
rqualPivotadaSemNA <- na.omit(rqualClusterNormalizadaFinal)

#identificando outlier
#nesse caso, foi retirado da base 1 observacao com outliers
boxplot(rqualPivotadaSemNA[,2:9])
rqualPivotadaSemNA <- rqualPivotadaSemNA[rqualPivotadaSemNA$Município != "Alvinlândia",]

#analises descritivas das variaveis normalizadas
summary(rqualPivotadaSemNA)
rqualNA <- subset(rqualPivotadaSemNA, is.na(rqualPivotadaSemNA$IND6))
rqualPivotadaSemNA <- na.omit(rqualPivotadaSemNA)
rqualPivotadaSemNA <- rqualPivotadaSemNA[rqualPivotadaSemNA$Município != "Alvinlândia",]
boxplot(rqualPivotadaSemNA$IND2, col = "skyblue", border = "black", las = 1, cex.axis = 1.2)
abline(h = seq(-10, 10, by = 1), col = "darkgray", lty = "dotted")
summary(data.frame(rqualPivotadaSemNA))


#analises descritivas das variaveis não normalizadas e IND2 invertida
summary(rqualClusterPivotada)
rqualNAsemZscore <- subset(rqualClusterPivotada, is.na(rqualClusterPivotada$IND6))
rqualPivotadaSemNAsemZscore <- na.omit(rqualClusterPivotada)
rqualPivotadaSemNAsemZscore <- rqualPivotadaSemNAsemZscore[rqualPivotadaSemNAsemZscore$Município != "Alvinlândia",]
rqualPivotadaSemNAsemZscore$IND2 <- 100 - rqualPivotadaSemNAsemZscore$IND2
boxplot(rqualPivotadaSemNAsemZscore[,2:9], col = "skyblue", border = "black", las = 1, cex.axis = 1.2)
abline(h = seq(0, 100, by = 2), col = "darkgray", lty = "dotted")
summary(data.frame(rqualPivotadaSemNAsemZscore))

#metodo aglomerativo - hierarquico
#calcular a distancia entre pares de observacoes
rqualClusterDissimilaridade <- rqualPivotadaSemNAsemZscore%>%dist(method = "euclidean")

# Visualizando a matriz de dissimilaridades
data.matrix(rqualClusterDissimilaridade) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 10)

#escolha do metodo de encadeamento para analises dos dendrogramas
#(average linkage, complete linkage, single linkage)
#nesse caso foi escolhido o complete por gerar clusters com mais observacoes
rqualSingle <- agnes(x=rqualClusterDissimilaridade, method = "single")
fviz_dend(x=rqualSingle)

rqualComplete <- agnes(x=rqualClusterDissimilaridade, method = "complete")
fviz_dend(x=rqualComplete)

rqualAverage <- agnes(x=rqualClusterDissimilaridade, method = "average")
fviz_dend(x=rqualAverage)

# As distâncias para as combinações em cada estágio
coeficientesComplete <- sort(rqualComplete$height, decreasing = FALSE) 
coeficientesComplete

#detalhando o esquema hierarquico
coeficientes <- sort(rqualComplete$height, decreasing = FALSE) 
esquema <- as.data.frame(cbind(rqualComplete$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

# Visualizando o esquema hierárquico de aglomeração
esquema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 10)


#gerando um dendrograma com a visualizacao por altura (18) 
fviz_dend(x = rqualComplete,
          h = 18,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          rect_border = "black",
          lwd = 1,
          show_labels = F,
          ggtheme = theme_bw())

#gerando um dendrograma com a visualizaçao por definicao de k(quantidade de clusters)
fviz_dend(x = rqualComplete,
          k = 4,
          k_colors = c("deeppink4", "darkviolet", "deeppink", "darkblue"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())


# Criando e adicionando variável categórica para indicação do cluster no banco de dados
## O argumento 'k' indica a quantidade de clusters
rqualPivotadaSemNAsemZscore$cluster_H <- factor(cutree(tree = rqualComplete, k = 4))

# Visualização da base de dados com a alocação das observações nos clusters
rqualPivotadaSemNAsemZscore %>%
  select(Município, cluster_H) %>% 
  arrange(Município) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 10)


# Estatísticas descritivas da variável 'perda de pacotes de conexao de dados'
group_by(rqualPivotadaSemNAsemZscore, cluster_H) %>%
  summarise(
    mean = mean(IND7, na.rm = TRUE),
    sd = sd(IND7, na.rm = TRUE),
    min = min(IND7, na.rm = TRUE),
    max = max(IND7, na.rm = TRUE))

# Estatísticas descritivas da variável 'Latência Bidirecional da conexào de dados'
group_by(rqualPivotadaSemNAsemZscore, cluster_H) %>%
  summarise(
    mean = mean(IND5, na.rm = TRUE),
    sd = sd(IND5, na.rm = TRUE),
    min = min(IND5, na.rm = TRUE),
    max = max(IND5, na.rm = TRUE))

# Estatísticas descritivas da variável 'Conexão de chamadas na rede de acesso'
group_by(rqualPivotadaSemNAsemZscore, cluster_H) %>%
  summarise(
    mean = mean(IND1, na.rm = TRUE),
    sd = sd(IND1, na.rm = TRUE),
    min = min(IND1, na.rm = TRUE),
    max = max(IND1, na.rm = TRUE))

# Estatísticas descritivas da variável 'Conexão de dados medida na rede de acesso'
group_by(rqualPivotadaSemNAsemZscore, cluster_H) %>%
  summarise(
    mean = mean(IND3, na.rm = TRUE),
    sd = sd(IND3, na.rm = TRUE),
    min = min(IND3, na.rm = TRUE),
    max = max(IND3, na.rm = TRUE))

# Estatísticas descritivas da variável 'Cumprimento de velocidade de download e upload'
group_by(rqualPivotadaSemNAsemZscore, cluster_H) %>%
  summarise(
    mean = mean(IND4, na.rm = TRUE),
    sd = sd(IND4, na.rm = TRUE),
    min = min(IND4, na.rm = TRUE),
    max = max(IND4, na.rm = TRUE))

# Estatísticas descritivas da variável 'Disponibilidade'
group_by(rqualPivotadaSemNAsemZscore, cluster_H) %>%
  summarise(
    mean = mean(IND8, na.rm = TRUE),
    sd = sd(IND8, na.rm = TRUE),
    min = min(IND8, na.rm = TRUE),
    max = max(IND8, na.rm = TRUE))

# Estatísticas descritivas da variável 'Variação de latência da conexão de dados'
group_by(rqualPivotadaSemNAsemZscore, cluster_H) %>%
  summarise(
    mean = mean(IND6, na.rm = TRUE),
    sd = sd(IND6, na.rm = TRUE),
    min = min(IND6, na.rm = TRUE),
    max = max(IND6, na.rm = TRUE))

# Estatísticas descritivas da variável 'Queda de chamadas'
group_by(rqualPivotadaSemNAsemZscore, cluster_H) %>%
  summarise(
    mean = mean(IND2, na.rm = TRUE),
    sd = sd(IND2, na.rm = TRUE),
    min = min(IND2, na.rm = TRUE),
    max = max(IND2, na.rm = TRUE))

# Análise de variância de um fator (ANOVA). Interpretação do output:

## Mean Sq do cluster_H: indica a variabilidade entre grupos
## Mean Sq dos Residuals: indica a variabilidade dentro dos grupos
## F value: estatística de teste (Sum Sq do cluster_H / Sum Sq dos Residuals)
## Pr(>F): p-valor da estatística 
## p-valor < 0.05: pelo menos um cluster apresenta média estatisticamente diferente dos demais

## A variável mais discriminante dos grupos contém maior estatística F (e significativa)

# ANOVA da variável 'Perda de pacotes da conexão de dados'
summary(anova_IND7_H <- aov(formula = IND7 ~ cluster_H, data = rqualPivotadaSemNA))

# ANOVA da variável 'Latência bidirecional da conexão de dados'
summary(anova_IND5_H <- aov(formula = IND5 ~ cluster_H, data = rqualPivotadaSemNA))

# ANOVA da variável 'Conexão de chamadas na rede de acesso'
summary(anova_IND1_H <- aov(formula = IND1 ~ cluster_H, data = rqualPivotadaSemNA))

# ANOVA da variável 'Conexão de dados medida na rede de acesso'
summary(anova_IND3_H <- aov(formula = IND3 ~ cluster_H, data = rqualPivotadaSemNA))

# ANOVA da variável 'Cumprimento de velocidade de download e upload'
summary(anova_IND4_H <- aov(formula = IND4 ~ cluster_H, data = rqualPivotadaSemNA))

# ANOVA da variável 'Disponibilidade'
summary(anova_IND8_H <- aov(formula = IND8 ~ cluster_H, data = rqualPivotadaSemNA))

# ANOVA da variável 'Variação de latência da conexão de dados'
summary(anova_IND6_H <- aov(formula = IND6 ~ cluster_H, data = rqualPivotadaSemNA))

# ANOVA da variável 'Variação de latência da conexão de dados'
summary(anova_IND2_H <- aov(formula = IND2 ~ cluster_H, data = rqualPivotadaSemNA))

#adicionando a variavel cluster_H no data frame nao normalizado
rqualPivotadaComClusterDesnormalizada <- cbind(rqualPivotadaSemNAsemZscore, cluster_H = rqualPivotadaSemNA[,10])

# O que os cluster indicam? Interpretando algumas variáveis médias:
análise_H <- group_by(rqualPivotadaSemNAsemZscore, cluster_H) %>%
  summarise(`Perda de pacotes` = mean(IND7, na.rm = TRUE),
            `Latência bidirecional` = mean(IND5, na.rm = TRUE),
            `Conexão de chamadas` = mean(IND1, na.rm = TRUE),
            `Conexão de dados medida` = mean(IND3, na.rm = TRUE),
            `Cumprimento de velocidade` = mean(IND4, na.rm = TRUE),
            `Disponibilidade` = mean(IND8, na.rm = TRUE),
            `Variação de latência` = mean(IND6, na.rm = TRUE),
            `Queda de chamadas` = mean(IND2, na.rm = TRUE))

#
#metodo nao-hierarquico / k-means
# Método de Elbow para identificação do número ótimo de clusters
#neste caso, definiu-se 7 clusters
fviz_nbclust(rqualPivotadaSemNAsemZscore[,2:9], kmeans, method = "wss", k.max = 10)

# Criando clusterização não hieráquica k-means com base no método de Elbow
# nesse caso, identificou-se que 4 cluster é aceitável
cluster_kmeans <- kmeans(rqualPivotadaSemNAsemZscore[,2:9],
                         centers = 4)

# Adicionando a variável categórica para indicação do cluster no banco de dados
rqualPivotadaSemNAsemZscore$cluster <- factor(cluster_kmeans$cluster)

# Visualizando a base de dados
rqualPivotadaSemNAsemZscore %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 10)

# Estatísticas descritivas dos clusters por variável
# Estatísticas descritivas da variável 'perda de pacotes de conexao de dados' IND7
group_by(rqualPivotadaSemNAsemZscore, cluster) %>%
  summarise(
    mean = mean(IND7, na.rm = TRUE),
    sd = sd(IND7, na.rm = TRUE),
    min = min(IND7, na.rm = TRUE),
    max = max(IND7, na.rm = TRUE))

# Estatísticas descritivas da variável 'Latência Bidirecional da conexào de dados' IND5
group_by(rqualPivotadaSemNAsemZscore, cluster) %>%
  summarise(
    mean = mean(IND5, na.rm = TRUE),
    sd = sd(IND5, na.rm = TRUE),
    min = min(IND5, na.rm = TRUE),
    max = max(IND5, na.rm = TRUE))

# Estatísticas descritivas da variável 'Conexão de chamadas na rede de acesso' IND1
group_by(rqualPivotadaSemNAsemZscore, cluster) %>%
  summarise(
    mean = mean(IND1, na.rm = TRUE),
    sd = sd(IND1, na.rm = TRUE),
    min = min(IND1, na.rm = TRUE),
    max = max(IND1, na.rm = TRUE))

# Estatísticas descritivas da variável 'Conexão de dados medida na rede de acesso' IND3
group_by(rqualPivotadaSemNAsemZscore, cluster) %>%
  summarise(
    mean = mean(IND3, na.rm = TRUE),
    sd = sd(IND3, na.rm = TRUE),
    min = min(IND3, na.rm = TRUE),
    max = max(IND3, na.rm = TRUE))

# Estatísticas descritivas da variável 'Cumprimento de velocidade de download e upload' IND4
group_by(rqualPivotadaSemNAsemZscore, cluster) %>%
  summarise(
    mean = mean(IND4, na.rm = TRUE),
    sd = sd(IND4, na.rm = TRUE),
    min = min(IND4, na.rm = TRUE),
    max = max(IND4, na.rm = TRUE))

# Estatísticas descritivas da variável 'Disponibilidade' IND8
group_by(rqualPivotadaSemNAsemZscore, cluster) %>%
  summarise(
    mean = mean(IND8, na.rm = TRUE),
    sd = sd(IND8, na.rm = TRUE),
    min = min(IND8, na.rm = TRUE),
    max = max(IND8, na.rm = TRUE))

# Estatísticas descritivas da variável 'Variação de latência da conexão de dados' IND6
group_by(rqualPivotadaSemNAsemZscore, cluster) %>%
  summarise(
    mean = mean(IND6, na.rm = TRUE),
    sd = sd(IND6, na.rm = TRUE),
    min = min(IND6, na.rm = TRUE),
    max = max(IND6, na.rm = TRUE))

# Estatísticas descritivas da variável 'Queda de chamadas' IND2
group_by(rqualPivotadaSemNAsemZscore, cluster) %>%
  summarise(
    mean = mean(IND2, na.rm = TRUE),
    sd = sd(IND2, na.rm = TRUE),
    min = min(IND2, na.rm = TRUE),
    max = max(IND2, na.rm = TRUE))


# O que os cluster indicam? Interpretando algumas variáveis médias:
análise <- group_by(rqualPivotadaSemNAsemZscore, cluster) %>%
  summarise(`IND7` = mean(IND7, na.rm = TRUE),
            `IND5` = mean(IND5, na.rm = TRUE),
            `IND1` = mean(IND1, na.rm = TRUE),
            `IND3` = mean(IND3, na.rm = TRUE),
            `IND4` = mean(IND4, na.rm = TRUE),
            `IND8` = mean(IND8, na.rm = TRUE),
            `IND6` = mean(IND6, na.rm = TRUE),
            `IND2` = mean(IND2, na.rm = TRUE))

# Análise de variância de um fator (ANOVA). Interpretação do output:

## Mean Sq do cluster: indica a variabilidade entre grupos
## Mean Sq dos Residuals: indica a variabilidade dentro dos grupos
## F value: estatística de teste (Sum Sq do cluster / Sum Sq dos Residuals)
## Pr(>F): p-valor da estatística 
## p-valor < 0.05: pelo menos um cluster apresenta média estatisticamente diferente dos demais

## A variável mais discriminante dos grupos contém maior estatística F (e significativa)
# ANOVA da variável 'Perda de pacotes da conexão de dados'
summary(anova_IND7 <- aov(formula = IND7 ~ cluster, data = rqualPivotadaSemNAsemZscore))

# ANOVA da variável 'Latência bidirecional da conexão de dados'
summary(anova_IND5 <- aov(formula = IND5 ~ cluster, data = rqualPivotadaSemNAsemZscore))

# ANOVA da variável 'Conexão de chamadas na rede de acesso'
summary(anova_IND1 <- aov(formula = IND1 ~ cluster, data = rqualPivotadaSemNAsemZscore))

# ANOVA da variável 'Conexão de dados medida na rede de acesso'
summary(anova_IND3 <- aov(formula = IND3 ~ cluster, data = rqualPivotadaSemNAsemZscore))

# ANOVA da variável 'Cumprimento de velocidade de download e upload'
summary(anova_IND4 <- aov(formula = IND4 ~ cluster, data = rqualPivotadaSemNAsemZscore))

# ANOVA da variável 'Disponibilidade'
summary(anova_IND8 <- aov(formula = IND8 ~ cluster, data = rqualPivotadaSemNAsemZscore))

# ANOVA da variável 'Variação de latência da conexão de dados'
summary(anova_IND6 <- aov(formula = IND6 ~ cluster, data = rqualPivotadaSemNAsemZscore))

# ANOVA da variável 'Queda de chamada'
summary(anova_IND6 <- aov(formula = IND2 ~ cluster, data = rqualPivotadaSemNAsemZscore))


#teste de silhouette 
#avaliando os clusters formados em cada método
#metodo nao hierarquico
distNH<-dist(rqualPivotadaSemNAsemZscore[,2:9])
sil <- silhouette(cluster_kmeans$cluster, distNH)
sil_cluster <- cbind(sil, Cluster = cluster_kmeans$cluster)
mean_sil <- mean(sil[, "sil_width"])
mean_sil_by_cluster <- aggregate(sil_width ~ cluster, data = sil_cluster, FUN = mean)
print(paste("Média da silhueta por cluster, método não Hierárquico:", mean_sil_by_cluster))
print(paste("Média da silhueta, total:", mean_sil))
#métdo hierarquico
rqualPivotadaSemNAsemZscore$cluster_H_semFactor <- cutree(tree = rqualComplete, k = 4)
distH<-dist(rqualPivotadaSemNAsemZscore[,2:9], method = "euclidean")
silH <- silhouette(rqualPivotadaSemNAsemZscore$cluster_H_semFactor, distH)
sil_clusterH <- cbind(silH, Cluster = rqualPivotadaSemNAsemZscore$cluster_H_semFactor)
mean_silH <- mean(silH[,"sil_width"])
mean_sil_by_clusterH <- aggregate(sil_width ~ cluster, data = sil_clusterH, FUN = mean)
print(paste("Média da silhueta, método Hierárquico:", mean_sil_by_clusterH))
print(paste("Média da silhueta, total:", mean_silH))

#count das observacoes por cluster
count_cluster_H <- count(rqualPivotadaSemNAsemZscore, cluster_H)
