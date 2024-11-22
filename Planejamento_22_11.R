y = c(80, 83, 83, 85, 75, 75, 79, 79, 74, 73, 76, 77, 67, 72, 74, 74, 62, 62, 67, 69, 60, 61, 64, 66)
Diametro = c(0.37,  0.37, 0.37, 0.37, 0.51, 0.51, 0.51, 0.51, 0.71, 0.71, 0.71, 0.71, 1.02, 1.02, 1.02, 1.02, 1.40, 1.40, 1.40, 1.40, 1.99, 1.99, 1.99, 1.99)

cbind(Diametro, y)
Diametro_f = factor(Diametro)
plot(y~Diametro, pch = 19)
plot(y~Diametro_f, pch = 19)
points(Diametro_f, y, pch = 19)

library(ExpDes.pt)

summary(y)
tapply(y, Diametro, mean)
tapply(y, Diametro, sd)
tapply(y, Diametro, min)
tapply(y, Diametro, max)
tapply(y, Diametro, length)
table(Diametro)
100*tapply(y, Diametro, sd)/tapply(y, Diametro, mean) #CV(Coeficiente de Variação)

dic(Diametro, y, quali = T, hvar = "bartlett")
saida1 = dic(Diametro_f, y, quali = T, mcomp = "tukey", hvar = "bartlett")
#a) Existe evidência estatística suficiente para concluir que o tamanho do orifício afeta significativamente a média percentual de liberação de radônio
#b) 3.1595×10^-8 aparece na saída do dic
#c) Como 0.1247 > 0.05 Os resíduos podem ser considerados normais ao nível de significância de 5%
#O teste de Bartlett foi aplicado, resultando em um valor-p de 0.8733 Como 0.8733 > 0.05 As variâncias dos grupos podem ser consideradas homogêneas ao nível de significância de 5%
# a análise gráfica não apresenta possíveis outliers além dos resíduos estarem aleatóriamente distribuidos ao redor de zero
#d)
tapply(y, Diametro, mean)
media_1.40 = 65
QM = 7.347
ni = 4
gl_residuos = 18
alfa = 0.05

t_critico <- qt(1 - alfa/2, df = gl_residuos)
EP <- sqrt(QM / ni)
limite_inferior <- media_1.40 - t_critico * EP
limite_superior <- media_1.40 + t_critico * EP

cat("Intervalo de confiança de 95%: [", limite_inferior, ",", limite_superior, "]\n")


#e)
limite_inferior <- medias - qt(0.975, df = 18) * EP
limite_superior <- medias + qt(0.975, df = 18) * EP

tapply(y, Diametro, mean)
medias = c(82.75, 77.00, 75.00, 71.75, 65.00, 62.75)
  
dados <- data.frame(
  Tratamento = Diametro_f,
  Media = medias,
  LimiteInferior = limite_inferior,
  LimiteSuperior = limite_superior
)
library(ggplot2)
ggplot(dados, aes(x = Tratamento, y = Media)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = LimiteInferior, ymax = LimiteSuperior), width = 0.2) +
  labs(
    title = "Médias por Tratamento com Intervalos de Confiança",
    x = "Diâmetro do Orifício",
    y = "Média do Percentual de Radônio Liberado"
  ) +
  theme_minimal()

plot(saida1$residuos/sqrt(7.347), ylim = c(-3,3))
abline(h = 0)
abline(h = 2)
abline(h = -2)


medias2 = tapply(y, Diametro, mean)
C1 = c(1, -3, 2, 0, 0, 0)
SQC1 = ni * sum(C1*medias2)^2 /sum(C1*C1); SQC1
FC1 = SQC1/7.347; FC1

pf(FC1, 1, 18, lower.tail = F)

#f) defina e teste, três contrastes ortogonais, que envolvam 3 médias ou mais

