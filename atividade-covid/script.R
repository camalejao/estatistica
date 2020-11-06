setwd('/home/jao/Documentos/PLE/Estatistica/estatistica/atividade-covid')

# Lendo dados
alagoas <- read.csv('pandemia-alagoas.csv')
View(alagoas)

# Filtrando registros para Marechal Deodoro
marechal <- subset(alagoas, municipio_residencia == 'Marechal Deodoro')
View(marechal)

dim(marechal)

# Variavel qualitativa: sexo
sexo.tb <- table(marechal$sexo)

#Grafico de Barras
barplot(sexo.tb, col = c("orange", "cyan"),
        ylab = "Número de pessoas", xlab = "Sexo",
        cex.names = 1.5, cex.lab = 1.25, ylim=c(0,1400),
        main = "Proporção entre sexo masculino e feminino")

# Grafico de Pizza
labs <- paste(c("Feminino = ", "Masculino = "),
              100 * round(sexo.tb/length(marechal$sexo), digits = 2),
              "%")

pie(sexo.tb, labels = labs,
    col = c("orange", "cyan"),
    main = "Proporção entre sexo masculino e feminino",
    cex = 1.2)

# Variavel numerica: Idade
idade <- na.omit(as.numeric(marechal$idade))
sort(idade)

# Amplitude de Classe
amplitude <- max(idade) - min(idade); amplitude
nk <- round(1 + 3.322 * log10(length(idade))); nk
ampClasse <- amplitude / nk; ampClasse; ampClasse <- 8.75

limites = c(1, 9.75, 18.5, 27.25, 36, 44.75, 53.5,
            62.25, 71, 79.75, 88.5, 97.25, 106)

classes <- c("1.00-9.75", "9.75-18.50", "18.50-27.25", "27.25-36.00",
             "36.00-44.75", "44.75-53.50", "53.50-62.25", "62.25-71.00",
             "71.00-79.75", "79.75-88.50", "88.50-97.25", "97.25-106.00")

# Frequencia Absoluta
freq <- table(cut(idade, breaks = limites, right = FALSE, lables = classes))
freqAc <- cumsum(freq)

# Frequencia Relativa
freqRel <- prop.table(freq)
freqRelAc <- cumsum(freqRel)

# Tabela com todas as frequencias
tabResul = cbind(freq, freqAc, freqRel = round(freqRel*100, digits = 2),
                 freqRelAc = round(freqRelAc*100, digits = 2))
tabResul

# Histograma
h = hist(idade, breaks=limites,
         ylab="Frequencias absolutas", 
         main="Histograma",
         xlim=c(4,110), ylim=c(0,570),
         col="orange")
text(h$mids, 150, labels=classes, adj=c(0, 0.2), srt=90)

lines(c(min(h$breaks), h$mids, max(h$breaks)), 
      c(0,h$counts, 0), type = "l")







# Boxplot sexo vs idade
# Classificacao dos quartis
quantile(marechal$idade, na.rm=TRUE)
idade.cut <- cut(marechal$idade,
                 breaks = quantile(marechal$idade, na.rm = TRUE),
                 include.lowest = TRUE)

# Tabela de frequencias absolutas
sexo.idade.tb <- table(marechal$sexo, idade.cut)
sexo.idade.tb

# Boxplot
boxplot(idade ~ sexo, data = marechal, col = c("orange", "cyan"))


