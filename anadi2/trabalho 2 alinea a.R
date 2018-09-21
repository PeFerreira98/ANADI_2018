# ANADI Trabalho 2 
# alinea a)

# importar o ficheiro performance_comparison.txt do diretorio onde se encontra.

dadosraw = data.frame(performance_comparison)
dadosraw



#Data frame com a tecnica e o desempenho.
#metrica =  (TN + TP)/ (All)
dadosrawdesempenho = data.frame( tec = dadosraw$Technique , 
                                des = (dadosraw$TN + dadosraw$TP) / 
                                (dadosraw$TN + dadosraw$TP + dadosraw$FP + dadosraw$FN) )  


# Contagem do numero de amostras.                                
unique(dadosraw$Technique)
#[1] RF  SVM NN 
#Levels: NN RF SVM
table(dadosraw$Technique)
#> table(dadosraw$Technique)
#NN  RF SVM 
#100 100 100



# Dataframe com a tecnica, tp, tn, fp, fn, desempenho , e o numero de saudaveis, doentes reais e numero de pessoas.
#dados[ tecnica, tp, tn, fp, fn, desempenho, saudaveis, doentes, pessoas]

saudaveis = dadosraw$TN + dadosraw$FP
doentes = dadosraw$TP + dadosraw$FN
pessoas = saudaveis + doentes

dados = data.frame( tecnica = dadosraw$Technique
                    , tp = dadosraw$TP
                    , tn = dadosraw$TN
                    , fp = dadosraw$FP
                    , fn = dadosraw$FN
                    , desempenho = dadosrawdesempenho$des
                    , saudaveis
                    , doentes
                    , pessoas = saudaveis + doentes
                    )
#View(dados)
dados



dados.pop.nn = dados[dados$tecnica == "NN",]
hist(dados.pop.nn$doentes,main = "NN", col = "red", ylab = "Frequencia", xlab = "doentes")

dados.pop.rf = dados[dados$tecnica == "RF",]
hist(dados.pop.rf$doentes, main = "RF", col = "blue", ylab = "Frequencia", xlab = "doentes")

dados.pop.svm = dados[dados$tecnica == "SVM",]
hist(dados.pop.svm$doentes, main = "SVM", col = "green", ylab = "Frequencia", xlab = "doentes")

cor.doentes = c("red", "blue", "green")
boxplot(dados$doentes ~dados$tecnica, col = cor.doentes, ylab = "doentes")


# Outro dataframe, com o desempenho sob a forma de tabular.
# nome da tecnica:  [ RF, SVM , NN ]
# desempenho:         80%, 95%, 79%     
# select para criar uma data frame sob a forma de tabela do desempenho.
rf = dadosrawdesempenho$des[dadosrawdesempenho$tec == "RF"]
svm = dadosrawdesempenho$des[dadosrawdesempenho$tec == "SVM"]
nn = dadosrawdesempenho$des[dadosrawdesempenho$tec == "NN"]

desempenho = data.frame(rf,svm,nn)
desempenho

#View(dadosrawdesempenho)
#View(desempenho)



##############################################
##   AED - Analise exploratoria dos dados   ##
##############################################


#Boxplot
cor.performance = c( "blue", "green","red")
boxplot(desempenho, col = cor.performance, ylab = "desempenho")

boxplot(desempenho)

#Histogramas
hist(desempenho$rf)
hist(desempenho$svm)
hist(desempenho$nn)


# Passar varios parametros de analise de desempenho de cada tecnica para uma tabela.
# aed["tecnica","sd","avg","max","min","kurtosis","skewness"]

library(moments)
a.kurtosis.rf = kurtosis(desempenho$rf) 
a.kurtosis.svm = kurtosis(desempenho$svm) 
a.kurtosis.nn = kurtosis(desempenho$nn)

a.skewness.rf = skewness(desempenho$rf)
a.skewness.svm = skewness(desempenho$svm)
a.skewness.nn = skewness(desempenho$nn)


aed = data.frame(c("RF","SVM", "NN"),
                 c(
                   sd(desempenho$rf),
                   sd(desempenho$svm),
                   sd(desempenho$nn)
                   ),
                 c(
                   mean(desempenho$rf),
                   mean(desempenho$svm),
                   mean(desempenho$nn)
                 ),
                 c(
                   max(desempenho$rf),
                   max(desempenho$svm),
                   max(desempenho$nn)
                 ),
                 c(
                   min(desempenho$rf),
                   min(desempenho$svm),
                   min(desempenho$nn)
                 ),
                 c(
                   a.kurtosis.rf,
                   a.kurtosis.svm,
                   a.kurtosis.nn
                 ),
                 c( a.skewness.rf,
                    a.skewness.svm,
                    a.skewness.nn
                    )
                 )
names(aed) = c("tecnica","sd","avg","max","min","kurtosis","skewness")
aed

# skewness - assimetria da distribuição normal
# kurtosis - achatamento da curva noramal, indica se a cauda é curta ou longa

# https://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm
# The skewness for a normal distribution is zero, and any symmetric data should have 
# a skewness near zero. Negative values for the skewness indicate data that are skewed
# left and positive values for the skewness indicate data that are skewed right. By 
# skewed left, we mean that the left tail is long relative to the right tail. 
# Similarly, skewed right means that the right tail is long relative to the left tail.
# If the data are multi-modal, then this may affect the sign of the skewness. 


#The kurtosis for a standard normal distribution is three. For this reason, some sources
# use the following definition of kurtosis (often referred to as "excess kurtosis"): 

#This definition is used so that the standard normal distribution has a kurtosis of zero.
#In addition, with the second definition positive kurtosis indicates a "heavy-tailed" 
# distribution and negative kurtosis indicates a "light tailed" distribution.

#  tecnica         sd       avg       max       min kurtosis     skewness
#1      RF 0.02107853 0.9110615 0.9664804 0.8547486 2.999794 -0.017066454
#2     SVM 0.01816703 0.9247486 0.9776536 0.8826816 2.786281  0.003402452
#3      NN 0.01921151 0.9108939 0.9497207 0.8324022 4.572058 -0.766033159

aed
#View(aed)
#write.table(aed , file="aed2.csv", sep = ";" , row.names = FALSE)

# Levene test.

# Analise de variancia, temos de confirmar que a variancia dos 3 groupos é a mesma:

library(car)
leveneTest(dados$desempenho ~ dados$tecnica, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
#leveneTest(dados9$Tempo.gasto.nas.redes.sociais..h. ~ dados9$Sexo, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

#Levene's Test for Homogeneity of Variance (center = median: FALSE)
#       Df F value Pr(>F)
#group   2  0.7541 0.4713
#      297               

# P-value > 0.05 -> não rejeitar H0 que as variancias são iguais.

# ======================================================

# Vamos agora analisar a normalidade dos dados de performance de cada um dos algoritmos.
#normalidade:  ver #TP05 ex07
library("nortest")
lillie.test(rf)
# Lilliefors (Kolmogorov-Smirnov) normality test
#data:  rf
# D = 0.071542, p-value = 0.2369
shapiro.test(rf)
#Shapiro-Wilk normality test
#data:  rf
#W = 0.99145, p-value = 0.7797


lillie.test(svm)
#Lilliefors (Kolmogorov-Smirnov) normality test
#data:  svm
#D = 0.085266, p-value = 0.07007
shapiro.test(svm)
#Shapiro-Wilk normality test
#data:  svm
#W = 0.98327, p-value = 0.2368


lillie.test(nn)
#Lilliefors (Kolmogorov-Smirnov) normality test
#data:  nn
#D = 0.090056, p-value = 0.04425
shapiro.test(nn)   # FALHA
#Shapiro-Wilk normality test
#data:  nn
#W = 0.95823, p-value = 0.003013

# É possivel observar que a distribuição do desempenho para o algoritmo Neural Networks NN
# não é normal. Visto ambos os valores de prova serem inferiores ao alfa de  0.05
# obrigandonos a rejeitar a H0 que seguem uma distribuição normal.

hist(nn)
plot(density(nn))

# Da analise grafica, notamos a situação. Mas visto apenas o algoritmo NN falhar,
# e o numero de observações ser elevado (100), vamos efetuar mesmo assim a analise atraves 
# de metodos parametricos. Mas por caução e curiosidade academica iremos tambem recorer 
# a metodos não parametricos QUE SE ENCONTRAM NO FINAL DESTE DOCUMENTO.


# ==================================================

# Verificar a diferença de desempenho.

aux.anova = lm(dados$desempenho ~ dados$tecnica, data = dados)
anova(aux.anova)

#Analysis of Variance Table
#
#Response: dados$desempenho
#Df   Sum Sq   Mean Sq F value    Pr(>F)    
#dados$tecnica   2 0.012644 0.0063220  16.587 1.483e-07 ***
#  Residuals     297 0.113199 0.0003811                      
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Como Pr < 0.05, rejeitamos H0, e existe pelo menos um grupo com uma variancia diferente de outro.


# VAMOS AGORA FAZER O POST HOC
# https://stats.idre.ucla.edu/r/faq/how-can-i-do-post-hoc-pairwise-comparisons-in-r/

aov(dados$desempenho ~ dados$tecnica)
#> aov(dados$desempenho ~ dados$tecnica)
#Call:
#  aov(formula = dados$desempenho ~ dados$tecnica)
#
#Terms:
#  dados$tecnica  Residuals
#Sum of Squares     0.01264401 0.11319934
#Deg. of Freedom             2        297
#
#Residual standard error: 0.01952287
#Estimated effects may be unbalanced

pairwise.t.test(dados$desempenho, dados$tecnica, p.adj = "none")
#> pairwise.t.test(dados$desempenho, dados$tecnica, p.adj = "none")
#
#Pairwise comparisons using t tests with pooled SD 
#
#data:  dados$desempenho and dados$tecnica 
#
#        NN      RF     
#  RF    0.95    -      
#  SVM   9.0e-07 1.2e-06
#
#P value adjustment method: none 

# DO teste pair wise temos a indicação que a medias RF - NN são iguais. 
# Mas SVM difere de todos os outros.

obj_aov <- aov(dados$desempenho ~ dados$tecnica)
TukeyHSD(obj_aov)
#> TukeyHSD(obj_aov)
#Tukey multiple comparisons of means
#95% family-wise confidence level
#
#Fit: aov(formula = dados$desempenho ~ dados$tecnica)
#
#$`dados$tecnica`
#               diff          lwr         upr     p adj
#RF-NN  0.0001675978 -0.006335891 0.006671087 0.9979705
#SVM-NN 0.0138547486  0.007351260 0.020358237 0.0000027
#SVM-RF 0.0136871508  0.007183662 0.020190640 0.0000036

# Claramento o test Tukey Honest Significant Differences comprova.
# O valor da performance de SVM é diferente da dos outros testes.
# valor de prova P.adj RF-NN > 0.05 portanto a H0 é valida que são iguais.
# Mas P.adj SVM-NN e SVM-RF claramente < 0.05 obrigando a rejeitar que sejam iguais.

# Vamos fazer T-Tests para confirmar.
#alternative = "greater" is the alternative that x has a larger mean than y.
# H0 as medias são iguais. H1 media svm > media rf
t.test(desempenho$svm,desempenho$rf, alternative = "greater", conf.level = 0.95,paired = FALSE)
#Welch Two Sample t-test
#
#data:  desempenho$svm and desempenho$rf
#t = 4.9186, df = 193.78, p-value = 9.275e-07
#alternative hypothesis: true difference in means is greater than 0
#95 percent confidence interval:
#  0.009088019         Inf
#sample estimates:
#  mean of x mean of y 
#0.9247486 0.9110615 

t.test(desempenho$svm,desempenho$nn, alternative = "greater", conf.level = 0.95,paired = FALSE)
#Welch Two Sample t-test
#
#data:  desempenho$svm and desempenho$nn
#t = 5.2399, df = 197.38, p-value = 2.054e-07
#alternative hypothesis: true difference in means is greater than 0
#95 percent confidence interval:
#  0.009485093         Inf
#sample estimates:
#  mean of x mean of y 
#0.9247486 0.9108939 

t.test(desempenho$rf,desempenho$nn, alternative = "two.sided", conf.level = 0.95,paired = FALSE)
#Welch Two Sample t-test
#
#data:  desempenho$rf and desempenho$nn
#t = 0.058765, df = 196.32, p-value = 0.9532
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.005456878  0.005792074
#sample estimates:
#  mean of x mean of y 
#0.9110615 0.9108939 


# SUmario do T-Testes.
#H0 SVM = RF : H1 SVM >  RF - > p-value = 9.275e-07 < 0.05 , Rejeitar H0 -> SVM >  RF
#H0 SVM = RF : H1 SVM >  NN - > p-value = 2.054e-07  < 0.05 , Rejeitar H0 -> SVM >  NN
#H0 RF  = NN : H1 RF =/= NN - > p-value = 0.9532    > 0.05 , Aceitar  H0 -> RF  =  RR

# Claramente o melhor desempenho pertence a SVM.
# Recorda-se que NN não segue uma distribuição normal, e portanto não deviamos estar 
# a usar testes parametricos.
# Mas RF e SVM seguem a distribuição normal, portanto esse t-test é claramente valido.




# METODOS NAO PARAMETRICOS.
# ===================================================


# Veirificar se existe diferença de desempenho significativa. Vamos recorrer ao Kruskal.

kruskal.test(formula = dados$desempenho ~ dados$tecnica, data = dados)
#Kruskal-Wallis rank sum test
#data:  dados$desempenho by dados$tecnica
#Kruskal-Wallis chi-squared = 29.229, df = 2, p-value = 4.497e-07

# Clarament de novo rejeita-se H0 que o desempenho é igual a um nivel de confiança de 95%.
# Devido ao baixo valor do valor de prova. Assim pelo menos um dos grupos tem media diferente.


# POST HOC
# PAra efetuar o post-hoc temos varias alternativas:
# Podemos correr o mesmo teste mas com apenas dois grupos.

smvnn = dados[dados$tecnica != "RF",]
smvrf = dados[dados$tecnica != "NN",]
rfnn = dados[dados$tecnica != "SVM",]

kruskal.test(formula = smvnn$desempenho ~ smvnn$tecnica, data = dados)
#Kruskal-Wallis rank sum test
#data:  smvnn$desempenho by smvnn$tecnica
#Kruskal-Wallis chi-squared = 22.477, df = 1, p-value = 2.127e-06

kruskal.test(formula = smvrf$desempenho ~ smvrf$tecnica, data = dados)
#Kruskal-Wallis rank sum test
#data:  smvrf$desempenho by smvrf$tecnica
#Kruskal-Wallis chi-squared = 21.305, df = 1, p-value = 3.917e-06

kruskal.test(formula = rfnn$desempenho  ~ rfnn$tecnica, data = dados)
#Kruskal-Wallis rank sum test
#data:  rfnn$desempenho by rfnn$tecnica
#Kruskal-Wallis chi-squared = 0.047651, df = 1, p-value = 0.8272

# Dos testes tem-se que apenas o par RF-NN tem um valor de prova superior a 0.05,
# portanto SVM sera o diferente no grupo.

# Mas podemos efetuar um teste especifico mais detalhado que faz isto automaticamente.
#install.packages("PMCMR")
library(PMCMR)
posthoc.kruskal.nemenyi.test(x=dados$desempenho, g=dados$tecnica, method="Tukey")

#Pairwise comparisons using Tukey and Kramer (Nemenyi) test	
#with Tukey-Dist approximation for independent samples 
#
#data:  dados$desempenho and dados$tecnica 
#
#      NN      RF     
#  RF  0.99    -      
#  SVM 1.2e-05 7.2e-06
#
#vP value adjustment method: none 

# De novo confirmamos que SVM é o que tem media diferente.
# Agora bastava efetuar um t-test entre SVM e RF visto ambos terem uma distribuição normal.
# Mas visto ter sido já feito na secção acima, não o vamos repetir.

