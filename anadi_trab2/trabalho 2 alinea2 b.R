# ANADI TRABALHO 2 ALINEA B


# Importar o performance_parameter_c
dados_c = data.frame(performance_parameter_c)


regresao.res = lm(formula = dados_c$performance ~ dados_c$c)

plot(dados_c$performance ~ dados_c$c, xlab = "C", ylab = "performance",col = "blue")
abline(regresao.res, col = "red")
legend( "bottomright", legend = c("dados", "regressão linear"), col= c("blue", "red"), pch=15)


cor.test(dados_c$performance , dados_c$c)
#Pearson's product-moment correlation
#
#data:  dados_c$performance and dados_c$c
#t = 2.9527, df = 177, p-value = 0.003578
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#0.0722914 0.3521477
#sample estimates:
#cor 
#0.2166663 


summary(regresao.res)$r.squared
# R^2 = 0.0469443
#Claramente, invalido. 

regresao.res$coefficients
#(Intercept)    dados_c$c 
#0.9212457799 0.0002218577 

summary(regresao.res)
#> summary(regresao.res)
#
#Call:
#  lm(formula = dados_c$performance ~ dados_c$c)
#
#Residuals:
#  Min         1Q     Median         3Q        Max 
#-0.0253419 -0.0005107  0.0005204  0.0009302  0.0025492 
#
#Coefficients:
#  Estimate Std. Error  t value Pr(>|t|)    
#(Intercept) 9.212e-01  3.899e-04 2362.882  < 2e-16 ***
#  dados_c$c   2.219e-04  7.514e-05    2.953  0.00358 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.002597 on 177 degrees of freedom
#Multiple R-squared:  0.04694,	Adjusted R-squared:  0.04156 
#F-statistic: 8.718 on 1 and 177 DF,  p-value: 0.003578


#Validação dos Residuos.
#Homocedasticidade - 
# Graficamente.
plot(fitted(regresao.res),residuals(regresao.res)
     ,xlab="Valores ajustados",ylab="residuos", col = "green"
     ,ylim = c(-0.030,0.01)
     )
abline(h = 0, col = "purple")
legend( "topright", legend = c("residuos", "residuos = 0"), col= c("green", "purple"), pch=15)

# NAO COLOQUEI ESTES GRAFICOS NO ARTIGO PORQUE TENHO O PRIMEIRO ACIMA,
# E O SEGUNDO FAÇO UM TESTE A NORMALIDADE.
#par ( mfrow =c(1 ,2))
#plot ( regresao.res )
#par(mfrow=c(1,1))

# Metodo de comparar as variancias inferiors a mediana e superior
mediana = median(dados_c$c);mediana
var.test(residuals(regresao.res)[dados_c$c > mediana],residuals(regresao.res)[dados_c$c < mediana])

#> var.test(residuals(regresao.res)[dados_c$c > mediana],residuals(regresao.res)[dados_c$c < mediana])
#
#F test to compare two variances
#
#data:  residuals(regresao.res)[dados_c$c > mediana] and residuals(regresao.res)[dados_c$c < mediana]
#F = 0.06145, num df = 88, denom df = 88, p-value < 2.2e-16
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  0.04035214 0.09357977
#sample estimates:
#  ratio of variances 
#0.06145034

# Claramente o valor de prova é inferior a alfa = 0.05 portanto, rejeita-se a hipotese H0 
# que as variancias são iguais.


# NORMALIDADE DOS RESIDUOS.

shapiro.test(residuals(regresao.res))
#> shapiro.test(residuals(regresao.res))
#
#Shapiro-Wilk normality test
#
#data:  residuals(regresao.res)
#W = 0.4532, p-value < 2.2e-16

# NAO È NORMAL
# t.test(residuals(regresao.res)) #NAO FAZ SENTIDO.

mean(residuals(regresao.res))
#[1] -9.941505e-19

hist(residuals(regresao.res))
plot(density(residuals(regresao.res)))

# INDEPENDENCIA DO RESIDUOS

library(car)
durbinWatsonTest(regresao.res)
#> durbinWatsonTest(regresao.res)
#lag Autocorrelation D-W Statistic p-value
#1        0.643696     0.1726438       0
#Alternative hypothesis: rho != 0

# pvalue <  alfa 0.05 logo H0 é falso, conclui-se que os residuos são dependentes.

###############
#             #
#  b.ii  II   #
#             #
###############

t1_max_value_of_c = max(dados_c$c)  # max c = 8.95  valor maximo existente nos dados
t1_min_value_of_c_for_linear_regression = 0.10     # valor inicial com pontos suficinetes para criar uma regressao


                # criar um vetor com os valores de C
valoresC = dados_c$c[dados_c$c <= t1_max_value_of_c & dados_c$c >= t1_min_value_of_c_for_linear_regression]
valoresC = sort(valoresC)

t1.c = c(); t1.r2 = c()
for(c in valoresC){
  dadosTestar = dados_c[dados_c$c <= c , ]
  t1_lm = lm(formula = dadosTestar$performance ~ dadosTestar$c)
  t1.c = c(t1.c, c)
  t1.r2 = c(t1.r2, round(summary(t1_lm)$r.squared , digits = 3 ) )
}
tabela1 = data.frame(t1.c , t1.r2)
names(tabela1) = c("cMax", "R2")
tabela1

# GRafico
plot(tabela1, col = "blue", ylab = "coeficiente de determinação", xlab = "c")
abline(h = 0.75, col = "red")


# obter o Cmax para r2 > 0.75
tabela1_cMax = max(tabela1[tabela1$R2 > 0.75,]$cMax)

# O VALOR DE CMAX = 0.65 com um R2 = 0.763

# GRAFICO:

t1_final <- dados_c[dados_c$c <= tabela1_cMax,]
t1_final.lm = lm(formula = t1_final$performance ~ t1_final$c)

#Grafico total
plot(dados_c$performance ~ dados_c$c, col = "green")
axis(side=1, at=seq(0, 8, by=0.1))
abline(t1_final.lm, col = "purple")  # linha de regressão
abline(v = cMax, col = "red")   # linha vertical do cMax
legend( "topright", legend = c("performance", "recta de ajuste", "cMax"), col= c("green", "purple", "red"), pch=15)

#Grafico Zoom
plot(t1_final$performance ~ t1_final$c, col = "green", 
     xlab = "C", ylab = "performance",
     ylim = c(0.89, 0.93), xlim = c(0,0.7))
axis(side=1, at=seq(0, 0.8, by=0.1))
axis(side=2, at=seq(0.89, 0.930, by=0.01))
abline(t1_final.lm, col = "purple")  # linha de regressão
abline(v = tabela1_cMax, col = "red")   # linha vertical do cMax
legend( "bottomright", legend = c("performance", "recta de ajuste", "cMax"), col= c("green", "purple", "red"), pch=15)

t1_final.lm
#Coefficients:
#  (Intercept)   t1_final$c  
# 0.90409      0.03714  

# RECTA =>   Y = 0.90 + 0.37 * X
summary(t1_final.lm)

#tabela reduzida para o paper.
tabela1_2 = tabela1[tabela1$cMax <= 0.9,]
tabela1_2

# Vamos apresentar sob a forma de um grafico:
plot(tabela1_2, col = "green", ylim = c(0.5, 1), xlim = c(0.1,1))
axis(side=1, at=seq(0, 1, by=0.1))
axis(side=2, at=seq(0.5, 1, by=0.1))
abline(h = 0.75, col = "red")


# =============================================================
#########
#       #
# b.iii #   Valores previstos para desempenho de C =  0.23, 0.37 ,0.44 ,0.58
#       #
#########

rm(t3.lm, t3.valores,tabela3,c)
t3.cMAx = 0.65
tabela3 = dados_c[dados_c$c <= t3.cMAx ,]

y = tabela3$performance
x = tabela3$c

t3.lm = lm(y ~x)
t3.valores = data.frame( x = c( 0.23, 0.37 ,0.44 ,0.58))

predict(t3.lm, newdata = t3.valores, interval="confidence", level = 0.95)
#        fit       lwr       upr
#1 0.9126364 0.9095866 0.9156863
#2 0.9178364 0.9152547 0.9204181
#3 0.9204364 0.9175877 0.9232851
#4 0.9256363 0.9215681 0.9297045

pred = data.frame(predict(t3.lm, newdata = t3.valores, interval="confidence", level = 0.95))
pred.c = c( 0.23, 0.37 ,0.44 ,0.58)

limites = data.frame(predict(t3.lm, newdata = data.frame(x = tabela3$c), interval="confidence", level = 0.95))

plot(t1_final$performance ~ t1_final$c, col = "green", 
     xlab = "C", ylab = "performance",
     ylim = c(0.89, 0.93), xlim = c(0,0.7))
axis(side=1, at=seq(0, 0.8, by=0.1))
axis(side=2, at=seq(0.89, 0.930, by=0.01))
abline(t1_final.lm, col = "purple")         # linha de regressão
points(pred.c,pred$fit,col="blue",pch=13)   # Pontos estimados
points(pred.c,pred$upr,col="pink",pch=25)   # Limites superior dos pontos
points(pred.c,pred$lwr,col="pink",pch=24)   # limite inferior dos pontos
# LIMITES
lines(tabela3$c, limites$upr, col = "grey") # Limites do ajuste
lines(tabela3$c, limites$lwr, col = "grey") # limites do ajuste

legend( "bottomright", legend = c("dados", "recta de ajuste", "estimativas", "limites"), col= c("green", "purple", "blue", "grey"), pch=15)


# ================================================================
#########
#       #
# b.iv  #   coefeciente de correlacao de pierson
#       #
#########


# OBtenção dos valos de C para os quais a performance é maxima. 
t4.maxPerformance = max(dados_c$performance)    # performance max = 0.9238948
t4.c_com_perfomance_maxima = dados_c[dados_c$performance == t4.maxPerformance, ]
#       c performance
#9   0.45   0.9238948
#11  0.55   0.9238948
#12  0.60   0.9238948
#37  1.85   0.9238948
#38  1.90   0.9238948
#39  1.95   0.9238948
#109 5.45   0.9238948
#110 5.50   0.9238948
#111 5.55   0.9238948


t4.vetor_de_C_com_perfomance_maxima = t4.c_com_perfomance_maxima$c
t4.vetor_de_C_com_perfomance_maxima = sort(t4.vetor_de_C_com_perfomance_maxima)


v4_c = dados_c$c[dados_c$c >= 0.20]
v4_c = sort(v4_c)

v4c = c(); v4p = c();v4p.pv = c(); v4isTrue = c();
for (i_cpmax in v4_c){
  dados4 <- dados_c[dados_c$c <= i_cpmax,]
  
  aux4 = cor(dados4$performance, dados4$c)
  v4c = c(v4c, i_cpmax)
  v4p = c(v4p, aux4)
  
  # teste
  aux4t = cor.test(dados4$performance, dados4$c)
  v4p.pv = c(v4p.pv , round(aux4t$p.value, digits = 3))
  v4isTrue = c(v4isTrue, is.element(i_cpmax, t4.vetor_de_C_com_perfomance_maxima))
}
tabela4 = data.frame( v4c, v4p, v4p.pv, v4isTrue)
names(tabela4) = c("cpMax", "coefeciente pierson", "pValue" , "is a Max Perform")
tabela4

# Tabela faltam os primeiros pontos porque nao consege fazer o teste.
#    cpMax coefeciente pierson pValue is a Max Perform
#1    0.20           0.9293813  0.071            FALSE
#2    0.25           0.8926437  0.042            FALSE
#3    0.30           0.8892202  0.018            FALSE
#4    0.35           0.9040101  0.005            FALSE
#5    0.40           0.9219138  0.001            FALSE
#6    0.45           0.9292816  0.000             TRUE
#7    0.50           0.9159249  0.000            FALSE
#8    0.55           0.9086935  0.000             TRUE
#9    0.60           0.8972876  0.000             TRUE
#10   0.65           0.8737342  0.000            FALSE
#11   0.70           0.8515019  0.000            FALSE
#12   0.75           0.8133406  0.000            FALSE
#13   0.80           0.7857781  0.000            FALSE


tabela4_max = tabela4[tabela4$`is a Max Perform` == TRUE,]
tabela4_max

# Grafico do pierson para o max = True
plot(tabela4_max$cpMax, tabela4_max$`coefeciente pierson`, xlim = c(0,6), ylim = c(0,1),
     col = "blue", xlab = "cpMax", ylab = "coeficiente")
lines(tabela4$cpMax, tabela4$`coefeciente pierson`, col = "grey")
axis(side=2, at=seq(0, 1, by=0.1))
axis(side=1, at=seq(0, 6, by=1))
legend( "topright", legend = c("performance maxima", "coeficientes"), col= c("blue", "gray"), pch=15)

# ==============================================================
# #  EXTRA   - Analise critica.
# ==============================

# Validação das regressões lineares com R2 > 0.75 e validação dos residuos.
# Vamos descartar os primeiros pontos.


# vetor ordenado de C's. Limitado aos valores que interesam.
v_c = dados_c$c[dados_c$c <= 0.9 & dados_c$c >= 0.2]
v_c = sort(v_c)

# Vamos agora iterar os valores de C crescentemente e construir
# uma tabela das diferentes regresões.
par.r2 = c(); par.Cmax = c();par.isNormal = c();par.isNormal.pValue = c();
par.mediaR = c(); par.isMedia0 = c(); par.isMedia0.pValue = c();
par.independencia = c(); par.independencia.pValue = c();
par.homo = c(); par.homo.pValue = c();
for(i in v_c){
  # Construir um vetor de dados limitado.
  parcial <- dados_c[dados_c$c <= i,]
  
  # valor de Cmax
  par.Cmax = c(par.Cmax, i)
  
  # fazer a regresão
  par.lm = lm(formula = parcial$performance ~ parcial$c)
  par.r2 = c(par.r2, round( summary(par.lm)$r.squared , digits = 3 ))
  
  # 1º Homocedasticidade
  for.homo.aux = 0;
  for.isHomo = "false"
  #if( i >= 0.20){
  mx1=median(parcial$c)
  for.homo = var.test(residuals(par.lm)[parcial$c>mx1],residuals(par.lm)[parcial$c<mx1])
  for.homo.aux = for.homo$p.value
  #}
  #else for.isHomo = " N.A. "
  if(for.homo.aux > 0.05) for.isHomo = "true"
  par.homo = c(par.homo, for.isHomo)
  par.homo.pValue = c(par.homo.pValue, round(for.homo.aux, digits = 3) )
  
  # 2.1º Normalidade dos residuos
  for.sh = shapiro.test(par.lm$residuals)
  for.normal = "false"
  if( for.sh$p.value > 0.05) for.normal = "true"
  par.isNormal = c( par.isNormal, for.normal)
  par.isNormal.pValue = c(par.isNormal.pValue, round(for.sh$p.value, digits = 3))
  
  # 2.2º Normal de media zero
  par.mediaR = c( par.mediaR, signif(mean(par.lm$residuals), digits = 4))
  for.ttest = t.test(par.lm$residuals, lm = 0)
  for.isNormal = "false"
  if (for.ttest$p.value > 0.05) for.isNormal = "true"
  par.isMedia0 = c(par.isMedia0, for.isNormal)
  par.isMedia0.pValue = c(par.isMedia0.pValue, round(for.ttest$p.value, digits = 3 ))
  
  #3 Independencia dos residuos (Autocorrelacao nula)
  library(car)
  for.dw = durbinWatsonTest(par.lm)
  for.independencia = "false"
  if (for.dw$p > 0.05) for.independencia = "true"
  par.independencia = c( par.independencia , for.independencia)
  par.independencia.pValue = c(par.independencia.pValue, round(for.dw$p, digits = 3))
  
  # Multicolinearidade: As vari aveis X 1 ; : : : ; X p devem ser linearmente independentes.
  # So temos uma variavel, portanto, nao é necessario verificar.
  
}
tabela = data.frame(
  par.Cmax, par.r2, 
  par.homo, par.homo.pValue,
  par.isNormal, par.isNormal.pValue, par.mediaR, par.isMedia0, par.isMedia0.pValue, 
  par.independencia ,par.independencia.pValue
  )
names(tabela) = c(
  "Cmax", "R2",
  "isHomo", "homo_pValue",
  "isNormal", "Npv", "mediaN", "isMedia0", "media0.pv",
  "independencia", "indep_pValue"
  )
tabela

plot(tabela$Cmax ~ tabela$R2)
#reta nos 0.75.

#tabela
#   Cmax    R2 isHomo homo_pValue isNormal   Npv     mediaN isMedia0 media0.pv independencia indep_pValue
#1  0.20 0.864   true       0.973    false 0.036  0.000e+00     true         1          true        0.274
#2  0.25 0.797   true       0.590     true 0.663 -8.665e-20     true         1          true        0.050
#3  0.30 0.791   true       0.278     true 0.965 -6.867e-19     true         1          true        0.054
#4  0.35 0.817  false       0.047     true 0.916  3.717e-19     true         1         false        0.042
#5  0.40 0.850  false       0.010     true 0.756  2.982e-19     true         1         false        0.042
#6  0.45 0.864  false       0.024     true 0.493  1.205e-19     true         1         false        0.036
#7  0.50 0.839   true       0.166     true 0.059 -1.951e-19     true         1         false        0.022
#8  0.55 0.826   true       0.262     true 0.053  1.975e-20     true         1         false        0.010
#9  0.60 0.805   true       0.327    false 0.045 -1.193e-18     true         1         false        0.006
#10 0.65 0.763   true       0.439    false 0.024  2.503e-19     true         1         false        0.006
#11 0.70 0.725   true       0.469    false 0.033 -1.704e-19     true         1         false        0.000
#12 0.75 0.662   true       0.444    false 0.027  2.893e-19     true         1         false        0.000
#13 0.80 0.617   true       0.372    false 0.037  5.421e-20     true         1         false        0.000
#14 0.85 0.570   true       0.230    false 0.049  1.145e-19     true         1         false        0.000
#15 0.90 0.538   true       0.161    false 0.044 -6.748e-19     true         1         false        0.000



# Se o cMax que valida os residuos é 0.30 vamos entao fazer os graficos com estes valores.
tabela5 = data.frame(tabela$Cmax,tabela$R2,tabela$isHomo,tabela$isNormal,tabela$independencia)
#write.table(tabela5 , file="tab total.csv", sep = ";" , row.names = FALSE)

# GRAFICO:
cMax = 0.30
final <- dados_c[dados_c$c <= cMax,]
final.lm = lm(formula = final$performance ~ final$c)

#Grafico total
plot(dados_c$performance ~ dados_c$c, col = "green")
axis(side=1, at=seq(0, 8, by=0.1))
abline(final.lm, col = "purple")  # linha de regressão
abline(v = cMax, col = "red")   # linha vertical do cMax
legend( "topright", legend = c("residuos", "recta de ajuste", "cMax"), col= c("green", "purple", "red"), pch=15)

#Grafico Zoom
plot(final$performance ~ final$c, col = "green", ylim = c(0.89, 0.93))
axis(side=1, at=seq(0, 3.6, by=0.1))
axis(side=2, at=seq(0.89, 0.930, by=0.01))
abline(final.lm, col = "purple")  # linha de regressão
abline(v = cMax, col = "red")   # linha vertical do cMax
legend( "topleft", legend = c("residuos", "recta de ajuste", "cMax"), col= c("green", "purple", "red"), pch=15)







                 