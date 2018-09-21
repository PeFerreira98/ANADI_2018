# ANADI Analise de funcionamento dos servidores VPN do DEI

# PONTO 2 - Perguntas.

# Carregar o ficheiro "output.csv" atraves de IMPORT. Com os dados pre processados.
dados = data.frame(output)


# 2.a) Disponibilidade: Calculada como taxa das falhas / (falhas + nao falhas)
#                       segundo indicação do Mooodle.

# Total

 v_estado = "false";
 
 auxd = dados$falha;
 
 disp.total = round(sum(auxd == v_estado) / length(auxd), digits = 2);
 
 v_server = "vsrv10"; v_ano = 2017;
 auxd = dados$falha[dados$server == v_server & dados$aaaa == v_ano];
 disp.vsrv10 = round(sum(auxd == v_estado) / length(auxd), digits = 2);
 
 v_server = "vsrv16"; v_mes = "201703";
 auxd = dados$falha[dados$server == v_server & dados$aaaaamm == v_mes];
 disp.vsrv16 = round(sum(auxd == v_estado) / length(auxd), digits = 2);
 
 
 
 v_server = "vsrv17"; v_ano7 = "2017-02-28";
 auxd = dados$falha[dados$server == v_server & dados$data == v_ano7];
 disp.vsrv17 = round(sum(auxd == v_estado) / length(auxd), digits = 2);
 
 
 print("Disponibilidade")
 print(sprintf("Total: %s", disp.total))
 print(sprintf("vsrv10 em 2017: %s " , disp.vsrv10))
 print(sprintf("vsrv16 em %s: %s " , v_mes, disp.vsrv16))
 print(sprintf("vsrv17 em %s: %s " , v_ano7, disp.vsrv17))
 
 
# "Disponibilidade"
# "Total: 0.82"
 #"vsrv10 em 2017: 0.87 "
 #"vsrv16 em 201703: 0.80 "
 #"vsrv17 em 2017-02-28: 0.90 "
 
 #----------------------------------------------------------------
 
 #2.b)  funcao fiabilidade no vsrv8 de 01-01-2017 a 28-02-2017 (31 + 28 = 59 dias)
 
 # y = numero de falhas / tempo
 b_server = "vsrv8"; b_inicio = 201701; b_fim = 201702;
 auxd = dados$falha[dados$server == v_server & (dados$aaaaamm == b_inicio | dados$aaaaamm == b_fim)];
 v_estado = "true"
 b_nfalhas = sum(auxd == v_estado)
 b_tempo = (31 + 28) * 24 * 60 # minutos
 b.y = b_nfalhas / b_tempo;
 b.y.jan = length(dados$falha[dados$server == v_server & dados$falha == "true" & dados$aaaaamm == b_inicio])/ (31 * 24 * 60)
 b.y.fev = length(dados$falha[dados$server == v_server & dados$falha == "true" & dados$aaaaamm == b_fim])/ (28 * 24 * 60)
 
     # Função fiabilidade.
   fiabilidade <- function(y_value,x_tempo) {
      retorno <- exp(-1 * y_value * x_tempo)
      return(retorno)
   }

     # Pontos para grafico   
   b.tempo = c(1:1000);   # Vamos criar uns vectores e preencherlos para fazer grafos.
   b.j = c();
   b.f = c();
   b.jf = c();
   
   for (i in b.tempo){
     b.jf <- c(b.jf , fiabilidade(b.y,i))
     b.j <- c(b.j , fiabilidade(b.y.jan,i))
     b.f <- c(b.f , fiabilidade(b.y.fev,i))
   }
   
   b.cor = rainbow(3);
   b.leg = c("Jan-Fev", "Jan", "Fev")
   
   plot( b.tempo , b.jf, type = "l", col = b.cor[1] , xlab = "t min", ylab = "R(t)", main = "Fiabilidade")
   lines( b.tempo , b.j, type = "l", col = b.cor[2])
   lines( b.tempo , b.f, type = "l", col = b.cor[3])
   legend( "topright", legend = b.leg, col= b.cor, pch=15)

   # REMAKE de 2.b com nova formula:
   
       #Janeiro  
   b.r.jan.s = length(dados$falha[dados$server == v_server & dados$falha == "false" & dados$aaaaamm == b_inicio])
   b.r.jan.f = length(dados$falha[dados$server == v_server & dados$falha == "true" & dados$aaaaamm == b_inicio])
   b.r.jan.time = (31 * 24 * 60) #min
   b.r.jan.delta_t = b.r.jan.time / (b.r.jan.s + b.r.jan.f)
   b.r.jan.y = b.r.jan.f / (b.r.jan.s * b.r.jan.delta_t)
   
   b.r.fev.s = length(dados$falha[dados$server == v_server & dados$falha == "false" & dados$aaaaamm == b_fim])
   b.r.fev.f = length(dados$falha[dados$server == v_server & dados$falha == "true" & dados$aaaaamm == b_fim])
   b.r.fev.time = (28 * 24 * 60) #min
   b.r.fev.delta_t = b.r.fev.time / (b.r.fev.s + b.r.fev.f)
   b.r.fev.y = b.r.fev.f / (b.r.fev.s * b.r.fev.delta_t)
   
   b.r.janfev.s = length(dados$falha[dados$server == v_server & dados$falha == "false" & (dados$aaaaamm == b_inicio | dados$aaaaamm == b_fim)])
   b.r.janfev.f = length(dados$falha[dados$server == v_server & dados$falha == "true" & (dados$aaaaamm == b_inicio | dados$aaaaamm == b_fim)])
   b.r.janfev.time = ((31 +28) * 24 * 60) #min
   b.r.janfev.delta_t = b.r.janfev.time / (b.r.janfev.s + b.r.janfev.f)
   b.r.janfev.y = b.r.janfev.f / (b.r.janfev.s * b.r.janfev.delta_t)
   
   # Fazer os vectores para o grafico.
   b.r.j = c(); b.r.f = c(); b.r.jf = c();
     for (i in b.tempo){
       b.r.jf <- c(b.r.jf , fiabilidade(b.r.janfev.y,i))
       b.r.j <- c(b.r.j , fiabilidade(b.r.jan.y,i))
       b.r.f <- c(b.r.f , fiabilidade(b.r.fev.y,i))
     }
   
   
   b.r.cor = rainbow(3);
   b.r.leg = c("Jan-Fev R", "Jan R", "Fev R")
   
   plot( b.tempo , b.r.jf, type = "l", col = b.r.cor[1] , xlab = "t min", ylab = "R(t)", main = "Fiabilidade R")
   lines( b.tempo , b.r.j, type = "l", col = b.r.cor[2])
   lines( b.tempo , b.r.f, type = "l", col = b.r.cor[3])
   legend( "topright", legend = b.r.leg, col= b.r.cor, pch=15)
   
   
   b.all.cor = c("red", "green" , "blue", "red4", "green4" , "blue4")
   b.all.leg = c("Jan-Fev R", "Jan R", "Fev R", "Jan-Fev", "Jan", "Fev")
   plot( b.tempo , b.r.jf, type = "l", col = b.all.cor[1] , xlab = "t min", ylab = "R(t)", main = "Fiabilidade - Comparação de formulas formulas")
   lines( b.tempo , b.r.j, type = "l", col = b.all.cor[2])
   lines( b.tempo , b.r.f, type = "l", col = b.all.cor[3])
   lines( b.tempo , b.jf, type = "l", col = b.all.cor[4])
   lines( b.tempo , b.j, type = "l", col = b.all.cor[5])
   lines( b.tempo , b.f, type = "l", col = b.all.cor[6])
   legend( "topright", legend = b.all.leg, col= b.all.cor, pch=15)
      
  # -----------------------------------
   
   # 2.c Taxa de falhas:
   
   # EXISTEM DUAS DEFINICOES DE TAXA DE FALHAS, falhas por tempo, e falhas por sessoes.
   # Vamos usar falhas por tempo.
   
   #Funcao para calcular taxa com novo metodo.
   calcular_taxa <- function(s,f,tempo){
     f_delta_t = tempo / (s+f)
     f_taxa = f / ( s * f_delta_t)
     return(f_taxa)
   }
   
              # Vamos fazer um select aos dados apenas para aquele Mes e servidor:
   c.server = "vsrv16"
   c.aaaamm = 201712
   c.dados <- dados[dados$aaaaamm == c.aaaamm & dados$server == c.server,]
   c.dias = c(1:31)
   c.minutosdia = 24*60
   
   # construir um data frame:
   rm(c.nfalhas, c.taxa);
   c.nfalhas = c();
   c.taxa = c();
   
   # Gerar os dados para cada dia do mes.
   for(i in c.dias){
     
     # Obter o dia no formato AAAA-MM-DD
     v_data = as.Date(as.character(20171200+i), "%Y%m%d")
     auxdata = as.character(v_data) 
     
     # criacao de um vector com todas as falhas/nao falhas do dia.
     auxd = c.dados$falha[c.dados$data == auxdata]
     
     # Acrescentar o numero de falhas desse dia a um vector c.nfalhas
     c.nfalhas <- c(c.nfalhas , sum(auxd == "true"))
     
     # Acrescentar a taxa de falhas desse dia a um vector c.nfalhas
     #c.taxa <- c(c.taxa , sum(auxd == "true")/c.minutosdia) # FORMULA ANTIGA.
     c.f.s = sum(auxd == "false")
     c.f.f = sum(auxd == "true")
     c.f.taxa = calcular_taxa(s = c.f.s, f = c.f.f, tempo = c.minutosdia)
     c.taxa <- c(c.taxa,  c.f.taxa   )
   }
   
   # Criar um data frame com os 3 vectores, "dia" "numero falhas" "taxa falhas"
   c.dataframe = data.frame("dia" = c.dias, "falhas" = c.nfalhas, "taxa" = c.taxa)
   c.dataframe
   c.n = length(c.dias);
   
   c.media = 0.01;
   c.desvio = c.taxa - c.media;
   
   c.alfa = 0.05      # nivel de confianca de 95%
   
   # Queremos comparar se a media da taxa de falhas do mes esta abaixo de 0.01.
   # Vamos usar um teste de hipoteses, UNILATERAL a ESQUERDA
   # H0: u = u0 vs H1: u < u0
   
   c.test = t.test(c.taxa, mu = c.media, conf.level = 1- c.alfa, alternative = "less")
   c.test
   
   # One Sample t-test
   
   # data:  c.taxa
   # t = -3.69, df = 30, p-value = 0.0004441
   # alternative hypothesis: true mean is less than 0.01
   # 95 percent confidence interval:
   #   -Inf 0.00812311
   # sample estimates:
   #   mean of x 
   # 0.006524542 
   
   
   # alfa    = 0.05
   # p.value = 0.0004441
   # p < alfa : portanto rejeita-se H0 sendo H1 altamente provavel.
   # true mean is less than 0.01

   
   #plot(density(c.desvio))
   
   # GRAFICO DA diferenca entre a nossa e a media indicada de 0.01
   hist(c.desvio, freq=FALSE,prob=TRUE, breaks=10, main = "2.c) distribuição taxa de falhas" , xlab = "diferença falhas/min", col = "green")
   lines(density(c.desvio), col = "red")
 
   #grafico da nossa.
   hist(c.taxa, freq=FALSE,prob=TRUE, breaks=10, main = "2.c) distribuição taxa de falhas" , xlab = "falhas/min", col = "blue")
   lines(density(c.taxa), col = "red")
  
   # Grafico do numero de falhas por dia.
   barplot(c.nfalhas, names.arg = c.dias, main = "Falhas por dia em vsrv16" , ylab = "falhas",xlab = "dia", col = "pink")
   
   barplot(c.taxa, names.arg = c.dias, main = "Taxa de Falhas por dia em vsrv16" , ylab = "falhas/min" ,xlab = "dia", col = "purple")

   
   
   
   
   #---------------------------------------
   
  #2.d) Analise se os tempos medios entre falhas de vsr10 e vsr17 sao iguais.

   # a nossa dataframe ja tem o MTBF processado, apenas temos fazer um select para apanhar todos os valores.
   # O ultimo valor não tem falha a seguir portanto removemos.

   rm(d.f10,d.f17)
   v_server = "vsrv10"
   auxd =    dados$mtbf_servidor[dados$server == v_server & dados$falha == "true"];
   d.f10 = head( auxd, -1); # Remove o ultimo valor para podermos fazer a media em condições.
   
   v_server = "vsrv17"
   auxd =    dados$mtbf_servidor[dados$server == v_server & dados$falha == "true"];
   d.f17 = head( auxd, -1)
   
   confianca = 0.95;   
   length(d.f10)
   length(d.f17)
   
   # Amostras independentes, portanto temos de confirmar se a as variancias são iguais.
   
   #https://stats.stackexchange.com/questions/15722/how-to-use-levene-test-function-in-r
   rm(d.value, d.server, d.set)
   #rm(d.dados, d.Mdados, d.y, d.group, d.set, d.h)
   
   # criacao de um data set do tipo:     evita o uso do MELT.
   # Server | Valor MTBF
   # vsrv10 |   100
   # vsrv10 |   20
   # vsrv17 |   120
   
   # concetanecao dos dois vectores de valores.
   d.value <- c(d.f10, d.f17)  
   # indicacao de que servidor pertencem.
   d.server = c( rep_len("vsrv10", length(d.f10)), rep_len("vsrv17", length(d.f17)))
   #criacao do data set
   d.set = data.frame(d.server,d.value)
   names(d.set)= c("Servidor", "MTBF")
   
   leveneTest(MTBF ~ Servidor  , d.set, center=mean)
   
   # Levene's Test for Homogeneity of Variance (center = mean)
   #Df F value    Pr(>F)    
   #group    1  50.191 1.685e-12 ***
   # 3426                      
   #---
   # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
   # Valor de Pr = 1.685x10EXP-12 , Pr < 0.05 implica que variancias diferentes.
   
   
   # Efectuar T-test bilateral de amostras independentes de variancias diferentes.
   
   d.test = t.test( d.f10 , d.f17, conf.level = confianca,  alternative = "two.sided", var.equal = FALSE, paired = FALSE)
   d.test
  
   #Welch Two Sample t-test
   #
   #data:  d.f10 and d.f17
   #t = 4.7556, df = 297.76, p-value = 3.087e-06
   #alternative hypothesis: true difference in means is not equal to 0
   #95 percent confidence interval:
   #   118.9606 286.9235
   #sample estimates:
   #   mean of x mean of y 
   # 401.4400  198.4979 
   
   # Valor de P-value = 3.087e-06 , P < 0.05 alfa , -> H0 As medias são diferentes.
   
   
   
   
   plot(density(d.f17), main = "distribuição do tempo entre falhas - MTBF" , xlab = "mtbf (min)", col = "red")
   lines(density(d.f10), col = "green")
   legend( "topright", legend = c("vsrv10", "vsrv17"), col= c("green", "red"), pch=15)

   plot(density(d.f17), main = "distribuição do tempo entre falhas - MTBF" , xlim = c(-100, 500) ,xlab = "mtbf (min)", col = "red")
   lines(density(d.f10), col = "green")
   legend( "topright", legend = c("vsrv10", "vsrv17"), col= c("green", "red"), pch=15)
   
   boxplot(d.f10, d.f17, outline = FALSE, main = "MTBF", xlab = "Servidores",ylab = "min", names = c("vsrv10", "vsrv17"), border = c("green", "red"))
   points(1,mean(d.f10),col="green",pch=13)
   points(2,mean(d.f17),col="red",pch=13)
   text (1,mean(d.f10),trunc(mean(d.f10)),pos = 4)
   text (2,mean(d.f17),trunc(mean(d.f17)),pos = 4)
   mean(d.f10)
   
   d.test = t.test( d.f10 , d.f17, conf.level = confianca,  alternative = "greater", var.equal = FALSE, paired = FALSE)
   d.test
   
  # Welch Two Sample t-test
  # 
  # data:  d.f10 and d.f17
  # t = 4.7556, df = 297.76, p-value = 1.544e-06
  # alternative hypothesis: true difference in means is greater than 0
  # 95 percent confidence interval:
  #   132.53    Inf
  # sample estimates:
  #   mean of x mean of y 
   #401.4400  198.4979 
   
   
   #Esperança
   #----------------------------------------------------------------------
   
   # 2.e) Teste ANOVA
   
   e.servidores = unique(dados$server)
   e.servidores
   rm(e.s8, e.s10, e.s11, e.s16, e.s17)
   
   # dados de todos os servidores.
   e.s8 = dados$mtbf_servidor[dados$server == "vsrv8" & dados$falha == "true"]
   e.s8 = head( e.s8, -1) #eleminar o ultimo valor que cai fora do periudo e e zero
   e.s10 = dados$mtbf_servidor[dados$server == "vsrv10" & dados$falha == "true"]
   e.s10 = head( e.s10, -1)
   e.s11 = dados$mtbf_servidor[dados$server == "vsrv11" & dados$falha == "true"]
   e.s11 = head( e.s11, -1)
   e.s16 = dados$mtbf_servidor[dados$server == "vsrv16" & dados$falha == "true"]
   e.s16 = head( e.s16, -1)
   e.s17 = dados$mtbf_servidor[dados$server == "vsrv17" & dados$falha == "true"]
   e.s17 = head( e.s17, -1)
   
   
   # criacao de um data set do tipo:     evita o uso do MELT.
   # Server | Valor MTBF
   # vsrv10 |   100
   # vsrv10 |   20
   # vsrv17 |   120
   
   # concetanecao dos dois vectores de valores.
   e.value <- c(e.s8, e.s10, e.s11, e.s16 , e.s17)  
   # indicacao de que servidor pertencem.
   e.server = c( 
       rep_len("vsrv08", length(e.s8))
     , rep_len("vsrv10", length(e.s10))
     , rep_len("vsrv11", length(e.s11))
     , rep_len("vsrv16", length(e.s16))
     , rep_len("vsrv17", length(e.s17))
     )
   #criacao do data set
   e.set = data.frame(e.server,e.value)
   names(e.set)= c("Servidor", "MTBF")
   e.set
   
   e.cor = rainbow(5)
   boxplot(e.set$MTBF ~ e.set$Servidor, outline = FALSE, main = "MTBF", xlab = "Servidores",ylab = "min", border = e.cor)
   points(1,mean(e.s8) ,col=e.cor[1],pch=13 )
   points(2,mean(e.s10),col=e.cor[2],pch=13)
   points(3,mean(e.s11),col=e.cor[3],pch=13)
   points(4,mean(e.s16),col=e.cor[4],pch=13)
   points(5,mean(e.s17),col=e.cor[5],pch=13)
   
   
   text (1,mean(e.s8),trunc(mean(e.s8)),pos = 4)
   text (2,mean(e.s10),trunc(mean(e.s10)),pos = 4)
   text (3,mean(e.s11),trunc(mean(e.s11)),pos = 4)
   text (4,mean(e.s16),trunc(mean(e.s16)),pos = 4)
   text (5,mean(e.s17),trunc(mean(e.s17)),pos = 4)
   
   mean(e.s8)
   mean(e.s10)
   mean(e.s11)
   mean(e.s16)
   mean(e.s17)
   
   
            #Teste ANOVA:
   e.mod1 = lm(e.set$MTBF ~ e.set$Servidor, data = e.set)
   anova(e.mod1)
   
   #Analysis of Variance Table
   #
   #Response: e.set$MTBF
   #Df     Sum Sq  Mean Sq F value    Pr(>F)    
   #e.set$Servidor     4   79661799 19915450   57.42 < 2.2e-16 ***
   #  Residuals      11640 4037185688   346837                      
   # ---
   # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
   
   
   #são diferentes as variancias.
   
   
              # Tukey HSD
   # Fonte:
   # https://stats.idre.ucla.edu/r/faq/how-can-i-do-post-hoc-pairwise-comparisons-in-r/
   e.a1 <- aov(e.set$MTBF ~ e.set$Servidor) 
   summary(e.a1)
   TukeyHSD(e.a1)
   
   #Tukey multiple comparisons of means
   #95% family-wise confidence level
   #
   #Fit: aov(formula = e.set$MTBF ~ e.set$Servidor)
   #
   #$`e.set$Servidor`
   #diff        lwr        upr     p adj
   #vsrv10-vsrv08  216.820041  116.08591  317.55417 0.0000000
   #vsrv11-vsrv08  238.584465  188.68254  288.48639 0.0000000
   #vsrv16-vsrv08    2.662794  -36.52458   41.85017 0.9997366
   #vsrv17-vsrv08   13.877980  -25.85493   53.61089 0.8759744
   #vsrv11-vsrv10   21.764424  -83.67595  127.20480 0.9803129
   #vsrv16-vsrv10 -214.157248 -314.96956 -113.34494 0.0000001
   #vsrv17-vsrv10 -202.942062 -303.96768 -101.91644 0.0000004
   #vsrv16-vsrv11 -235.921671 -285.98121 -185.86213 0.0000000
   #vsrv17-vsrv11 -224.706485 -275.19422 -174.21875 0.0000000
   #vsrv17-vsrv16   11.215186  -28.71550   51.14588 0.9402563
   