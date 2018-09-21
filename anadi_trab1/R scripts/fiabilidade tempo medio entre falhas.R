
# Analise do tempo medio entre falhas 

# fazer o import do ficheiro csv output.txt
alldata = data.frame(output)

# Função de limitar o tempo.
rm(restricao_temporal)
restricao_temporal <- function(tempo_inicio,tempo_fim) {
  
  
  tempo_inicio = as.Date( as.character(tempo_inicio), "%Y-%m-%d")
  tempo_fim = as.Date( as.character(tempo_fim), "%Y-%m-%d")
  
  alldata = data.frame(output)
  
  # https://stackoverflow.com/questions/14471640/r-subset-by-date
  alldata$data2 <- as.Date( as.character(alldata$data), "%Y-%m-%d")
  
  
  
  dados <- alldata[alldata$data2 >= tempo_inicio & alldata$data2 <= tempo_fim,]
  retorno <- dados
  return(retorno)
}

#---11111111111111111111111111111111111111111111111   MTBF

tempo_inicio = as.Date("2017-01-01")
tempo_fim = as.Date("2017-12-31")
dados0 = restricao_temporal(tempo_inicio = tempo_inicio,tempo_fim = tempo_fim) # restringir ao tempo.
dados1 = dados0[dados0$falha == 'true' ,]  #apenas as falhas apresentam MTBF

d1.cores = rainbow(5)

d1.nserver = sort(unique(dados1$server))
d1.nprotocolo = sort(unique(dados1$protocolo))


m1.nserver = sort(unique(dados1$server))
n = c(1:5)
m1.mtbf = list() #c(c(),c(),c(),c(),c())
for(i in n){
  aux = dados1$mtbf_servidor[dados1$server == m1.nserver[i]]
  aux = head( aux, -1); # Remove o ultimo valor para podermos fazer a media em condições.
  m1.mtbf[[i]] = aux
}

m1.avg = c();
m1.min = c();
m1.max = c();
m1.sd = c();
for(i in n){
  m1.avg = c(m1.avg, trunc( mean(m1.mtbf[[i]]) ) )
  m1.min = c(m1.min,  min(m1.mtbf[[i]])  )
  m1.max = c(m1.max,  max(m1.mtbf[[i]])  )
  m1.sd = c(m1.sd, trunc( sd(m1.mtbf[[i]]) ) )
}

# tabela para ver min max, avg e sd.
m1.df = data.frame(m1.nserver, m1.avg,m1.min,m1.max,m1.sd)
m1.df
#m1.nserver m1.avg m1.min m1.max m1.sd
#1     vsrv10    332      1   3535   518
#2     vsrv11    400      1  15869  1125
#3     vsrv16    176      1   5722   409
#4     vsrv17    184      1   6228   440
#5      vsrv8    171      1   5897   445







# criacao do dataframe sob forma:
# Servidor | valor de MTBF
#  srvr11  |   12314
#  srvr12  |   1242

m2.server = c(
    rep( as.character(m1.nserver[1]), length(m1.mtbf[[1]]))
  , rep( as.character(m1.nserver[2]), length(m1.mtbf[[2]]))
  , rep( as.character(m1.nserver[3]), length(m1.mtbf[[3]]))
  , rep( as.character(m1.nserver[4]), length(m1.mtbf[[4]]))
  , rep( as.character(m1.nserver[5]), length(m1.mtbf[[5]]))
)
m2.mtbf = c(m1.mtbf[[1]], m1.mtbf[[2]], m1.mtbf[[3]], m1.mtbf[[4]], m1.mtbf[[5]])


m2.df = data.frame(m2.server,m2.mtbf)
names(m2.df) = c('server','mtbf')

m2.cores = rainbow(5)

# GRAFICO:
boxplot(m2.df$mtbf ~ m2.df$server
        , outline = FALSE
        , main = "Tempo médio entre falhas 2017 por servidor"
        , xlab = "Servidores"
        , ylab = "min"
        , border = m2.cores)

# rm(ppp) ; ppp= rep(9,5)#c(13,13,13,13,13)
points(n, m1.avg, col=m2.cores,pch=rep(9,5))
text (n,m1.avg,m1.avg,pos = 4)
text (n,600,paste("sd=",m1.sd),pos = 1)



# Histograma do vsrv11.

hist( m1.mtbf[[2]]
      
      ,prob=FALSE
      , breaks=10000
      , main = "Distribuição tempo entre falhas do servidor vsrv11"
      , xlab = "min"
      , col = "green"
      , xlim = c(0, 100)
      , ylim = c(0, 400)
)

hist( m1.mtbf[[2]]
      
      ,prob=FALSE
      , breaks=100
      , main = "Distribuição tempo entre falhas do servidor vsrv11"
      , xlab = "min"
      , col = "green"
      , xlim = c(100, 5000)
      , ylim = c(0, 1000)
)


 # valores maximos, plot.
barplot(m1.max
        ,col = m2.cores
        ,xlab = "servidores"
        , ylab = 'min'
        ,main = 'Valores maximos de tempos entre falhas por servidores'
        , names.arg = m1.nserver
        , ylim = c(0 ,18000)
        )
text (c(0.9,2,3,4.5,5.5),m1.max,m1.max,pos = rep(1,5))

# 333333333333333333333333333333333333333



