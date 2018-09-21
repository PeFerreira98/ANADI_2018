
# Analise da duracao das sessoes

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

#---11111111111111111111111111111111111111111111111

tempo_inicio = as.Date("2017-01-01")
tempo_fim = as.Date("2017-12-31")
dados0 = restricao_temporal(tempo_inicio = tempo_inicio,tempo_fim = tempo_fim) # restringir ao tempo.
dados1 = dados0[dados0$falha == 'false' ,]  #apenas as nao falhas.

d1.cores = rainbow(5)

d1.nserver = sort(unique(dados1$server))
d1.nprotocolo = sort(unique(dados1$protocolo))

d1.s.avg = c();
d1.s.min = c();
d1.s.max = c();
d1.s.sd = c();

for(s1 in d1.nserver){
  aux = min(dados1$duracao[dados1$server == s1])
  #aux = trunc(aux)
  d1.s.min = c(d1.s.min , aux )
  aux = max(dados1$duracao[dados1$server == s1])
  #aux = trunc(aux)
  d1.s.max = c(d1.s.max , aux )
  aux = mean(dados1$duracao[dados1$server == s1])
  aux = trunc(aux)
  d1.s.avg = c(d1.s.avg , aux )
  aux = sd(dados1$duracao[dados1$server == s1])
  aux = trunc(aux)
  d1.s.sd = c(d1.s.sd ,aux )
}


boxplot(dados1$duracao ~dados1$server
        , outline = FALSE
        , main = "Duração sessões em 2017 por servidor"
        , xlab = "Servidores"
        ,ylab = "min"
        , border = d1.cores)
n = c(1:5)
# rm(ppp) ; ppp= rep(9,5)#c(13,13,13,13,13)
points(n, d1.s.avg, col=d1.cores,pch=rep(9,5))
text (n,d1.s.avg,d1.s.avg,pos = 4)
text (n,150,paste("sd=",d1.s.sd),pos = 1)
#points(n, d1.s.avg+d1.s.sd, col=d1.cores,pch=rep(24,5))
#points(n, d1.s.avg-d1.s.sd, col=d1.cores,pch=rep(25,5))

#a = rep('m = ',5)
#b = c(1:5)
#paste(a,b)


# por PROTOCOLO:


d1.s.avg = c();
d1.s.min = c();
d1.s.max = c();
d1.s.sd = c();

for(p1 in d1.nprotocolo){
  aux = min(dados1$duracao[dados1$protocolo == p1])
  #aux = trunc(aux)
  d1.s.min = c(d1.s.min , aux )
  aux = max(dados1$duracao[dados1$protocolo == p1])
  #aux = trunc(aux)
  d1.s.max = c(d1.s.max , aux )
  aux = mean(dados1$duracao[dados1$protocolo == p1])
  aux = trunc(aux)
  d1.s.avg = c(d1.s.avg , aux )
  aux = sd(dados1$duracao[dados1$protocolo == p1])
  aux = trunc(aux)
  d1.s.sd = c(d1.s.sd ,aux )
}

boxplot(dados1$duracao ~dados1$protocolo
        , outline = FALSE
        , main = "Duração sessões em 2017 por protocolo"
        , xlab = "Protocolos"
        ,ylab = "min"
        , border = d1.cores)
n = c(1:5)
# rm(ppp) ; ppp= rep(9,5)#c(13,13,13,13,13)
points(n, d1.s.avg, col=d1.cores,pch=rep(9,5))
text (n,d1.s.avg,d1.s.avg,pos = 4)
text (n,200,paste("sd=",d1.s.sd),pos = 1)



