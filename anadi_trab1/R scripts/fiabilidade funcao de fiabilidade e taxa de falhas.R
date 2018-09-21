# taxa de falhas, e funcao de fiabilidade:



# fazer o import do ficheiro csv output.txt
alldata = data.frame(output)

## read in date/time info in format 'm/d/y h:m:s'
#dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
#times <- c("23:03:20", "22:29:56", "01:03:30", "18:21:03", "16:56:26")
#x <- paste(dates, times)
#strptime(x, "%m/%d/%y %H:%M:%S")



ff.dates <- alldata$data
ff.hora0 <- alldata$hora_inicio
aux <- paste(ff.dates, ff.hora0)
# strftime - time to char, strptime - char to time.
# strftime(ff.t0, format = "%Y-%m-%d %H:%M", tz = "UTC", usetz = FALSE)  
ff.t0 <- strptime(aux, format = "%Y-%m-%d %H:%M")  # tempo de inicio.
ff.t1 = ff.t0 + (alldata$duracao * 60)   #Criacao do tempo de fim.

ff.data0 = alldata
ff.data0$t0 = ff.t0
ff.data0$t1 = ff.t1


# Limitar o tempo: PODE SER ALTERADO.
tempo_inicio = strptime("2017-01-01 00:00", format = "%Y-%m-%d %H:%M")
tempo_final = strptime("2018-01-01 00:00", format = "%Y-%m-%d %H:%M")
ff.duracao = difftime(tempo_final, tempo_inicio,units="mins")
duracao = as.numeric(ff.duracao, units = "mins")

# Filtrar para apenas o periudo temporal.
ff.data1 = ff.data0[ff.data0$t0 >= tempo_inicio & ff.data0$t1 < tempo_final,]


#Funcao para calcular taxa com novo metodo.
calcular_taxa <- function(s,f,tempo){
  f_delta_t = tempo / (s+f)
  f_taxa = f / ( s * f_delta_t)
  return(f_taxa)
}


# Obter os parametros
ff.nserver = unique(ff.data1$server)

ff.server.s = c();  #numero sessoes nao falha nos servidores
ff.server.f = c();  # numero falhas nos servidores
ff.server.taxa = c();  # taxa por servidor

for(s1 in ff.nserver){
  
  aux = ff.data1$falha[ff.data1$server == s1]
  ns = sum(aux == 'false')
  ff.server.s = c(ff.server.s , ns)
  nf = sum(aux == 'true')
  ff.server.f = c(ff.server.f , nf)
  y = calcular_taxa(s = ns, f = nf, tempo = duracao)
  ff.server.taxa = c(ff.server.taxa , y)
}


ff.dfserver = data.frame(ff.nserver, ff.server.s, ff.server.f, ff.server.taxa)
names(ff.dfserver) = c('server', 's', 'f', 'taxa')




     # para os protocolos:

ff.nprotocolo = unique(ff.data1$protocolo)

ff.protocolo.s = c();  #numero sessoes nao falha nos prot
ff.protocolo.f = c();  # numero falhas nos prot
ff.protocolo.taxa = c();  # taxa por protocolo

for(p1 in ff.nprotocolo){
  
  aux = ff.data1$falha[ff.data1$protocolo == p1]
  ns = sum(aux == 'false')
  ff.protocolo.s = c(ff.protocolo.s , ns)
  nf = sum(aux == 'true')
  ff.protocolo.f = c(ff.protocolo.f , nf)
  y = calcular_taxa(s = ns, f = nf, tempo = duracao)
  ff.protocolo.taxa = c(ff.protocolo.taxa , y)
}
ff.dfprotocolo = data.frame(ff.nprotocolo, ff.protocolo.s, ff.protocolo.f, ff.protocolo.taxa)
names(ff.dfprotocolo) = c('protocolo', 's', 'f', 'taxa')




# Graficos

 #servidores
barplot( ff.dfserver$taxa , names.arg = ff.dfserver$server, col = rainbow(5)
         ,xlab = "Servidores", ylab = "min-1" , main = "Taxa de falhas dos servidores em 2017"
         )
text (c(0.8,2,3.1,4.3,5.3),0.005,round(ff.dfserver$taxa,4),pos = 3)

 # protocolos
barplot( ff.dfprotocolo$taxa , names.arg = ff.dfprotocolo$protocolo, col = rainbow(5)
         ,xlab = "Protocolos", ylab = "min-1" , main = "Taxa de falhas dos protocolos em 2017"
         ,ylim = c(0,0.012)
)
text (c(0.8,2,3.1,4.3,5.4),0.005,round(ff.dfprotocolo$taxa,4),pos = 3)


# Função fiabilidade.
fiabilidade <- function(y_value,x_tempo) {
  retorno <- exp(-1 * y_value * x_tempo)
  return(retorno)
}


vetor_fiabilidade <- function(v_tempos, taxa){
  ret = c();
  for(i in v_tempos){
    ret = c(ret , fiabilidade(y_value = taxa, x_tempo = i) )
  }
            return(ret)
}


#Construcao do dataframe para as funcoes de fiabilidade dos servidores.

ffn = c(1:5)
fft = c(1:1000)
ff.server.curva = list();
ff.protocolo.curva = list();

# Obter os pontos das curvas.
for (i in ffn){
  ff.server.curva[[i]] = vetor_fiabilidade(v_tempos = fft, taxa = ff.dfserver$taxa[i])
  ff.protocolo.curva[[i]] = vetor_fiabilidade(v_tempos = fft, taxa = ff.dfprotocolo$taxa[i])
}



ff.cor = rainbow(5)

# Grafico:

plot(x = fft, y = ff.server.curva[[1]], type = 'l', col = ff.cor[1]
     ,xlab = 'minutos', ylab = 'R', main = "Fiabilidade dos servidores em 2017")
lines(x = fft , y = ff.server.curva[[2]] , col = ff.cor[2])
lines(x = fft , y = ff.server.curva[[3]] , col = ff.cor[3])
lines(x = fft , y = ff.server.curva[[4]] , col = ff.cor[4])
lines(x = fft , y = ff.server.curva[[5]] , col = ff.cor[5])
legend( "topright", legend = ff.nserver, col= ff.cor, pch=15)


plot(x = fft, y = ff.protocolo.curva[[1]], type = 'l', col = ff.cor[1]
     ,xlab = 'minutos', ylab = 'R', main = "Fiabilidade dos protocolos em 2017")
lines(x = fft , y = ff.protocolo.curva[[2]] , col = ff.cor[2])
lines(x = fft , y = ff.protocolo.curva[[3]] , col = ff.cor[3])
lines(x = fft , y = ff.protocolo.curva[[4]] , col = ff.cor[4])
lines(x = fft , y = ff.protocolo.curva[[5]] , col = ff.cor[5])
legend( "topright", legend = ff.nprotocolo, col= ff.cor, pch=15)



