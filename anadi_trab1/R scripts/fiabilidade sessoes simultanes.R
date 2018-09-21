# sessoes simultaneas

# EXTREMAMENTE LENTO

# fazer o import do ficheiro csv output.txt
alldata = data.frame(output)

## read in date/time info in format 'm/d/y h:m:s'
#dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
#times <- c("23:03:20", "22:29:56", "01:03:30", "18:21:03", "16:56:26")
#x <- paste(dates, times)
#strptime(x, "%m/%d/%y %H:%M:%S")



ss.dates <- alldata$data
ss.hora0 <- alldata$hora_inicio
aux <- paste(ss.dates, ss.hora0)
# strftime - time to char, strptime - char to time.
# strftime(ff.t0, format = "%Y-%m-%d %H:%M", tz = "UTC", usetz = FALSE)  
ss.t0 <- strptime(aux, format = "%Y-%m-%d %H:%M")  # tempo de inicio.
ss.t1 = ss.t0 + (alldata$duracao * 60)   #Criacao do tempo de fim.

ss.data0 = alldata
ss.data0$t0 = ss.t0
ss.data0$t1 = ss.t1

# Tempo na semana de entrega de ASIST em 2017 a 10 de Dezembro domingo.

# Limitar o tempo: PODE SER ALTERADO.
tempo_inicio = strptime("2017-12-04 00:00", format = "%Y-%m-%d %H:%M")
tempo_final = strptime("2017-12-11 00:00", format = "%Y-%m-%d %H:%M")
ss.duracao = difftime(tempo_final, tempo_inicio,units="mins")
duracao = as.numeric(ss.duracao, units = "mins")

# Filtrar para apenas o periudo temporal.
ss.data1 = ss.data0[ss.data0$t0 >= tempo_inicio & ss.data0$t1 < tempo_final,]

# criar vector datas.

ss.v_minutos = c(0:duracao)
ss.datas = seq(tempo_inicio, tempo_final, by="mins")  # sequencia de datas min a minuto.
ss.nsessoes = c();

# construir o vector de numero de sessoes por minuto. # EXTREMAMENTE LENTO
for(ssi in ss.datas){
  aux2 = ss.data1$falha[ss.data1$falha == 'false' & ss.data1$t0 <= ssi & ss.data1$t1 >= ssi]
  ns = sum(aux2 == 'false')
  ss.nsessoes = c(ss.nsessoes , ns)
}

ss.df = data.frame(ss.v_minutos, ss.datas , ss.nsessoes)
names(ss.df) = c("min", "data","n")


plot(ss.df$n ~ss.df$min,type = 'l', col = 'blue'
     ,xlab = 'minutos', ylab = 'n sessoes', main = "Sessões simultaneas de 04 a 11 de Dezembro de 2017" )

plot(ss.df$n ~ss.df$data,type = 'l', col = 'blue'
     ,xlab = 'tempo', ylab = 'n sessoes'
     , main = "Sessões simultaneas de 04 a 11 de Dezembro de 2017" 
     ,xaxt = "n"
     )

# Add an axis to the plot 
axis(side = 1, 
     at = seq(tempo_inicio, tempo_final, by="day"), 
     labels = seq(tempo_inicio, tempo_final, by="day"),
     tck=-.05)

# Reduzir para 2 dia.

tg1 = strptime("2017-12-04 00:00", format = "%Y-%m-%d %H:%M")
tg2 = strptime("2017-12-06 00:00", format = "%Y-%m-%d %H:%M")



ss.df2 = ss.df[ss.df$data >= tg1 & ss.df$data <= tg2,]


plot(ss.df2$n ~ss.df2$data,type = 'l', col = 'green'
     ,xlab = 'hora', ylab = 'n sessoes'
     , main = "Sessões simultaneas de 04 a 6 de Dezembro de 2017" 
     ,xaxt = "n"
)

# Add an axis to the plot 
v_seq = seq(tg1, tg2, by="3 hour")
v_seq2 = c( 0,3,6,9,12,15,18,21,0,3,6,9,12,15,18,21,0)
v_seq2 = paste0( v_seq2,':00', collapse = NULL);v_seq2
axis(side = 1, 
     at = v_seq, 
     labels = v_seq2,
     tck=-.05)

#valos medios.

ss.min = min(ss.df$n); ss.min
ss.max = max(ss.df$n); ss.max
ss.avg = mean(ss.df$n); ss.avg
ss.sd = sd(ss.df$n); ss.sd


mean(alldata$duracao[alldata$falha == 'false'])



