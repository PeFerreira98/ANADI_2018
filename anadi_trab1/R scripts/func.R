
# ANADI Fiabilidade.


# fazer o import do ficheiro .csv output.csv

#Numero de sessoes simultaneas:

dados.T = data.frame(output)

length(dados.T$sessaoSimultaneaTotal)

v.sst.max = max(dados.T$sessaoSimultaneaTotal); v.sst.max
v.sst.min = min(dados.T$sessaoSimultaneaTotal); v.sst.min
v.sst.avg = mean(dados.T$sessaoSimultaneaTotal); v.sst.avg
v.sst.sd = sd(dados.T$sessaoSimultaneaTotal); v.sst.sd

v.sst.max = max(dados.T[]$sessaoSimultaneaTotal); v.sst.max
  
boxplot(dados.T$sessaoSimultaneaTotal)

#Apagar:
rm(d.ss)
                          # dataframe com apenas as colunas que interesam
d.ss <- data.frame(
  "server" = dados.T$server, 
  "protocolo" = dados.T$protocolo,
  "sessaoSimultaneaTotal" = dados.T$sessaoSimultaneaTotal
  , "sessaoSimultaneaServidor" = dados.T$sessaoSimultaneaServidor
  , "sessaoSimultaneaProtocolo" = dados.T$sessaoSimultaneaProtocolo
  ,"aaaamm" = dados.T$aaaaamm
  ) 

#unique(dados.T$server)
# vsrv11 vsrv17 vsrv8  vsrv16 vsrv10
#unique(dados.T$protocolo)
# SOFTETHER  PPTP       SSTP       OPENVPN_L2 OPENVPN_L3



# Vectores para o grafico de bigodes.

v.ss.total = d.ss$sessaoSimultaneaTotal         # Total

# extrair apenas para um servidor.
aux = d.ss[,1] == "vsrv11"
v.ss.vsrv11 = d.ss[aux,]$sessaoSimultaneaServidor

aux = d.ss[,1] == "vsrv17"
v.ss.vsrv17 = d.ss[aux,]$sessaoSimultaneaServidor

aux = d.ss[,1] == "vsrv8"
v.ss.vsrv8 = d.ss[aux,]$sessaoSimultaneaServidor

aux = d.ss[,1] == "vsrv16"
v.ss.vsrv16 = d.ss[aux,]$sessaoSimultaneaServidor

aux = d.ss[,1] == "vsrv10"
v.ss.vsrv10 = d.ss[aux,]$sessaoSimultaneaServidor

boxplot(v.ss.total)

#Lixo nao usar.
#boxplot(v.ss.vsrv11 , v.ss.vsrv17 , v.ss.vsrv8 , v.ss.vsrv16 , v.ss.vsrv10         ,main = "Sessões simultaneas por servidores",        xlab = "servidor",        ylab = "n"        )


boxplot(sessaoSimultaneaServidor ~ server , data = d.ss
        ,main = "Sessões simultaneas no mesmo servidor",
        xlab = "servidor",
        ylab = "n"
)

boxplot(sessaoSimultaneaProtocolo ~ protocolo , data = d.ss
        ,main = "Sessões simultaneas no mesmo protocolo",
        xlab = "servidor",
        ylab = "n"
) 


boxplot(sessaoSimultaneaProtocolo ~ protocolo , data = d.ss
        ,main = "Sessões simultaneas no mesmo protocolo",
        xlab = "protocolo",
        ylab = "n"
) 

v.ss.total = dados.T$sessaoSimultaneaTotal; v.ss.total
v.sst.min = min(dados.T$sessaoSimultaneaTotal); v.sst.min
v.sst.avg = mean(dados.T$sessaoSimultaneaTotal); v.sst.avg
v.sst.sd = sd(dados.T$sessaoSimultaneaTotal); v.sst.sd



# DURACAO DA SESSAO.

boxplot( dados.T$duracao
        ,main = "duração sessões",
        #xlab = "total",
        ylab = "min"
        , outline=FALSE
)




boxplot(duracao ~ server , data = dados.T
        ,main = "duração sessões por servidor",
        xlab = "servidor",
        ylab = "min"
        , outline=FALSE
) 

boxplot(duracao ~ protocolo , data = dados.T
        ,main = "duração sessões por protocolo",
        xlab = "protocolo",
        ylab = "min"
        , outline=FALSE
) 

    #funcao de densidade:
#hist(dados.T$duracao, breaks=100)
#plot(density(dados.T$duracao))
#density(dados.T$duracao)
#plot(density(dados.T$duracao, to = 1000))

#aux = dados.T[,1] == "vsrv11"
#aux2 = dados.T[aux,]$duracao
#plot(density(aux2, to = 1000))

#plot(density(dados.T[dados.T[,1] == "vsrv8",]$duracao, to = 500), xlab = "min", main = "Duração de sessões", col="red", sub = "1")
#lines(density(dados.T[dados.T[,1] == "vsrv10",]$duracao, to = 500), xlab = "min", main = "Duração de sessões", col="blue", sub = "1")


     # Ciclo for que itera os servidores e constroi um grafico de com a distribuicao da duracao
rm(dsfor)
dsfor <- data.frame("server" = unique(dados.T$server), "cor" = rainbow(5))
index = c(1:nrow(dsfor))

plot(density(dados.T$duracao, to = 500), xlab = "min", main = "Duração de sessões", col="red")
for(i in index){
  vserver = dsfor[i,"server"]
  vcor = dsfor[i,"cor"]
  lines(density(dados.T[dados.T[,1] == vserver,]$duracao, to = 500), xlab = "min", main = "Duração de sessões", col = vcor)
}
legend( "topright", legend = dsfor$server, col= dsfor$cor, pch=15)

# TEMPO ENTRE FALHAS CONSECUTIVAS

