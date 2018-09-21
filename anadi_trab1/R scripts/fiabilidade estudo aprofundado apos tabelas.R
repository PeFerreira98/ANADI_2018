

# Analise de fiabilidade direcionada apos tabelas.

#Facilmente salta a vista que determinadas combinações não existem. Por exemplo o servidor "vsrv10" não possui o protocolo "OPENVPN_L3". Ainda mais visível nas restantes tabelas. Outro facto a constatar é que o protocolo "OPENVPN_L3" tem uma elevada taxa de falhas. Assim podemos dirigir o nosso estudo para direções mais relevantes.

# Ver combinações que nao existem.
# server vs protocolo em 2017
# por mes.
# 

# fazer o import do ficheiro .csv output.txt


# 111111111111111111111111111111111111111111111111111111111111111111111111111
# 1) Combinacoes em 2017

alldata = data.frame(output)
dados1 = alldata[alldata$aaaa == '2017',]

d1.meses = unique(dados1$aaaaamm)
d1.nserver = sort(unique(dados1$server))
d1.nprotocolo = sort(unique(dados1$protocolo))

d1.mes = c();
d1.s = c();
d1.p = c();
d1.nf = c(); # n falhas
d1.ns = c(); # n sessoes
d1.taxa = c(); # n falhas / falhas + sessoes


for(m1 in d1.meses){
  for(s1 in d1.nserver){
    for(p1 in d1.nprotocolo){
      d1.mes = c( d1.mes , m1);
      d1.s = c( d1.s, s1);
      d1.p = c( d1.p , p1);
      aux = dados1$falha[dados1$server == s1 & dados1$protocolo == p1 & dados1$aaaaamm == m1]
      d1.nf = c(d1.nf , sum(aux == 'true') )
      d1.ns = c(d1.ns , sum(aux == 'false') )
    }
  }
}

d1.tabela = data.frame( d1.mes,d1.s,d1.p,d1.nf,d1.ns)
names(d1.tabela) = c("mes","servidor","protocolo","nf","ns")
d1.tabela2 = d1.tabela[d1.tabela$ns == 0,]
View(d1.tabela2)


 # Como a tabela e muito longa, vamos mudar e fazer uma tabela mais quadrada.
#-----------------------------------------

dados2 = alldata[alldata$aaaa == '2017',]


d2.nserver = sort(unique(dados2$server))
d2.nprotocolo = sort(unique(dados2$protocolo))



d2.s = c();
d2.p = c();

d2.s01 = c(); d2.s02 = c(); d2.s03 = c();
d2.s04 = c(); d2.s05 = c(); d2.s06 = c();
d2.s07 = c(); d2.s08 = c(); d2.s09 = c();
d2.s10 = c(); d2.s11 = c(); d2.s12 = c();

d2.f01 = c(); d2.f02 = c(); d2.f03 = c();
d2.f04 = c(); d2.f05 = c(); d2.f06 = c();
d2.f07 = c(); d2.f08 = c(); d2.f09 = c();
d2.f10 = c(); d2.f11 = c(); d2.f12 = c();



  for(s2 in d2.nserver){
    for(p2 in d2.nprotocolo){
      d2.s = c( d2.s, s2);
      d2.p = c( d2.p , p2);
      aux2 = dados2$falha[dados2$server == s2 & dados2$protocolo == p2 & dados2$aaaaamm == '201701']
      n2 = sum(aux2 == 'false')
      d2.s01 = c( d2.s01 , n2)
      n2 = sum(aux2 == 'true')
      d2.f01 = c(d2.f01 , n2)
      
      aux2 = dados2$falha[dados2$server == s2 & dados2$protocolo == p2 & dados2$aaaaamm == '201702']
      n2 = sum(aux2 == 'false')
      d2.s02 = c( d2.s02 , n2)
      n2 = sum(aux2 == 'true')
      d2.f02 = c(d2.f02 , n2)
      
      aux2 = dados2$falha[dados2$server == s2 & dados2$protocolo == p2 & dados2$aaaaamm == '201703']
      n2 = sum(aux2 == 'false')
      d2.s03 = c( d2.s03 , n2)
      n2 = sum(aux2 == 'true')
      d2.f03 = c(d2.f03 , n2)
      
      aux2 = dados2$falha[dados2$server == s2 & dados2$protocolo == p2 & dados2$aaaaamm == '201704']
      n2 = sum(aux2 == 'false')
      d2.s04 = c( d2.s04 , n2)
      n2 = sum(aux2 == 'true')
      d2.f04 = c(d2.f04 , n2)
      
      aux2 = dados2$falha[dados2$server == s2 & dados2$protocolo == p2 & dados2$aaaaamm == '201705']
      n2 = sum(aux2 == 'false')
      d2.s05 = c( d2.s05 , n2)
      n2 = sum(aux2 == 'true')
      d2.f05 = c(d2.f05 , n2)
      
      aux2 = dados2$falha[dados2$server == s2 & dados2$protocolo == p2 & dados2$aaaaamm == '201706']
      n2 = sum(aux2 == 'false')
      d2.s06 = c( d2.s06 , n2)
      n2 = sum(aux2 == 'true')
      d2.f06 = c(d2.f06 , n2)
      
      aux2 = dados2$falha[dados2$server == s2 & dados2$protocolo == p2 & dados2$aaaaamm == '201707']
      n2 = sum(aux2 == 'false')
      d2.s07 = c( d2.s07 , n2)
      n2 = sum(aux2 == 'true')
      d2.f07 = c(d2.f07 , n2)
      
      aux2 = dados2$falha[dados2$server == s2 & dados2$protocolo == p2 & dados2$aaaaamm == '201708']
      n2 = sum(aux2 == 'false')
      d2.s08 = c( d2.s08 , n2)
      n2 = sum(aux2 == 'true')
      d2.f08 = c(d2.f08 , n2)
      
      aux2 = dados2$falha[dados2$server == s2 & dados2$protocolo == p2 & dados2$aaaaamm == '201709']
      n2 = sum(aux2 == 'false')
      d2.s09 = c( d2.s09 , n2)
      n2 = sum(aux2 == 'true')
      d2.f09 = c(d2.f09 , n2)
      
      aux2 = dados2$falha[dados2$server == s2 & dados2$protocolo == p2 & dados2$aaaaamm == '201710']
      n2 = sum(aux2 == 'false')
      d2.s10 = c( d2.s10 , n2)
      n2 = sum(aux2 == 'true')
      d2.f10 = c(d2.f10 , n2)
      
      aux2 = dados2$falha[dados2$server == s2 & dados2$protocolo == p2 & dados2$aaaaamm == '201711']
      n2 = sum(aux2 == 'false')
      d2.s11 = c( d2.s11 , n2)
      n2 = sum(aux2 == 'true')
      d2.f11 = c(d2.f11 , n2)
      
      aux2 = dados2$falha[dados2$server == s2 & dados2$protocolo == p2 & dados2$aaaaamm == '201712']
      n2 = sum(aux2 == 'false')
      d2.s12 = c( d2.s12 , n2)
      n2 = sum(aux2 == 'true')
      d2.f12 = c(d2.f12 , n2)
    }
  }


d2.tabela.s = data.frame( d2.s, d2.p
                        ,d2.s01, d2.s02, d2.s03
                        ,d2.s04, d2.s05, d2.s06
                        ,d2.s07, d2.s08, d2.s09
                        ,d2.s10, d2.s11, d2.s12
)

names(d2.tabela.s) = c("servidor","protocolo"
                     ,"s01","s02","s03"
                     ,"s04","s05","s06"
                     ,"s07","s08","s09"
                     ,"s10","s11","s12"
                     )

d2.nomes = c("servidor","protocolo", month.abb )

names(d2.tabela.s) = d2.nomes

d2.tabela.s
write.table(d2.tabela.s , file="analise tabela sesoes em 2017.csv", sep = ";" , row.names = FALSE)


# 33333333333333333333333333333333333333333333333333333333333333333333333333333333333

# Vamos mostrar graficos do numero se sessões por protocolo e servidor.


dados3 = alldata[alldata$aaaa == '2017',]


d3.nserver = sort(unique(dados3$server))
d3.nprotocolo = sort(unique(dados3$protocolo))


d3.server.f = c();
d3.server.s = c();
for(s3 in d3.nserver){
  aux3 = dados3$falha[dados3$server == s3]
  n3 = sum(aux3 == 'false')
  d3.server.s = c(d3.server.s , n3 )
  n3 = sum(aux3 == 'true')
  d3.server.f = c(d3.server.f , n3 )
  
}

barplot(d3.server.s, names.arg = d3.nserver)

d3.df = data.frame( d3.nserver, d3.server.s, d3.server.f)
d3.nomes = c( "Servidor", "nsessao", "nfalha")
names(d3.df) = d3.nomes

#library(ggplot2)
#p<-ggplot(data=d3.df, aes(x=d3.df$Servidor, y=d3.df$`n Sessão`)) +
#  geom_bar(stat="identity", fill= rainbow(5))
#p + ggplot(data=d3.df, aes(x=d3.df$Servidor, y=d3.df$`n Falha`))



# https://plot.ly/r/bar-charts/
#install.packages("plotly")
library(plotly)
p <- plot_ly(d3.df, x = d3.df$Servidor, y = d3.df$nsessao, type = 'bar', name = 'sessões') %>%
  add_trace(y = d3.df$nfalha, name = 'falhas') %>%
  layout(
    title = "Sessões em 2017 por Servidor"
    ,yaxis = list(title = 'Count')
    , xaxis = list(title = 'Servidores')
    , barmode = 'stack')
p
 
                             # para cada protocolo
d3.protocolo.s = c();
d3.protocolo.f = c();
for(p3 in d3.nprotocolo){
  aux3 = dados3$falha[dados3$protocolo == p3]
  n3 = sum(aux3 == 'false')
  d3.protocolo.s = c(d3.protocolo.s , n3 )
  n3 = sum(aux3 == 'true')
  d3.protocolo.f = c(d3.protocolo.f , n3 )
  
}  

d3.df2 = data.frame(d3.nprotocolo, d3.protocolo.s, d3.protocolo.f)

library(plotly)
p2 <- plot_ly(d3.df2, x = d3.df2$d3.nprotocolo, y = d3.df2$d3.protocolo.s, type = 'bar', name = 'sessões') %>%
  add_trace(y = d3.df2$d3.protocolo.f, name = 'falhas') %>%
  layout(
    title = "Sessões em 2017 por Protocolo"
    ,yaxis = list(title = 'Count')
    , xaxis = list(title = 'Protocolo')
    , barmode = 'stack')
p2

