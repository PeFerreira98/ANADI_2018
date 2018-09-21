
# ANADI Fiabilidade.

#TABELAS

# fazer o import do ficheiro .csv output.csv



dados = data.frame(output)
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

# Funcao para escrever a tabela para csv.
rm(escrever_tabela)
escrever_tabela <- function(nome_ficheiro, tabela) {
write.table(tabela , file=nome_ficheiro, sep = ";" , row.names = FALSE)
}






# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# FUNCAO PARA CRIAR A TABELA:
# Alterar para correr como funcao de funcao.


rm(criar_tabela)
criar_tabela <- function(dados) {
  


nserver = sort(unique(dados$server));nserver
nprotocolo = sort(unique(dados$protocolo));nprotocolo





# START
# 0 Criação dos vectores da tabela/dataframe

rm(v.server , v.protocolo)
v.server = c(); v.protocolo = c();

rm(v.dur.max , v.dur.min , v.dur.mean , v.dur.sd );   # Apagar tudo.
v.dur.max = c(); v.dur.min = c(); v.dur.mean = c(); v.dur.sd = c();
v.dur.max; v.dur.min; v.dur.mean; v.dur.sd;

rm(v.ns.max , v.ns.min , v.ns.mean , v.ns.sd )
v.ns.max = c(); v.ns.min = c(); v.ns.mean = c(); v.ns.sd = c();

rm(v.mtbf.max , v.mtbf.min , v.mtbf.mean , v.mtbf.sd );
v.mtbf.max = c(); v.mtbf.min = c(); v.mtbf.mean = c(); v.mtbf.sd = c();

rm(v.tf)
v.tf = c();






# 1 Conjunto de todos os servidores e protocolos.
 rm(todos);todos = "Todos"
 aux = todos; v.server <- c(v.server , aux)
 aux = todos; v.protocolo <- c(v.protocolo , aux)

 auxd = dados$duracao[dados$falha == "false"];
 

 aux = max(auxd); v.dur.max <- c(v.dur.max , aux)
 aux = min(auxd); v.dur.min <- c(v.dur.min , aux)
 aux = round(mean(auxd), digits = 2); v.dur.mean <- c(v.dur.mean , aux)
 aux = round(sd(auxd),digits = 2); v.dur.sd <- c(v.dur.sd , aux)

 auxd = dados$sessaoSimultaneaTotal;
 
 aux = max(auxd); v.ns.max <- c(v.ns.max, aux);
 aux = min(auxd); v.ns.min <- c(v.ns.min, aux);
 aux = round(mean(auxd), digits = 2); v.ns.mean <- c(v.ns.mean, aux);
 aux = round(sd(auxd), digits = 2); v.ns.sd <- c(v.ns.sd, aux);

 #auxd = dados$mtbf_total;
 auxd = dados$mtbf_total[dados$falha == "true"];
 auxd = head( auxd, -1)
 
 aux = max(auxd); v.mtbf.max <- c(v.mtbf.max , aux);
 aux = min(auxd); v.mtbf.min <- c(v.mtbf.min , aux);
 aux = round(mean(auxd), digits = 2); v.mtbf.mean <- c(v.mtbf.mean , aux);
 aux = round(sd(auxd), digits = 2); v.mtbf.sd <- c(v.mtbf.sd , aux);
 
 auxd = dados$falha;
 
 aux = round(sum(auxd == "true") / length(auxd), digits = 2);
 
 v.tf <- c(v.tf , aux);
 
 
 
 
# 2 Para cada Servidor

for( i in nserver){
  
  aux = i; v.server <- c(v.server , aux)
  aux = todos; v.protocolo <- c(v.protocolo , aux)
  
  
  auxd = dados$duracao[dados$server == i & dados$falha == "false"]   # select dos dados
  
  aux = max(auxd); v.dur.max <- c(v.dur.max , aux)
  aux = min(auxd); v.dur.min <- c(v.dur.min , aux)
  aux = round(mean(auxd), digits = 2); v.dur.mean <- c(v.dur.mean , aux)
  aux = round(sd(auxd),digits = 2); v.dur.sd <- c(v.dur.sd , aux)
  
  
  auxd = dados[dados[,1] == i,]$sessaoSimultaneaServidor   # select dos dados
  
  aux = max(auxd); v.ns.max <- c(v.ns.max, aux);
  aux = min(auxd); v.ns.min <- c(v.ns.min, aux);
  aux = round(mean(auxd), digits = 2); v.ns.mean <- c(v.ns.mean, aux);
  aux = round(sd(auxd), digits = 2); v.ns.sd <- c(v.ns.sd, aux);
  
  #auxd = dados[dados[,1] == i,]$mtbf_servidor;
  auxd = dados$mtbf_servidor[dados$server == i & dados$falha == "true"];
  auxd = head( auxd, -1)
  
  aux = max(auxd); v.mtbf.max <- c(v.mtbf.max , aux);
  aux = min(auxd); v.mtbf.min <- c(v.mtbf.min , aux);
  aux = round(mean(auxd), digits = 2); v.mtbf.mean <- c(v.mtbf.mean , aux);
  aux = round(sd(auxd), digits = 2); v.mtbf.sd <- c(v.mtbf.sd , aux);
  
  
  #auxd = dados[dados[,1] == i,]$falha;
  auxd = dados$falha[dados$server == i];
  
  aux = round(sum(auxd == "true") / length(auxd), digits = 2);
  
  v.tf <- c(v.tf , aux);
  
}

 # 3 No conjuto de servidores para cada protocolo
 
 for( i in nprotocolo){
   
   aux = todos; v.server <- c(v.server , aux)
   aux = i; v.protocolo <- c(v.protocolo , aux)
   
   
   auxd = dados$duracao[ dados$protocolo == i & dados$falha == "false"]   # select dos dados
   
   aux = max(auxd); v.dur.max <- c(v.dur.max , aux)
   aux = min(auxd); v.dur.min <- c(v.dur.min , aux)
   aux = round(mean(auxd), digits = 2); v.dur.mean <- c(v.dur.mean , aux)
   aux = round(sd(auxd),digits = 2); v.dur.sd <- c(v.dur.sd , aux)
   
   
   auxd = dados$sessaoSimultaneaProtocolo[dados$protocolo == i]   # select dos dados
   
   aux = max(auxd); v.ns.max <- c(v.ns.max, aux);
   aux = min(auxd); v.ns.min <- c(v.ns.min, aux);
   aux = round(mean(auxd), digits = 2); v.ns.mean <- c(v.ns.mean, aux);
   aux = round(sd(auxd), digits = 2); v.ns.sd <- c(v.ns.sd, aux);
   
   
   auxd = dados$mtbf_protocolo[dados$protocolo == i & dados$falha == "true"];
   auxd = head( auxd, -1)
   
   aux = max(auxd); v.mtbf.max <- c(v.mtbf.max , aux);
   aux = min(auxd); v.mtbf.min <- c(v.mtbf.min , aux);
   aux = round(mean(auxd), digits = 2); v.mtbf.mean <- c(v.mtbf.mean , aux);
   aux = round(sd(auxd), digits = 2); v.mtbf.sd <- c(v.mtbf.sd , aux);
   
   
   auxd = dados$falha[dados$protocolo == i];
   
   aux = round(sum(auxd == "true") / length(auxd), digits = 2);
   v.tf <- c(v.tf , aux);
   
 }
 
 # Para cada servidor, para cada tipo de protocolo
 rm(i)
 for( s in nserver){
   for (p in nprotocolo){
     
     aux = s; v.server <- c(v.server , aux)
     aux = p; v.protocolo <- c(v.protocolo , aux)
     
     
     auxd = dados$duracao[ dados$server == s & dados$protocolo == p & dados$falha == "false"]   # select dos dados
     
     aux = max(auxd); v.dur.max <- c(v.dur.max , aux)
     aux = min(auxd); v.dur.min <- c(v.dur.min , aux)
     aux = round(mean(auxd), digits = 2); v.dur.mean <- c(v.dur.mean , aux)
     aux = round(sd(auxd),digits = 2); v.dur.sd <- c(v.dur.sd , aux)
     
     
     auxd = dados$sessaoSimultaneaServidorProtocolo[dados$protocolo == p & dados$server == s]   # select dos dados
     
     aux = max(auxd); v.ns.max <- c(v.ns.max, aux);
     aux = min(auxd); v.ns.min <- c(v.ns.min, aux);
     aux = round(mean(auxd), digits = 2); v.ns.mean <- c(v.ns.mean, aux);
     aux = round(sd(auxd), digits = 2); v.ns.sd <- c(v.ns.sd, aux);
     
     
     auxd = dados$mtbf_servidor_protocolo[dados$protocolo == p & dados$server == s & dados$falha == "true"];
     auxd = head( auxd, -1)

     aux = max(auxd); v.mtbf.max <- c(v.mtbf.max , aux);
     aux = min(auxd); v.mtbf.min <- c(v.mtbf.min , aux);
     aux = round(mean(auxd), digits = 2); v.mtbf.mean <- c(v.mtbf.mean , aux);
     aux = round(sd(auxd), digits = 2); v.mtbf.sd <- c(v.mtbf.sd , aux);
     
     
     auxd = dados$falha[dados$server == s & dados$protocolo == p];
     
     aux = round(sum(auxd == "true") / length(auxd), digits = 2);
     v.tf <- c(v.tf , aux);
     
     
     
   }
 }
 
 
 

# Ciclo for para server vs protocolo.


# Construção da tabela final.

rm(tabela)
tabela = data.frame(
  v.server , v.protocolo ,
  v.dur.max , v.dur.min , v.dur.mean , v.dur.sd 
  , v.ns.max , v.ns.min , v.ns.mean , v.ns.sd
  , v.mtbf.max , v.mtbf.min , v.mtbf.mean , v.mtbf.sd 
  , v.tf
)
tabela  #show

return (tabela)
}

# FIM da funcao de criar tabela.
# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


# O seginte SCRIPT cria os ficheiros csv para todas as datas peretendidas e coloca em:
# nos meus documentos do utilizador do windows.

#CORRER:

# Ano de 2017
tempo_inicio = as.Date("2017-01-01")
tempo_fim = as.Date("2017-12-31")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)
#write.table(tabela , file="2017.csv", sep = ";" , row.names = FALSE)


# Total
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2016-01-01")
tempo_fim = as.Date("2018-12-31")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "total.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)

# Janeiro
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2017-01-01")
tempo_fim = as.Date("2017-01-31")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017 janeiro.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)


# Fevereiro
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2017-02-01")
tempo_fim = as.Date("2017-02-28")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017 fevereiro.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)

# Marco
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2017-03-01")
tempo_fim = as.Date("2017-03-31")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017 marco.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)


# Abril
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2017-04-01")
tempo_fim = as.Date("2017-04-30")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017 Abril.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)
warnings()

# Maio
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2017-05-01")
tempo_fim = as.Date("2017-05-31")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017 Maio.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)
warnings()

# Junho
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2017-06-01")
tempo_fim = as.Date("2017-06-30")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017 Junho.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)
warnings()

# Julho
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2017-07-01")
tempo_fim = as.Date("2017-07-31")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017 Julho.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)
warnings()


# Agosto
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2017-08-01")
tempo_fim = as.Date("2017-08-31")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017 Agosto.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)
warnings()

# Setembro
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2017-09-01")
tempo_fim = as.Date("2017-09-30")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017 Setembro.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)
warnings()

# Outubro
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2017-10-01")
tempo_fim = as.Date("2017-10-31")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017 Outubro.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)
warnings()

# Novembro
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2017-11-01")
tempo_fim = as.Date("2017-11-30")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017 Novembro.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)
warnings()


# Dezembro
cat("\014")  #Limpa consola.
tempo_inicio = as.Date("2017-12-01")
tempo_fim = as.Date("2017-12-31")
dados = restricao_temporal(tempo_inicio, tempo_fim)
tabela = criar_tabela(dados = dados)
tabela
ficheiro = "2017 Dezembro.csv"
escrever_tabela(nome_ficheiro = ficheiro, tabela = tabela)
warnings()



#LIXO: NAO CORRER DAQUI PARA BAIXO.

# Output de DADOS para ficheiro:
write.table(tabela , file="tab total.csv", sep = ";" , row.names = FALSE)

# DATAS:
dados = data.frame(output)
dados <- dados[dados$aaaa == 2017,]
write.table(tabela , file="tab 2017.csv", sep = ";" , row.names = FALSE)

write.csv(tabela , file="tab 2017.csv", sep = ";" , row.names = FALSE)


tempo_inicio = as.Date( as.character("2017-01-01"), "%Y-%m-%d")
tempo_fim = as.Date( as.character("2017-12-31"), "%Y-%m-%d")

# COMBINACAO SEMDADOS
#s = "vsrv10"
#p = "OPENVPN_L3"
#auxd = dados$duracao[ dados$server == s & dados$protocolo == p]
# auxd;
