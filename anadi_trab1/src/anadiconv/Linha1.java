/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package anadiconv;

import java.text.SimpleDateFormat;
import java.time.format.DateTimeFormatter;
import java.util.Date;

/**
 *
 * @author Hugo
 */
public class Linha1 implements Comparable<Linha1>{
/*    
vsrv11     SOFTETHER  2016-12-25 08:38 10:43 ( 125 min.)
vsrv8      PPTP       2016-12-25 11:43 13:04 (  81 min.)
vsrv17     SSTP       2016-12-25 12:55 13:57 (  62 min.)
vsrv16     PPTP       2016-12-25 14:11 14:15 (   4 min.)
vsrv16     SSTP       2016-12-25 15:00 15:04 (   4 min.)
vsrv17     PPTP       2016-12-25 15:31 15:36 (   5 min.)
*/
    
    public String server;
    public String protocolo;
    public String data;
    public String hora_inicio;
    public String hora_fim;
    public int duracao;
    public int tempo_inicio;
    public int tempo_fim;
    public int tempo_inicio_zero;
    public boolean falha = false;
    public int sessaoSimultaneaTotal; 
    public int sessaoSimultaneaServidor;
    public int sessaoSimultaneaProtocolo;
    public int sessaoSimultaneaServidorProtocolo;
    
    public Date dataInicioAux;
    
    public int mtbf_total;
    public int mtbf_servidor;
    public int mtbf_protocolo;
    public int mtbf_servidor_protocolo;
    
    static public int tempoZero;
    
    public String AAAAMM;
    public String AAAA;
    
    
    public static void setTempoZero(){
        //2016-12-25
                SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm");
        try
        {
            Date date = simpleDateFormat.parse("2016-12-25 00:00");
            
            
            long minutos;
            minutos = date.getTime() / (1000 * 60);
            Linha1.tempoZero = (int) minutos;
            System.out.println("Tempo zero processasdo com sucesso.");
        }
        catch(Exception e){
            System.out.println("Exception "+e);
        }
    }
    
    
    public void processTempo(){
        //2016-12-25
                String s = this.data + " " + hora_inicio; //03/24/2013 21:54";
        //SimpleDateFormat simpleDateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm");
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm");
        try
        {
            Date date = simpleDateFormat.parse(s);
            this.dataInicioAux = date;
            
            long minutos;
            minutos = date.getTime() / (1000 * 60);
            this.tempo_inicio = (int) minutos;
            this.tempo_fim = this.tempo_inicio + this.duracao;
            this.tempo_inicio_zero = this.tempo_inicio - tempoZero;         // Tempso a começar de zero. +-
            
            if(this.duracao <= 1) this.falha = true;
            
            // coluna do MesAno e do Ano apenas.
            int ano = date.getYear()+ 1900;
            int mes = date.getMonth()+1;
            int anomes = (ano * 100) + mes;
            this.AAAA = String.valueOf(ano);
            
            this.AAAAMM = String.valueOf(anomes);
            
            
            
            //System.out.println("date : "+simpleDateFormat.format(date) + " - " + this.toString());
            


            //System.out.println("Long: " + minutos + " int: " + this.tempo_inicio);
            
            
        }
        catch (Exception ex)
        {
            System.out.println("Exception "+ex);
        }
    }
            
    public String toString(){
        String sep = ";";
        //return Boolean.toString(falha);
        //falha?"True":"False"
        return server + sep + protocolo + sep + data + sep + hora_inicio + sep + hora_fim + sep + duracao 
                + sep + falha + sep + sessaoSimultaneaTotal + sep + sessaoSimultaneaServidor + sep + sessaoSimultaneaProtocolo + sep + sessaoSimultaneaServidorProtocolo
                + sep + tempo_inicio + sep + tempo_fim +sep + tempo_inicio_zero
                + sep + mtbf_total
                + sep + mtbf_servidor
                + sep + mtbf_protocolo
                + sep + mtbf_servidor_protocolo
                + sep + this.AAAAMM + sep + this.AAAA
                ;
    }
    
    public static String toStringHeader(){
        String sep = ";";
        return "server" + sep + "protocolo" + sep + "data" + sep + "hora_inicio" + sep + "hora_fim" + sep + "duracao" 
                + sep + "falha" + sep + "sessaoSimultaneaTotal" + sep + "sessaoSimultaneaServidor" + sep + "sessaoSimultaneaProtocolo" + sep + "sessaoSimultaneaServidorProtocolo"
                + sep + "tempo_inicio" + sep + "tempo_fim" + sep + "tempo_inicio_zero"
                + sep + "mtbf_total"
                + sep + "mtbf_servidor"
                + sep + "mtbf_protocolo"
                + sep + "mtbf_servidor_protocolo"
                
                + sep + "aaaaamm" + sep + "aaaa"
                ;
    }
    
    public String dataInicioToString(){
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm");        
        return simpleDateFormat.format(this.dataInicioAux);
    }
    
        
    public void checkSameSession(Linha1 other){
        //Verificar numero de sessoes simultaneas
        if (this == other) return; //ignoramos o mesmo objecto.
        if (this.falha || other.falha) return; // ignoramos as falhas.
        if (   // se o inicio ou fim de algum deles occorrer dentro do intervalo do outro...
                (this.tempo_inicio >= other.tempo_inicio && this.tempo_inicio <= other.tempo_fim) ||
                (this.tempo_fim >= other.tempo_inicio && this.tempo_fim <= other.tempo_fim) ||
                (other.tempo_inicio >= this.tempo_inicio && other.tempo_inicio <= this.tempo_fim) ||
                (other.tempo_fim >= this.tempo_inicio && other.tempo_fim <= this.tempo_fim)                
            ){
            // Temos sessao simultanea, portanto vamos marcar
            this.sessaoSimultaneaTotal++;
            if (this.server.equals(other.server)) this.sessaoSimultaneaServidor++;
            if (this.protocolo.equals(other.protocolo)) this.sessaoSimultaneaProtocolo++;
            if (
                    this.server.equals(other.server) && 
                    this.protocolo.equals(other.protocolo)
                ) {
                this.sessaoSimultaneaServidorProtocolo++;
            }            
        }
    }

    @Override
    public int compareTo(Linha1 o) {
        return this.tempo_inicio - o.tempo_inicio;
    }
    
    
}
