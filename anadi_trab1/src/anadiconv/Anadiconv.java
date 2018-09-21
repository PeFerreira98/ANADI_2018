/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package anadiconv;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author Hugo
 */
public class Anadiconv {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        
        //String local = "D:/testc/lanadi/vpnsessions.txt";
        String local = "vpnsessions.txt";
        try{transform(local);}
        catch(Exception e){
            System.out.println(e.getMessage());
        }
    }
    
    public static void transform(String ficheiro) throws Exception{
        
        Linha1.setTempoZero();
        
        File file = new File(ficheiro);
        BufferedReader reader = null;
        
        //^(vsrv[0-9]{1,2})\s+([a-zA-Z0-9_]+)\s+([0-9]{4}-[0-9]{2}-[0-9]{2})\s+([0-9]{2}:[0-9]{2})\s+([0-9]{2}:[0-9]{2})\s+\(\s+([0-9]+)\s+min.\)\s*$
        //Pattern pattern = Pattern.compile("^[\\s]*no\\(([a-zA-Z0-9\\s]+),([0-9]+\\.[0-9]+),(-?[0-9]+\\.[0-9]+)\\)\\.");
        //Pattern pattern2 = Pattern.compile("^[\\s]*ligacao\\(([a-zA-Z0-9\\s]+),([0-9]+\\.[0-9]+),(-?[0-9]+\\.[0-9]+)#([a-zA-Z0-9\\s]+),([0-9]+\\.[0-9]+),(-?[0-9]+\\.[0-9]+)#\\)\\.[\\s]*");
        //Pattern plinha = Pattern.compile("^(vsrv[0-9]{1,2})\\s+([a-zA-Z0-9_]+)\\s+([0-9]{4}-[0-9]{2}-[0-9]{2})\\s+([0-9]{2}:[0-9]{2})\\s+([0-9]{2}:[0-9]{2})\\s+\\(\\s+([0-9]+)\\s+min.\\)\\s*$");
        Pattern plinha = Pattern.compile("^(vsrv[0-9]{1,2})\\s+([a-zA-Z0-9_]+)\\s+([0-9]{4}-[0-9]{2}-[0-9]{2})\\s+([0-9]{2}:[0-9]{2})\\s+([0-9]{2}:[0-9]{2})\\s+\\(\\s*([0-9]+)\\s+min.\\)?\\s*$");

        // Grupos
        // 1 servidor
        // 2 protocolo
        // 3 data
        // inicio
        // fim
        // duracao

        reader = new BufferedReader(new FileReader(file));
        String text = null;
        List<String> lista2 = new ArrayList<String>();
        List<Linha1> lista = new ArrayList<Linha1>();

        int nmax = 66383;
        int nlidas = 0;
        int n = 0;
        while ((text = reader.readLine()) != null) {
           
            Matcher m = plinha.matcher(text);
            n++;
            if(m.matches()){
                String sservidor = m.group(1);
                String sprotocolo = m.group(2);
                String sdata = m.group(3);
                String sinicio = m.group(4);
                String sfim = m.group(5);
                String sduracao = m.group(6);
                
                int duracao = Integer.parseInt(sduracao);
                
                Linha1 linha= new Linha1();
                linha.server = sservidor;
                linha.protocolo = sprotocolo;
                linha.data = sdata;
                linha.hora_inicio = sinicio;
                linha.hora_fim = sfim;
                linha.duracao = duracao;
                lista.add(linha);
                nlidas++;
            }
            else{
                System.out.println("Linha: " + n + " Falha: " + text);
            }
        }
        
        for (Linha1 l : lista){
            l.processTempo();
        }
        
        Collections.sort(lista);   // JA TEMOS A LISTA ORDENADA POR TEMPO DE INICIO
        
        System.out.println("Processando falhas ");// Processa
        
        for (Linha1 l : lista){
            
            boolean MTBF_total = false;
            boolean MTBF_servidor = false;
            boolean MTBF_protocolo = false;
            boolean MTBF_protocolo_servidor = false;
            
            for (Linha1 lo : lista){
                l.checkSameSession(lo);
                
                //Calculos MTBF
                if( l.falha && lo.falha && l.tempo_inicio < lo.tempo_inicio ){
                    
                    //MTBF Total:
                    if(!MTBF_total){
                        l.mtbf_total = lo.tempo_inicio - l.tempo_inicio;  // intervalo entre falha.
                        MTBF_total = true; 
                    }
                    
                    //MTBF Servidor:
                    if(!MTBF_servidor && l.server.equals(lo.server)){
                        l.mtbf_servidor = lo.tempo_inicio - l.tempo_inicio;  // intervalo entre falha.
                        MTBF_servidor = true; 
                    }
                    
                    //MTBF Protocolo:
                    if(!MTBF_protocolo && l.protocolo.equals(lo.protocolo)){
                        l.mtbf_protocolo = lo.tempo_inicio - l.tempo_inicio;  // intervalo entre falha.
                        MTBF_protocolo = true; 
                    }
                    
                    //MTBF Protocolo & SERVIDOR:
                    if(!MTBF_protocolo_servidor && l.protocolo.equals(lo.protocolo) && l.server.equals(lo.server)){
                        l.mtbf_servidor_protocolo = lo.tempo_inicio - l.tempo_inicio;  // intervalo entre falha.
                        MTBF_protocolo_servidor = true; 
                    }
                    
                    int i = 0;
                }
                
                
                
            }
        }
        
        
        boolean toprint = false;
        toprint = true;
        if (toprint) {
            for(Linha1 l : lista){
                System.out.println(l.toString());
            }
        }
        System.out.println("Total: "+ nmax + " lidas= " + lista.size());
        reader.close();
        
        
        escrever(lista);
        //lista.get(0).processTempo();
        //System.out.println(lista.get(0).toString());
        
        //Linha: 15663 Falha: vsrv8      SSTP       2017-03-16 15:52 17:49 (10197 min.
        //Linha: 29011 Falha: vsrv16     PPTP       2017-09-20 19:14 03:05 (11991 min.
        
        
        
    }
    
    public static void escrever(List<Linha1> lista) {
        BufferedWriter writer = null;
        String local = "output.txt";
        try {
            //create a temporary file
            //String timeLog = new SimpleDateFormat("yyyyMMdd_HHmmss").format(Calendar.getInstance().getTime());
            File logFile = new File(local);

            // This will output the full path where the file will be written to...
            System.out.println(logFile.getCanonicalPath());

            writer = new BufferedWriter(new FileWriter(logFile));
            
            writer.write(Linha1.toStringHeader());
            writer.newLine();
            for (Linha1 l : lista){
                writer.write(l.toString());
                writer.newLine();
            }
            
            
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                // Close the writer regardless of what happens...
                writer.close();
            } catch (Exception e) {
            }
        }
    }
    
}
