import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;


public class Guess {

    private static List<Program> possiblePrograms;
    private static ValueStorage values;
    private static Evaluator eval;
    private static boolean buffered = false;


    public static void main(String[] args) {


        ServerConnector.round_start = 0;
        ServerConnector.problemID = getID(args);
        List<Value> mismatches = ServerConnector.getNewValues(256);    
        possiblePrograms = callBonusGenerator(getSize(args), getOps(args), mismatches);
        values = new ValueStorage();
        eval = new Evaluator();

        while (true) {
            ServerConnector.checkTimeLeft(System.currentTimeMillis());
            List<Program> goodProgs = new ArrayList<Program>();
            System.out.println("Anzahl möglicher Programme: "+possiblePrograms.size());
            if (ServerConnector.guess(possiblePrograms.get(0), values)) return;              
            Value current = values.getNextValue();
            mismatches.add(current);
            for (Program p : possiblePrograms) {
                if (current.result.equals(evaluate(p, current.param))) goodProgs.add(p);                   
            }

            if ((goodProgs.size() < 1)) {
                System.out.println("Frage Generator.");
                goodProgs = callBonusGenerator(getSize(args), getOps(args), mismatches);
            }

            if (goodProgs.size() == 0) {
                System.out.println("!ALLE PROGRAMME VERBRAUCHT!\n");
                return ;
            }
            possiblePrograms = goodProgs;
        }
    }



    private static List<Program> callBonusGenerator(Integer size, String ops, List<Value> values) {
        String[] params = new String[5];
        StringBuilder valueString = new StringBuilder();
        for (Value v : values) {
            valueString.append(v.param+"="+v.result+",");
        }
        valueString.deleteCharAt(valueString.length()-1);

        params[0] = "C:\\Users\\jan\\Dropbox\\ICFPC\\2013\\bin\\Main";            
        params[1] = "bonus";
        params[2] = size.toString();
        params[3] = ops;
        params[4] = valueString.toString();

        List<Program> progs = callExternal(params);
        return progs;
    }

    private static List<Program> callGenerator(Integer size, String ops, boolean backup) {
        String[] params = new String[4];
        if (backup) {
            params[0] = "C:\\Users\\jan\\Dropbox\\ICFPC\\2013\\bin\\Main_backup"; 
        } else {
            params[0] = "C:\\Users\\jan\\Dropbox\\ICFPC\\2013\\bin\\Main";            
        }
        params[1] = "gen";
        params[2] = size.toString();
        params[3] = ops;
        List<Program> progs = callExternal(params);
        return progs;
    }

    private static Integer getSize(String[] args) {
        return Integer.valueOf(args[1]);
    }

    private static String getOps(String args[]) {
        return args[2];
    }

    private static String getID(String[] args) {
        return args[0];
    }

    private static BigInteger longToBigint(long val) {
        BigInteger result = BigInteger.valueOf(val);
        if (val < 0) result = result.add(BigInteger.ONE.shiftLeft(64));
        return result;
    }

    private static BigInteger evaluate(Program prog, BigInteger param) {
        eval.setProgram(prog.toStringWithoutQuotes());
        return longToBigint(eval.eval(param.longValue()));
    }

    private static List<Program> callExternal(String[] params) {
        List<Program> result = new ArrayList<Program>();
        try {
            long time = System.currentTimeMillis();
            ProcessBuilder builder = new ProcessBuilder(params);
            builder.redirectErrorStream(true);
            Process p = builder.start();
            BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
            String line;
            boolean ready = false;

            File file = new File("E:\\buffer.txt");            
            if (!file.exists()) {
                file.createNewFile();
            }
            FileWriter fw = new FileWriter(file.getAbsoluteFile());
            BufferedWriter bw = new BufferedWriter(fw);

            buffered = false;
            Integer i = 0;
            while (true) {
                line = input.readLine();
                if (line == null && ready) break;
                if (line == null && !ready) continue;
                ready = true;
                if (line.contains("Main")) {
                    System.out.println("Fehler in generator: " + line);
                    System.exit(-1);
                }
                if (i < 10 || (System.currentTimeMillis() - time) < 20000) {
                    result.add(new Program(line));
                } else {
                    break;
                }
                i++;
            }
            p.destroy();
            input.close();
            bw.close();
            System.out.println(Math.floor(i) + " Programme generiert in: " + Math.floor((System.currentTimeMillis() - time)/1000) + " Sekunden.");
        } catch (Exception e) {
            System.out.println(e);
        }

        return result;
    }

}
