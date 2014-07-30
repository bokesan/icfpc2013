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
        possiblePrograms = callGenerator(getSize(args), getOps(args), false);
        values = new ValueStorage();
        eval = new Evaluator();

        search();
//        if (search() < 0) {
//            System.out.println("BACKUP GENERATOR WIRD VERWENDET!");
//            possiblePrograms = callGenerator(getSize(args), getOps(args), true);
//            if (search() < 0) {
//                System.out.println("BACKUP FEHLGESCHLAGEN!");
//            }
//            System.exit(-1);
//        }
    }


    private static Integer search() {
        try {           
            BufferedReader in = new BufferedReader(new FileReader("D:\\icfpc\\buffer.txt"));
            String zeile = null;

            while (true) {
                Value current = values.getNextValue();
                if (!ServerConnector.checkTimeLeft(System.currentTimeMillis())) return -1;
                List<Program> goodProgs = new ArrayList<Program>();
                System.out.println("Anzahl möglicher Programme: "+possiblePrograms.size());
                for (Program p : possiblePrograms) {
                    if (current.result.equals(evaluate(p, current.param))) goodProgs.add(p);
                }

                if (possiblePrograms.size() == goodProgs.size()) {
                    if (ServerConnector.guess(possiblePrograms.get(0), values)) return 1;
                }

                if (buffered && (goodProgs.size() < 50000)) {
                    if (ServerConnector.guess(possiblePrograms.get(0), values)) return 1;
                    System.out.println("Lese aus Puffer.");
                    Integer i = 0;
                    while (i < 2000000 && ((zeile = in.readLine()) != null)) {
                        i++;
                        goodProgs.add(new Program(zeile));
                    }
                    buffered = !(i < 2000000);
                }

                if (goodProgs.size() == 0) {
                    System.out.println("!ALLE PROGRAMME VERBRAUCHT!\n");
                    return -1;
                }
                possiblePrograms = goodProgs;
            }
        } catch (Exception e) {
            System.out.println("Buffer lesen fehlgeschlagen: " + e);
            return(-1);
        }
    }


    private static List<Program> callGenerator(Integer size, String ops, boolean backup) {
        String[] params = new String[4];
        if (backup) {
            params[0] = "D:\\icfpc\\bin\\Main_backup"; 
        } else {
            params[0] = "D:\\icfpc\\bin\\Main";            
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

            File file = new File("D:\\icfpc\\buffer.txt");            
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
                if (i < 2000000) {
                    result.add(new Program(line));
                } else if (i > 50000000) {
                    System.out.println("Mehr als 50 Millionen Programme wollen wir garnicht. :)");
                    break;
                } else {
                    bw.write(line + "\n");
                    buffered = true;
                    if (i == 10000000) System.out.println("10 millionen Programme generiert in: " + Math.floor((System.currentTimeMillis() - time)/1000) + " Sekunden.");
                }
                i++;
            }
            p.destroy();
            input.close();
            bw.close();
            System.out.println(Math.floor(i/1000000) + " millionen Programme generiert in: " + Math.floor((System.currentTimeMillis() - time)/1000) + " Sekunden.");
        } catch (Exception e) {
            System.out.println(e);
        }

        return result;
    }

}
