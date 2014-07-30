import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.math.BigInteger;
import java.net.*;


public class ServerConnector {

    private final static String AUTH_TOKEN = "0134IByLamsRGKk9k363lMuaGCpN1Ab53VJ1IzR4";
    private final static String SUFFIX = "vpsH1H";
    private final static String SERVER_URL = "http://icfpc2013.cloudapp.net/";
    public static String problemID;
    
    public static long round_start = 0;
    private static Integer next_slot = 0;
    private static long[] timers = {0,0,0,0,0};

    public static List<Value> getNewValues(Integer numberOfValues) {
        Long[] params = new Long[numberOfValues];
        StringBuilder args = new StringBuilder();
        Random generator = new Random();
        params[0] = generator.nextLong();
        args.append("\"").append("0x").append(Long.toHexString(params[0]).toUpperCase()).append("\"");
        for (Integer i = 1; i < numberOfValues; i++) {
            params[i] = generator.nextLong();
            args.append(",").append("\"").append("0x").append(Long.toHexString(params[i]).toUpperCase()).append("\"");
        }
        String param = "{\"id\":\"" + problemID + "\",\"arguments\":[" + args + "]}";
        String answer = askServer("eval", param);
        if (answer.equals("")) {
            System.out.println("Keine Antwort für eval von Server.");
            System.exit(-1);
        }
        
        List<Value> results = new ArrayList<Value>();
        String[] answerParts = answer.substring(27, answer.length() - 2).replace("\"", "").replace("0x", "").split(",");
        for (Integer i = 0; i < numberOfValues; i++) {
            results.add(new Value(BigInteger.valueOf(params[i]), new BigInteger(answerParts[i], 16)));
        }
        return results;
    }

    public static Integer getNumberOfGuessesStillPossible() {
        return (int) ((Math.floor((300000 - (System.currentTimeMillis() - round_start)) / 21000) - 5) * 5);
    }

    public static void guessAll(List<Program> progs) {
        for (Program p : progs) {
            if (guess(p)) {
                System.out.println("Gelöst nach " + Math.floor((System.currentTimeMillis() - round_start) / 1000) + " Sekunden.\n");
                round_start = 0;
                return;
            }
        }
    }
    
    public static boolean guess(Program prog) {
        String param = "{\"id\":\"" + problemID + "\",\"program\":" + prog + "}";
        String answer = askServer("guess", param);
        System.out.println(answer);
        return answer.contains("win");
    }

    public static boolean guess(Program prog, ValueStorage storage) {
        String param = "{\"id\":\"" + problemID + "\",\"program\":" + prog + "}";
        String answer = askServer("guess", param);
        System.out.println(answer);
        if (answer.contains("mismatch")) {
            String newParam = answer.substring(34, 50);
            String newResult = answer.substring(55, 71);
            storage.setNextValue(new Value(new BigInteger(newParam, 16), new BigInteger(newResult,16)));
        } else if (answer.contains("win")) {
            System.out.println("Gelöst nach " + Math.floor((System.currentTimeMillis() - round_start) / 1000) + " Sekunden.\n");
            round_start = 0;
        }
        return answer.contains("win");
    }

    private static void checkTime() {
        long time = System.currentTimeMillis();
        if (round_start == 0) {
            round_start = time;
            return;
        }
        checkTimeLeft(time);
        if (timers[next_slot] + 21000 > time) {
            try {
            Thread.sleep(timers[next_slot] + 21000 - time);               
            } catch (Exception e) {
                System.out.println("checkTime sleep fehlgeschlagen: " + e);
            }
        }
        timers[next_slot] = System.currentTimeMillis();
        next_slot = (next_slot + 1) % 5;
    }

    public static void checkTimeLeft(long time) {
        if (round_start + 300000 < time) {
            System.out.println("Zeit abgelaufen.");
            round_start = 0;
            System.exit(0);
        }
    }

    public static String askServer(String path, String parameters) {
        checkTime();
        String result = "";
        try {

            URL url = new URL(SERVER_URL + path + "?auth=" + AUTH_TOKEN + SUFFIX);
            URLConnection conn = url.openConnection();

            conn.setDoOutput(true);

            OutputStreamWriter writer = new OutputStreamWriter(conn.getOutputStream());

            System.out.println(path + ": " + parameters);
            writer.write(parameters);
            writer.flush();

            BufferedReader reader = new BufferedReader(new InputStreamReader(conn.getInputStream()));

            result = reader.readLine();
            writer.close();
            reader.close();   
        } catch (Exception e) {
            if (e.toString().contains("429")) {
                System.out.println("Server sagt \"bitte warten\", 5 Sekunden Pause.");
                try { Thread.sleep(5000); } catch (Exception ex) {System.out.println("Sleep unterbrochen: " + ex);}
                return askServer(path, parameters);
            }
            System.out.println("askServer Exception: " + e);
        }

        return result;
    }
}
