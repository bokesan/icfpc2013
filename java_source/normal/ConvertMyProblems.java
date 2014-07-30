import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;


public class ConvertMyProblems {


    public static void main(String[] args) {

        Integer currSize = 17;
        StringBuilder builder = new StringBuilder();

        try {
            BufferedReader in = new BufferedReader(new FileReader("D:\\icfpc\\testset.txt"));
            String zeile = null;
            while ((zeile = in.readLine()) != null) {
                builder.append(zeile);
            }
            
            File file = new File("D:\\icfpc\\next.txt");
            
            if (!file.exists()) {
                    file.createNewFile();
            }

            FileWriter fw = new FileWriter(file.getAbsoluteFile());
            BufferedWriter bw = new BufferedWriter(fw);


            String[] probs = builder.toString().replace(" ", "").split("id\":\"");
            List<Problem> psl = new ArrayList<Problem>();
            for (String prob : probs) {
                if (prob.contains("solved")) continue;
                if (!prob.contains("\"size\":" + currSize + ",")) continue;
                String id = prob.substring(0, 24);
                String ops = prob.substring(prob.indexOf('[')+1,prob.indexOf(']')).replace("\"", "");
                //if (ops.replaceAll("[^,]", "").length() > 3) continue;
                //if (!ops.contains("tfold")) continue;
                //bw.write(id + " " + currSize + " " + ops + "\n");
                psl.add(new Problem(id, ops));
            }
            Problem[] ps = psl.toArray(new Problem[psl.size()]);
            java.util.Arrays.sort(ps, null);
            for (Problem p : ps) {
                bw.write(p.id + " " + currSize + " " + p.ops + "\n");
            }
            
            bw.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static class Problem implements Comparable<Problem> {
        final String id;
        final String ops;
        final int numOps;

        Problem(String id, String ops) {
            this.id = id;
            this.ops = ops;
            numOps = count(ops, ',');
        }

        public boolean equals(Object other) {
            if (!(other instanceof Problem))
                return false;
            Problem p = (Problem) other;
            return id.equals(p.id);
        }

        @Override
        public int hashCode() {
            return id.hashCode();
        }

        public boolean hasTFold() {
            return ops.indexOf("tfold") >= 0;
        }

        public int compareTo(Problem p) {
            if (hasTFold() && !p.hasTFold())
                return -1;
            if (p.hasTFold() && !hasTFold())
                return 1;
            if (numOps < p.numOps)
                return -1;
            if (numOps > p.numOps)
                return 1;
            return id.compareTo(p.id);
        }

    }

    private static int count(String s, char c) {
        int n = s.length();
        int cnt = 0;
        for (int i = 0; i < n; i++) {
            if (s.charAt(i) == c)
                cnt++;
        }
        return cnt;
    }

}
