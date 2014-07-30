import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;


public class SolveMyProblems {


    public static void main(String[] args) {
        try {
            StringBuilder builder = new StringBuilder();
            BufferedReader in = new BufferedReader(new FileReader("C:\\Users\\jan\\Dropbox\\ICFPC\\2013\\next.txt"));
            String zeile = null;
            while ((zeile = in.readLine()) != null) {
                builder.append(zeile).append("\n");
            }
            




            String[] probs = builder.toString().split("\n");
            for (String prob : probs) {   
                Guess.main(prob.split(" "));         }
            
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

}
