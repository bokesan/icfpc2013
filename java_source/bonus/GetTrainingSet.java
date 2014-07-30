
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;


public class GetTrainingSet {


    public static void main(String[] args) {
        Integer lvl = 14;
        Integer count = 10;

        String param = "{\"size\":" + lvl + "}";
        StringBuilder set = new StringBuilder();
        for (Integer i = 0; i < count; i++) {
            String answer = ServerConnector.askServer("train", param);
            set.append(answer).append("\n");
        }
        try {
            File file = new File("C:\\Users\\jan\\Dropbox\\ICFPC\\2013\\testset.txt");
            
            if (!file.exists()) {
                    file.createNewFile();
            }

            FileWriter fw = new FileWriter(file.getAbsoluteFile());
            BufferedWriter bw = new BufferedWriter(fw);


            bw.write(set.toString());
            
            bw.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

}
