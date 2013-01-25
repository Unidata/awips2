package edu.uci.ics.crawler4j.url;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Set;

public class TLDList {

    private final static Set<String> tldSet = new HashSet<String>();

    public static boolean contains(String str) {
        if (tldSet != null) {
            return tldSet.contains(str);
        } else {
            return false;
        }
    }

    static {
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new InputStreamReader(
                    TLDList.class.getClassLoader().getResourceAsStream(
                            "tld-names.txt")));
            String line;

            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty() || line.startsWith("//")) {
                    continue;
                }
                tldSet.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    // Nothing we can do
                }
            }
        }
    }

}
