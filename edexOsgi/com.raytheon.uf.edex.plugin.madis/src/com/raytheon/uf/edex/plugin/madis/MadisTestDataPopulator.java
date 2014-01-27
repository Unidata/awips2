package com.raytheon.uf.edex.plugin.madis;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.util.FileUtil;

/**
 * Utility to populate EDEX with Data for DAT tools
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 12 May, 2011   *    dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class MadisTestDataPopulator {

    public static String DATA_STORE_DIR = null;

    public static String MANUAL_INGEST_DIR = null;

    public HashMap<String, File> scanDirs = null;

    public ConcurrentHashMap<String, ArrayList<String>> madismodifieds = null;


    /**
     * @param args
     *            data_store dir,
     */
    public static void main(String[] args) {
        DATA_STORE_DIR = args[0];
        MANUAL_INGEST_DIR = args[1];
       
        MadisTestDataPopulator stdp = new MadisTestDataPopulator();
        stdp.monitor();
    }

    /**
     * public const
     */
    public MadisTestDataPopulator() {
        setMadisDirectories();
    }

    /**
     * Setup the directories to be monitored by program
     */
    private void setMadisDirectories() {

        scanDirs = new HashMap<String, File>();
        madismodifieds = new ConcurrentHashMap<String, ArrayList<String>>();

        ArrayList<File> madisDirs = new ArrayList<File>();
        madisDirs.add(new File(DATA_STORE_DIR + "/madis"));
            
        for (File madisDir: madisDirs) {  

            if (madisDir.isDirectory()) {
    
                scanDirs.put(madisDir.getName(), madisDir);
            }
        }
    }
    

    /**
     * deploy checker
     */
    private void monitor() {
        DirChecker checker = new DirChecker();
        checker.run();
    }

    /**
     * Inner class to check directories
     * 
     * @author dhladky
     * 
     */
    private class DirChecker implements Runnable {

        @Override
        public void run() {

            while (scanDirs != null) {

                evaluate();

                try {
                    Thread.sleep(15000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }

        private void evaluate() {

            ArrayList<File> madisDirs = new ArrayList<File>();
            madisDirs.add(new File(DATA_STORE_DIR + "/madis"));
           
            for (File madisDir : madisDirs) {
                System.out.println("Evaluating directory: "
                        + madisDir.getName());
                if (madisDir != null) {

                    try {
                        checkMadisMod(madisDir);
                    } catch (Exception e) {
                        System.err.println("Bad path: "
                                + madisDir.getAbsolutePath());
                    }

                }
            }
        }

        private void checkMadisMod(File dir) {

            if (dir.isDirectory()) {

                ArrayList<String> newNames = getFiles(dir.listFiles());

                boolean check = false;

                if (dir != null && newNames.size() > 0) {

                    String copyfile = null;

                    if (madismodifieds.containsKey(dir.getPath())) {

                        ArrayList<String> oldNames = madismodifieds.get(dir
                                .getPath());

                        for (String checkFile : newNames) {
                            if (!oldNames.contains(checkFile)) {
                                copyfile = checkFile;
                                check = true;
                                madismodifieds.replace(dir.getPath(), newNames);
                                break;
                            }
                        }
                    } else {
                        // first run
                        madismodifieds.put(dir.getPath(), newNames);
                        check = true;
                        copyfile = newNames.get(newNames.size() - 1);
                    }

                    if (check && copyfile != null) {
                        File file = new File(dir.getAbsolutePath() + "/"
                                + copyfile);
                        copyFile(file);
                        System.out.println("File copied: " + copyfile);
                    }
                }
            }
        }


        /**
         * gets the files as an arrayList
         * 
         * @param list
         * @return
         */
        public ArrayList<String> getFiles(File[] list) {

            ArrayList<String> fileNames = new ArrayList<String>();
            for (File file : list) {
                if (!file.isDirectory() && !file.isHidden()) {
                    fileNames.add(file.getName());
                }
            }

            return fileNames;
        }

        /**
         * Copy the file
         * 
         * @param file
         */
        private void copyFile(File file) {

            File outfile = new File(FileUtil.join(MANUAL_INGEST_DIR,
                    file.getName()));
            InputStream in = null;
            try {
                in = new FileInputStream(file);
            } catch (FileNotFoundException e1) {
                e1.printStackTrace();
            }

            // For Overwrite the file.
            OutputStream out = null;
            try {
                out = new FileOutputStream(outfile);
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }

            byte[] buf = new byte[1024];
            int len;
            try {
                while ((len = in.read(buf)) > 0) {
                    out.write(buf, 0, len);
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
            try {
                in.close();
                out.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}