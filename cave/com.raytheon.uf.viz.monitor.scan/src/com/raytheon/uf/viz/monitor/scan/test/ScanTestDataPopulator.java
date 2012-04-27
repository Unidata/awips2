package com.raytheon.uf.viz.monitor.scan.test;

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

public class ScanTestDataPopulator {

    public static String DATA_STORE_DIR = null;

    public static String MANUAL_INGEST_DIR = null;

    public static ArrayList<String> sites = null;

    public static ArrayList<String> ffgs = null;

    public HashMap<String, ArrayList<File>> scanDirs = null;

    public HashMap<String, ArrayList<File>> ffgFiles = null;

    public ConcurrentHashMap<String, ArrayList<String>> radarmodifieds = null;

    public ConcurrentHashMap<String, ArrayList<String>> ffgmodifieds = null;

    /**
     * @param args
     *            data_store dir,
     */
    public static void main(String[] args) {
        DATA_STORE_DIR = args[0];
        MANUAL_INGEST_DIR = args[1];
        sites = new ArrayList<String>();
        ffgs = new ArrayList<String>();

        for (int i = 2; i < args.length; i++) {
            if (args[i].length() == 3 || args[i].startsWith("k")) {
                sites.add(args[i]);
            } else {
                ffgs.add(args[i]);
            }
        }

        ScanTestDataPopulator stdp = new ScanTestDataPopulator();
        stdp.monitor();
    }

    /**
     * public const
     */
    public ScanTestDataPopulator() {
        setScanDirectories();
    }

    /**
     * Setup the directories to be monitored by program
     */
    private void setScanDirectories() {

        scanDirs = new HashMap<String, ArrayList<File>>();
        radarmodifieds = new ConcurrentHashMap<String, ArrayList<String>>();

        for (String site : sites) {

            File siteDir = new File(DATA_STORE_DIR + "/radar/" + site);

            if (siteDir.isDirectory()) {
                File[] files = siteDir.listFiles();
                ArrayList<File> monitoredDirs = new ArrayList<File>();

                for (File file : files) {
                    // product dirs
                    if (file.isDirectory()) {
                        monitoredDirs.add(file);
                    }
                }
                scanDirs.put(site, monitoredDirs);
            }
        }

        ffgmodifieds = new ConcurrentHashMap<String, ArrayList<String>>();

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

            for (String site : sites) {
                System.out.println("Evaluating site: " + site);
                ArrayList<File> dirs = scanDirs.get(site);
                if (dirs == null) {
                    break;
                }
                for (File dir : dirs) {
                    try {
                        checkRadarMod(dir);
                    } catch (Exception e) {
                        System.err
                                .println("Bad path: " + dir.getAbsolutePath());
                    }
                }
            }

            for (String ffg : ffgs) {
                System.out.println("Evaluating ffg: " + ffg);
                try {
                    checkFFGMod(ffg);
                } catch (Exception e) {
                    System.err.println("Bad path: " + ffg);
                }
            }

        }

        private void checkRadarMod(File dir) {

            if (dir.isDirectory()) {

                ArrayList<String> newNames = getFiles(dir.listFiles());

                boolean check = false;

                if (dir != null && newNames.size() > 0) {

                    String copyfile = null;

                    if (radarmodifieds.containsKey(dir.getPath())) {

                        ArrayList<String> oldNames = radarmodifieds.get(dir
                                .getPath());

                        for (String checkFile : newNames) {
                            if (!oldNames.contains(checkFile)) {
                                copyfile = checkFile;
                                check = true;
                                radarmodifieds.replace(dir.getPath(), newNames);
                                break;
                            }
                        }
                    } else {
                        // first run
                        radarmodifieds.put(dir.getPath(), newNames);
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

        private void checkFFGMod(String ffgName) {

            File ffgDir = new File(DATA_STORE_DIR + "/grib/ffg");
            ArrayList<String> newNames = new ArrayList<String>();

            if (ffgDir.isDirectory()) {

                if (ffgDir != null) {

                    for (File file : ffgDir.listFiles()) {
                        String[] splitter = file.getName().split("_");
                        String name = splitter[1];

                        if (name.equals(ffgName)) {
                            newNames.add(file.getName());
                        }
                    }

                    if (ffgmodifieds.containsKey(ffgName)) {
                        // gets old files from dir
                        ArrayList<String> oldNames = ffgmodifieds.get(ffgName);
                        boolean match = false;

                        for (String checkFile : newNames) {
                            if (!oldNames.contains(checkFile)) {
                                copyFile(new File(DATA_STORE_DIR + "/grib/ffg/"
                                        + checkFile));
                                System.out.println("File copied: " + checkFile);
                                match = true;
                            }
                        }

                        if (match) {
                            ffgmodifieds.replace(ffgName, newNames);
                        }
                    } else {
                        // first run
                        ffgmodifieds.put(ffgName, newNames);

                        for (String checkFile : newNames) {
                            copyFile(new File(DATA_STORE_DIR + "/grib/ffg/"
                                    + checkFile));
                            System.out.println("File copied: " + checkFile);
                        }
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
