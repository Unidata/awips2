/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.log;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import com.raytheon.uf.edex.database.purge.PurgeLogger;

/**
 * 
 * PurgeLogs compresses or removes log files ( and archives ) from logDirectory
 * based on the millisecond timestamp in the filename
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 4, 2011            ekladstrup     Initial creation
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class PurgeLogs {
    private static final Pattern logTimePattern = Pattern
            .compile("^(.*)(\\d{4}\\d{2}\\d{2})(.*)\\.log$");

    private static final Pattern zipTimePattern = Pattern
            .compile("^(\\d{4}\\d{2}\\d{2})\\.zip$");

    private static final SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");

    private static final String plugin = "Purge Logs";

    private String logDirectory;

    public PurgeLogs() {

    }

    public synchronized void purge() {
        // System.out.println("start purge logs");
        PurgeLogger.logInfo("---------START LOG PURGE---------", plugin);
        int skipped = 0;
        // get log directory
        if (logDirectory != null) {
            File logDir = new File(logDirectory);
            if (logDir.exists()) {
                Matcher m = null;
                // from edex log directory get age of logs and archives per day
                HashMap<Date, ArrayList<String>> logsByDay = new HashMap<Date, ArrayList<String>>();
                String[] fileNames = logDir.list();
                for (String fileName : fileNames) {
                    m = null;
                    // check that it is a log file
                    int group = 0;
                    if (fileName.endsWith(".log")) {
                        // get timestamp from the log
                        m = logTimePattern.matcher(fileName);
                        group = 2;
                    } else if (fileName.endsWith(".zip")) {
                        // check date on the zip
                        m = zipTimePattern.matcher(fileName);
                        group = 1;
                    } else {
                        skipped++;
                        PurgeLogger.logInfo(
                                "Skipped unknown file: " + fileName, plugin);
                        continue;
                    }

                    if (m != null && m.find()) {
                        // found match
                        try {
                            Date day = sdf.parse(m.group(group));
                            if (logsByDay.containsKey(day)) {
                                logsByDay.get(day).add(fileName);
                            } else {
                                ArrayList<String> files = new ArrayList<String>();
                                files.add(fileName);
                                logsByDay.put(day, files);
                            }
                        } catch (ParseException e) {
                            skipped++;
                            // improper date format, just skip this file.
                            PurgeLogger.logError(
                                    "Invalid date format encountered in filename: "
                                            + fileName, plugin);
                        }
                    } else {
                        skipped++;
                        PurgeLogger.logInfo(
                                "Skipped file with invalid fileName: "
                                        + fileName, plugin);
                    }
                }

                // any zip files over 30 days delete
                removeMonthOldFiles(logsByDay);

                // remove and .zip files before compressing logs
                removeZipFilesFromList(logsByDay);

                // any logs over 7 days remaining need to be zipped up
                compressWeekOldLogs(logsByDay);

            }
        }
        PurgeLogger.logInfo("Skipped processing " + skipped + " files", plugin);
        PurgeLogger.logInfo("---------END LOG PURGE-----------", plugin);
        // System.out.println("purge logs finished");
    }

    /**
     * remove zip files from the list of files
     * 
     * @param logsByDay
     */
    private void removeZipFilesFromList(
            HashMap<Date, ArrayList<String>> logsByDay) {
        Set<Date> keySet = logsByDay.keySet();
        for (Date key : keySet) {
            ArrayList<String> files = logsByDay.get(key);
            ArrayList<String> filesToRemove = new ArrayList<String>();
            for (String file : files) {
                if (file.endsWith(".zip")) {
                    filesToRemove.add(file);
                }
            }
            for (String file : filesToRemove) {
                files.remove(file);
            }
        }
    }

    /**
     * purges month old log and zip files
     * 
     * @param logsByDay
     */
    private void removeMonthOldFiles(HashMap<Date, ArrayList<String>> logsByDay) {
        long thirtyDays = 30L * 24L * 60L * 60L * 1000L;
        Date now = new Date();
        int count = 0;
        Set<Date> keys = logsByDay.keySet();
        for (Date key : keys) {
            if (now.getTime() - key.getTime() >= thirtyDays) {
                ArrayList<String> filesToRemove = new ArrayList<String>();
                for (String file : logsByDay.get(key)) {
                    filesToRemove.add(file);
                    // delete the file
                    // System.out.println("deleting month old file");
                    String fullPath = logDirectory + "/" + file;
                    File tmp = new File(fullPath);
                    if (tmp.exists()) {
                        count++;
                        tmp.delete();
                    }
                }
                ArrayList<String> all = logsByDay.get(key);
                for (String file : filesToRemove) {
                    all.remove(file);
                }
            }
        }
        PurgeLogger.logInfo("Removed " + count + " old files", plugin);
    }

    /**
     * compresses ( then removes the loose files ) logs more than 7 days old
     * 
     * @param logsByDay
     */
    private void compressWeekOldLogs(HashMap<Date, ArrayList<String>> logsByDay) {
        // any log files over 7 days old tarball per day and delete files
        Date now = new Date();
        long sevenDays = 7L * 24L * 60L * 60L * 1000L;
        int count = 0;
        // go through all keys and all which are more than 7 days old
        // tarball up
        Set<Date> keys = logsByDay.keySet();
        for (Date key : keys) {
            if (now.getTime() - key.getTime() >= sevenDays
                    && logsByDay.get(key) != null
                    && logsByDay.get(key).size() > 0) {
                // add all files in the arraylist into YYYYMMDD.zip
                String name = sdf.format(key) + ".zip";
                try {
                    ZipOutputStream zos = new ZipOutputStream(
                            new FileOutputStream(logDirectory + "/" + name));
                    for (String file : logsByDay.get(key)) {
                        String fullPath = logDirectory + "/" + file;
                        File tmpFile = new File(fullPath);
                        if (tmpFile.exists()) {
                            count++;
                            FileInputStream in = new FileInputStream(fullPath);

                            zos.putNextEntry(new ZipEntry(file));

                            int len;
                            byte[] buffer = new byte[4096];
                            while ((len = in.read(buffer)) > 0) {
                                zos.write(buffer, 0, len);
                            }

                            zos.closeEntry();
                            // System.out.println("deleting week old file");
                            tmpFile.delete();
                            in.close();
                        }
                    }
                    zos.close();
                } catch (FileNotFoundException e) {
                    // we check if the file exists before opening, this should
                    // never happen
                    e.printStackTrace();
                    PurgeLogger.logError("Unexpected exception caught", plugin,
                            e);

                } catch (IOException e) {
                    // This should not happen either, could be caused by
                    // attempting to write in a folder where the user does not
                    // have proper permissions
                    e.printStackTrace();
                    PurgeLogger.logError("Unexpected excetion caught", plugin,
                            e);
                }
            }
        }
        PurgeLogger.logInfo("Archived " + count + " files", plugin);
    }

    /**
     * set the log directory
     * 
     * @param logDirectory
     */
    public void setLogDirectory(String logDirectory) {
        this.logDirectory = logDirectory;
    }

    /**
     * get the log directory
     * 
     * @return
     */
    public String getLogDirectory() {
        return logDirectory;
    }
}
