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
package com.raytheon.uf.edex.plugin.acarssounding;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.plugin.acarssounding.tools.ACARSAircraftInfo;
import com.raytheon.uf.edex.plugin.acarssounding.tools.ACARSSoundingTools;

/**
 * Splits ACARS sounding data into ACARSAircraftInfo objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2010            jkorman     Initial creation
 * May 14, 2014 2536       bclement    removed TimeTools usage
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class ACARSSoundingSplitter implements Iterator<Object> {

    private Log logger = LogFactory.getLog(getClass());
    
    // Cutoff time for cleaning files
    private static final int FILE_CLEANUP_CUTOFF = 7;
    
    public static final String CREATE_PROCESS_BEAN = "TRUE";

    public static final int BASE_DIR_NULL = 1;
    public static final int DATA_DIR_NULL = 2;
    
    public static final int NO_BASE_DIR = 3;
    public static final int NO_DATA_DIR = 4;
    
    
    private File baseDir = null;

    private File dataDir = null;

    private Iterator<ACARSAircraftInfo> iterator = null;

    // List of tail numbers that need to processed because data was
    // added to the tail number list.
    private List<ACARSAircraftInfo> acftInfo = null;

    private boolean failSafe = false;
    
    private int failReason = 0;
    
    /**
     * 
     */
    public ACARSSoundingSplitter() {
        logger.info("Creating splitter singleton");
        cleanUpAcftObs();
    }

    /**
     * 
     */
    private ACARSSoundingSplitter(String process, boolean failSafe) {
        if(failSafe) {
            iterator = new Iterator<ACARSAircraftInfo>() {

                @Override
                public boolean hasNext() {
                    return false;
                }

                @Override
                public ACARSAircraftInfo next() {
                    return null;
                }

                @Override
                public void remove() {
                }
            };
        } else {
            cleanUpAcftObs();
            
            if (CREATE_PROCESS_BEAN.equals(process)) {
                logger.info("Creating process bean");

                try {
                    getACARSData();
                } catch (Exception e) {
                    logger.error("Error building sounding list", e);
                }
            } else {
                logger.info("Creating splitter singleton");
            }
        }
    }

    /**
     * 
     * @see java.util.Iterator#hasNext()
     */
    @Override
    public boolean hasNext() {
        boolean hasNext = false;
        if (iterator != null) {
            hasNext = iterator.hasNext();
        }
        return hasNext;
    }

    /**
     * 
     * @see java.util.Iterator#next()
     */
    @Override
    public ACARSAircraftInfo next() {
        ACARSAircraftInfo next = null;
        if (iterator != null) {
            next = iterator.next();
            if (!iterator.hasNext()) {
                iterator = null;
                acftInfo.clear();
                acftInfo = null;
            }
        }
        return next;
    }

    /**
     * This iterator does not support the remove method.
     * 
     * @see java.util.Iterator#remove()
     */
    @Override
    public void remove() {
        throw new UnsupportedOperationException("Remove not supported");
    }

    /**
     * 
     * @return
     */
    public boolean isFailSafe() {
        return failSafe;
    }

    /**
     * Get the failure reason.
     * @return
     */
    public int getFailReason() {
        return failReason;
    }
    
    /**
     * 
     */
    private void getACARSData() {

        if (logger.isDebugEnabled()) {
            logger.debug("Memory Before " + Runtime.getRuntime().freeMemory());
        }
        long startTime = System.currentTimeMillis();

        // Get a list of all files in the rawData directory
        ArrayList<ACARSRecord> recs = new ArrayList<ACARSRecord>();
        File[] files = ACARSSoundingTools.getDataFiles(baseDir);
        for (File f : files) {
            RandomAccessFile raf = null;
            boolean forceDelete = false;
            try {
                raf = new RandomAccessFile(f, "rw");

                String s = null;
                long pos = raf.getFilePointer();
                // Read data from the file, enter any new data into the
                // records list.
                while ((s = raf.readLine()) != null) {
                    long currPos = raf.getFilePointer();
                    // Check for new data only
                    if (s.startsWith(ACARSSoundingTools.ACARS_NEW_OBS)) {
                        String[] parts = splitBuilder(s);
                        if (parts != null) {
                            ACARSRecord rec = new ACARSRecord(parts[2]);
                            rec.setTimeObs(TimeTools.newCalendar(Long
                                    .parseLong(parts[1])));
                            recs.add(rec);
                        }
                        // tag items what we have read.
                        raf.seek(pos);
                        raf.write(ACARSSoundingTools.ACARS_OLD_OBS);
                        raf.seek(currPos);
                    }
                    pos = currPos;
                }
            } catch (Exception ioe) {
                logger.info("Error reading acars old obs");
                forceDelete = true;
            } finally {
                if (raf != null) {
                    try {
                        raf.close();
                    } catch (IOException ioe) {
                        logger.info("Error closing acars old obs");
                    }
                }
            }
            // Determine if the file needs to be deleted.
            if (forceDelete) {
                if (!f.delete()) {
                    logger.error("Could not delete file " + f.getName());
                }
            } else {
                deleteFile(f, logger);
            }
        } // for
          // Create a map for tailNumbers --> records.
        HashMap<String, HashMap<String, ACARSRecord>> tNumbers = new HashMap<String, HashMap<String, ACARSRecord>>();
        if (recs.size() > 0) {
            for (ACARSRecord r : recs) {
                HashMap<String, ACARSRecord> recMap = null;
                if (tNumbers.containsKey(r.getTailNumber())) {
                    recMap = tNumbers.get(r.getTailNumber());
                } else {
                    recMap = new HashMap<String, ACARSRecord>();
                    tNumbers.put(r.getTailNumber(), recMap);
                }
                String uri = r.getDataURI();
                if (!recMap.containsKey(uri)) {
                    recMap.put(uri, r);
                }
            }
            // Create a list of sorted unique tailnumbers to process.
            List<String> tailNumbers = new ArrayList<String>();
            tailNumbers.addAll(tNumbers.keySet());
            Collections.sort(tailNumbers);

            acftInfo = new ArrayList<ACARSAircraftInfo>();

            // Check for the output directory, create it if
            // it doesn't exist.
            if (!dataDir.exists()) {
                dataDir.mkdirs();
            }

            // For each tailnumber
            for (String tailNumber : tailNumbers) {
                HashMap<String, ACARSRecord> recMap = tNumbers.get(tailNumber);
                if ((recMap != null) && (recMap.size() > 0)) {
                    // We have a tailnumber and some number of of observations
                    // that we need to write out.
                    File out = new File(dataDir, tailNumber + ".acft");
                    FileWriter writer = null;

                    ArrayList<ACARSRecord> aList = new ArrayList<ACARSRecord>();
                    for (String key : recMap.keySet()) {
                        aList.add(recMap.get(key));
                    }

                    // Combine any existing data for this tailnumber with
                    // the new data.
                    combineLists(out, aList, tailNumber);

                    Collections.sort(aList);
                    try {
                        // We've combined and sorted all the data, so overwrite
                        // the
                        // existing file.
                        writer = new FileWriter(out, ACARSSoundingTools.NO_APPEND);

                        long startObs = Long.MAX_VALUE;
                        long stopObs = Long.MIN_VALUE;

                        // write out each datauri for this tail number.
                        for (ACARSRecord r : aList) {
                            startObs = Math.min(startObs, r.getTimeObs()
                                    .getTimeInMillis());
                            stopObs = Math.max(stopObs, r.getTimeObs()
                                    .getTimeInMillis());

                            writer.write(String.format(
                                    ACARSSoundingTools.OBS_FMT, r.getTimeObs()
                                            .getTimeInMillis(), r.getDataURI()));
                        }
                        // Get the full filepath to the tail number file and
                        // put it in the aircraft info
                        ACARSAircraftInfo info = new ACARSAircraftInfo(
                                tailNumber, out.getCanonicalPath());
                        info.setStartTime(startObs);
                        info.setStopTime(stopObs);

                        acftInfo.add(info);
                    } catch (IOException ioe) {
                        // TODO : Logging here
                    } finally {
                        if (writer != null) {
                            try {
                                writer.close();
                            } catch (IOException ioe) {
                                // TODO : Logging here
                            }
                        }
                    }
                }
            } // for
        }
        // All done with the tail number and records so clean up the collections
        tNumbers.clear();
        recs.clear();

        iterator = (acftInfo != null) ? acftInfo.iterator() : null;

        if (logger.isDebugEnabled()) {
            logger.debug("Memory After " + Runtime.getRuntime().freeMemory());
        }
        logger.info("Elapsed time = "
                + (System.currentTimeMillis() - startTime));
    }

    /**
     * 
     * @param f
     * @param obs
     * @param tailnumber
     */
    private void combineLists(File f, List<ACARSRecord> obs, String tailnumber) {
        HashSet<String> dups = new HashSet<String>();
        for (ACARSRecord r : obs) {
            dups.add(r.getDataURI());
        }
        obs = ACARSSoundingTools.readAircraftData(f, obs, dups, logger);
    }

    /**
     * 
     */
    private void cleanUpAcftObs() {
        final String START_FMT = "ACARS aircraft info cleanup [started] on [%6d] files";
        final String END_FMT = "ACARS aircraft info cleanup [complete] deleted [%6d] files";

        int tries = 3;
        while ((dataDir == null)&&(tries > 0)) {
            setupFiles();
            if(dataDir == null) {
                ACARSSoundingTools.delay(10000L);
                tries--;
            }
        }
        if(tries > 0) {
            int deletedFiles = 0;
            if (dataDir.exists() && dataDir.isDirectory()) {
                File[] files = ACARSSoundingTools.getDataFiles(dataDir);
                logger.info(String.format(START_FMT, files.length));
                for (File f : files) {
                    boolean deleted = false;
                    try {
                        if (f.isFile()) {
                            String fName = f.getName();
                            if (fName.endsWith(".acft")) {
                                List<String> newList = processFile(f);;
                                if (newList.size() > 0) {
                                    ACARSSoundingTools.writeAircraftData(f,
                                            newList, logger);
                                } else {
                                    logger.info("Deleting empty tailnumber file " + fName);
                                    // come here to delete the tailnumberfile
                                    deleted = ACARSSoundingTools.deleteFile(f, logger, "Attempt to delete %s failed");
                                }
                            } else {
                                // a garbage file got into the directory
                                // remove it.
                                deleted = ACARSSoundingTools.deleteFile(f, logger, "Attempt to delete ill-formed file %s failed");
                            }
                        }
                    } catch (Throwable t) {
                        deleted = ACARSSoundingTools.deleteFile(f, logger, "Attempt to delete %s failed");
                    }
                    if (deleted) {
                        deletedFiles++;
                    }
                } // for
                logger.info(String.format(END_FMT, deletedFiles));
            }
        }
    }
    
    /**
     * Age out any old data. 
     * @param obs
     * @return
     */
    private List<String> processFile(File tailNumberFile) {
        long cTime = ACARSSoundingTools.getCutoffTime(FILE_CLEANUP_CUTOFF);
        
        Set<String> uris = new HashSet<String>();
        ArrayList<String> newList = new ArrayList<String>();
        BufferedReader bf = null;
        try {
            bf = new BufferedReader(new FileReader(tailNumberFile));
            String s = null;
            while ((s = bf.readLine()) != null) {
                try {
                    // Check for possible duplicates
                    if (s != null) {
                        s = ACARSSoundingTools.removeTrailingSpace(s);
                        if (uris.contains(s)) {
                            if (logger.isDebugEnabled()) {
                                logger.debug("Removing duplicate ["
                                        + s + "]");
                            }
                        } else {
                            uris.add(s);
                            if (s.length() > ACARSSoundingTools.OBS_TIME_SIZE) {
                                long oTime = ACARSSoundingTools.parseLong(s.substring(0,20), 0);
                                if (oTime > 0) {
                                    if (logger.isDebugEnabled()) {
                                        logger.debug("Ill-formed datauri entry ["
                                                + s + "]");
                                    }
                                    if (oTime > cTime) {
                                        newList.add(s);
                                    } else {
                                        if (logger.isDebugEnabled()) {
                                            logger.debug("Aging out datauri [" + s
                                                    + "]");
                                        }
                                    }
                                }
                            }
                        }
                    }
                } catch (Exception e) {
                    logger.error("Exception in processList",e);
                    // nothing else. The URI will go away.
                }
            }
        } catch (IOException ioe) {
            logger.error("Error reading data for tailnumber "
                    + tailNumberFile.getName());
        } finally {
            if (bf != null) {
                try {
                    bf.close();
                } catch (IOException ioe) {
                    logger.error("Error closing tailnumber file "
                            + tailNumberFile.getName());
                }
            }
        }
        return newList;
    }

    /**
     * Set up file resources
     */
    private void setupFiles() {
        failSafe = true;
        try {
            PathManager pathMgr = (PathManager) PathManagerFactory.getPathManager();

            LocalizationContext ctx = pathMgr.getContext(
                    LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);

            baseDir = pathMgr.getFile(ctx, ACARSSoundingTools.BASE_PATH
                    + ACARSSoundingTools.RAW_PATH);

            dataDir = pathMgr.getFile(ctx, ACARSSoundingTools.BASE_PATH
                    + ACARSSoundingTools.DATA_PATH);

            if(baseDir != null) {
                if (!baseDir.exists()) {
                    baseDir.mkdirs();
                }
                if(baseDir.exists()) {
                    if(dataDir != null) {
                        if (!dataDir.exists()) {
                            dataDir.mkdirs();
                        }
                        if(dataDir.exists()) {
                            // Ok, resources exist
                            failSafe = false;
                        } else {
                            failReason = NO_DATA_DIR;
                        }
                    } else {
                        failReason = DATA_DIR_NULL;
                    }
                } else {
                    failReason = NO_BASE_DIR;
                }
            } else {
                failReason = BASE_DIR_NULL;
            }
        } catch (Exception e) {
            logger.error("Attempting to setup files",e);
        }
    }

    /**
     * 
     * @param s
     * @return
     */
    private String[] splitBuilder(String s) {
        String[] parts = null;
        if ((s != null) && (s.length() > 60)) {
            parts = new String[3];

            parts[0] = s.substring(0, 2);
            parts[1] = s.substring(3, 23).trim();
            parts[2] = s.substring(24).trim();
        }
        return parts;
    }

    /**
     * Performs a conditional delete of the specified file based on the
     * timestamp of the filename.
     * 
     * @param file
     *            Target file to delete.
     * @param logger
     *            Logger to receive possible error messages.
     */
    private static void deleteFile(File file, Log logger) {
        if ((file != null) && (file.exists())) {
            if (file.isFile()) {
                // Now that we've processed out this file, see if it
                // was from the past hour. If so we can delete it.
                String dateTail = file.getName().substring(9);
                int year = Integer.parseInt(dateTail.substring(0, 4));
                int month = Integer.parseInt(dateTail.substring(4, 6));
                int day = Integer.parseInt(dateTail.substring(6, 8));
                int hour = Integer.parseInt(dateTail.substring(8, 10));
                Calendar c = TimeUtil.newGmtCalendar(year, month, day);
                c.set(Calendar.HOUR_OF_DAY, hour);
                Calendar cCurr = TimeTools.copyToNearestHour(TimeUtil
                        .newGmtCalendar());
                if (c.getTimeInMillis() < cCurr.getTimeInMillis()) {
                    if (!file.delete()) {
                        // We come here to log a delete failure.
                        logger.error("Could delete acars intermediate "
                                + file.getName());
                    }
                }
            }
        }
    }

    /**
     * Factory method to return a separator to the client.
     * 
     * @param rawMessage
     * @return
     */
    public ACARSSoundingSplitter getSeparator() {
        ACARSSoundingSplitter splitter = new ACARSSoundingSplitter(CREATE_PROCESS_BEAN, failSafe); 
        
        return splitter;
    }

}
