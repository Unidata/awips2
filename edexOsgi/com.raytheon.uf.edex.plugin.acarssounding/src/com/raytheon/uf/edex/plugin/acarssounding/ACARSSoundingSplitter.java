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
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.acars.ACARSRecord;
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
 * Aug 18, 2014 3530       bclement    removed rest of TimeTools usage
 * Dec 10, 2015 5166       kbisanz     Update logging to use SLF4J
 * Jul 26, 2016 5757       nabowle     Move processing out of localization structure.
 * Aug 09, 2016 5757       nabowle     Cleanup code. Splitting is stateless.
 *
 * </pre>
 *
 * @author jkorman
 */

public class ACARSSoundingSplitter {

    private Logger logger = LoggerFactory.getLogger(getClass());

    /** Cutoff time for cleaning files. */
    private static final int FILE_CLEANUP_CUTOFF = 7;

    /**
     * Constructor.
     */
    public ACARSSoundingSplitter() {
        super();
    }

    /**
     * Factory method to return a separator to the client.
     *
     * @param rawMessage
     * @return
     */
    public Iterator<ACARSAircraftInfo> getSeparator() {
        File baseDir = ACARSSoundingTools
                .getInitializedDirectory(ACARSSoundingTools.RAW_PATH);
        File dataDir = ACARSSoundingTools
                .getInitializedDirectory(ACARSSoundingTools.DATA_PATH);

        if (baseDir == null || dataDir == null) {
            return Collections.emptyIterator();
        }

        cleanUpAcftObs(dataDir);
        return getACARSData(baseDir, dataDir);
    }

    /**
     * Sorts recent acars data into the tailnumbers files, and returns an
     * iterator over the ACARSAircraftInfos.
     */
    private Iterator<ACARSAircraftInfo> getACARSData(File baseDir, File dataDir) {
        long startTime = System.currentTimeMillis();

        // Get a list of all files in the rawData directory
        List<ACARSRecord> recs = getRecordsFromFiles(baseDir);
        if (recs.isEmpty()) {
            return Collections.emptyIterator();
        }

        Map<String, Map<String, ACARSRecord>> tNumbers = createTailNumberMap(recs);

        // Create a list of sorted unique tailnumbers to process.
        List<String> tailNumbers = new ArrayList<>(tNumbers.keySet());

        List<ACARSAircraftInfo> acftInfo = new ArrayList<>(tailNumbers.size());
        for (String tailNumber : tailNumbers) {
            Map<String, ACARSRecord> recMap = tNumbers.get(tailNumber);
            if (recMap == null || recMap.isEmpty()) {
                continue;
            }
            /*
             * We have a tailnumber and some number of of observations that we
             * need to write out.
             */
            File out = new File(dataDir, tailNumber + ".acft");

            List<ACARSRecord> aList = new ArrayList<>(recMap.values());

            // Combine any existing data for this tailnumber with the new data.
            combineLists(out, aList);

            try (FileWriter writer = new FileWriter(out,
                    ACARSSoundingTools.NO_APPEND)) {
                /*
                 * We've combined and sorted all the data, so overwrite the
                 * existing file.
                 */
                long startObs = Long.MAX_VALUE;
                long stopObs = Long.MIN_VALUE;

                // write out each datauri for this tail number.
                for (ACARSRecord r : aList) {
                    long obsTime = r.getTimeObs().getTimeInMillis();
                    startObs = Math.min(startObs, obsTime);
                    stopObs = Math.max(stopObs, obsTime);

                    writer.write(String.format(ACARSSoundingTools.OBS_FMT,
                            obsTime, r.getDataURI()));
                }
                /*
                 * Get the full filepath to the tail number file and put it in
                 * the aircraft info
                 */
                ACARSAircraftInfo info = new ACARSAircraftInfo(tailNumber,
                        out.getCanonicalPath());
                info.setStartTime(startObs);
                info.setStopTime(stopObs);

                acftInfo.add(info);
            } catch (IOException ioe) {
                logger.error("Error rewriting aircraft file.", ioe);
            }
        }

        logger.info("Elapsed time = "
                + (System.currentTimeMillis() - startTime));

        return acftInfo.iterator();
    }

    /**
     * Create a map for tailNumbers --> map(uri->record).
     *
     * @param recs
     * @return
     */
    public Map<String, Map<String, ACARSRecord>> createTailNumberMap(
            List<ACARSRecord> recs) {
        Map<String, Map<String, ACARSRecord>> tNumbers = new TreeMap<>();
        for (ACARSRecord r : recs) {
            Map<String, ACARSRecord> recMap = null;
            recMap = tNumbers.get(r.getTailNumber());
            if (recMap == null) {
                recMap = new TreeMap<>();
                tNumbers.put(r.getTailNumber(), recMap);
            }
            String uri = r.getDataURI();
            if (!recMap.containsKey(uri)) {
                recMap.put(uri, r);
            }
        }
        return tNumbers;
    }

    /**
     * @param baseDir
     * @return
     */
    public List<ACARSRecord> getRecordsFromFiles(File baseDir) {
        List<ACARSRecord> recs = new ArrayList<>();
        File[] files = ACARSSoundingTools.getDataFiles(baseDir);
        for (File f : files) {
            boolean forceDelete = false;
            try (RandomAccessFile raf = new RandomAccessFile(f, "rw")) {
                String s;
                long pos = raf.getFilePointer();
                /*
                 * Read data from the file, enter any new data into the records
                 * list.
                 */
                while ((s = raf.readLine()) != null) {
                    long currPos = raf.getFilePointer();
                    // Check for new data only
                    if (s.startsWith(ACARSSoundingTools.ACARS_NEW_OBS)) {
                        String[] parts = splitBuilder(s);
                        if (parts != null) {
                            ACARSRecord rec = new ACARSRecord(parts[2]);
                            rec.setTimeObs(TimeUtil.newGmtCalendar(new Date(
                                    Long.parseLong(parts[1]))));
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
                logger.error("Error reading acars old obs", ioe);
                forceDelete = true;
            }
            // Determine if the file needs to be deleted.
            if (forceDelete) {
                if (!f.delete()) {
                    logger.error("Could not delete file " + f.getName());
                }
            } else {
                deleteFile(f, logger);
            }
        }
        return recs;
    }

    /**
     * Combines the list of ACARSRecords with the ACARSRecords currently unused
     * in a Sounding. obs will be modified and returned sorted.
     *
     * @param f
     *            The tailnumber file.
     * @param obs
     *            The list of ACARSRecords. This list will be modified and
     *            resorted.
     */
    private void combineLists(File f, List<ACARSRecord> obs) {
        Set<String> dups = new HashSet<>(obs.size());
        for (ACARSRecord r : obs) {
            dups.add(r.getDataURI());
        }
        ACARSSoundingTools.readAircraftData(f, obs, dups, logger);
        Collections.sort(obs);
    }

    /**
     * Deletes URIs from tailnumber files that have not been used as part of a
     * sounding and are older than {@link #FILE_CLEANUP_CUTOFF} hours. If the
     * tailnumber file no longer contains any URIs, the file is deleted.
     */
    private void cleanUpAcftObs(File dataDir) {
        final String START_FMT = "ACARS aircraft info cleanup [started] on [%6d] files";
        final String END_FMT = "ACARS aircraft info cleanup [complete] deleted [%6d] files";

        int deletedFiles = 0;
        if (dataDir.isDirectory()) {
            File[] files = ACARSSoundingTools.getDataFiles(dataDir);
            logger.info(String.format(START_FMT, files.length));
            for (File f : files) {
                boolean deleted = false;
                try {
                    if (f.isFile()) {
                        String fName = f.getName();
                        if (fName.endsWith(".acft")) {
                            List<String> newList = getCurrentURIs(f);
                            if (newList.size() > 0) {
                                ACARSSoundingTools.writeAircraftData(f,
                                        newList, logger);
                            } else {
                                logger.info("Deleting empty tailnumber file "
                                        + fName);
                                // come here to delete the tailnumberfile
                                deleted = ACARSSoundingTools.deleteFile(f,
                                        logger, "Attempt to delete %s failed");
                            }
                        } else {
                            // a garbage file got into the directory remove it.
                            deleted = ACARSSoundingTools
                                    .deleteFile(f, logger,
                                            "Attempt to delete ill-formed file %s failed");
                        }
                    }
                } catch (Throwable t) {
                    deleted = ACARSSoundingTools.deleteFile(f, logger,
                            "Attempt to delete %s failed");
                }
                if (deleted) {
                    deletedFiles++;
                }
            } // for
            logger.info(String.format(END_FMT, deletedFiles));
        }
    }

    /**
     * Reads a tail number file and returns a list of the URIs that are newer
     * than {@link #FILE_CLEANUP_CUTOFF} hours.
     *
     * @param tailNumberFile
     *            The tailnumber file.
     * @return The list of current URIs from the tailnumber file.
     */
    private List<String> getCurrentURIs(File tailNumberFile) {
        long cTime = ACARSSoundingTools.getCutoffTime(FILE_CLEANUP_CUTOFF);

        Set<String> uris = new HashSet<>();
        List<String> newList = new ArrayList<>();
        try (BufferedReader bf = new BufferedReader(new FileReader(
                tailNumberFile))) {
            String s;
            while ((s = bf.readLine()) != null) {
                try {
                    // Check for possible duplicates
                    if (s != null) {
                        s = ACARSSoundingTools.removeTrailingSpace(s);
                        if (uris.contains(s)) {
                            if (logger.isDebugEnabled()) {
                                logger.debug("Removing duplicate [" + s + "]");
                            }
                        } else {
                            uris.add(s);
                            if (s.length() > ACARSSoundingTools.OBS_TIME_SIZE) {
                                long oTime = ACARSSoundingTools.parseLong(
                                        s.substring(0, 20), 0);
                                if (oTime > 0) {
                                    if (logger.isDebugEnabled()) {
                                        logger.debug("Ill-formed datauri entry ["
                                                + s + "]");
                                    }
                                    if (oTime > cTime) {
                                        newList.add(s);
                                    } else {
                                        if (logger.isDebugEnabled()) {
                                            logger.debug("Aging out datauri ["
                                                    + s + "]");
                                        }
                                    }
                                }
                            }
                        }
                    }
                } catch (Exception e) {
                    logger.error("Exception in processList", e);
                    // nothing else. The URI will go away.
                }
            }
        } catch (IOException ioe) {
            logger.error(
                    "Error reading data for tailnumber "
                            + tailNumberFile.getName(), ioe);
        }
        return newList;
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
    private static void deleteFile(File file, Logger logger) {
        if (file != null && file.isFile()) {
            /*
             * Now that we've processed out this file, see if it was from the
             * past hour. If so we can delete it.
             */
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
