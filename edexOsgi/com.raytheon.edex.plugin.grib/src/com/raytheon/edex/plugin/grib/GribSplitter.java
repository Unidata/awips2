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
package com.raytheon.edex.plugin.grib;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import ucar.grib.GribChecker;
import ucar.grib.grib1.Grib1Input;
import ucar.grib.grib1.Grib1Record;
import ucar.grib.grib2.Grib2IndicatorSection;
import ucar.grib.grib2.Grib2Input;
import ucar.grib.grib2.Grib2Record;
import ucar.unidata.io.RandomAccessFile;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EdexException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GribSplitter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribSplitter.class);

    private static final long TEN_MEGABYTES = 10485760;

    private static final String suffixFormat = "%s_record_%d";

    private static final Pattern suffixPattern = Pattern
            .compile(".*_record_\\d+$");

    private final File tmpFileDirectory;

    public GribSplitter(String tmpFileLocation) {
        this.tmpFileDirectory = new File(tmpFileLocation);
        if (!tmpFileDirectory.exists()) {
            tmpFileDirectory.mkdirs();
        }
    }

    public void clean(Headers headers) {
        String ingestFileName = (String) headers.get("ingestfilename");
        File file = new File(ingestFileName);
        if (tmpFileDirectory.equals(file.getParentFile())
                && suffixPattern.matcher(file.getName()).find()) {
            file.delete();
        }
    }

    public List<String> split(String fileName) {
        File file = new File(fileName);
        if (file.length() > TEN_MEGABYTES) {
            RandomAccessFile raf = null;
            try {
                raf = new RandomAccessFile(file.getAbsolutePath(), "r");
                raf.order(RandomAccessFile.BIG_ENDIAN);
                int edition = GribChecker.getEdition(raf);
                raf.seek(0);
                List<Long> recordLengths = new ArrayList<Long>();
                if (edition == 1) {
                    Grib1Input g1i = new Grib1Input(raf);
                    g1i.scan(false, false);
                    List<Grib1Record> gribRecords = g1i.getRecords();
                    for (int i = 0; i < gribRecords.size(); i++) {
                        recordLengths.add(gribRecords.get(i).getIs()
                                .getGribLength());
                    }
                } else if (edition == 2) {
                    Grib2Input g2i = new Grib2Input(raf);

                    g2i.scan(false, false);
                    List<Grib2Record> gribRecords = g2i.getRecords();

                    Grib2IndicatorSection lastIs = null;
                    for (int i = 0; i < gribRecords.size(); i++) {
                        // 2 records with the same indicator section cannot be
                        // split, this occurs with uW and vW that are encoded
                        // together.
                        Grib2IndicatorSection is = gribRecords.get(i).getIs();
                        if (lastIs != is) {
                            lastIs = is;
                            recordLengths.add(is.getGribLength());
                        }
                    }
                    // If there was more than one grib record in this file, we
                    // split the file up into individual records and send them
                    // back through the manual ingest endpoint
                    if (recordLengths.size() > 1) {
                        raf.seek(0);
                        return splitFile(file.getName(), raf, recordLengths);
                    }
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Error splitting grib file", e);
            } finally {
                try {
                    if (raf != null) {
                        raf.close();
                    }
                } catch (IOException e) {
                    statusHandler.handle(Priority.ERROR,
                            "Unable to close RandomAccessFile!", e);
                }
            }
        }
        return Arrays.asList(file.getAbsolutePath());
    }

    /**
     * Splits a collective file into individual records.
     * 
     * @param fileName
     *            The name of the file being split
     * @param raf
     *            The Random Access File object
     * @param sizes
     *            The sizes of the individual records inside the collection
     * @throws IOException
     * @throws EdexException
     */
    private List<String> splitFile(String fileName, RandomAccessFile raf,
            List<Long> sizes) throws IOException, EdexException {
        FileOutputStream out = null;
        byte[] transfer = null;
        List<String> result = new ArrayList<String>(sizes.size());
        for (int i = 0; i < sizes.size(); i++) {
            transfer = new byte[(int) sizes.get(i).longValue()];
            raf.seek(seekRecordStart(raf, raf.length()));
            raf.read(transfer);

            try {

                File tmpFile = new File(tmpFileDirectory, String.format(
                        suffixFormat, fileName, i));
                out = new FileOutputStream(tmpFile);
                out.write(transfer);
                out.close();
                result.add(tmpFile.getPath());
            } finally {
                if (out != null) {
                    out.close();
                }
            }
        }
        return result;
    }

    /**
     * Moves the filepointer on the random access file to the beginning of the
     * next grib record in the file
     * 
     * @param raf
     *            The random access file
     * @param fileLength
     *            The total length of the file
     * @return The index to the next grib record in the collection. -1 is
     *         returned if there are no more records in the file
     * @throws IOException
     */
    private long seekRecordStart(RandomAccessFile raf, long fileLength)
            throws IOException {
        int matches = 0;
        while (raf.getFilePointer() < fileLength) {
            char c = (char) raf.readByte();
            if (c == 'G') {
                matches = 1;
            } else if ((c == 'R') && (matches == 1)) {
                matches = 2;
            } else if ((c == 'I') && (matches == 2)) {
                matches = 3;
            } else if ((c == 'B') && (matches == 3)) {
                matches = 4;
                // Subtract 4 because we want the absolute beginning of the grib
                // file
                return raf.getFilePointer() - 4;
            } else {
                matches = 0;
            }
        }
        return -1;
    }
}
