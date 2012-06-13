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
import java.util.List;

import ucar.grib.GribChecker;
import ucar.grib.grib1.Grib1Input;
import ucar.grib.grib1.Grib1Record;
import ucar.grib.grib2.Grib2Input;
import ucar.grib.grib2.Grib2Record;
import ucar.unidata.io.RandomAccessFile;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataplugin.grib.exception.GribException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.python.decoder.PythonDecoder;

/**
 * Generic decoder for decoding grib files
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/12/10      4758       bphillip     Initial creation
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GribDecoder {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(GribDecoder.class);

    private static final long TEN_MEGABYTES = 10485760;

    public GribDecoder() {

    }

    public GribRecord[] decode(File file) {

        GribRecord[] records = null;
        RandomAccessFile raf = null;
        int edition = 0;
        List<Long> recordLengths = new ArrayList<Long>();
        try {
            raf = new RandomAccessFile(file.getAbsolutePath(), "r");
            raf.order(RandomAccessFile.BIG_ENDIAN);
            edition = GribChecker.getEdition(raf);

            // If the file is greater than ten megs, split it
            if (file.length() > TEN_MEGABYTES) {
                raf.seek(0);
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
                    try {
                        g2i.scan(false, false);
                    } catch (Exception e) {
                        throw new Exception(
                                "Error determining grib record count.", e);
                    }
                    List<Grib2Record> gribRecords = g2i.getRecords();

                    for (int i = 0; i < gribRecords.size(); i++) {
                        recordLengths.add(gribRecords.get(i).getIs()
                                .getGribLength());
                    }
                }

                // If there was more than one grib record in this file, we split
                // the
                // file up into individual records and send them back through
                // the
                // manual ingest endpoint
                if (recordLengths.size() > 1) {
                    raf.seek(0);
                    splitFile(file.getName(), raf, recordLengths, edition);
                    return new GribRecord[] {};
                }
            }
            if (edition == 1) {
                records = new Grib1Decoder().decode(file.getAbsolutePath());
            } else if (edition == 2) {
                PythonDecoder pythonDecoder = new PythonDecoder();
                pythonDecoder.setPluginName("grib");
                pythonDecoder.setPluginFQN("com.raytheon.edex.plugin.grib");
                pythonDecoder.setModuleName("GribDecoder");
                pythonDecoder.setRecordClassname(GribRecord.class.toString());
                pythonDecoder.setCache(true);
                try {
                    PluginDataObject[] pdos = pythonDecoder.decode(file);
                    records = new GribRecord[pdos.length];
                    for (int i = 0; i < pdos.length; i++) {
                        records[i] = (GribRecord) pdos[i];
                    }
                } catch (Exception e) {
                    throw new GribException("Error decoding grib file!", e);
                }
            } else {
                throw new GribException("Unknown grib version detected ["
                        + edition + "]");
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Failed to decode file: ["
                            + file.getAbsolutePath() + "]", e);
            records = new GribRecord[0];
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

        return records;
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
     */
    private void splitFile(String fileName, RandomAccessFile raf,
            List<Long> sizes, int edition) throws IOException {
        FileOutputStream out = null;
        byte[] transfer = null;
        for (int i = 0; i < sizes.size(); i++) {
            transfer = new byte[(int) sizes.get(i).longValue()];
            raf.seek(seekRecordStart(raf, raf.length()));
            raf.read(transfer);

            try {
                out = new FileOutputStream(System.getProperty("edex.home")
                        + "/data/manual/grib/grib" + edition + "LargeSplit/" + fileName + "_record_" + (i + 1));
                out.write(transfer);
                out.close();
            } finally {
                if (out != null) {
                    out.close();
                }
            }
        }
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
