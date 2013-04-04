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
package gov.noaa.nws.ncep.edex.plugin.ncgrib;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;
import gov.noaa.nws.ncep.edex.plugin.ncgrib.util.TableTimeStamp;

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
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.python.decoder.PythonDecoder;

/**
 * Generic decoder for decoding grib files
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/12/10      4758       bphillip     Initial creation
 * 10/13/10     276        llin			Modified for NC GRIB.
 * 01/19/12                xguo         Split large files
 * 05/23/12                xguo         Split large file to each record file
 * 06/12/12     00609      djohnson     Use EDEXUtil for EDEX_HOME.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class NcgribDecoder {

	private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(NcgribDecoder.class);
	
	private static final long TEN_MEGABYTES = 10485760;
	
	private static final long FIVE_MEGABYTES = 5242880;
	
    public NcgribDecoder() {

    }

    public NcgribRecord[] decode(File file) throws GribException {

        NcgribRecord[] records = null;
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

                if (recordLengths.size() > 1) {
                    raf.seek(0);
                    splitFile(file.getName(), raf, recordLengths);
                    try {
                        if (raf != null) {
                            raf.close();
                        }
                    } catch (IOException e) {
                        throw new GribException("", e);
                    }
                    return new NcgribRecord[] {};
                }
            }
            if (edition == 1) {
                records = new Ncgrib1Decoder().decode(file.getAbsolutePath());
            } else if (edition == 2) {
            	//System.out.println("In decoder class edition 2, to update xml tables if necessary...");
            	TableTimeStamp.updateXmlTables();
                PythonDecoder pythonDecoder = new PythonDecoder();
                pythonDecoder.setPluginName("ncgrib");
                pythonDecoder.setPluginFQN("gov.noaa.nws.ncep.edex.plugin.ncgrib");
                pythonDecoder.setModuleName("NcgribDecoder");
                pythonDecoder.setRecordClassname(NcgribRecord.class.toString());
                pythonDecoder.setCache(true);
                try {             
                    PluginDataObject[] ncpdos = pythonDecoder.decode(file);
                    records = new NcgribRecord[ncpdos.length];
                    for (int i = 0; i < ncpdos.length; i++) {
                        records[i] = (NcgribRecord) ncpdos[i];
                    }
                } catch (Exception e) {
                    throw new GribException("Error decoding ncgrib file!", e);
                } 
                
            } else {
            	statusHandler.handle(Priority.ERROR,"Unknown ncgrib version detected [" + edition
                        + "]");
            }
        } catch (Exception e) {
        	statusHandler.handle(Priority.ERROR, "Failed to decode file: ["
                    + file.getAbsolutePath() + "]", e);
        	records = new NcgribRecord[0];
        } finally {
            try {
                if (raf != null) {
                    raf.close();
                }
            } catch (IOException e) {
                throw new GribException("", e);
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
            List<Long> sizes) throws IOException {
        FileOutputStream out = null;
        byte[] transfer = null;
        long rdSize=0;
        int num = 0;
        for (int i = 0; i < sizes.size(); i++) {
        	if ( rdSize == sizes.get(i).longValue() ) continue;
        	num ++;
        	rdSize = sizes.get(i).longValue();
        	transfer = new byte[(int) sizes.get(i).longValue()];
            raf.seek(seekRecordStart(raf, raf.length()));
            raf.read(transfer);

            try {
                out = new FileOutputStream(EDEXUtil.EDEX_HOME
                        + "/data/sbn/ncgrib/" + fileName + "_" + num);
                out.write(transfer);
                out.close();
            } finally {
                if (out != null) {
                    out.close();
                }
            }
            
        }
    }
    /*
     private void splitFile(String fileName, RandomAccessFile raf,
            List<Long> sizes) throws IOException {
        FileOutputStream out = null;
        byte[] transfer = null;
        long fileSize = 0,seekLoc;
        int num = 0;
        for (int i = 0; i < sizes.size(); i++) {
        	fileSize += sizes.get(i).longValue();
        	if ( (i < sizes.size() -1 ) && 
        			((fileSize + sizes.get(i+1).longValue())>FIVE_MEGABYTES)) {
        		num ++;
        		seekLoc = seekRecordStart(raf, raf.length());
        		if ( seekLoc < 0 ) return;
        		transfer = new byte[(int) fileSize];
        		raf.seek(seekLoc);
        		raf.read(transfer);

        		try {
        			out = new FileOutputStream(EDEXUtil.EDEX_HOME
                            + "/data/sbn/ncgrib/" + fileName + "_" + num);
        			out.write(transfer);
        			out.close();
        		} finally {
        			if (out != null) {
        				out.close();
        			}
        		}
        		fileSize = 0;
        	}
        }
        if ( fileSize > 0 ) {
        	num ++;
        	seekLoc = seekRecordStart(raf, raf.length());
    		if ( seekLoc < 0 ) return;
            transfer = new byte[(int) fileSize];
            raf.seek(seekLoc);
            raf.read(transfer);

            try {
                out = new FileOutputStream(EDEXUtil.EDEX_HOME
                        + "/data/sbn/ncgrib/" + fileName + "_" + num);
                out.write(transfer);
                out.close();
            } finally {
                if (out != null) {
                    out.close();
                }
            }
        }
    }
     */
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
