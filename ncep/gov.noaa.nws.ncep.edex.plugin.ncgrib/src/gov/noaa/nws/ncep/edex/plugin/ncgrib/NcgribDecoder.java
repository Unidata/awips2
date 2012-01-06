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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import ucar.grib.GribChecker;
import ucar.unidata.io.RandomAccessFile;

import com.raytheon.uf.common.dataplugin.PluginDataObject;

import com.raytheon.uf.edex.python.decoder.PythonDecoder;
import gov.noaa.nws.ncep.common.dataplugin.ncgrib.NcgribRecord;

import gov.noaa.nws.ncep.common.dataplugin.ncgrib.exception.GribException;
import gov.noaa.nws.ncep.edex.plugin.ncgrib.util.TableTimeStamp;

/**
 * Generic decoder for decoding grib files
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/12/10      4758       bphillip     Initial creation
 * 10/13/10     276        llin			Modified for NC GRIB.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class NcgribDecoder {

    public NcgribDecoder() {

    }

    public NcgribRecord[] decode(File file) throws GribException {

        NcgribRecord[] records = null;
        RandomAccessFile raf = null;
        int edition = 0;
        try {
            raf = new RandomAccessFile(file.getAbsolutePath(), "r");
            if (raf == null){
                throw new FileNotFoundException("Grib file does not exist ["+file.getAbsolutePath()+"]");
            }
            raf.order(RandomAccessFile.BIG_ENDIAN);
            edition = GribChecker.getEdition(raf);
        } catch (IOException e) {
            throw new GribException("Error checking ncgrib version", e);
        } finally {
            try {
                if (raf != null) {
                    raf.close();
                }
            } catch (IOException e) {
                throw new GribException("", e);
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
            throw new GribException("Unknown ncgrib version detected [" + edition
                    + "]");
        }

        return records;

    }
}
