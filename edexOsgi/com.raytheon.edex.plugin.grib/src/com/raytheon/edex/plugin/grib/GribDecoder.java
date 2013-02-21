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
import java.io.IOException;
import java.util.Map;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

import ucar.grib.GribChecker;
import ucar.unidata.io.RandomAccessFile;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
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
 * 02/12/2013   1615       bgonzale     public decode method to a Processor exchange method.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GribDecoder implements Processor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GribDecoder.class);

    /**
     * @see org.apache.camel.Processor.process(Exchange)
     */
    @Override
    public void process(Exchange exchange) throws Exception {
        final String DATA_TYPE = "dataType";
        final String GRIB = "grib";

        File file = (File) exchange.getIn().getBody();
        Map<String, Object> headers = exchange.getIn().getHeaders();

        RandomAccessFile raf = null;
        int edition = 0;
        GridRecord[] records = null;
        try {
            raf = new RandomAccessFile(file.getAbsolutePath(), "r");
            raf.order(RandomAccessFile.BIG_ENDIAN);
            edition = GribChecker.getEdition(raf);
            exchange.getIn().setHeader(DATA_TYPE, GRIB + edition);

            switch (edition) {
            case 1:
                records = new Grib1Decoder().decode(file.getAbsolutePath());
                break;
            case 2:
                records = decodeGrib2(file);
                break;
            default:
                throw new GribException("Unknown grib version detected ["
                        + edition + "]");
            }

            String datasetId = (String) headers.get("datasetid");
            String secondaryId = (String) headers.get("secondaryid");
            String ensembleId = (String) headers.get("ensembleid");

            if (secondaryId != null || datasetId != null || ensembleId != null) {
                for (GridRecord record : records) {
                    if (datasetId != null) {
                        record.setDatasetId(datasetId);
                    }
                    if (secondaryId != null) {
                        record.setSecondaryId(secondaryId);
                    }
                    if (ensembleId != null) {
                        record.setEnsembleId(ensembleId);
                    }
                    record.setDataURI(null);
                    record.constructDataURI();
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, "Failed to decode file: ["
                    + file.getAbsolutePath() + "]", e);
            records = new GridRecord[0];
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
        exchange.getIn().setBody(records);
    }

    /**
     * Decode a grib 2 file.
     * 
     * @param file
     *            Grib 2 file
     * @return Array of GribRecords parsed from the file.
     * @throws PluginException
     */
    private GridRecord[] decodeGrib2(File file)
            throws PluginException {
        GridRecord[] records = null;
        PythonDecoder pythonDecoder = new PythonDecoder();

        pythonDecoder.setPluginName("grib");
        pythonDecoder.setPluginFQN("com.raytheon.edex.plugin.grib");
        pythonDecoder.setModuleName("GribDecoder");
        pythonDecoder.setRecordClassname(GridRecord.class.toString());
        pythonDecoder.setCache(true);
        try {
            PluginDataObject[] pdos = pythonDecoder.decode(file);
            records = new GridRecord[pdos.length];
            for (int i = 0; i < pdos.length; i++) {
                records[i] = (GridRecord) pdos[i];
            }
        } catch (Exception e) {
            throw new GribException("Error decoding grib file!", e);
        }
        return records;
    }
}
