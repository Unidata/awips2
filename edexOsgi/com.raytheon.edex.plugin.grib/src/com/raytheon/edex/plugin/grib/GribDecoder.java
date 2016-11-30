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

import java.util.Map;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Generic decoder for decoding grib files
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mat 12, 2010  4758     bphillip    Initial creation
 * Feb 12, 2013  1615     bgonzale    public decode method to a Processor
 *                                    exchange method.
 * Mar 19, 2013  1785     bgonzale    Added performance status handler and
 *                                    added status to process.
 * Oct 07, 2013  2402     bsteffen    Decode GribDecodeMessage instead of
 *                                    files.
 * Sep 14, 2015  4868     rjpeter     Added logging of file being decoded.
 * Aug 03, 2016  ----     mjames      Removed logging.
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class GribDecoder implements Processor {

    /**
     * @see org.apache.camel.Processor.process(Exchange)
     */
    @Override
    public void process(Exchange exchange) throws GribException {
        Map<String, Object> headers = exchange.getIn().getHeaders();
        GribDecodeMessage inMessage = (GribDecodeMessage) exchange.getIn()
                .getBody();
        byte gribEdition = inMessage.getGribEdition();
        exchange.getIn().setHeader("dataType", "grib" + gribEdition);

        ITimer timer = TimeUtil.getTimer();
        GridRecord[] records = null;
        timer.start();
        switch (gribEdition) {
        case 1:
            records = new Grib1Decoder().decode(inMessage);
            break;
        case 2:
            records = new Grib2Decoder().decode(inMessage);
            break;
        default:
            throw new GribException("Unknown grib version detected ["
                    + gribEdition + "] in file: [" + inMessage.getFileName()
                    + "]");
        }

        String datasetId = (String) headers.get("datasetid");
        String secondaryId = (String) headers.get("secondaryid");
        String ensembleId = (String) headers.get("ensembleid");

        if ((secondaryId != null) || (datasetId != null)
                || (ensembleId != null)) {
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
            }
        }
        timer.stop();
        exchange.getIn().setBody(records);

    }

}
