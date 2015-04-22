package gov.noaa.nws.ncep.edex.plugin.ncscd.decoder;

import gov.noaa.nws.ncep.common.dataplugin.ncscd.NcScdRecord;
import gov.noaa.nws.ncep.edex.plugin.ncscd.util.NcScdParser;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.Calendar;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * NcScdDecoder
 * 
 * This java class decodes SCD (Supplementary Climatological Data).
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket# Engineer  Description
 * ------------ ------- --------- -----------
 * 11/2008      41      T. Lee    Initial coding
 * 04/2009      41      T. Lee    Migrated to TO10
 * 05/2009      41      T. Lee    Set report type
 * 07/2009      41      T. Lee    Migrated to TO1 1
 * 11/2009      41      T. Lee    Migrated to TO11D6
 * 06/2011      41      F. J. Yen Renamed SCD and converted to HDF5
 * 09/2011      457     S. Gurung Renamed H5 to Nc and h5 to nc
 * Aug 30, 2013 2298   rjpeter    Make getPluginName abstract
 * May 14, 2014 2536   bclement   moved WMO Header to common, removed pluginName
 * </pre>
 * 
 * @author T.Lee
 * @version 1.0
 * 
 */
public class NcScdDecoder extends AbstractDecoder {

    private NcScdRecord record;

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    @Deprecated
    public NcScdDecoder(String name) throws DecoderException {
    }

    /**
     * 
     */
    public NcScdDecoder() {

    }

    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {
        byte[] messageData = null;
        String traceId = null;
        WMOHeader hd;
        NcScdSeparator sep = NcScdSeparator.separate(data, headers);

        /*
         * Check if there are more bulletins.
         */
        if (sep.hasNext()) {
            messageData = sep.next();
        } else {
            throw new DecoderException("Out of data");
        }
        String theMessage = new String(messageData);
        record = new NcScdRecord();

        /*
         * Set issuance time.
         */
        hd = new WMOHeader(messageData);
        Calendar cal = null;
        Calendar issueTime = UtilN.findDataTime(hd.getYYGGgg(), cal);
        record.setIssueTime(issueTime);
        record.setReportType("SCD");

        /*
         * Parse SCD report.
         */
        try {
            NcScdParser.processNcScd(theMessage, record);
        } catch (Exception e) {
            logger.info(String.format("%s", traceId) + e.getMessage());
            record = null;
        }

        /*
         * Return the NcScdRecord record object.
         */
        if (record != null) {
            if (headers != null) {
                traceId = (String) headers.get("traceId");
            }
            record.setTraceId(traceId);
        }

        /*
         * Return NCSCD record object.
         */
        if (record == null) {
            return new PluginDataObject[0];
        }
        return new PluginDataObject[] { record };
    }
}
