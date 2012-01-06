/**
 * H5ScdDecoder
 * 
 * This java class decodes SCD (Supplementary Climatological Data).
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 11/2008		41			T. Lee		Initial coding
 * 04/2009		41			T. Lee		Migrated to TO10
 * 05/2009	 	41			T. Lee		Set report type
 * 07/2009		41			T. Lee		Migrated to TO11
 * 11/2009      41			T. Lee		Migrated to TO11D6
 * 06/2011		41			F. J. Yen	Renamed SCD and converted to HDF5

 * </pre>
 * 
 * @author T.Lee
 * @version 1.0
 * 
 */

package gov.noaa.nws.ncep.edex.plugin.h5scd.decoder;

import gov.noaa.nws.ncep.common.dataplugin.h5scd.H5ScdRecord;
import gov.noaa.nws.ncep.edex.plugin.h5scd.util.H5ScdParser;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.Calendar;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

public class H5ScdDecoder extends AbstractDecoder {
    private static String pluginName;

    private H5ScdRecord record;

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public H5ScdDecoder(String name) throws DecoderException {
        pluginName = name;
    }

    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {
        byte[] messageData = null;
        String traceId = null;
        WMOHeader hd;
        H5ScdSeparator sep = H5ScdSeparator.separate(data, headers);

        /*
         * Check if there are more bulletins.
         */
        if (sep.hasNext()) {
            messageData = sep.next();
        } else {
            throw new DecoderException("Out of data");
        }
        String theMessage = new String(messageData);
        record = new H5ScdRecord();

        /*
         * Set issuance time.
         */
        hd = new WMOHeader(messageData);
        Calendar cal = null;
        Calendar issueTime = UtilN.findDataTime(hd.getYYGGgg(), cal);
        record.setIssueTime(issueTime);
        record.setReportType("H5SCD");

        /*
         * Parse SCD report.
         */
        H5ScdParser.processH5Scd(theMessage, record);

        /*
         * Return the H5ScdRecord record object.
         */
        if (record != null) {
            try {
                if (headers != null) {
                    traceId = (String) headers.get("traceId");
                }
                record.setTraceId(traceId);
                record.setPluginName(pluginName);
                record.constructDataURI();
            } catch (PluginException e) {
                throw new DecoderException("Unable to construct dataURI", e);
            }
        }

        /*
         * Return H5SCD record object.
         */
        if (record == null) {
            return new PluginDataObject[0];
        }
        return new PluginDataObject[] { record };
    }
}
