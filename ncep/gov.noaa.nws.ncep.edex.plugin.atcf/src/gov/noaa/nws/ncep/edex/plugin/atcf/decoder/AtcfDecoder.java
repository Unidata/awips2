/**
 *
 * AtcfDecoder
 * 
 * Decoder Plug-In for Automated Tropical Cyclone Forecast ATCF.
 * 
 * 12 December 2008
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 */

package gov.noaa.nws.ncep.edex.plugin.atcf.decoder;

import gov.noaa.nws.ncep.common.dataplugin.atcf.AtcfRecord;
import gov.noaa.nws.ncep.edex.plugin.atcf.util.AtcfParser;

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;

/**
 * 
 * AtcfDecoder
 * 
 * Decoder implementation for ATCF Plug-In
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/23/10		208			F. J. Yen	Initial creation
 * 
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 * 
 */

public class AtcfDecoder extends AbstractDecoder {
    // Name of the plugin controlling this decoder.
    public final String pluginName;

    protected Matcher regexMatcher;

    protected Pattern regexPattern;

    public Calendar observTime = null;

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public AtcfDecoder(String name) throws DecoderException {
        pluginName = name;
    }

    public synchronized PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {
        String traceId = "";
        String theBulletin = null;
        byte[] messageData = null;
        // ATCF_DATA is REGEX for a ATCF record
        final String ATCF_DATA = "(WP|IO|SH|CP|EP|AL), +\\d{1,2}, +\\d{10}, +\\d{1,2}, +\\w{1,4}, +(-|\\d)\\d{0,2}, +\\d{1,3}(N|S), +\\d{1,4}(E|W), +.*?\\x0a";
        final Pattern atcfDataPattern = Pattern.compile(ATCF_DATA);
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }
        /*
         * Check if there are more records
         */
        AtcfSeparator sep = AtcfSeparator.separate(data, headers);
        messageData = sep.next();
        String theMessage = new String(messageData);
        theBulletin = theMessage;
        AtcfRecord record = null;
        try {
            Matcher atcfDataMatcher = atcfDataPattern.matcher(theBulletin);

            if (atcfDataMatcher.find()) {
                // System.out.println("ATCF--atcfDataMatcher found");
                record = new AtcfRecord();
                /*
                 * Set report type in record.
                 */
                record.setReportType(pluginName);
            } else {
                System.out.println("ATCF WARNING:  Ignored invalid record:  "
                        + theBulletin);
            }
        } catch (Exception e) {
            System.out.println("ATCF WARNING exception:  Unable to decode:  "
                    + theBulletin);
            e.printStackTrace();
        }

        /*
         * Process the ATCF fields
         */
        record = AtcfParser.processFields(theBulletin);

        /*
         * Check the ATCF record object
         */
        if (record != null) {
            try {
                record.setTraceId(traceId);
                record.setPluginName(pluginName);
                record.constructDataURI();
            } catch (PluginException e) {
                throw new DecoderException(
                        "ATCF WARNING:  Unable to construct dataURI--exception:  ",
                        e);
            }
        }
        /*
         * Return ATCF record object if not null
         */
        if (record == null) {
            return new PluginDataObject[0];
        } else {
            return new PluginDataObject[] { record };
        }
    }
}
