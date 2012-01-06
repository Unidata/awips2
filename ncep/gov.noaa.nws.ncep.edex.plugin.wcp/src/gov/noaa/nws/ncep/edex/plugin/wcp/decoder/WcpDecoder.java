/**
 *
 * WcpDecoder
 * 
 * Decoder Plug-In for Watch Corner Point WCP.
 * 
 * 12 December 2008
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 */

package gov.noaa.nws.ncep.edex.plugin.wcp.decoder;

import gov.noaa.nws.ncep.common.dataplugin.wcp.WcpRecord;
import gov.noaa.nws.ncep.edex.plugin.wcp.util.WcpParser;

import java.util.Calendar;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * 
 * WcpDecoder
 * 
 * Decoder implementation for WCP Plug-In
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12Dec2008		37		F. J. Yen	Initial creation
 * 17Apr2009		37		F. J. Yen	Refactored for TO10 and to allow for more unit testing
 * 24Aug2009		37		F. J. Yen	Modified for TO11 migration
 * 10Dec2009		37		F. J. Yen	Modified for To11d6
 * 
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 * 
 */

public class WcpDecoder extends AbstractDecoder {
    // Name of the plugin controlling this decoder.
    public final String pluginName;

    protected Matcher regexMatcher;

    protected Pattern regexPattern;

    public final Integer maxSegment = 100;

    public Integer segmentIndex = 0;

    public Calendar createdTime = null;

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public WcpDecoder(String name) throws DecoderException {
        pluginName = name;
    }

    public PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {

        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }
        /*
         * SEVRRPRT_EXP is the regular expression for the remainder of the
         * string for the "SEVR" type of report.
         */
        final String SEVRRPRT_EXP = "(\\d{6}) (\\d{4}) W(T|S)(\\d{4}) (\\d{4})\\x0d\\x0d\\x0a";
        final Pattern sevrrptPattern = Pattern.compile(SEVRRPRT_EXP);
        /*
         * NOWATCH_EXP is the regular expression for the remainder of the string
         * for the "NO WATCHES CURRENTLY ACTIVE" report.
         */
        final String NOWATCH_EXP = "CURRENTLY ACTIVE";
        final Pattern nowatchPattern = Pattern.compile(NOWATCH_EXP);
        String theBulletin = null;
        byte[] messageData = null;
        /*
         * The bulletin delimeter is etx which is "\003" or ^c;
         */
        String etx = IDecoderConstants.ETX;
        /*
         * Check if there are more bulletins
         */
        WcpSeparator sep = WcpSeparator.separate(data, headers);
        messageData = sep.next();
        String theMessage = new String(messageData);
        /*
         * There may be multiple duplicate bulletins. Only get the first
         * bulletin and eliminate the remaining bulletins after the first
         * bulletin by excluding the duplicate report after the first ^c
         */
        Scanner cc = new Scanner(theMessage).useDelimiter(etx);
        if (cc.hasNext()) {
            theBulletin = cc.next();
        } else {
            theBulletin = theMessage;
        }
        WcpRecord record = null;
        record = new WcpRecord();
        /*
         * Replace white spaces with blank
         */
        record.setBullMessage((theBulletin.substring(5)).replace('\r', ' ')
                .replace('\003', ' ').replace('\000', ' ').replace('\001', ' '));
        /*
         * Decode the File Created time line
         */
        createdTime = WcpParser.processFileCreatedDate(theBulletin, record);
        /*
         * Decode the WMO line
         */
        WcpParser.processWMO(theBulletin, createdTime, record);
        /*
         * Set report type
         */
        record.setReportType(pluginName);
        /*
         * Check the WCP record object
         */
        if (record != null) {
            try {
                record.setTraceId(traceId);
                record.setPluginName(pluginName);
                record.constructDataURI();
            } catch (PluginException e) {
                throw new DecoderException("Unable to construct dataURI", e);
            }
        }
        /*
         * Break up bulletin into segments with possible delimiters for both
         * types of records.
         */
        String segmentDelim = "((SEVR )|(NO WATCHES ))";
        Scanner sc = new Scanner(theBulletin).useDelimiter(segmentDelim);
        String[] segment = new String[maxSegment];
        int segmentIndex = 0;
        while (sc.hasNext() && (segmentIndex < maxSegment)) {
            segment[segmentIndex] = sc.next();
            segmentIndex++;
        }
        try {
            for (int idx = 0; (idx < segmentIndex) && (idx < maxSegment); idx++) {
                Matcher sevrMatcher = sevrrptPattern.matcher(segment[idx]);
                Matcher nowatchMatcher = nowatchPattern.matcher(segment[idx]);
                if (sevrMatcher.find()) {
                    /*
                     * Have a SEVR report, so process it
                     */
                    WcpParser.processSevr(segment[idx], record);
                } else if (nowatchMatcher.find()) {
                    /*
                     * Have "NO WATCHES CURRENTLY ACTIVE" report. Decoding for
                     * this type (NA) is no longer necessary. So no further
                     * processing.
                     */
                }
            }
        } catch (Exception e) {
            throw new DecoderException(
                    "Unable to decode WCP record due to formatting errors", e);
        }
        /*
         * Return record object if not null
         */
        if (record == null) {
            return new PluginDataObject[0];
        } else {
            return new PluginDataObject[] { record };
        }
    }
}
