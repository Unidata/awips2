package gov.noaa.nws.ncep.edex.plugin.idft.decoder;

import gov.noaa.nws.ncep.common.dataplugin.idft.IdftRecord;
import gov.noaa.nws.ncep.edex.plugin.idft.util.IdftParser;
import gov.noaa.nws.ncep.edex.plugin.idft.util.IdftUtil;

import java.util.Calendar;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.DataFormatException;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;
import com.raytheon.uf.edex.decodertools.core.IDecoderConstants;

/**
 * 
 * IdftDecoder
 * 
 * Decoder Plug-In for IDFT (Ice Drift text files).
 * 
 * This code has been developed by the SIB for use in the AWIPS II system.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ -------- ----------- --------------------------
 * 1 June 2009  100      F. J. Yen   Initial creation
 * 5 Oct  2009  100      F. J. Yen   Synchronized decode
 * 9 Dec  2009  100      F. J. Yen   Modified from to11d3 to to11d6
 * 27 May 2010  100      F. J. Yen   Migrated from to11dr3 to to11dr11
 * 27 Oct 2010  100      F. J. Yen   For migration from to11dr11 to R1G1-4,
 *                                   added forecast time.
 * Aug 30, 2013 2298     rjpeter     Make getPluginName abstract
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 */
public class IdftDecoder extends AbstractDecoder {
    // Name of the plugin controlling this decoder.
    public static String pluginName;

    protected String traceId = "";

    private IdftRecord record;

    public int kkk = 0;

    public Calendar valTime = null;

    protected Matcher regexMatcher;

    protected Pattern regexPattern;

    /**
     * Constructor
     * 
     * @throws DecoderException
     * @throws Exception
     */
    public IdftDecoder(String name) throws DecoderException, Exception {
        pluginName = name;
        IdftParser.readIdftLocs();

    }

    // public PluginDataObject[] decode(byte[] data) throws DecoderException {
    public synchronized PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {
        // TODO Auto-generated method stub

        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }

        String theBulletin = null;
        byte[] messageData = null;
        WMOHeader hd;
        final String IDFT_DATALN = "(\\d{1,3}) +(\\d{1,3}) +(\\d{1,3})\\.(\\d) \\r\\r\\n";
        final Pattern dataLnPattern = Pattern.compile(IDFT_DATALN);
        final String IDFT_DATALN2 = "(\\d{1,4}) +(\\d{0,2})\\.(\\d)(N|S) +(\\d{0,3})\\.(\\d)(W|E) +(\\d{1,3}) +(\\d{1,4})\\.(\\d)\\r\\r\\n";
        final Pattern dataLnPattern2 = Pattern.compile(IDFT_DATALN2);

        Calendar issueTime;

        /*
         * The bulletin delimeter is etx which is "\003" or ^c;
         */
        String etx = IDecoderConstants.ETX;
        /*
         * Check if there are more bulletins
         */
        IdftSeparator sep = IdftSeparator.separate(data, headers);
        if (sep.hasNext()) {
            messageData = sep.next();
        } else {
            throw new DecoderException("Out of data");
        }
        String theMessage = new String(messageData);
        record = new IdftRecord();
        /*
         * Exclude the duplicate report after the first ^c
         */
        Scanner cc = new Scanner(theMessage).useDelimiter(etx);
        if (cc.hasNext()) {
            theBulletin = cc.next();
        } else {
            theBulletin = theMessage;
        }
        /*
         * Process the valid time string and set validTime
         */
        valTime = IdftUtil.processValidTime(theBulletin, record);
        if (valTime != null) {
            record.setValidTime(valTime);
            /*
             * Parse the issue time and set issueTime.
             */
            hd = new WMOHeader(messageData);
            try {
                issueTime = Util.findCurrentTime(hd.getYYGGgg());
                record.setIssueTime(issueTime);
                /*
                 * Put the issue time in the second field of the DataURI using
                 * setDataTime. Calculate the forecast time in seconds and set
                 * it.
                 */
                int fcstSecs = (int) (valTime.getTime().getTime() / 1000)
                        - (int) (issueTime.getTime().getTime() / 1000);
                DataTime dataTime = new DataTime(issueTime, fcstSecs);
                record.setDataTime(dataTime);
            } catch (DataFormatException e) {
                System.out.println("Unable to getYYGGgg");
                e.printStackTrace();
            }
        }
        /*
         * Parse IDFT report.
         */
        try {
            /*
             * m for matching when there is no lat/lon in record m2 for matching
             * when lat/lon is in record
             */
            Matcher m = dataLnPattern.matcher(theBulletin);
            Matcher m2 = dataLnPattern2.matcher(theBulletin);
            if (m.find()) {
                IdftParser.processIdft(m, 0, record);
            } else if (m2.find()) {
                IdftParser.processIdft(m2, 6, record);
            }
        } catch (Exception e) {
            System.out.println("Invalid IDFT point record format");
            e.printStackTrace();
        }
        /*
         * Set report type
         */
        record.setReportType(pluginName);
        if (record != null) {
            try {
                record.setTraceId(traceId);
                record.constructDataURI();
            } catch (PluginException e) {
                throw new DecoderException("Unable to construct dataURI", e);
            }
        }
        /*
         * Return IDFT record object if not null
         */
        if (record == null) {
            return new PluginDataObject[0];
        } else {
            return new PluginDataObject[] { record };
        }
    }
}
