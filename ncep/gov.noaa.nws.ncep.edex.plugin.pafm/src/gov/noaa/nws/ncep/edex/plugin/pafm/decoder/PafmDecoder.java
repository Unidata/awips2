/**
 *
 * PafmDecoder
 * 
 * Decoder implementation for Point/Area Forecast Matrices PAFM Decoder Plug-In
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 21Aug2009	126			F. J. Yen	Initial creation
 * 30Sep2009	126			F. J. Yen	Add Fire Wx Point Forecast Matrices.  
 * 										Remove setting bullMessage (since deleting)
 * 09Dec2009	126			F. J. Yen	Updated from to11d3 to to11d6
 * 01Jun2010	126			F. J. Yen	Migrated from to11dr3 to to11dr11
 * 05Jan2011	126			F. J. Yen	Migrated from R1G1-4 to R1G1-6 needed 
 * 										to synchronize decode method
 * 10Feb2011	126			F. J. Yen	Do not process Fire Weather Point Forecast
 * 										Matrices since could have different formats
 * 
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 */

package gov.noaa.nws.ncep.edex.plugin.pafm.decoder;

import gov.noaa.nws.ncep.common.dataplugin.pafm.PafmRecord;
import gov.noaa.nws.ncep.common.dataplugin.pafm.PafmUgc;
import gov.noaa.nws.ncep.edex.plugin.pafm.util.PafmParser;
import gov.noaa.nws.ncep.edex.tools.decoder.MndTime;

import java.util.ArrayList;
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

public class PafmDecoder extends AbstractDecoder {
    // Name of the plugin controlling this decoder.
    private final String pluginName;

    protected String traceId = "";

    protected Matcher regexMatcher;

    protected Pattern regexPattern;

    public final Integer maxSegment = 100;

    public Integer segmentIndex = 0;

    public Calendar mndTime = null;

    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public PafmDecoder(String name) throws DecoderException, Exception {
        pluginName = name;
        /*
         * Read zones.xml.
         */
        PafmParser.readZoneLocs();
    }

    public synchronized PluginDataObject[] decode(byte[] data, Headers headers)
            throws DecoderException {
        // Regular expression for the county table
        final String UGC_EXP = "([A-Z]{3}\\d{3})(\\-|\\>)(\\S)+\\x0d\\x0d\\x0a((\\d{6}|\\d{3})\\-(\\S)*\\x0d\\x0d\\x0a)*";
        // Pattern used for extracting the UGC line
        Pattern ugcPattern = Pattern.compile(UGC_EXP);
        // String segmentDelim ="$$";
        String segmentDelim = "\\x24\\x24";
        // The bulletin delimeter is etx which is "\003" or ^c;
        String etx = IDecoderConstants.ETX;
        int mattypeInd;
        String theBulletin = null;
        byte[] messageData = null;
        PafmRecord record = null;
        String traceId = "";
        if (headers != null) {
            traceId = (String) headers.get("traceId");
        }
        /*
         * Check if there are more bulletins
         */
        PafmSeparator sep = PafmSeparator.separate(data, headers);
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
        // Set MND (Mass News Disseminator) time string and convert it into
        // Calendar object
        MndTime mt = new MndTime(theBulletin.getBytes());
        mndTime = mt.getMndTime();
        // Decode the WMO line and set the fields obtained from it.
        record = PafmParser.processWMO(theBulletin, mndTime);
        /*
         * Determine type of matrix (POINT, AREA, or FIRE WEATHER POINT) The
         * legacy system decoded the Point and Area Forecast Matrices. Fire
         * Weather Point Forecast Matrices were not decoded. Some have different
         * formats and also new parameters, so Fire Weather Point Matrices will
         * not be decoded. Test for "FIRE WEATHER POINT FORECAST MATRICES"
         * first. This insures that it is not a false "POINT FORECAST MATRICES"
         */
        mattypeInd = theBulletin
                .indexOf("FIRE WEATHER POINT FORECAST MATRICES");
        if (mattypeInd == -1) {
            mattypeInd = theBulletin.indexOf("POINT FORECAST MATRICES");
            if (mattypeInd == -1) {
                mattypeInd = theBulletin.indexOf("AREA FORECAST MATRICES");
                if (mattypeInd == -1) {
                    /*
                     * Do not process by setting record to null since matrix
                     * type is missing or not known.
                     */
                    record = null;
                } else {
                    record.setMatrixType("AREA");
                }
            } else {
                record.setMatrixType("POINT");
            }
        } else {
            /*
             * Type is Fire Wx Point Forecast Matrix, set record to null so it
             * won't be decoded.
             */
            record = null;
        }
        if (record != null) {
            ArrayList<String> segmentList = new ArrayList<String>();
            segmentList.clear();
            // Break the bulletin message into segments delimited by a "$$"
            Scanner sc = new Scanner(theBulletin).useDelimiter(segmentDelim);
            while (sc.hasNext()) {
                String segment = sc.next();
                Matcher ugcMatcher = ugcPattern.matcher(segment);
                // discard if the segment did not have a UGC line.
                if (ugcMatcher.find()) {
                    segmentList.add(segment);
                }
            }
            /*
             * Process each segment in method processUgc to decode the fields
             * for the child table ugc. The fields for ugc's two child tables
             * fips and parameters are also decoded and added to their child
             * tables.
             */
            for (String segment : segmentList) {
                Matcher ugcMatcher = ugcPattern.matcher(segment);
                if (ugcMatcher.find()) {
                    PafmUgc ugc = PafmParser.processUgc(ugcMatcher.group(),
                            segment, mndTime);
                    record.addPafmUGC(ugc);
                }
            }

            record.setReportType(pluginName);
            record.setTraceId(traceId);
            record.setPluginName(pluginName);
            /*
             * If unable to construct the DataURI, throw exception.
             */
            try {
                record.constructDataURI();
            } catch (PluginException e) {
                throw new DecoderException("Error constructing dataURI", e);
            }
            // Set MND remark
            record.setMndTime(mt.getMndTimeString());
        }
        /*
         * Return the PafmRecord object if not null
         */
        if (record == null) {
            return new PluginDataObject[0];
        } else {
            return new PluginDataObject[] { record };
        }
    }
}
