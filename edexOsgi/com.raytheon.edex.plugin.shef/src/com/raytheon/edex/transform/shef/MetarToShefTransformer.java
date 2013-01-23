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
package com.raytheon.edex.transform.shef;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.transform.TransformerException;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.transform.shef.obs.ObsToSHEFOptions;
import com.raytheon.edex.transform.shef.obs.SHEF_Metar_Codes;
import com.raytheon.edex.transform.shef.obs.SHEF_Obs_Codes;
import com.raytheon.edex.transform.shef.obs.Utilities;
import com.raytheon.uf.common.dataplugin.obs.metar.MetarRecord;
import com.raytheon.uf.edex.decodertools.core.DecoderTools;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.wmo.message.WMOHeader;

/**
 * Transforms a decoded METAR observation into a series of SHEF encoded data
 * lines.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2008       1659 jkorman     Initial creation
 * ======================================
 * AWIPS2 DR Work
 * 20120918           1185 jkorman     Added save to archive capability.  
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class MetarToShefTransformer extends
        AbstractShefTransformer<MetarRecord> {

    private static final String WMO_HEADER_FMT = CRCRLF
            + "SRXX99 %4s %2$td%2$tH%2$tM";

    private static final String RMKS_PATTERN = " RMK";

    private static final String METAR = "METAR";

    private static final String SPECI = "SPECI";

    private static final String SENS_TYPE_MANUAL = "MANUAL";

    private static final String SENS_TYPE_AUTO = " AUTO";

    private static final DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
    static {
        dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));        
    }

    
    private static final int P1_MIN  = 50;
    private static final int P2_MAX  =  5;

    private static String cfgFileName="metar.cfg";
    private static String cmdLnOptions="";
    private static boolean refreshOptions=true;

    /**
     * Construct an instance of this transformer.
     * @param cmdLine Command line options that may be used if these
     * options are not present in the Apps_defaults.
     */
    public MetarToShefTransformer(String cmdLine) {
        super(cmdLine, WMO_HEADER_FMT);
    }

    /**
     * Attempt to transform a single metar observation into SHEF encoded observations.
     * @param report A metar report to encode.
     * @param The system headers associated with the original metar message.
     * @return The encoded SHEF report as a byte array. May return an empty
     * array if the report should not have been encoded or some error occured.
     * @throws TransformerException
     */
    @Override
    public byte[] transformReport(MetarRecord report, Headers headers)
            throws TransformerException {

        // Transformed METAR PluginDataObject to SHEF
        byte[] result = null;

        Calendar nowCalendar = TimeTools.getSystemCalendar();

        Calendar metarTime = TimeTools.getSystemCalendar((String) headers
                .get(DecoderTools.INGEST_FILE_NAME));

        logger.debug("report object type = " + report.getClass().getName());

        incrementMessageCount();
        int place = 0;
        try {
            String ccc = null;
            String msg = report.getMessageData();
            WMOHeader hdr = new WMOHeader(msg.getBytes());
            if ((hdr != null) && (hdr.isValid())) {
                ccc = hdr.getCccc();
                if (ccc != null) {
                    if (ccc.length() > 3) {
                        ccc = ccc.substring(ccc.length() - 3);
                    }
                }
            }

            if (encodeThisStation(report)) {
                place = 1;
                String stnId = report.getStationId();
                place = 2;
                StringBuilder sb = makeWMOHeader(openWMOMessage(200), stnId,
                        headers, hdr);
                String fileName = makeWMOHeader(new StringBuilder(20), stnId, headers, hdr).toString().trim().replace(' ', '_');
                place = 3;

                startMessageLine(sb);
                if (ccc != null) {
                    sb.append(ccc);
                }
                sb.append(METAR_2_SHEF_NNN);
                if (stnId.length() == 4) {
                    sb.append(stnId.substring(1));
                } else if (stnId.length() == 3) {
                    sb.append(stnId);
                }

                startMessageLine(sb);
                metarTime = report.getTimeObs();
                if (metarTime.compareTo(nowCalendar) > 0) {
                    sb.append(": WARNING: observation time is greater than the system time for the same day");
                    startMessageLine(sb);
                    sb.append(":  Observation time = ");
                    sb.append(report.getDataTime());
                    sb.append(" System time= ");
                    synchronized(dateFormat) {
                        sb.append(dateFormat.format(nowCalendar.getTime()));
                    }
                }
                sb.append(": SHEF derived data created by MetarToShefTransformer:");
                startMessageLine(sb);
                sb.append(": TRACEID = ");
                sb.append(report.getWmoHeader());
                sb.append(":");
                place = 4;
                String shef = closeWMOMessage(encodeShef(sb, report, headers))
                        .toString();

                if (options.isOptVerbose()) {
                    logger.info(String.format("MetarToShef: = %s", shef));
                }
                archiveSHEFObs(shef, fileName);

                setLastMessage(shef);
                result = shef.getBytes();
            }
        } catch (Exception e) {
            logger.error("Error in transform:place " + place, e);
        } finally {
            if (result == null) {
                result = new byte[0];
            }
        }

        return result;
    }

    /**
     * Create the body of a SHEF message that encodes various elements contained
     * in the PluginDataObject being transformed. Each line within the message
     * must start with a CRCRLF segment.
     * 
     * @param buffer
     *            StringBuilder to receive the encoded data.
     * @param report
     *            The PluginDataObject being transformed.
     * @return The StringBuilder instance.
     */
    @Override
    protected StringBuilder encodeShef(StringBuilder buffer,
            MetarRecord wxReport, Headers headers) {
        int place = 0;
        try {
            MetarRecord report = wxReport;

            Calendar c = TimeTools.getSystemCalendar((String) headers
                    .get(DecoderTools.INGEST_FILE_NAME));

            StringBuilder lineHdr = new StringBuilder();
            lineHdr.append(SHEF_A_RECORD);
            if (report.getCorrection() != null) {
                lineHdr.append(SHEF_REVISION);
            }

            // ***********
            lineHdr.append(" ");
            String stnId = report.getStationId();
            place = 1;
            if (options.isOptStripICAO()) {
                lineHdr.append(stnId.substring(1));
            }

            lineHdr.append(" :");
            if (METAR.equals(report.getReportType())) {
                lineHdr.append(METAR);
            } else {
                lineHdr.append(SPECI);
            }
            lineHdr.append(" ");
            if (report.getAutoStationType() != null) {
                lineHdr.append(SENS_TYPE_AUTO); // type_sensor (AUTO|MANUAL)
            } else {
                lineHdr.append(SENS_TYPE_MANUAL); // type_sensor (AUTO|MANUAL)
            }
            lineHdr.append(" ");
            lineHdr.append("  "); // type_format
            lineHdr.append(": ");

            c = report.getTimeObs();
            if(METAR.equals(report.getReportType())) {
                c = checkTimeRounding(c, options, report);
            }
            
            place = 2;
            if (options.isOptCentury()) {
                lineHdr.append(String.format(SHEF_OBS_DATEY2K_FMT, c));
            } else {
                lineHdr.append(String.format(SHEF_OBS_DATE_FMT, c));
            }
            lineHdr.append(String.format(SHEF_OBS_TIME_FMT, c));
            if (options.isOptCentury()) {
                lineHdr.append(String.format(SHEF_OBS_BASISTIMEY2K_FMT, c));
            } else {
                lineHdr.append(String.format(SHEF_OBS_BASISTIME_FMT, c));
            }

            // Remove all carriage control and turn into spaces.
            place = 3;
            StringBuilder rptText = new StringBuilder(report.getReport());
            for (int i = 0; i < rptText.length(); i++) {
                switch (rptText.charAt(i)) {
                // Everything falls through. No default needed.
                case 0x0A:
                case 0x0D:
                case 0x03: {
                    rptText.setCharAt(i, ' ');
                }
                }
            }
            buffer = writeObs(buffer, rptText.toString());
            
            // Now grab the remarks. This is all we need in the formatter
            // section.
            place = 4;
            Pattern p = Pattern.compile(RMKS_PATTERN);
            Matcher m = p.matcher(rptText);
            String reportText = null;
            if (m.find()) {
                reportText = rptText.substring(m.start());
                if(Utilities.isAutoASOS(reportText)) {
                    options.setGeneralProperty(Utilities.IS_ASOS,"T");
                }
            } else {
                reportText = "";
            }
            for (SHEF_Obs_Codes<MetarRecord> value : SHEF_Metar_Codes.values()) {
                try {
                    StringBuilder lineData = new StringBuilder();
                    place = 5;
                    value.format(lineData, report, reportText, options);
                    place = 6;
                    if (lineData.length() > 0) {
                        startMessageLine(buffer).append(lineHdr);
                        buffer.append(SHEF_SEPARATOR).append(lineData);
                    }
                } catch (RuntimeException e) {
                    logger.error("Error in encode:place " + place, e);
                }
            }
        } catch (Exception e) {
            logger.error("Error in encode:place " + place, e);
        }
        return buffer;
    }

    private static Calendar checkTimeRounding(Calendar c, ObsToSHEFOptions options, MetarRecord wxReport) {
        if(c != null) {
            if(options != null) {
                if(options.isOptRoundObsTime()) {
                    
                    String id = wxReport.getStationId();
                    Integer durTime = null;
                    Integer pcReset = options.getPCReset(id);
                    // !null means that a reset is defined for this station.
                    if(pcReset == null) {
                        if (options.isOptStripICAO()) {
                            // Try again with 3 character identifier.
                            pcReset = options.getPCReset(id.substring(1));
                        }
                    }
                    // Did we find a pcReset value?
                    if (pcReset != null) {
                        // Get the tolerance value
                        Integer tol = options.getOptPCT();

                        int obMinute = wxReport.getTimeObs().get(Calendar.MINUTE);
                        durTime = (obMinute + 60 - pcReset) % 60;
                        
                        if((durTime <= tol) || (Math.abs(durTime - 60) <= tol)) {
                            durTime = 60;
                        }
                    }
                    // if null then the station does not have pcreset applied.
                    if(durTime == null) {
                        int min = c.get(Calendar.MINUTE);
                        int deltaHour = 0;
                        if(min >= 30) {
                            deltaHour = 1;
                        }
                        min = 0;
                        c.set(Calendar.MINUTE,min);
                        c.add(Calendar.HOUR_OF_DAY,deltaHour);
                    }
                }
            }
        }
        return c;
    }
    
    
    /**
     * Should this reports data be encoded?
     * 
     * @param report
     *            Data to be used to determine encoding status.
     * @return Should this reports data be encoded?
     */

    @Override
    protected boolean encodeThisStation(MetarRecord report) {
        boolean encodeOk = true;
        
        int mnTime = report.getTimeObs().get(Calendar.MINUTE);
        
        if(options.isOptSpeci()) {
            if((mnTime > P2_MAX) && (mnTime < P1_MIN)) {
                encodeOk = report.getPrecip1Hour() >= 0;
            }
            encodeOk &= "METAR".equals(report.getReportType());
        }
        
        String s = report.getStationId();
        return (encodeOk & options.checkName(s));
    }

    /**
     * Reformats observation text into a SHEF comment.
     * @param sb Buffer to receive the formatted data.
     * @param obs The observation to format.
     * @return The formatted data.
     */
    public static StringBuilder writeObs(StringBuilder sb, String obs) {
        boolean startLine = true;
        int count = 0;
        String indent = "";
        for(int i = 0;i < obs.length();i++) {
            if(startLine) {
                startMessageLine(sb);
                sb.append(":");
                sb.append(indent);
                startLine = false;
            }
            char c = obs.charAt(i);
            count++;
            if((c == '\r') || (c == '\n')) {
                c = ' ';
            }
            if(c != ' ') {
                sb.append(c);
            } else {
                // we have a space
                if(count >= 65) {
                    startLine = true;
                    count = 4;
                    indent = "   ";
                } else {
                    sb.append(c);
                }
            }
        }
        return sb;
    }

    public final byte[] transformMetar(MetarRecord report, Headers headers)
    throws TransformerException {
    	if (refreshOptions) {
    		logger.info("Metar to SHEF now use config file: "+cfgFileName+" with options:"+cmdLnOptions);
    		options.setCfgFileName(cfgFileName);
 			options.updateCommandLine(cmdLnOptions);
    		options.updateOptions();
    		refreshOptions=false;
    	}
    	configureArchiveDir();

    	return transformReport(report, headers);
    }

    public static void setCfgNOption (String cfg, String options){
    	cfgFileName=cfg;
    	cmdLnOptions=options;
    	refreshOptions=true;
    }
}