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
package com.raytheon.uf.edex.plugin.tcg.decoder;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 26, 2009            jsanchez     Initial creation
 *
 * </pre>
 *
 * @author jsanchez
 * @version 1.0	
 */

public class InternalReport {

    private static Log logger = LogFactory.getLog(InternalReport.class);
    
    //CHGHUR or CHGQLM or TCEAT4
    public static final String PRODUCT_PTRN = "^((CHG|TCE)(.*))";
    
    /*
     * Below are patterns mostly used for CHGQLM
     */

    //TROPICAL STORM IDA       11L
    public static final String STORM_TYPE_INFO = "^((TROPICAL|HURRICANE|TYPHOON|EXTRATROPICAL|DISTURBANCE) +(.*))";
    
    public static final String HOUR_PTRN = "(\\d{1,2})";
    
    public static final String MONTH_PTRN = "(\\w{3,3})";
    
    public static final String DAY_PTRN = "(\\d{1,2})";
    //INITIAL TIME  18Z NOV  4
    public static final String INIT_TIME_INFO = "^((INITIAL TIME)(\\s{2,3})" + HOUR_PTRN + "Z " + MONTH_PTRN + "(\\s{1,2})" + DAY_PTRN + ")";
    
    //  18            12.1             82.2            47./ 1.7
    public static final String FORECAST_POSITION_INFO = "^((\\s{1,3})(\\d{1,3})(\\s{12,14})((\\d{0,2}).\\d) +(.*))";
    
    /*
     * Below are patterns mostly used for CHGHUR
     */
    //16.5N 102.2W
    public static final String LATLON_PTRN ="(((\\d{0,2}|90).\\d{1,1}[NS])(\\s{1,4})(\\d{0,3}.\\d{1,1}[EW]))";
    
    //BAMS
    public static final String MODEL_PTRN ="(\\w{4,4})";
    
    //BAMS    16.5N 102.2W   17.5N 102.0W   18.2N 101.7W   18.6N 102.0W
    public static final String MODEL_INFO ="^(" + MODEL_PTRN + "(\\s{4,4})" + LATLON_PTRN + "+(.*))"; 
    
    //091029  1200
    public static final String DATETIME= "(\\d{6,6}\\s{2,2}\\d{4,4})";
    
    //        091029  1200   091030  0000   091030  1200   091031  0000
    public static final String DATETIME_INFO = "((.*)" + DATETIME + "+(.*))";
    
    //(EP952009)
    public static final String STATIONID_PTRN = "\\(\\w{2,2}\\d{6,6}\\)";
    
    //20091029 1200
    public static final String REFTIME_PTRN = "(\\d{8,8} \\d{4,4})";
    
    //DISTURBANCE INVEST (EP952009) 20091029 1200 UTC
    public static final String DATA_INFO = "((.*) " + STATIONID_PTRN + " " + REFTIME_PTRN + " " + "UTC)";
    
    //         ...INITIAL CONDITIONS...
    public static final String INIT_CONDITION_PTRN = "((.*)(...INITIAL CONDITIONS...)+(.*))";
    
    //STORM DISSIPATED AT  54 HRS AT THE ABOVE PSN.
    public static final String STORM_DISSIPATED = "^(STORM DISSIPATED +(.*))";
    
    /*
     * Below are used for TCE 
     */
    public static final String NWS_INFO = "^((NWS TPC)(.*)(\\w{2,2}\\d{6,6}))";
    //1100 PM CDT FRI SEP 12 2008
    public static final String TCE_REFTIME_INFO = "^(\\d{3,4} (AM|PM) \\w{3,3} \\w{3,3} \\w{3,3}\\s{1,2}\\d{1,2}(.*))";    
    
    public static final String TCE_REFHOUR = "^((.*)(\\.\\.\\.\\d{4,4}Z\\.\\.\\.)(.*))";
    
    public static final String LAT_PTRN = "(LATITUDE \\d{0,2}.\\d)";
    //RESERVE UNIT RECONNAISSANCE AIRCRAFT...TO BE NEAR LATITUDE 28.7
    public static final String LATITUDE_INFO = "^((.*)" + LAT_PTRN + "(.*))";
    
    public static final String LON_PTRN = "(LONGITUDE \\d{0,3}.\\d)";
    //NORTH...LONGITUDE 94.5 WEST OR ABOUT 45 MILES...75 KM...
    public static final String LONGITUDE_INFO = "^((.*)" + LON_PTRN + "(.*))";
    //FORECASTER STEWART
    public static final String FORECASTER_PTRN = "^((FORECASTER)(.*))";

    private final InternalType lineType;
    private final String reportLine;
    
    private List<InternalReport> subLines = null;
    
    public InternalReport(InternalType type, String line) {
        lineType = type;
        reportLine = line;
    }

    /**
     * @return the lineType
     */
    public InternalType getLineType() {
        return lineType;
    }

    /**
     * @return the reportLine
     */
    public String getReportLine() {
        return reportLine;
    }

    /**
     * 
     * @return
     */
    public List<InternalReport> getSubLines() {
        return subLines;
    }
    
    /**
     * 
     * @param buffer Buffer to receive String formatted internal data. If this
     * reference is null, a new StringBuilder instance is created.
     * @return The populated StringBuilder instance.
     */
    public StringBuilder toString(StringBuilder buffer) {
        if(buffer == null) {
            buffer = new StringBuilder();
        }
        buffer.append("[");
        buffer.append(lineType.name());
        buffer.append("]{");
        buffer.append(reportLine);
        buffer.append("}\n");
        return buffer;
    }
    
    /**
     * Create a string representation of this class instance.
     * @return The string representation of this class instance.
     */
    @Override
    public String toString() {
        StringBuilder sb = toString(null);
        if(subLines != null) {
            for(InternalReport r : subLines) {
                sb.append("   ");
                r.toString(sb);
            }
        }
        return sb.toString();
    }

    public static List<InternalReport> identifyMessage(byte [] message) {
        List<InternalReport> reports = new ArrayList<InternalReport>();
        List<String> lines = separateLines(message);
        if(lines != null) {
            Pattern p1 = Pattern.compile(PRODUCT_PTRN);
            Pattern p2 = Pattern.compile(DATA_INFO);
            Pattern p3 = Pattern.compile(DATETIME_INFO);
            Pattern p4 = Pattern.compile(MODEL_INFO);
            Pattern p5 = Pattern.compile(INIT_CONDITION_PTRN);         
            Pattern p6 = Pattern.compile(STORM_TYPE_INFO);
            Pattern p7 = Pattern.compile(INIT_TIME_INFO);
            Pattern p8 = Pattern.compile(FORECAST_POSITION_INFO);
            Pattern p9 = Pattern.compile(STORM_DISSIPATED);
            Pattern p10 = Pattern.compile(NWS_INFO);         
            Pattern p11 = Pattern.compile(TCE_REFTIME_INFO);
            Pattern p12 = Pattern.compile(LATITUDE_INFO);
            Pattern p13 = Pattern.compile(LONGITUDE_INFO);
            Pattern p14 = Pattern.compile(FORECASTER_PTRN);
            Pattern p15 = Pattern.compile(TCE_REFHOUR);
            
            InternalType t1 = InternalType.PRODUCT;
            InternalType t2 = InternalType.DATA_INFO;
            InternalType t3 = InternalType.DATETIME_INFO;
            InternalType t4 = InternalType.MODEL_INFO;
            InternalType t5 = InternalType.END;
            InternalType t6 = InternalType.STORM_TYPE_INFO;
            InternalType t7 = InternalType.INIT_TIME_INFO;
            InternalType t8 = InternalType.FORECAST_POSITION_INFO;
            InternalType t9 = InternalType.STORM_DISSIPATED;
            InternalType t10 = InternalType.STATIONID;
            InternalType t11 = InternalType.INIT_TIME_INFO;
            InternalType t12 = InternalType.LATITUDE;
            InternalType t13 = InternalType.LONGITUDE;
            InternalType t14 = InternalType.END;
            InternalType t15 = InternalType.TCE_REFHOUR;
            
            Pattern patterns[]   = {p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15};
            InternalType types[] = {t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15};
            
            for(String s : lines) {
                for(int i = 0; i < patterns.length; i++){
                    Matcher m = patterns[i].matcher(s);
                    if(m.matches()){
                        InternalReport rptLine = new InternalReport(types[i],s);
                        reports.add(rptLine);
                        break;
                    }
                }
            }

        }
        return reports;
    }
        
    /**
     * 
     * @param message
     * @return
     */
    private static List<String> separateLines(byte[] message) {
        List<String> reportLines = null;

        if (message != null) {
            BufferedReader reader = null;
            try {
                reader = new BufferedReader(new InputStreamReader(
                        new ByteArrayInputStream(message)));
                String s;
                reportLines = new ArrayList<String>();
                while ((s = reader.readLine()) != null) {
                    if (s.length() > 0) {
                        reportLines.add(s);
                    }
                }
            } catch (Exception e) {
                logger.error("Error reading from reader",e);
            } finally {
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (IOException ioe) {
                        logger.error("Error closing reader", ioe);
                    }
                }
            }
        }
        return reportLines;
    }
        
}
