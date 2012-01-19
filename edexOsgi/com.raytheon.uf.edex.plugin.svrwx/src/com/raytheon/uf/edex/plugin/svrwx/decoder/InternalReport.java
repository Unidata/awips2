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
package com.raytheon.uf.edex.plugin.svrwx.decoder;

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
 * Jan 10, 2010            jsanchez     Initial creation
 *
 * </pre>
 *
 * @author jsanchez
 * @version 1.0 
 */
public class InternalReport {
    
    private static Log logger = LogFactory.getLog(InternalReport.class);
    
    public static final String REPORT_TYPE_LN = "^((.*)(TORNADO REPORTS|LRG HAIL/STRONG WIND RPTS|OTHER SEVERE REPORTS)(.*))";
    
    public static final String EVENT_KEY = "(\\*TORN|WNDG|([AG]\\s{0,1}\\d{2,3}))";

    public static final String TIME = "(\\d{1,2}/\\d{4,4})";
    
    public static final String EVENT_LN = "^((.*)" + EVENT_KEY + "(.*)" + TIME + ")";
    
    public static final String LATLON = "(\\d{4,4}\\s{0,1}\\d{4,5})";
    
    public static final String STATIONID = "(\\w{3,3}/\\w{3,3})";
    
    public static final String RMK_LN = "^((.*)" + STATIONID + "(.*)" + LATLON + ")";
    
    public static final String REFTIME = "(\\d{2,2}CST\\s\\w{3,3}\\s\\w{3,3}\\s\\d{1,2}\\s{1,2}\\d{4,4})";
    
    public static final String TIME_RANGE_LN = "^((.*)FOR\\s" + REFTIME + "\\sTHRU\\s" + REFTIME +")";
    
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
            Pattern p1 = Pattern.compile(REPORT_TYPE_LN);
            Pattern p2 = Pattern.compile(EVENT_LN);
            Pattern p3 = Pattern.compile(RMK_LN);
            Pattern p4 = Pattern.compile(TIME_RANGE_LN);
            
            InternalType t1 = InternalType.REPORT_TYPE;
            InternalType t2 = InternalType.EVENT_LN;
            InternalType t3 = InternalType.REMARKS;
            InternalType t4 = InternalType.TIME_RANGE;
            
            Pattern patterns[] = {p1,p2,p3,p4};
            InternalType types[] = {t1,t2,t3,t4};
            boolean found;
            for(String s : lines) {
                found = false;
                for(int i = 0; i < patterns.length; i++){
                    Matcher m = patterns[i].matcher(s);
                    if(m.matches()){
                        InternalReport rptLine = new InternalReport(types[i],s);
                        reports.add(rptLine);
                        found = true;
                        break;
                    }
                }
                
                if(!found){
                    InternalReport rptLine = new InternalReport(InternalType.EXTRA,s);
                    reports.add(rptLine);
                }
            }
            
            InternalReport rptLine = new InternalReport(InternalType.END,"");
            reports.add(rptLine);
            
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
