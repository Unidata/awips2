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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Internal Report
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 10, 2010            jsanchez     Initial creation
 * Apr 10, 2014  2971      skorolev     Cleaned code.
 * Jun 25, 2014  3008      nabowle      Refactor for EventReport.
 *
 * </pre>
 *
 * @author jsanchez
 * @version 1.0
 */
public class InternalReport {

    private static IUFStatusHandler logger = UFStatus
            .getHandler(InternalReport.class);

    public static final String REPORT_TYPE_LN = "^((.*)(TORNADO REPORTS|LRG HAIL/STRONG WIND RPTS|OTHER SEVERE REPORTS)(.*))";

    private static final Pattern REPORT_TYPE_LN_PTRN = Pattern
            .compile(REPORT_TYPE_LN);

    public static final String EVENT_KEY = "(\\*TORN|WNDG|([AG]\\s{0,1}\\d{2,3}))";

    public static final String TIME = "(\\d{1,2}/\\d{4,4})";

    public static final String EVENT_LN = "^((.*)" + EVENT_KEY + "(.*)" + TIME
            + ")";

    private static final Pattern EVENT_LN_PTRN = Pattern.compile(EVENT_LN);

    public static final String LATLON = "(\\d{4,4}\\s{0,1}\\d{4,5})";

    public static final String STATIONID = "(\\w{3,3}/\\w{3,3})";

    public static final String RMK_LN = "^((.*)" + STATIONID + "(.*)" + LATLON
            + ")";

    public static final String REFTIME = "(\\d{2,2}CST\\s\\w{3,3}\\s\\w{3,3}\\s{1,2}\\d{1,2}\\s{1,2}\\d{4,4})";

    public static final String TIME_RANGE_LN = "^((.*)FOR\\s" + REFTIME
            + "\\sTHRU\\s" + REFTIME + ")";

    private static final Pattern TIME_RANGE_LN_PTRN = Pattern
            .compile(TIME_RANGE_LN);

    private final InternalType lineType;

    private final String reportLine;

    private List<InternalReport> subLines = null;

    /**
     *
     * @param type
     * @param line
     */
    public InternalReport(InternalType type, String line) {
        lineType = type;
        reportLine = line;
    }

    /**
     * Get Line Type.
     *
     * @return the lineType
     */
    public InternalType getLineType() {
        return lineType;
    }

    /**
     * Get Report Line.
     *
     * @return the reportLine
     */
    public String getReportLine() {
        return reportLine;
    }

    /**
     * Get SubLines.
     *
     * @return
     */
    public List<InternalReport> getSubLines() {
        return subLines;
    }

    /**
     *
     * @param buffer
     *            Buffer to receive String formatted internal data. If this
     *            reference is null, a new StringBuilder instance is created.
     * @return The populated StringBuilder instance.
     */
    public StringBuilder toString(StringBuilder buffer) {
        if (buffer == null) {
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
     *
     * @return The string representation of this class instance.
     */
    @Override
    public String toString() {
        StringBuilder sb = toString(null);
        if (subLines != null) {
            for (InternalReport r : subLines) {
                sb.append("   ");
                r.toString(sb);
            }
        }
        return sb.toString();
    }

    /**
     * Message identification.
     *
     * @param message
     * @return
     */
    public static List<InternalReport> identifyMessage(byte[] message) {
        List<InternalReport> reports = new ArrayList<InternalReport>();
        List<String> lines = separateLines(message);
        if (lines != null) {

            Pattern patterns[] = { REPORT_TYPE_LN_PTRN, TIME_RANGE_LN_PTRN };
            InternalType types[] = { InternalType.REPORT_TYPE,
                    InternalType.TIME_RANGE };

            boolean found;
            EventReport.Builder builder = null;
            Matcher m;
            for (String s : lines) {
                found = false;
                for (int i = 0; i < patterns.length; i++) {
                    m = patterns[i].matcher(s);
                    if (m.matches()) {

                        if (builder != null) {
                            reports.add(builder.build());
                            builder = null;
                        }

                        InternalReport rptLine = new InternalReport(types[i], s);
                        reports.add(rptLine);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    // TODO: An unrecognized event line will lead to either:
                    // The previous report's remarks containing the report or
                    // the report being tagged as EXTRA lines.
                    //
                    // In the former case, the issue may be noticed if the
                    // remarks of the report are examined. In the latter,
                    // the report will be lost without a trace.
                    // More info:
                    // http://www.nws.noaa.gov/directives/sym/pd01005012curr.pdf
                    // http://www.spc.noaa.gov/misc/about.html#Statistics
                    m = EVENT_LN_PTRN.matcher(s);
                    if (m.matches()) {
                        if (builder != null) {
                            reports.add(builder.build());
                        }
                        builder = new EventReport.Builder().withEventLine(s);
                    } else if (builder != null) {
                        builder.withRemarks(s);
                    } else {
                        InternalReport rptLine = new InternalReport(
                                InternalType.EXTRA, s);
                        reports.add(rptLine);
                    }
                }
            }

            if (builder != null) {
                reports.add(builder.build());
            }
        }
        return reports;
    }

    /**
     * Separate Lines.
     *
     * @param message
     * @return reportLines
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
                    if (s.trim().length() > 0) {
                        reportLines.add(s);
                    }
                }
            } catch (Exception e) {
                logger.error("Error reading from reader", e);
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
