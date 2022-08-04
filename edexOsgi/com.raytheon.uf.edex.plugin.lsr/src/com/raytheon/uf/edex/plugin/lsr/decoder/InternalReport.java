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
package com.raytheon.uf.edex.plugin.lsr.decoder;

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
 * Internal structure for parsing Local Storm Reports
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Oct 26, 2009           jkorman      Initial creation
 * Oct 23, 2013  16674    D. Friedman  Prevent infinite loop
 * Dec 10, 2013  2581     njensen      Use UFStatus for logging
 * Mar 12, 2018  6824     randerso     Cleaned up regex to work with more
 *                                     general date/time stamps and mixed case.
 *
 * </pre>
 *
 * @author jkorman
 */

public class InternalReport {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(InternalReport.class);

    private static final Pattern NWS_ID_LINE = Pattern.compile(
            "^(NATIONAL WEATHER SERVICE) +(.*)", Pattern.CASE_INSENSITIVE);

    /**
     * Product date/time line of the form:
     *
     * 544 PM EDT SUN OCT 18 2009
     *
     * 0407 AM ChST Wed Mar 14 2018
     *
     */
    public static final Pattern DATETIME = Pattern.compile(
            "^(([01]?\\d{3}) ([AP]M) (\\p{Upper}\\p{Alpha}?[DS]T) (?:(\\p{Upper}\\p{Alpha}{2}) ){2}([123][0-9]|0?[1-9]) (\\d{4}))");

    // 0953 AM NON-TSTM WND DMG FAIRHAVEN 41.65N 70.82W
    private static final Pattern TIME_LINE = Pattern
            .compile("^((0[1-9]|1[0-2]))([0-5]\\d) ([AP]M) (.*)");

    // 10/18/2009 PLYMOUTH MA AMATEUR RADIO
    private static final Pattern DATE_LINE = Pattern
            .compile("^((0[1-9])|(1[0-2]))/(\\d{2})/(\\d{4}) .*");

    private final InternalType lineType;

    private final String reportLine;

    private List<InternalReport> subLines = null;

    /**
     * Constructor
     *
     * @param type
     *            type of line
     * @param line
     *            line text
     */
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
     * @return the subLines
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
     * Parse the message into a list of InternalReports
     *
     * @param message
     *            the message text
     * @return a list of InternalReports
     */
    public static List<InternalReport> identifyMessage(byte[] message) {
        List<InternalReport> reports = new ArrayList<>();
        List<String> lines = separateLines(message);
        if (lines != null) {
            for (String s : lines) {
                if ((s.length() > 0) && (s.startsWith("&&"))) {
                    // We don't care about any aux. data past this point.
                    break;
                }
                Matcher m = NWS_ID_LINE.matcher(s);
                if (m.matches()) {
                    InternalReport rptLine = new InternalReport(
                            InternalType.NWS_ID, m.group(2));
                    reports.add(rptLine);
                } else {
                    m = DATETIME.matcher(s);
                    if (m.matches()) {
                        InternalReport rptLine = new InternalReport(
                                InternalType.DATETIME_ZONE, s);
                        reports.add(rptLine);
                    } else {
                        m = TIME_LINE.matcher(s);
                        if (m.matches()) {
                            InternalReport rptLine = new InternalReport(
                                    InternalType.TIME, s);
                            reports.add(rptLine);
                        } else {
                            m = DATE_LINE.matcher(s);
                            if (m.matches()) {
                                InternalReport rptLine = new InternalReport(
                                        InternalType.DATE, s);
                                reports.add(rptLine);
                            } else if (s.startsWith("            ")) {
                                InternalReport rptLine = new InternalReport(
                                        InternalType.REMARK, s.trim());
                                reports.add(rptLine);
                            } else {
                                logger.debug("not identified [" + s + "]");
                            }
                        }
                    }
                }
            }
        }
        return adjust(reports);
    }

    /**
     *
     * @param message
     * @return
     */
    private static List<String> separateLines(byte[] message) {
        List<String> reportLines = null;

        if (message != null) {
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(new ByteArrayInputStream(message)))) {
                String s;
                reportLines = new ArrayList<>();
                while ((s = reader.readLine()) != null) {
                    if (s.length() > 0) {
                        reportLines.add(s);
                    }
                }
            } catch (IOException e) {
                logger.error("Error reading from reader", e);
            }
        }
        return reportLines;
    }

    /**
     * Collect subordinate report lines "below" the report lead-in line.
     *
     * @param reports
     *            List of report lines to adjust.
     * @return The adjusted report list.
     */
    private static List<InternalReport> adjust(List<InternalReport> reports) {
        if (reports != null) {
            InternalReport currRpt = null;
            for (int i = 0; i < reports.size();) {
                InternalReport r = reports.get(i);
                switch (r.lineType) {
                case DATETIME_ZONE: {
                    i++;
                    break;
                }
                case TIME: {
                    currRpt = r;
                    if (currRpt.subLines == null) {
                        currRpt.subLines = new ArrayList<>();
                    }
                    i++;
                    break;
                }
                case DATE: {
                    if (currRpt != null) {
                        currRpt.subLines.add(r);
                    }
                    reports.remove(r);
                    break;
                }
                case REMARK: {
                    if (currRpt != null) {
                        currRpt.subLines.add(r);
                    }
                    reports.remove(r);
                    break;
                }
                default: {
                    logger.debug(r.reportLine);
                    reports.remove(r);
                }
                }
            }
        }
        return reports;
    }
}
