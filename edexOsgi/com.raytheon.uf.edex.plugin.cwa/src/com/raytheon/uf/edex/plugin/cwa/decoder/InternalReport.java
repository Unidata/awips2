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
package com.raytheon.uf.edex.plugin.cwa.decoder;

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
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date    Ticket#         Engineer    Description
 * ------- ----------      ----------- --------------------------
 * Feb 01, 2010            jsanchez     Initial creation
 * Mar 26, 2014            skorolev    Updated logger.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class InternalReport {
    private static IUFStatusHandler logger = UFStatus
            .getHandler(InternalReport.class);

    private static final String ISSUANCE = "^([A-Z]{3})([0-9]) +U?CWA +([0-9]{6}) *";

    private static final String VALID_TO = "^([A-Z]{3}) +U?CWA +([0-9])([0-9]{2}) +.*TIL ([0-9]{6}) *";

    public static final String VICINITY = "(VI?CI?NI?TY ([A-Z]+))";

    public static final String LINE_GEOM = "(LI?NE?|AREA)[^[:digit:]]+([0-9]+) *NM.*(WIDE|WD)";

    public static final String LINE_GEOM2 = "([[:space:]]*)([0-9]+) *NM.*(WIDE|WD)";

    public static final String AREA_GEOM = "[[:space:]]*DIAM(ETER)? +([0-9]+) *NM";

    public static final String POINT_GEOM2 = "[[:space:]]*([0-9]+) *NM DIAM(ETER)?";

    public static final String PIREPS_TABLE = "^[[:space:]]*([^[:space:]]+)[[:space:]].*[[:space:]]([-0-9]{3,})[[:space:]]+([-0-9]{3,})";

    public static final String DIRDIST = "([0-9]+)(([NSWE]+))";

    public static final String TIME = "(\\d{6,6})";

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

    public static List<InternalReport> identifyMessage(byte[] message) {
        List<InternalReport> reports = new ArrayList<InternalReport>();
        List<String> lines = separateLines(message);
        if (lines != null) {
            Matcher m = null;

            Pattern p1 = Pattern.compile(ISSUANCE);
            Pattern p2 = Pattern.compile(VALID_TO);
            Pattern p3 = Pattern.compile(VICINITY);

            InternalType t1 = InternalType.ISSUANCE;
            InternalType t2 = InternalType.VALID_TO;

            Pattern patterns[] = new Pattern[] { p1, p2 };
            InternalType types[] = new InternalType[] { t1, t2 };

            boolean issuanceFound = false;
            boolean validtoFound = false;
            boolean coordinateFound = false;

            for (String s : lines) {
                if (!(issuanceFound && validtoFound)) {
                    for (int i = 0; i < patterns.length; i++) {
                        m = patterns[i].matcher(s);
                        if (m.matches()) {
                            reports.add(new InternalReport(types[i], s));
                            if (types[i].equals(InternalType.ISSUANCE)) {
                                issuanceFound = true;
                            } else if (types[i].equals(InternalType.VALID_TO)) {
                                validtoFound = true;
                            }
                        }
                    }
                } else {
                    m = p3.matcher(s);
                    if (m.matches()) {
                        reports.add(new InternalReport(InternalType.VICINITY, s));
                    } else if (issuanceFound && validtoFound
                            && !coordinateFound) {
                        reports.add(new InternalReport(InternalType.COORDS, s));
                        coordinateFound = true;
                    } else if (coordinateFound) {
                        reports.add(new InternalReport(InternalType.TEXT, s));
                    }
                }
            }
            reports.add(new InternalReport(InternalType.END, ""));
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
