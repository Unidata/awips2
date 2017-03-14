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
package com.raytheon.uf.edex.plugin.vaa.decoder;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.wmo.WMOHeader;

/**
 * Data structure for a Volcanic Ash Advisory
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2009            jkorman     Initial creation
 * Nov 26, 2013 2582       njensen     Fix where OBS DTG is on separate lines
 * May 14, 2014 2536       bclement    moved WMO Header to common
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class InternalReport {

    public static final Pattern VAAC_CNTR_P = Pattern
            .compile("^((VAAC)(?:\\:) +(.*))");

    // PSN: N1642 W06210
    // LOCATION: N1642 W06210
    public static final Pattern VOL_PSN_P = Pattern
            .compile("(^(LOCATION|PSN)(?:\\:) +(.*))");

    public static final Pattern LAT_LON_P = Pattern
            .compile("([NS])(\\d{2,2})(\\d{2,2}) +([EW])(\\d{3,3})(\\d{2,2})");

    public static final Pattern DTG_P = Pattern
            .compile("^(OBS) +(VA|ASH) +DTG: +(.*)");

    public static final Pattern ANAL_P = Pattern
            .compile("^(OBS) +(VA|ASH) +(CLD|CLOUD): +(.*)");

    public static final Pattern FCST_P = Pattern
            .compile("^(FCST) +(VA|ASH) +(CLD|CLOUD) \\+(\\d{1,2})HR: +(.*)");

    //
    // public static final String DATE_LINE =
    // "^(0\\d)|(1[0-2])/(\\d{2,2})/(\\d{4,4}) .*";

    private final InternalType lineType;

    private final String reportLine;

    private List<InternalReport> subLines = null;

    public InternalReport(InternalType type, String line) {
        lineType = type;
        reportLine = line;
    }

    public List<InternalReport> getSubLines() {
        return subLines;
    }

    public void setSubLines(List<InternalReport> subLines) {
        this.subLines = subLines;
    }

    public InternalType getLineType() {
        return lineType;
    }

    public String getReportLine() {
        return reportLine;
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
        buffer.append("}");
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
                sb.append("\n");
                sb.append("   ");
                r.toString(sb);
            }
        }
        return sb.toString();
    }

    public static List<InternalReport> identifyMessage(byte[] message,
            Headers headers) {
        List<InternalReport> reports = new ArrayList<InternalReport>();

        String fileName = (String) headers.get(WMOHeader.INGEST_FILE_NAME);
        WMOHeader hdr = new WMOHeader(message, fileName);
        if (hdr.isValid()) {
            reports.add(new InternalReport(InternalType.WMO_HEADER, hdr
                    .getWmoHeader()));
            int len = message.length - hdr.getMessageDataStart();
            StringBuilder sb = new StringBuilder(new String(message,
                    hdr.getMessageDataStart(), len));
            for (int i = 0; i < sb.length();) {
                if (sb.charAt(i) == '\r') {
                    sb.setCharAt(i, '\n');
                } else if ((sb.charAt(i) == 0x01) || (sb.charAt(i) == 0x03)) {
                    sb.deleteCharAt(i);
                } else {
                    i++;
                }
            }
            char lastCh = ' ';
            for (int i = 0; i < sb.length();) {
                if ((lastCh == '\n') && (sb.charAt(i) == '\n')) {
                    sb.deleteCharAt(i);
                } else {
                    lastCh = sb.charAt(i);
                    i++;
                }
            }
            reports.add(new InternalReport(InternalType.MESSAGE, sb.toString()));
        }
        List<String> lines = separateLines(message);
        if (lines != null) {
            Matcher m;
            String previousLine = null;
            for (String s : lines) {
                if (s.length() > 0) {
                    m = VAAC_CNTR_P.matcher(s);
                    if (m.find()) {
                        reports.add(new InternalReport(InternalType.VAAC_CNTR,
                                m.group(3)));
                    } else {
                        m = VOL_PSN_P.matcher(s);
                        if (m.find()) {
                            reports.add(new InternalReport(
                                    InternalType.VOLCANO_PSN, s));
                        } else {
                            m = FCST_P.matcher(s);
                            if (m.find()) {
                                reports.add(new InternalReport(
                                        InternalType.FCST, s));
                            } else if (s.equals(hdr.getWmoHeader())) {
                                // nothing
                            } else if (s.startsWith("VA ADVISORY")) {
                                reports.add(new InternalReport(
                                        InternalType.ADVISORY_LEAD, s));
                            } else if (s.startsWith("DTG:")) {
                                if (previousLine != null
                                        && previousLine.startsWith("OBS")) {
                                    reports.add(new InternalReport(
                                            InternalType.OBS_DTG, s));
                                } else {
                                    reports.add(new InternalReport(
                                            InternalType.MESSAGE_DTG, s
                                                    .substring(5)));
                                }
                            } else if (s.startsWith("VOLCANO:")) {
                                reports.add(new InternalReport(
                                        InternalType.VOLCANO_ID, s.substring(9)));
                            } else if (s.startsWith("AREA:")) {
                                reports.add(new InternalReport(
                                        InternalType.GEO_AREA, s.substring(5)));
                            } else if (s.startsWith("SUMMIT ELEV:")) {
                                reports.add(new InternalReport(
                                        InternalType.SUMMIT_ELEV, s
                                                .substring(13)));
                            } else if (s.startsWith("ADVISORY NR:")) {
                                reports.add(new InternalReport(
                                        InternalType.ADVISORY_NR, s
                                                .substring(13)));
                            } else if (s.startsWith("INFO SOURCE:")) {
                                reports.add(new InternalReport(
                                        InternalType.INFO_SOURCE, s
                                                .substring(13)));
                            } else if (s.startsWith("ERUPTION DETAILS:")) {
                                reports.add(new InternalReport(
                                        InternalType.ERUPTION_DETAIL, s
                                                .substring(18)));
                            } else if (s.startsWith("OBS")) {
                                m = ANAL_P.matcher(s);
                                if (m.find()) {
                                    reports.add(new InternalReport(
                                            InternalType.OBS, s));
                                } else {
                                    m = DTG_P.matcher(s);
                                    if (m.find()) {
                                        reports.add(new InternalReport(
                                                InternalType.OBS_DTG, s));
                                    }
                                }
                            } else if (s.startsWith("NXT ADVISORY:")) {
                                reports.add(new InternalReport(
                                        InternalType.NXT_ADVISORY, s
                                                .substring(14)));
                            } else if (s.startsWith("RMK:")) {
                                reports.add(new InternalReport(
                                        InternalType.RMKS, s));
                            } else if (s.charAt(0) == 1) {
                                reports.add(new InternalReport(
                                        InternalType.SOM, ""));
                            } else if (s.charAt(0) == 3) {
                                reports.add(new InternalReport(
                                        InternalType.ETX, ""));
                            } else {
                                reports.add(new InternalReport(
                                        InternalType.NO_ID, s));
                            }
                        }
                    }
                }
                previousLine = s;
            } // for
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
                // logger.error("Error reading from reader",e);
            } finally {
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (IOException ioe) {
                        // logger.error("Error closing reader", ioe);
                    }
                }
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
                case ETX:
                case SOM: {
                    reports.remove(i);
                    break;
                }
                case ADVISORY_LEAD: {
                    // fall through
                }
                case MESSAGE_DTG:
                case VAAC_CNTR:
                case VOLCANO_ID:
                case VOLCANO_PSN:
                case GEO_AREA:
                case SUMMIT_ELEV:
                case ERUPTION_DETAIL:
                case NXT_ADVISORY: {
                    currRpt = null;
                    i++;
                    break;
                }
                case INFO_SOURCE:
                case FCST:
                case OBS: {
                    r.subLines = new ArrayList<InternalReport>();
                    currRpt = r;
                    i++;
                    break;
                }
                case RMKS: {
                    r.subLines = new ArrayList<InternalReport>();
                    currRpt = r;
                    i++;
                    break;
                }
                case NO_ID: {
                    if (currRpt != null) {
                        currRpt.subLines.add(r);
                        reports.remove(i);
                        break;
                    }
                }
                default: {
                    i++;
                }
                }
            }
        }
        return reports;
    }

    public static final void main(String[] args) {
        Matcher m;

        m = VOL_PSN_P.matcher("PSN: N1642 W06210");
        if (m.find()) {
            for (int i = 1; i <= m.groupCount(); i++) {
                System.out.println(String.format(" %3d  %s", i, m.group(i)));
            }
        }
        System.out
                .println("--------------------------------------------------------");
        m = VOL_PSN_P.matcher("LOCATION: N1642 W06210");
        if (m.find()) {
            for (int i = 1; i <= m.groupCount(); i++) {
                System.out.println(String.format(" %3d  %s", i, m.group(i)));
            }
        }
        System.out
                .println("--------------------------------------------------------");
        m = VAAC_CNTR_P.matcher("VAAC: WASHINGTON");
        if (m.find()) {
            for (int i = 1; i <= m.groupCount(); i++) {
                System.out.println(String.format(" %3d  %s", i, m.group(i)));
            }
        }
        System.out
                .println("--------------------------------------------------------");
        m = LAT_LON_P.matcher("N1642 W06210");
        if (m.find()) {
            for (int i = 1; i <= m.groupCount(); i++) {
                System.out.println(String.format(" %3d  %s", i, m.group(i)));
            }
        }

        System.out
                .println("--------------------------------------------------------");
        String FCST_P = "^(FCST) +(VA|ASH) +(CLD|CLOUD) \\+(\\d{1,2})HR: +(.*)";

        String FCST_1_P = "(\\d{2}/\\d{4}Z +)";

        Pattern p = Pattern.compile(FCST_P);
        // m =
        // p.matcher("FCST VA CLD +6HR: 05/0130Z SFC/FL220 5NM WIDE LINE N1638 W06614 - N1643 W06214");
        // m =
        // p.matcher("FCST VA CLD +6HR: 05/0130Z SFC/FL220 5NM WIDE LINE N1638 W06614 - N1643 W06214 SFC/FL100 5NM WIDE LINE N1638 W06614 - N1643 W06214");
        m = p.matcher("FCST VA CLD +6HR: 05/0130Z SFC/FL220 N0327 W07435 - N0244 W07448 - N0249 W07558 - N0302 W07556 - N0327 W07435");
        if (m.find()) {
            for (int i = 1; i <= 4; i++) {
                System.out.println(String.format(" %3d  %s", i, m.group(i)));
            }
            p = Pattern.compile(FCST_1_P);
            String s = m.group(5);
            m = p.matcher(s);
            if (m.find()) {
                System.out.println(s.substring(m.start(), m.end()));
                System.out.println(s.substring(m.end()));
            }
        }

        String FCST_2_P = "(SFC|FL\\d{3})/(SFC|FL\\d{3}) +\\d+NM WID(E?) LINE ([NS]\\d{4} +[EW]\\d{5}) ";
        p = Pattern.compile(FCST_2_P);
        String s = "SFC/FL100 5NM WIDE LINE N1638 W06614 - N1643 W06214 FL050/FL150 5NM WID LINE N1638 W06614 - N1643 W06214";
        m = p.matcher(s);
        int n = 0;
        while (m.find(n)) {
            if (n > 0) {
                System.out.println("    " + s.substring(n, m.start()));
            }
            System.out.println(s.substring(m.start(), m.end()));
            n = m.end();
        }
        if (n > 0) {
            System.out.println("    " + s.substring(n, s.length()));
        }
    }

}
