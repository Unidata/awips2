package com.raytheon.viz.texteditor.qc;

import java.util.regex.Matcher;

import com.raytheon.uf.viz.vtec.VtecObject;
import com.raytheon.uf.viz.vtec.VtecUtil;

/**
 * WMO Header Check
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- ------------------------------------------
 * Apr 02, 2013  15609    Qinglu Lin  Added code to prevent error message from
 *                                    creating while nnn is SVS for Extreme Wind
 *                                    Warning Followup.
 * Nov 03, 2016  5934     randerso    Moved VtecObject and VtecUtil to a
 *                                    separate plugin.
 * Apr 11, 2016  6251     dgilling    Support changes to QualityControl.
 *
 * </pre>
 *
 */
public class WmoHeaderCheck implements IQCCheck {

    @Override
    public String runQC(String header, String body, String nnn) {
        String errorMsg = "";

        if ((header == null) || (header.length() == 0)) {
            return "\nNo text found.\n";
        }

        String[] separatedLines = header.trim().split("\n");
        if ((separatedLines == null) || (separatedLines.length < 2)) {
            return "\nIncomlete product.\n";
        }

        Matcher m = QCCheckConstants.WMO_HEADER_PATTERN.matcher(separatedLines[0]);
        if (m.matches()) {
            // Check the Type in the TTAAii.
            if ((nnn.equals("SVR") && !m.group(1).equals("WU"))
                    || (nnn.equals("TOR") && !m.group(1).equals("WF"))
                    || (nnn.equals("SMW") && !m.group(1).equals("WH"))
                    || (nnn.equals("FFW") && !m.group(1).equals("WG"))) {
                errorMsg += nnn + " doesn't match " + m.group(1)
                        + "\n in TTAAii.\n";
            }

            // Check BBB.
            if ((m.group(4) != null) && !m.group(4).equals("")
                    && !m.group(4).equals("RR") && !m.group(4).equals("4")
                    && !m.group(4).equals("AA") && !m.group(4).equals("CC")) {
                errorMsg += "BBB: " + m.group(4) + m.group(5) + " error.\n";
            }

        } else {
            errorMsg += "First line is not WMO header or invalid format.\n";
        }

        m = QCCheckConstants.AWIPS_ID_PATTERN.matcher(separatedLines[1]);
        if (m.matches() == false) {
            errorMsg += "No NNNXXX on second line.\n";
        }

        VtecObject vtec = VtecUtil.parseMessage(body);
        if ((vtec == null) && (nnn.equals("MWS") == false)) {
            errorMsg += "\nNo VTEC line found.\n";
        } else if ((vtec != null) && (!QualityControl.getInstance().match(nnn,
                vtec.getPhenomena() + "." + vtec.getSignificance()))) {
            if (nnn.equals("SVS") && vtec.getPhenomena().equals("EW")
                    && vtec.getSignificance().equals("W")) {
                // DR 15609. Extreme Wind Warning Followup uses SVS for nnn, as
                // AWIPS I does.
                // Do not create error message.
            } else {
                errorMsg += "VTEC event type (" + vtec.getPhenomena() + "."
                        + vtec.getSignificance() + ") doesn't match NNN.\n";
            }
        }
        return errorMsg;
    }
}
