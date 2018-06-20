package com.raytheon.viz.texteditor.qc;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;

import com.raytheon.uf.viz.vtec.VtecObject;
import com.raytheon.uf.viz.vtec.VtecUtil;

/**
 * MND Header Check
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2016 5411       randerso    Moved upper case conversion for QC checks into the
 *                                      specific checks that need it.
 * Apr 11, 2016 6251       dgilling    Support changes to QualityControl.
 *
 * </pre>
 *
 * @version 1.0
 */
public class MndHeaderCheck implements IQCCheck {

    private static final Set<String> BULLETIN_NOT_REQUIRED_PILS = new HashSet<>(
            Arrays.asList("FFW", "SVS", "FFS", "FLW", "FLS", "MWS", "DSW",
                    "SQW"));

    private static final Set<String> NO_BULLETIN_LINE_PILS = new HashSet<>(
            Arrays.asList("SVS", "FFS", "FLW", "FLS", "MWS"));

    @Override
    public String runQC(String header, String body, String nnn) {
        final String nnnUpper = nnn.toUpperCase();
        body = body.toUpperCase();

        if (!BULLETIN_NOT_REQUIRED_PILS.contains(nnnUpper)
                && !body.contains("EAS ACTIVATION")
                && !body.contains("IMMEDIATE BROADCAST")) {
            return "BULLETIN line not found in the MND header.\n";
        }

        int bulletinState = 0;
        if (NO_BULLETIN_LINE_PILS.contains(nnnUpper)) {
            bulletinState = 1;
        }

        VtecObject vtec = VtecUtil.parseMessage(body);
        String phensig = (vtec != null) ? vtec.getPhensig() : null;

        boolean dateTested = false;
        StringBuilder errorMsg = new StringBuilder();
        for (String line : body.split("\n")) {
            if (line.contains("EAS ACTIVATION")
                    || line.contains("IMMEDIATE BROADCAST")) {
                bulletinState = 1;
                continue;
            }

            if (bulletinState == 1) {
                QualityControl qcInstance = QualityControl.getInstance();
                String eventType = qcInstance.getProductWarningType(phensig)
                        .orElseGet(() -> qcInstance
                                .getProductWarningType(nnnUpper).orElse(null));
                if (eventType == null) {
                    errorMsg.append("Invalid event type in MND header")
                            .append('\n');
                } else if (!line.contains(eventType.toUpperCase())) {
                    errorMsg.append(
                            "Event type in MND header does not\n match ")
                            .append(nnnUpper).append(".\n");
                }
                bulletinState++;
            } else if (bulletinState == 2) {
                if (line.contains("UNLOCALIZED SITE")) {
                    errorMsg.append("Unlocalized site in MND header.")
                            .append('\n');
                }
                bulletinState++;
            } else if ((bulletinState == 3) || (bulletinState == 4)) {
                if (line.startsWith("ISSUED BY NATIONAL WEATHER SERVICE")) {
                    if (line.endsWith("UNLOCALIZED SITE")) {
                        errorMsg.append(
                                "Unlocalized site in the service backup line.")
                                .append('\n');
                    }
                } else if (!dateTested) {
                    Matcher m = QCCheckConstants.DATE_PATTERN.matcher(line);
                    if (!m.find()) {
                        errorMsg.append("No date and time line in MND header.")
                                .append('\n');
                    }
                    dateTested = true;
                }
                bulletinState++;
            }

        }

        return errorMsg.toString();
    }

}
