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
package com.raytheon.edex.plugin.sfcobs.common;

import java.util.ArrayList;
import java.util.List;

/**
 * The SfcObsSubMessage represents the text weather message that follows the WMO
 * header in the full weather message. In some text data additional sub-messages
 * will be created if the YYGGIsubw entry changes with in the message.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20070925            391 jkorman     Initial Coding.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class SfcObsSubMessage {
    // The message report identifier.
    private SfcObsPart messagePart = null;

    // The YYGGIsubW part if found.
    private SfcObsPart YYGGIwPart = null;

    // List of all parts of the sub-message.
    private List<SfcObsPart> parts = null;

    // List of all reports extracted from the parts.
    private List<String> reports = null;

    public SfcObsSubMessage() {
        parts = new ArrayList<SfcObsPart>();
    }

    /**
     * @return the messagePart
     */
    public SfcObsPart getMessagePart() {
        return messagePart;
    }

    /**
     * Set the message part that identifies the report type. "AAXX" for synoptic
     * as an example.
     * 
     * @param messagePart
     *            The messagePart to set
     */
    public void setMessagePart(SfcObsPart messagePart) {
        this.messagePart = messagePart;
    }

    /**
     * @return the yYGGIwPart
     */
    public SfcObsPart getYYGGIwPart() {
        return YYGGIwPart;
    }

    /**
     * Set the YYGGIsubW part for this sub-message.
     * 
     * @param iwPart
     *            the yYGGIwPart to set
     */
    public void setYYGGIwPart(SfcObsPart iwPart) {
        YYGGIwPart = iwPart;
    }

    /**
     * Get the list of reports within this sub-message.
     * 
     * @return The report list.
     */
    public List<String> getReports() {
        if (reports == null) {
            reports = createReports();
        }
        return reports;
    }

    /**
     * The the list of parts in this sub-message.
     * 
     * @return The sub-message parts.
     */
    public List<SfcObsPart> getParts() {
        return parts;
    }

    /**
     * Create the list of individual reports from the sub-message part list.
     * 
     * @param messageParts
     * @return
     */
    private List<String> createReports() {
        List<String> reports = new ArrayList<String>();
        if (parts != null) {
            StringBuilder sb = null;
            boolean dataWritten = false;
            for (SfcObsPart part : parts) {
                if (sb == null) {
                    sb = createNewReport();
                    dataWritten = false;
                }
                if (SfcObsPart.RE_PART.equals(part)) {
                    reports.add(sb.toString());
                    sb = null;
                } else if (SfcObsPart.CR_PART.equals(part)) {
                    // just throw it away.
                } else if (SfcObsPart.ME_PART.equals(part)) {
                    break;
                } else {
                    sb.append(" ");
                    sb.append(part.getPartValue());
                    dataWritten = true;
                }
            }
            // We may have finished without seeing a report end part so
            // add the final report being built.
            if ((sb != null) && (dataWritten)) {
                reports.add(sb.toString());
            }
        }
        return reports;
    }

    /**
     * Create a new report with the messagePart (type) followed by the YYGGIw if
     * present.
     * 
     * @return A StringBuilder with the filled out data.
     */
    private StringBuilder createNewReport() {
        StringBuilder report = new StringBuilder();
        if (messagePart != null) {
            report.append(messagePart);
        }
        if (YYGGIwPart != null) {
            report.append(" ");
            report.append(YYGGIwPart);
        }

        return report;
    }

}
