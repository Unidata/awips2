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
package com.raytheon.uf.viz.alertviz.ui.util;

import java.text.SimpleDateFormat;
import java.util.TimeZone;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.viz.alertviz.config.TrayConfiguration;

/**
 * AlertViz Message Formatter
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2018 7454       randerso     Initial creation
 *
 * </pre>
 *
 * @author randerso
 */

public class MessageFormatter {
    private TrayConfiguration trayConfig;

    /**
     * Time format.
     */
    private static SimpleDateFormat timeFormat = new SimpleDateFormat(
            "HH:mm:ss");

    /**
     * Constructor
     *
     * @param trayConfig
     */
    public MessageFormatter(TrayConfiguration trayConfig) {
        this.trayConfig = trayConfig;
    }

    /**
     * @param trayConfig
     */
    public void setTrayConfig(TrayConfiguration trayConfig) {
        this.trayConfig = trayConfig;
    }

    /**
     * Get the status message in a formatted string.
     *
     * @param sm
     *            Status message.
     * @param trayConfig
     *            Global configuration.
     * @return The status message as a formatted string.
     */
    public String getFormattedMessage(StatusMessage sm) {
        StringBuilder strBld = new StringBuilder();
        String localTZ = System.getenv("FXA_LOCAL_TZ");
        if (localTZ == null) {
            localTZ = "GMT";
        }
        timeFormat.setTimeZone(TimeZone.getTimeZone(localTZ));

        strBld.append(timeFormat.format(sm.getEventTime())).append(" ");

        if (trayConfig.isPriorityShown()) {
            strBld.append("(").append(sm.getPriority().ordinal())
                    .append(") | ");
        }

        if (trayConfig.isSourceKeyShown()) {
            strBld.append(sm.getSourceKey()).append(" | ");
        }

        if (trayConfig.isCategoryShown()) {
            strBld.append(sm.getCategory()).append(" : ");
        }

        strBld.append(sm.getMessage());

        return strBld.toString();
    }

}
