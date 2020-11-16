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
package com.raytheon.viz.gfe.core.msgs;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.smarttool.MissingDataMode;

/**
 * Message containing the current missing data mode
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jan 21, 2010           randerso  Initial creation
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author randerso
 */

public class MissingDataModeMsg extends Message {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MissingDataModeMsg.class);

    private MissingDataMode mode;

    /**
     * Default constructor for use in {@link Message#inquireLastMessage(Class)}
     */
    public MissingDataModeMsg() {
        String modeString = GFEPreference.getString("MissingDataMode", "Stop");
        try {
            mode = MissingDataMode.valueFrom(modeString);
        } catch (Throwable e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading default Missing Data Mode", e);
            mode = MissingDataMode.STOP;
        }
    }

    /**
     * Constructor
     *
     * @param mode
     */
    public MissingDataModeMsg(MissingDataMode mode) {
        this.mode = mode;
    }

    /**
     * @return the missing data mode
     */
    public MissingDataMode getMode() {
        return mode;
    }

}
