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
package com.raytheon.viz.gfe.ui;

import com.raytheon.viz.gfe.core.msgs.ContourServerMsg;
import com.raytheon.viz.gfe.core.msgs.Message;

/**
 * Contour Server Algorithm Selection Menu
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 27, 2009           randerso  Initial creation
 * Jan 24, 2018  7153     randerso  Code cleanup
 *
 * </pre>
 *
 * @author randerso
 */

public class ContourServerAlgorithmMenu extends EnumMenu {
    private static final String COMMAND_ID = "com.raytheon.viz.gfe.actions.SetContourServerAlgorithm";

    public static enum ContourServerAlgorithm {
        CONTOUR_ANALYZER("Contour Analyzer"),
        SIRS_SERVER("Internal SIRS Server");

        private String displayString;

        private ContourServerAlgorithm(String displayString) {
            this.displayString = displayString;
        }

        @Override
        public String toString() {
            return this.displayString;
        }

        /**
         * @param displayString
         * @return ContourServerAlgorithm corresponding to displayString
         */
        public static ContourServerAlgorithm valueFrom(String displayString) {
            ContourServerAlgorithm result = null;
            for (ContourServerAlgorithm mode : values()) {
                if (mode.displayString.equals(displayString)) {
                    result = mode;
                    break;
                }
            }
            return result;
        }
    }

    /**
     * Constructor
     */
    public ContourServerAlgorithmMenu() {
        super();
    }

    @Override
    protected String getCommandId() {
        return COMMAND_ID;
    }

    @Override
    protected String getCommandName() {
        return "SetContourServerAlgorithm";
    }

    @Override
    protected Enum<?> getCurrentValue() {
        ContourServerMsg msg = Message
                .inquireLastMessage(ContourServerMsg.class);
        return msg.getAlgorithm();
    }

    @Override
    protected void setCurrentValue(Enum<?> value) {
        new ContourServerMsg((ContourServerAlgorithm) value).send();
    }

    @Override
    protected String getCategoryId() {
        return "GFE";
    }

}
