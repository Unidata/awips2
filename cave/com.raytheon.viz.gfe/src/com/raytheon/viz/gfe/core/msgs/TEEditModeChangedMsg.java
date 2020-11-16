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

import com.raytheon.viz.gfe.GFEPreference;

/**
 * Temporal Editor Mode Changed message
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

public class TEEditModeChangedMsg extends Message {
    private TEMode mode;

    /** Temporal Editor modes */
    public static enum TEMode {
        /** Absolute */
        ABSOLUTE,

        /** Relative */
        RELATIVE
    }

    /**
     * Default constructor for use in {@link Message#inquireLastMessage(Class)}
     */
    public TEEditModeChangedMsg() {
        boolean absoluteModeFlag = GFEPreference
                .getBoolean("TemporalEditorAbsoluteEditMode");
        mode = (absoluteModeFlag ? TEMode.ABSOLUTE : TEMode.RELATIVE);
    }

    /**
     * Constructor
     *
     * @param mode
     */
    public TEEditModeChangedMsg(TEMode mode) {
        this.mode = mode;
    }

    /**
     * @return the Temporal Editor Mode
     */
    public TEMode getMode() {
        return mode;
    }

}
