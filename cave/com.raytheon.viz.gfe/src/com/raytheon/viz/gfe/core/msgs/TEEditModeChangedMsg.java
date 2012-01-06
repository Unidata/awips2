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

import org.eclipse.jface.preference.IPreferenceStore;

import com.raytheon.viz.gfe.Activator;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2010            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class TEEditModeChangedMsg extends Message {
    private TEMode mode;

    public static enum TEMode {
        ABSOLUTE, RELATIVE
    }

    public TEEditModeChangedMsg() {
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();

        boolean absoluteModeFlag = prefs
                .getBoolean("TemporalEditorAbsoluteEditMode");
        mode = (absoluteModeFlag ? TEMode.ABSOLUTE : TEMode.RELATIVE);
    }

    public TEEditModeChangedMsg(TEMode mode) {
        this.mode = mode;
    }

    public TEMode getMode() {
        return mode;
    }

}
