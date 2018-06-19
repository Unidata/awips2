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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.constants.StatusConstants;
import com.raytheon.viz.gfe.smarttool.MissingDataMode;

/**
 * Message containing the current missing data mode
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

public class MissingDataModeMsg extends Message {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(MissingDataModeMsg.class);
    private MissingDataMode mode;

    public MissingDataModeMsg() {
        IPreferenceStore prefs = Activator.getDefault().getPreferenceStore();

        mode = MissingDataMode.STOP;
        String modeString;
        if (!(modeString = prefs.getString("MissingDataMode")).isEmpty()) {
            try {
                mode = MissingDataMode.valueFrom(modeString);
            } catch (Throwable e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error reading default Missing Data Mode", e);
            }
        }
    }

    /**
     * 
     */
    public MissingDataModeMsg(MissingDataMode mode) {
        this.mode = mode;
    }

    /**
     * @return the mode
     */
    public MissingDataMode getMode() {
        return mode;
    }

}
