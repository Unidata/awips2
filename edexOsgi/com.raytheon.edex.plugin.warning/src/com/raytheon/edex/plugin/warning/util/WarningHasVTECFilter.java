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

package com.raytheon.edex.plugin.warning.util;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 16, 2011            ekladstrup     Initial creation
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class WarningHasVTECFilter {

    public WarningHasVTECFilter() {

    }

    /**
     * returns true if any of the items in the array of pdos is a WarningRecord
     * and has a vtec string
     * 
     * @param pdo
     * @return
     */
    public boolean hasVTEC(PluginDataObject[] pdo) {
        boolean hadWarningRecord = false;

        // if just one of the objects has a vtec string return true
        for (PluginDataObject pd : pdo) {
            if (pd instanceof AbstractWarningRecord) {
                hadWarningRecord = true;
                AbstractWarningRecord wr = (AbstractWarningRecord) pd;
                if (wr.getVtecstr() != null
                        && !wr.getVtecstr().trim().isEmpty()) {
                    // found a vtec, return true. Ideally any without a vtec
                    // would not be sent to the active table and those with vtec
                    // would.
                    return true;
                }
            }
        }
        if (hadWarningRecord) {
            // return false if there were warning records ( should be all or
            // none ) but no vtec string in any of them
            return false;
        } else {
            // return a default value of true when no warning records are found
            // so that the default full route is followed when ingesting
            // warnings
            return true;
        }
    }
}
