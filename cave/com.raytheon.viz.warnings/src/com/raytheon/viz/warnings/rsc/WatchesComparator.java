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
package com.raytheon.viz.warnings.rsc;

import java.util.Comparator;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;

/**
 * Comparator class for SPC watches. The logic for the compare method differs
 * from the WarningRecordComparator.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 18, 2013            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class WatchesComparator implements Comparator<AbstractWarningRecord> {
    /**
     * Compares the WarningRecords by phenSig, ETN, action, then starttime
     * (ascending order)
     */
    @Override
    public int compare(AbstractWarningRecord wr1, AbstractWarningRecord wr2) {
        int rval = 0;

        if (wr1.getPhensig() != null && wr2.getPhensig() != null) {
            rval = wr1.getPhensig().compareTo(wr2.getPhensig());
        }

        if (rval == 0) {
            if (wr1.getEtn() != null && wr2.getEtn() != null) {
                rval = Double.compare(Double.parseDouble(wr1.getEtn()),
                        Double.parseDouble(wr2.getEtn()));
            }

            if (rval == 0) {
                if (wr1.getAct() != null && wr2.getAct() != null) {
                    WarningAction act1 = WarningAction.valueOf(wr1.getAct());
                    WarningAction act2 = WarningAction.valueOf(wr2.getAct());
                    if (act1 == act2) {
                        rval = 0;
                    } else if (act1 == WarningAction.NEW) {
                        rval = -1;
                    } else if (act2 == WarningAction.NEW) {
                        rval = 1;
                    } else if (act1 == WarningAction.CON
                            && (act2 == WarningAction.CAN || act2 == WarningAction.EXP)) {
                        return -1;
                    } else if (act2 == WarningAction.CON
                            && (act1 == WarningAction.CAN || act1 == WarningAction.EXP)) {
                        return 1;
                    } else {
                        rval = wr1.getAct().compareTo(wr2.getAct());
                    }
                }

            }

            if (rval == 0) {
                rval = wr1.getStartTime().compareTo(wr2.getStartTime());
            }
        }
        return rval;
    }
}
