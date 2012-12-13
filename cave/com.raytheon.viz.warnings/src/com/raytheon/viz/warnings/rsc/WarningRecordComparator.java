package com.raytheon.viz.warnings.rsc;

import java.util.Comparator;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;

/**
 * 
 * Compares the WarningRecords by phenSig, ETN, action, then starttime
 * (descending order)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2011            jsanchez     Initial creation
 * Sep 27, 2012     1149   jsanchez     Updated order of actions. Put CON before CAN,EXP
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class WarningRecordComparator implements
        Comparator<AbstractWarningRecord> {

    /**
     * Compares the WarningRecords by phenSig, ETN, action, then starttime
     * (descending order)
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

                if (rval == 0) {
                    rval = wr1.getStartTime().compareTo(wr2.getStartTime());
                    // sort warnings in descending order
                    if (wr1.getSig() != null) {
                        if (!wr1.getSig().equals("A")) {
                            rval *= -1;
                        }
                    }
                }
            }
        }
        return rval;
    }
}
