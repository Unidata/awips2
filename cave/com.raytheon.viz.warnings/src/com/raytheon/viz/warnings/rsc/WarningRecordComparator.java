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
 * 
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
    public int compare(AbstractWarningRecord wr1, AbstractWarningRecord wr2) {
        int rval = 0;

        rval = wr1.getPhensig().compareTo(wr2.getPhensig());

        if (rval == 0) {
            rval = Double.compare(Double.parseDouble(wr1.getEtn()),
                    Double.parseDouble(wr2.getEtn()));
            if (rval == 0) {
                if (wr1.getAct().equals(wr2.getAct())) {
                    rval = 0;
                } else if (wr1.getAct().equals(WarningAction.NEW.toString())) {
                    rval = -1;
                } else if (wr2.getAct().equals(WarningAction.NEW.toString())) {
                    rval = 1;
                } else {
                    rval = wr1.getAct().compareTo(wr2.getAct());
                }

                if (rval == 0) {
                    rval = -1
                            * wr1.getStartTime().compareTo(wr2.getStartTime());
                }
            }
        }
        return rval;
    }
}
