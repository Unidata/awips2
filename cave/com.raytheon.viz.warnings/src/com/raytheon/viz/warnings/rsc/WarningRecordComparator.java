package com.raytheon.viz.warnings.rsc;

import java.util.Comparator;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;

/**
 * 
 * Sorts the WarningRecords by phenSig, ETN, starttime (descending order), then
 * action
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
    public int compare(AbstractWarningRecord wr1, AbstractWarningRecord wr2) {
        int rval = 0;

        rval = wr1.getPhensig().compareTo(wr2.getPhensig());

        if (rval == 0) {
            rval = Double.compare(Double.parseDouble(wr1.getEtn()),
                    Double.parseDouble(wr2.getEtn()));
            if (rval == 0) {
                rval = -1 * wr1.getStartTime().compareTo(wr2.getStartTime());
                if (rval == 0) {
                    rval = wr1.getAct().compareTo(wr2.getAct());
                }
            }
        }
        return rval;
    }
}
