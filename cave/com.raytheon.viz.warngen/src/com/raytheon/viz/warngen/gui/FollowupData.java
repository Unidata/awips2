package com.raytheon.viz.warngen.gui;

import java.util.Calendar;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord;
import com.raytheon.uf.common.time.SimulatedTime;

public class FollowupData extends WarningRecord {

    private static final long serialVersionUID = 1L;

    /** String displayed in the drop down update list */
    public String displayString;

    /**
     * String used to test if this object is equivalent to one of the updated
     * items in the drop down
     */
    public String equvialentString;

    public FollowupData(WarningAction action, AbstractWarningRecord record) {
        setAct(action.toString());
        setOfficeid(record.getOfficeid());
        setPhen(record.getPhen());
        setSig(record.getSig());
        setEtn(record.getEtn());

        displayString = getDisplayString(action, record);
    }

    /**
     * Builds the string to describe the available followup product to the end
     * user in the "Update List" dropdown.
     * 
     * @param status
     * @param record
     * @return
     */
    private String getDisplayString(WarningAction status,
            AbstractWarningRecord record) {
        StringBuffer rval = new StringBuffer();
        if (record.getProductClass().equals("T")) {
            rval.append("T.");
        }
        rval.append(status.toString() + "-");
        rval.append(record.getOfficeid() + ".");
        rval.append(record.getPhen() + ".");
        rval.append(record.getSig() + ".");
        rval.append(record.getEtn());

        if (status != WarningAction.CAN) {
            rval.append(buildExpStr(status, record));
        }

        equvialentString = rval.toString().substring(0,
                record.getProductClass().equals("T") ? 20 : 18);

        return rval.toString();
    }

    /**
     * Builds a string informing the user when a product was issued or when it
     * will expire. This is appended to the product in the "Update List"
     * dropdown
     * 
     * @param status
     * @param record
     * @return
     */
    private String buildExpStr(WarningAction status,
            AbstractWarningRecord record) {
        StringBuffer rval = new StringBuffer();
        Calendar cal = Calendar.getInstance();
        cal.setTime(SimulatedTime.getSystemTime().getTime());
        // Positive means not yet expired
        long diffMins = (record.getEndTime().getTimeInMillis() - cal
                .getTimeInMillis()) / (60 * 1000);
        if (diffMins == 0) {
            rval.append(" Expired");
        } else if (diffMins > 0) {
            rval.append(" Exp in " + diffMins + " min");
        } else {
            rval.append(" Exp " + -diffMins + " min ago");
        }
        if (status == WarningAction.COR) {
            rval.delete(0, rval.length());
            diffMins = (cal.getTimeInMillis() - record.getIssueTime()
                    .getTimeInMillis()) / (60 * 1000);
            if (diffMins == 0) {
                rval.append(" Just Issued");
            } else if (diffMins > 0) {
                rval.append(" Issued " + diffMins + " min ago");
            } else if (diffMins < 0) {
                rval.append(" Issued " + diffMins + " min ago");
            }
        }
        return rval.toString();
    }

    public boolean equals(FollowupData obj) {
        return this.getAct().equals(obj.getAct())
                && this.getOfficeid().equals(obj.getOfficeid())
                && this.getPhen().equals(obj.getPhen())
                && this.getSig().equals(obj.getSig())
                && this.getEtn().equals(obj.getEtn());
    }

}
