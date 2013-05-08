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
package com.raytheon.viz.warngen.gui;

import java.util.Calendar;

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * A subclass of Warning Record to create the display strings needed by the GUI.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                                     Initial creation
 * May 7, 2013  1973       rferrel     Changes to properly display Issue Time.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class FollowupData extends WarningRecord {

    private static final long serialVersionUID = 1L;

    /**
     * String displayed in the drop down update list.
     */
    public String displayString;

    /**
     * String used to test if this object is equivalent to one of the updated
     * items in the drop down.
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
        StringBuilder rval = new StringBuilder();
        if (record.getProductClass().equals("T")) {
            rval.append("T.");
        }
        rval.append(status.toString()).append("-");
        rval.append(record.getOfficeid()).append(".");
        rval.append(record.getPhen()).append(".");
        rval.append(record.getSig()).append(".");
        rval.append(record.getEtn());

        if (status != WarningAction.CAN) {
            rval.append(buildExpStr(status, record));
        }

        equvialentString = rval.substring(0,
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
        StringBuilder rval = new StringBuilder();
        Calendar cal = Calendar.getInstance();
        cal.setTime(SimulatedTime.getSystemTime().getTime());
        if (status != WarningAction.COR) {
            // Positive means not yet expired
            long diffMins = (record.getEndTime().getTimeInMillis() - cal
                    .getTimeInMillis()) / TimeUtil.MILLIS_PER_MINUTE;
            if (diffMins == 0) {
                rval.append(" Expired");
            } else if (diffMins > 0) {
                rval.append(" Exp in ").append(diffMins).append(" min");
            } else {
                rval.append(" Exp ").append(-diffMins).append(" min ago");
            }
        } else {
            long diffMins = (cal.getTimeInMillis() - record.getIssueTime()
                    .getTimeInMillis()) / TimeUtil.MILLIS_PER_MINUTE;
            if (diffMins == 0) {
                rval.append(" Just Issued");
            } else {
                rval.append(" Issued ").append(diffMins).append(" min ago");
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
