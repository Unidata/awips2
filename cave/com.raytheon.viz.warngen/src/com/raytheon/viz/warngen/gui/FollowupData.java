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

import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.EmergencyType;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
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
 * Jul 22, 2013 2176       jsanchez    Added EMER to the display string in the update list.
 * Aug 7, 2013  2243       jsanchez    Set all the attributes of an AbstractWarningRecord and added an expiration string. Removed calendar object.
 * Aug 15,2013  2243       jsanchez    Improved the expiration string off by one minute. Fixed for practice mode.
 * Aug 15,2013  2243       jsanchez    Improved the expiration string off by one minute.
 * Sep  4,2013  2176       jsanchez    Used EmergencyType class to identify emergency products. 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class FollowupData extends AbstractWarningRecord {

    private static final long serialVersionUID = 1L;

    /**
     * String displayed in the drop down update list.
     */
    private final String displayString;

    /**
     * String used to test if this object is equivalent to one of the updated
     * items in the drop down.
     */
    private String equvialentString;

    /**
     * Information string used when the follow up is no longer valid or allowed.
     */
    private final String expirationString;

    public FollowupData(WarningAction action, AbstractWarningRecord record) {
        super(record);
        setAct(action.toString());

        displayString = createDisplayString(action, record);
        expirationString = createExpirationString(action);
    }

    /**
     * Builds the string to describe the available followup product to the end
     * user in the "Update List" dropdown.
     * 
     * @param status
     * @param record
     * @return
     */
    private String createDisplayString(WarningAction status,
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

        if (EmergencyType.isEmergency(record.getRawmessage())) {
            rval.append(" " + EmergencyType.EMER);
        }
        equvialentString = rval.substring(0,
                record.getProductClass().equals("T") ? 20 : 18);

        return rval.toString();
    }

    /**
     * Creates the expiration string based on the action. The expiration string
     * provides an explanation of why the follow up data is no longer valid.
     * 
     * @param action
     * @return
     */
    private String createExpirationString(WarningAction action) {
        String message = null;
        if (action == WarningAction.NEW) {
            message = "Reissue no longer allowed; after 30 minutes of warning expiration.";
        } else if (action == WarningAction.COR) {
            message = "Correction no longer allowed; after 10 minutes of warning issuance.";
        } else if (action == WarningAction.CAN) {
            message = "Cancellation no longer allowed; within 10 minutes of warning expiration.";
        } else if (action == WarningAction.CON) {
            message = "Continuation no longer allowed; within 5 minutes of warning expiration.";
        } else if (action == WarningAction.EXP) {
            message = "Expiration no longer allowed; after 10 minutes of warning expiration.";
        } else if (action == WarningAction.EXT) {
            message = "Extention no longer allowed; within 5 minutes of warning expiration.";
        }
        return message;
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
        long timeInMillis = SimulatedTime.getSystemTime().getMillis();
        if (status != WarningAction.COR) {
            // use double to keep precision until it's casted to an integer
            double diffMillis = record.getEndTime().getTimeInMillis()
                    - timeInMillis;
            int diffMins = (int) Math.round(diffMillis
                    / TimeUtil.MILLIS_PER_MINUTE);
            if (diffMins == 0) {
                rval.append(" Expired");
            } else if (diffMins > 0) {
                rval.append(" Exp in ").append(diffMins).append(" min");
            } else {
                rval.append(" Exp ").append(-diffMins).append(" min ago");
            }
        } else {
            // use double to keep precision until it's casted to an integer
            double diffMillis = timeInMillis
                    - record.getIssueTime().getTimeInMillis();
            int diffMins = (int) Math.round(diffMillis
                    / TimeUtil.MILLIS_PER_MINUTE);

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

    public String getDisplayString() {
        return displayString;
    }

    public String getEquvialentString() {
        return equvialentString;
    }

    public String getExpirationString() {
        return expirationString;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.dataplugin.PluginDataObject#getPluginName()
     */
    @Override
    public String getPluginName() {
        return "followupwarning";
    }
}
