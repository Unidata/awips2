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

package com.raytheon.edex.plugin.shef.ohdlib;

/**
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2008   1649     snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GagePPOptions {
    private shef_dup shef_duplicate;

    private upd_action upd_action;

    private int intpc;

    private int intlppp;

    private int intuppp;

    private float intppq;

    public static enum shef_dup {
        ALWAYS_OVERWRITE, USE_REVCODE, IF_DIFFERENT, IF_DIFFERENT_OR_REVCODE, IF_DIFFERENT_AND_REVCODE;

        public boolean equalsOption(String option) {
            return name().equals(option);
        }
    }

    public static enum upd_action {
        DONT_UPDATE_ACTION, UPDATE_ACTION, IF_DIFFERENT_UPDATE_ACTION;
        
        public boolean isAction(String action) {
            return name().equals(action);
        }
    }

    public static enum precip_rec {
        PRECIP_LID, PRECIP_PE, PRECIP_DUR, PRECIP_TS, PRECIP_EX, PRECIP_OBS, PRECIP_VAL, PRECIP_SHEF, PRECIP_QC, PRECIP_REV, PRECIP_ID, PRECIP_TIME, PRECIP_POST_TIME
    };

    public static enum dbmsResult {
        Ok, errNoDatabaseName, errSqlFail, errCloseFail
    };

    public GagePPOptions() {
    }

    /**
     * @return the shef_duplicate
     */
    public shef_dup getShef_duplicate() {
        return shef_duplicate;
    }

    /**
     * @param shef_duplicate the shef_duplicate to set
     */
    public void setShef_duplicate(shef_dup shef_duplicate) {
        this.shef_duplicate = shef_duplicate;
    }

    /**
     * @return the upd_action
     */
    public upd_action getUpd_action() {
        return upd_action;
    }

    /**
     * @param upd_action the upd_action to set
     */
    public void setUpd_action(upd_action upd_action) {
        this.upd_action = upd_action;
    }

    /**
     * @return the intpc
     */
    public int getIntpc() {
        return intpc;
    }

    /**
     * @param intpc the intpc to set
     */
    public void setIntpc(int intpc) {
        this.intpc = intpc;
    }

    /**
     * @return the intlppp
     */
    public int getIntlppp() {
        return intlppp;
    }

    /**
     * @param intlppp the intlppp to set
     */
    public void setIntlppp(int intlppp) {
        this.intlppp = intlppp;
    }

    /**
     * @return the intuppp
     */
    public int getIntuppp() {
        return intuppp;
    }

    /**
     * @param intuppp the intuppp to set
     */
    public void setIntuppp(int intuppp) {
        this.intuppp = intuppp;
    }

    /**
     * @return the intppq
     */
    public float getIntppq() {
        return intppq;
    }

    /**
     * @param intppq the intppq to set
     */
    public void setIntppq(float intppq) {
        this.intppq = intppq;
    }
    
}