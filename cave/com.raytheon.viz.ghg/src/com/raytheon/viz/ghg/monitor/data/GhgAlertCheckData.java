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
package com.raytheon.viz.ghg.monitor.data;

import com.raytheon.viz.ghg.monitor.data.GhgConfigData.AlertsEnum;

/**
 * Alert Check data object. Tells the current alert state ie, Alert1, Alert2,
 * Expired
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 4, 2010            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GhgAlertCheckData {
    /** Type of alert */
    private AlertsEnum alertType = AlertsEnum.NoAlerts;

    /** Time to purge */
    private int deltaP;

    /** Time to expiration */
    private int deltaE;

    /**
     * Constructor.
     * 
     * @param alertType
     *            AlertsEnum Type of alert
     * @param deltaP
     *            Number of minutes until purge time
     * @param deltaE
     *            Number of minutes until expiration time
     */
    public GhgAlertCheckData(AlertsEnum alertType, int deltaP, int deltaE) {
        this.alertType = alertType;
        this.deltaP = deltaP;
        this.deltaE = deltaE;
    }

    /**
     * Constructor.
     * 
     * @param alertType
     *            AlertsEnum Alert type
     */
    public GhgAlertCheckData(AlertsEnum alertType) {
        this.alertType = alertType;
    }

    /**
     * @return the alertType
     */
    public AlertsEnum getAlertType() {
        return alertType;
    }

    /**
     * @param alertType
     *            the alertType to set
     */
    public void setAlertType(AlertsEnum alertType) {
        this.alertType = alertType;
    }

    /**
     * @return the deltaP
     */
    public int getDeltaP() {
        return deltaP;
    }

    /**
     * @param deltaP
     *            the deltaP to set
     */
    public void setDeltaP(int deltaP) {
        this.deltaP = deltaP;
    }

    /**
     * @return the deltaE
     */
    public int getDeltaE() {
        return deltaE;
    }

    /**
     * @param deltaE
     *            the deltaE to set
     */
    public void setDeltaE(int deltaE) {
        this.deltaE = deltaE;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(alertType.name() + "\n");
        sb.append(deltaE + " : " + deltaP);
        return super.toString();
    }
}
