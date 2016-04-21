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
package com.raytheon.viz.ghg.monitor.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.viz.ghg.monitor.data.GhgAlertData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ghg.utilities.GhgUtilities;

/**
 * Contains the combined configuration data for GHG Monitor Alerts.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06Jun2008    1157       MW Fegan    Initial creation.
 * 17Jun2008    1157       MW Fegan    Converted List&lt;String&gt; to String[].
 * 20Jun2008    1157       MW Fegan    Made cloneable.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

@XmlRootElement(name="ghgAlerts")
@XmlAccessorType(XmlAccessType.NONE)
public class GhgAlertsXml implements Cloneable {
    /**
     * flag to limit alerting to local WFO only
     */
    @XmlElement
    private boolean local = false;

    /**
     * flag to enable alerting of test products.
     */
    @XmlElement
    private boolean test = false;

    /**
     * contains the "alert 1" data
     */
    @XmlElement
    private GhgAlertData alert1 = null;

    /**
     * contains the "alert 2" data
     */
    @XmlElement
    private GhgAlertData alert2 = null;

    /**
     * contains the "alert expired" data
     */
    @XmlElement
    private GhgAlertData expired = null;

    /**
     * List of actions on which to alert.
     */
    @XmlElement
    private String[] actions = null;

    /**
     * List of phenomena/significance pairs to alert.
     */
    @XmlElement
    private String[] phenSigs = null;

    /**
     * List of product indicators to alert.
     */
    @XmlElement
    private String[] pils = null;

    /**
     * Constructor. Creates an empty object.
     */
    public GhgAlertsXml() {
    }

    /**
     * Copy constructor.
     * 
     * @param rhs
     *            the object to copy.
     */
    public GhgAlertsXml(GhgAlertsXml rhs) {
        /* copy the simple flags */
        local = rhs.local;
        test = rhs.test;

        /* clone the alert data groups */
        alert1 = rhs.alert1.clone();
        alert2 = rhs.alert2.clone();
        expired = rhs.expired.clone();

        /* clone the variable lists */
        actions = GhgUtilities.arrayClone(rhs.actions);
        phenSigs = GhgUtilities.arrayClone(rhs.phenSigs);
        pils = GhgUtilities.arrayClone(rhs.pils);
    }

    /**
     * Single method to set all the alert data objects.
     * 
     * @param alerts
     *            the alerts to set.
     */
    public void setAlerts(GhgAlertData[] alerts) {
        for (GhgAlertData alert : alerts) {
            addAlert(alert);
        }
    }

    /**
     * Single method to retrieve all the alert data objects.
     * 
     * @return the alert data objects.
     */
    public GhgAlertData[] getAlerts() {
        return new GhgAlertData[] { alert1, alert2, expired };
    }

    /**
     * Returns the specified alert data object.
     * 
     * @param type
     *            type of alert data to return.
     * 
     * @return the alert data object.
     */
    public GhgAlertData getAlert(GhgConfigData.AlertsEnum type) {
        GhgAlertData retVal = new GhgAlertData();
        switch (type) {
        case AlertLvl1:
            retVal = alert1;
            break;
        case AlertLvl2:
            retVal = alert2;
            break;
        case ExpiredAlert:
            retVal = expired;
            break;
        }
        return retVal;
    }

    /**
     * Sets a single alert data object.
     * 
     * @param alert
     *            the alert data object to set.
     */
    public void addAlert(GhgAlertData alert) {
        switch (alert.getType()) {
        case AlertLvl1:
            alert1 = alert;
            break;
        case AlertLvl2:
            alert2 = alert;
            break;
        case ExpiredAlert:
            expired = alert;
            break;
        }
    }

    /**
     * Returns the alert selections for the specified alert type.
     * 
     * @param which
     *            the alert type
     */
    public String[] getSelections(GhgConfigData.AlertsFilterEnum which) {
        String[] retVal = null;
        switch (which) {
        case Action:
            retVal = actions;
            break;
        case PhenSig:
            retVal = phenSigs;
            break;
        case Pil:
            retVal = pils;
            break;
        }
        return retVal;
    }

    /**
     * Sets the list of alert selections for the specified alert type.
     * 
     * @param which
     *            the alert type
     * @param data
     *            the alert selections
     */
    public void setSelections(GhgConfigData.AlertsFilterEnum which, String[] data) {
        switch (which) {
        case Action:
            actions = data;
            break;
        case PhenSig:
            phenSigs = data;
            break;
        case Pil:
            pils = data;
            break;
        }
    }

    /**
     * @return the actions
     */
    public String[] getActions() {
        return actions;
    }

    /**
     * @param actions
     *            the actions to set
     */
    public void setActions(String[] actions) {
        this.actions = actions;
    }

    /**
     * @return the phenSigs
     */
    public String[] getPhenSigs() {
        return phenSigs;
    }

    /**
     * @param phenSigs
     *            the phenSigs to set
     */
    public void setPhenSigs(String[] phenSigs) {
        this.phenSigs = phenSigs;
    }

    /**
     * @return the pils
     */
    public String[] getPils() {
        return pils;
    }

    /**
     * @param pils
     *            the pils to set
     */
    public void setPils(String[] pils) {
        this.pils = pils;
    }

    /**
     * @return the local
     */
    public boolean isLocal() {
        return local;
    }

    /**
     * @param local
     *            the local to set
     */
    public void setLocal(boolean local) {
        this.local = local;
    }

    /**
     * @return the test
     */
    public boolean isTest() {
        return test;
    }

    /**
     * @param test
     *            the test to set
     */
    public void setTest(boolean test) {
        this.test = test;
    }

    @Override
    public GhgAlertsXml clone() {
        return new GhgAlertsXml(this);
    }
}