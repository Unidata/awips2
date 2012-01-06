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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Contains the alert level specific information needed to define a single GHG
 * Monitor Alert.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06Jun2008    1157       MW Fegan    Initial creation.
 * 20Jun2008    1157       MW Fegan    Made cloneable.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class GhgAlertData implements Cloneable {
    /**
     * Identifies the alert type.
     */
    @XmlElement
    private GhgConfigData.AlertsEnum type = null;

    /**
     * Flag to determine if this alert is enabled.
     */
    @XmlElement
    private boolean enabled = false;

    /**
     * Flag to determine if the monitor should display a flag.
     */
    @XmlElement
    private boolean banner = true;

    /**
     * Time (in minutes) before expiration to trigger the alert.
     */
    @XmlElement
    private int time = 0;
    
    /**
     * Constructor. Creates an alert object with default values.
     */
    public GhgAlertData() {
        // intentionally empty.
    }

    /**
     * Constructor. Creates an alert object with the specified values.
     * 
     * @param enabled
     *            alert enabled flag.
     * @param banner
     *            display banner flag.
     * @param time
     *            alert time value
     * @param type
     *            alert type
     */
    public GhgAlertData(boolean enabled, boolean banner, int time,
            GhgConfigData.AlertsEnum type) {
        this.enabled = enabled;
        this.banner = banner;
        this.time = time;
        this.type = type;
    }

    /**
     * Copy constructor.
     * 
     * @param rhs
     *            the object to copy
     */
    public GhgAlertData(GhgAlertData rhs) {
        enabled = rhs.enabled;
        banner = rhs.banner;
        time = rhs.time;
        type = rhs.type;
    }

    /**
     * @return the enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * @param enabled
     *            the enabled to set
     */
    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * @return the banner
     */
    public boolean isBanner() {
        return banner;
    }

    /**
     * @param banner
     *            the banner to set
     */
    public void setBanner(boolean banner) {
        this.banner = banner;
    }

    /**
     * @return the time
     */
    public int getTime() {
        return time;
    }

    /**
     * @param time
     *            the time to set
     */
    public void setTime(int time) {
        this.time = time;
    }

    /**
     * @return the type
     */
    public GhgConfigData.AlertsEnum getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(GhgConfigData.AlertsEnum type) {
        this.type = type;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public GhgAlertData clone() {
        return new GhgAlertData(this);
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[[" + type + "][Banner: " + banner + "][Enabled: " + enabled + "]]");
        return sb.toString();
    }
}