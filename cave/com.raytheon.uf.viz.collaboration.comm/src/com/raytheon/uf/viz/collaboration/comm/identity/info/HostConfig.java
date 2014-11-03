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
package com.raytheon.uf.viz.collaboration.comm.identity.info;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * Collaboration host configuration object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 9, 2014  3708      bclement     moved from SiteConfigurationInformation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class HostConfig {
    @XmlAttribute
    private String hostname;

    @XmlAttribute
    private String prettyName;

    @XmlAttribute
    private boolean removed = false;

    /**
     * 
     */
    public HostConfig() {
    }

    public HostConfig(String hostname) {
        this.hostname = hostname;
    }

    /**
     * @return the hostname
     */
    public String getHostname() {
        return hostname;
    }

    /**
     * @param hostname
     *            the hostname to set
     */
    public void setHostname(String hostname) {
        this.hostname = hostname;
    }

    /**
     * @return the prettyName
     */
    public String getPrettyName() {
        return prettyName;
    }

    /**
     * @param prettyName
     *            the prettyName to set
     */
    public void setPrettyName(String prettyName) {
        this.prettyName = prettyName;
    }

    /**
     * Format for display to the user
     */
    @Override
    public String toString() {
        if (prettyName == null) {
            return "Site Server : " + hostname;
        } else {
            return prettyName + " : " + hostname;
        }
    }

    /**
     * Remove description name from hostname
     * 
     * @param text
     * @return
     */
    public static String removeDescription(String text) {
        int firstColon = text.indexOf(':');
        if (firstColon >= 0) {
            text = text.substring(firstColon + 1);
        }
        return text.trim();
    }

    /**
     * @return the removed
     */
    public boolean isRemoved() {
        return removed;
    }

    /**
     * @param removed
     *            the removed to set
     */
    public void setRemoved(boolean removed) {
        this.removed = removed;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((hostname == null) ? 0 : hostname.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        HostConfig other = (HostConfig) obj;
        if (hostname == null) {
            if (other.hostname != null)
                return false;
        } else if (!hostname.equals(other.hostname))
            return false;
        return true;
    }

}
