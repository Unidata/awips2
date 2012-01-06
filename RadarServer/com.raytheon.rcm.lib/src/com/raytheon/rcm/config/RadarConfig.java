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
package com.raytheon.rcm.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement
@XmlType(name = "radar", propOrder = {})
@XmlAccessorType(XmlAccessType.NONE)
/*
 * Encapsulates the parameters needed to use a RPG.
 * 
 * The setters should not be considered public.
 */
public class RadarConfig implements Cloneable, Comparable<RadarConfig> {
    @XmlElement
    private String radarID;

    @XmlElement
    private int nexradID;

    @XmlElement
    private boolean enabled = true;

    @XmlElement
    private boolean dedicated;

    @XmlElement
    private boolean collectionEnabled;

    @XmlElement
    private boolean sendEnvironmentalData;

    @XmlElement(defaultValue = "TCP_WAN")
    private LinkType linkType;

    @XmlElementWrapper(name = "links")
    @XmlElement(name = "link")
    private LinkResource[] linkResources;

    /**
     * Indicates that the product availability field in GSMs is useful. On test
     * systems (those with a real RDA), that field always reports a failure.
     */
    @XmlElement
    private boolean productAvailabilityFieldUsable;

    /*
     * Usually, isDedicated() implies that the connection always active and
     * not-isDedicated implies the the connection is on demand. There is
     * currently no need to change this policy. The point of this is to allow
     * disabling a RPG connection.
     */
    public static final int ACTIVATE_NEVER = 0;

    public static final int ACTIVATE_ALWAYS = 1;

    public static final int ACTIVATE_ON_DEMAND = 2;

    public String getRadarID() {
        return radarID;
    }

    public void setRadarID(String radarID) {
        this.radarID = radarID;
    }

    public int getNexradID() {
        return nexradID;
    }

    public void setNexradID(int nexradID) {
        this.nexradID = nexradID;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public boolean isDedicated() {
        return dedicated;
    }

    public void setDedicated(boolean dedicated) {
        this.dedicated = dedicated;
    }

    public boolean isCollectionEnabled() {
        return collectionEnabled;
    }

    public void setCollectionEnabled(boolean collectionEnabled) {
        this.collectionEnabled = collectionEnabled;
    }

    public boolean isSendEnvironmentalData() {
        return sendEnvironmentalData;
    }

    public void setSendEnvironmentalData(boolean sendEnvironmentalData) {
        this.sendEnvironmentalData = sendEnvironmentalData;
    }

    public LinkType getLinkType() {
        return linkType;
    }

    public void setLinkType(LinkType linkType) {
        this.linkType = linkType;
    }

    public LinkResource[] getLinkResources() {
        return linkResources;
    }

    public void setLinkResources(LinkResource[] linkResources) {
        this.linkResources = linkResources;
    }

    public int getActivation() {
        return enabled ? (dedicated ? ACTIVATE_ALWAYS : ACTIVATE_ON_DEMAND)
                : ACTIVATE_NEVER;
    }

    public boolean isProductAvailabilityFieldUsable() {
        return productAvailabilityFieldUsable;
    }

    public void setProductAvailabilityFieldUsable(
            boolean productAvailabilityFieldUsable) {
        this.productAvailabilityFieldUsable = productAvailabilityFieldUsable;
    }

    public RadarConfig duplicate() {
        RadarConfig other;
        try {
            other = (RadarConfig) clone();
        } catch (CloneNotSupportedException e) {
            throw new UnsupportedOperationException(e);
        }
        if (linkResources != null) {
            other.linkResources = new LinkResource[linkResources.length];
            for (int i = 0; i < other.linkResources.length; ++i)
                other.linkResources[i] = (LinkResource) linkResources[i]
                        .clone();
        }
        return other;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(RadarConfig o) {
        return this.radarID.compareTo(o.radarID);
    }
}
