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
package com.raytheon.uf.common.dataplugin.sfcobs;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 25, 2007  391      jkorman     Initial Coding.
 * Dec 03, 2013  2537     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class AncCloud implements Serializable {

    private static final long serialVersionUID = 1L;

    @XmlAttribute
    @DynamicSerializeElement
    private Integer cloudObsType = null;

    /**
     * Altitude of the upper cloud (meters)
     */
    @XmlAttribute
    @DynamicSerializeElement
    private Integer altUpperSfcCloud = null;

    /**
     * Cloud amount WMO BUFR Table 020011
     */
    @XmlAttribute
    @DynamicSerializeElement
    private Integer cloudAmount = null;

    /**
     * genusOfCloud WMO BUFR Table 020012
     */
    @XmlAttribute
    @DynamicSerializeElement
    private Integer cloudgenus = null;

    /**
     * Height of the cloud base in meters
     */
    @XmlAttribute
    @DynamicSerializeElement
    private Integer cloudHeight = null;

    /**
     * Description of the top of cloud whose base is below the level of the
     * station WMO table 0552
     */
    @XmlAttribute
    @DynamicSerializeElement
    private Integer cloudTopDescription = null;

    /**
     * Section that contained this cloud data
     */
    @XmlAttribute
    @DynamicSerializeElement
    private Integer reportsection = null;

    /**
     * Vertical significance WMO BUFR Table 008002
     */
    @XmlAttribute
    @DynamicSerializeElement
    private Integer vertSignificance = null;

    /**
     * Construct an empty base.
     */
    public AncCloud() {
    }

    /**
     * Constructor with known parent and observation section.
     * 
     * @param parent
     * @param reportSection
     */
    public AncCloud(Integer reportSection) {
        this.reportsection = reportSection;
    }

    /**
     * @return the cloudObsType
     */
    public Integer getCloudObsType() {
        return cloudObsType;
    }

    /**
     * @param cloudObsType
     *            the cloudObsType to set
     */
    public void setCloudObsType(Integer cloudObsType) {
        this.cloudObsType = cloudObsType;
    }

    /**
     * @return the altUpperSfcCloud
     */
    public Integer getAltUpperSfcCloud() {
        return altUpperSfcCloud;
    }

    /**
     * @param altUpperSfcCloud
     *            the altUpperSfcCloud to set
     */
    public void setAltUpperSfcCloud(Integer altUpperSfcCloud) {
        this.altUpperSfcCloud = altUpperSfcCloud;
    }

    /**
     * @return the cloudAmount
     */
    public Integer getCloudAmount() {
        return cloudAmount;
    }

    /**
     * @param cloudAmount
     *            the cloudAmount to set
     */
    public void setCloudAmount(Integer cloudAmount) {
        this.cloudAmount = cloudAmount;
    }

    /**
     * @return the cloudgenus
     */
    public Integer getCloudgenus() {
        return cloudgenus;
    }

    /**
     * @param cloudgenus
     *            the cloudgenus to set
     */
    public void setCloudgenus(Integer cloudgenus) {
        this.cloudgenus = cloudgenus;
    }

    /**
     * @return the cloudHeight
     */
    public Integer getCloudHeight() {
        return cloudHeight;
    }

    /**
     * @param cloudHeight
     *            the cloudHeight to set
     */
    public void setCloudHeight(Integer cloudHeight) {
        this.cloudHeight = cloudHeight;
    }

    /**
     * @return the cloudTopDescription
     */
    public Integer getCloudTopDescription() {
        return cloudTopDescription;
    }

    /**
     * @param cloudTopDescription
     *            the cloudTopDescription to set
     */
    public void setCloudTopDescription(Integer cloudTopDescription) {
        this.cloudTopDescription = cloudTopDescription;
    }

    /**
     * @return the reportsection
     */
    public Integer getReportsection() {
        return reportsection;
    }

    /**
     * @param reportsection
     *            the reportsection to set
     */
    public void setReportsection(Integer reportsection) {
        this.reportsection = reportsection;
    }

    /**
     * @return the vertSignificance
     */
    public Integer getVertSignificance() {
        return vertSignificance;
    }

    /**
     * @param vertSignificance
     *            the vertSignificance to set
     */
    public void setVertSignificance(Integer vertSignificance) {
        this.vertSignificance = vertSignificance;
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
        result = prime
                * result
                + ((altUpperSfcCloud == null) ? 0 : altUpperSfcCloud.hashCode());
        result = prime * result
                + ((cloudAmount == null) ? 0 : cloudAmount.hashCode());
        result = prime * result
                + ((cloudHeight == null) ? 0 : cloudHeight.hashCode());
        result = prime * result
                + ((cloudObsType == null) ? 0 : cloudObsType.hashCode());
        result = prime
                * result
                + ((cloudTopDescription == null) ? 0 : cloudTopDescription
                        .hashCode());
        result = prime * result
                + ((cloudgenus == null) ? 0 : cloudgenus.hashCode());
        result = prime * result
                + ((reportsection == null) ? 0 : reportsection.hashCode());
        result = prime
                * result
                + ((vertSignificance == null) ? 0 : vertSignificance.hashCode());
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
        final AncCloud other = (AncCloud) obj;
        if (altUpperSfcCloud == null) {
            if (other.altUpperSfcCloud != null)
                return false;
        } else if (!altUpperSfcCloud.equals(other.altUpperSfcCloud))
            return false;
        if (cloudAmount == null) {
            if (other.cloudAmount != null)
                return false;
        } else if (!cloudAmount.equals(other.cloudAmount))
            return false;
        if (cloudHeight == null) {
            if (other.cloudHeight != null)
                return false;
        } else if (!cloudHeight.equals(other.cloudHeight))
            return false;
        if (cloudObsType == null) {
            if (other.cloudObsType != null)
                return false;
        } else if (!cloudObsType.equals(other.cloudObsType))
            return false;
        if (cloudTopDescription == null) {
            if (other.cloudTopDescription != null)
                return false;
        } else if (!cloudTopDescription.equals(other.cloudTopDescription))
            return false;
        if (cloudgenus == null) {
            if (other.cloudgenus != null)
                return false;
        } else if (!cloudgenus.equals(other.cloudgenus))
            return false;
        if (reportsection == null) {
            if (other.reportsection != null)
                return false;
        } else if (!reportsection.equals(other.reportsection))
            return false;
        if (vertSignificance == null) {
            if (other.vertSignificance != null)
                return false;
        } else if (!vertSignificance.equals(other.vertSignificance))
            return false;
        return true;
    }

}
