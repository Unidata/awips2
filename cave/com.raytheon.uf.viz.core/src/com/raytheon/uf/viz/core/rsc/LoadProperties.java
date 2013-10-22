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
package com.raytheon.uf.viz.core.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.viz.core.comm.PerspectiveSpecificLoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.Capabilities;

/**
 * LoadProperties
 * 
 * Contains options for loading data
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 15, 2007           chammack    Initial Creation.
 * Feb 26, 2009	 2032     jsanchez	  Added a loadWithoutData.
 * Oct 22, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlType(name = "loadProperties")
@XmlAccessorType(XmlAccessType.NONE)
public class LoadProperties {

    private Capabilities capabilities;

    @XmlElement
    private ResourceType resourceType;

    @XmlAttribute
    private Integer recordLimit;

    @XmlAttribute
    private boolean loadWithoutData = false;

    /*
     * List of unique (no duplicates and a single element for each nested
     * property class) property objects.
     */
    @XmlElement
    private PerspectiveSpecificLoadProperties perspectiveProperty;

    public boolean isLoadWithoutData() {
        return loadWithoutData;
    }

    public void setLoadWithoutData(boolean loadWithoutData) {
        this.loadWithoutData = loadWithoutData;
    }

    /**
     * Constructor
     */
    public LoadProperties() {
        this.resourceType = ResourceType.PLAN_VIEW;
    }

    /**
     * @return the capabilities
     */
    @XmlElement(name = "capabilities")
    public Capabilities getCapabilities() {
        if (capabilities == null) {
            capabilities = new Capabilities();
        }

        return capabilities;
    }

    /**
     * @param capabilities
     *            the capabilities to set
     */
    public void setCapabilities(Capabilities capabilities) {
        Capabilities caps = getCapabilities();
        for (AbstractCapability cap : capabilities) {
            caps.addCapability(cap);
        }
    }

    /**
     * @param capabilities
     *            the capabilities to set
     */
    public void overrideCapabilities(Capabilities capabilities) {
        this.capabilities = capabilities;
    }

    /**
     * @return the resourceType
     */

    public ResourceType getResourceType() {
        return resourceType;
    }

    /**
     * @param resourceType
     *            the resourceType to set
     */
    public void setResourceType(ResourceType resourceType) {
        this.resourceType = resourceType;
    }

    /**
     * @return the recordLimit
     */
    public Integer getRecordLimit() {
        return recordLimit;
    }

    /**
     * @param recordLimit
     *            the recordLimit to set
     */
    public void setRecordLimit(Integer recordLimit) {
        this.recordLimit = recordLimit;
    }

    /**
     * @return the perspectiveProperty
     */
    public PerspectiveSpecificLoadProperties getPerspectiveProperty() {
        return perspectiveProperty;
    }

    /**
     * @param perspectiveProperty
     *            the perspectiveProperty to set
     */
    public void setPerspectiveProperty(
            PerspectiveSpecificLoadProperties perspectiveProperty) {
        this.perspectiveProperty = perspectiveProperty;
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
                + ((resourceType == null) ? 0 : resourceType.hashCode());
        result = prime
                * result
                + ((perspectiveProperty == null) ? 0 : perspectiveProperty
                        .hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        LoadProperties other = (LoadProperties) obj;
        if (resourceType == null) {
            if (other.resourceType != null) {
                return false;
            }
        } else if (!resourceType.equals(other.resourceType)) {
            return false;
        }
        if (perspectiveProperty == null) {
            if (other.perspectiveProperty != null) {
                return false;
            }
        } else if (!perspectiveProperty.equals(other.perspectiveProperty)) {
            return false;
        }
        return true;
    }

}
