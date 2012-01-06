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
package com.raytheon.uf.viz.core.rsc.capabilities;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;

/**
 * A base implementation for describing persistable capabilities of
 * visualizations (AbstractVizResources).
 * 
 * This is primarily intended for commonly used metadata for drawing (e.g.
 * colors, colormaps, line sizes, etc.). For capabilities that are actions,
 * these should probably be implemented as interfaces that the resource itself
 * implements.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 2, 2009             chammack    Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public abstract class AbstractCapability implements ISerializableObject {

    /**
     * The resource pointer. Primarily useful for communication with the
     * resource when capabilities change.
     */
    protected transient AbstractResourceData resourceData;

    protected transient boolean suppressingMenuItems;

    /**
     * Default constructor
     */
    public AbstractCapability() {
        this.suppressingMenuItems = false;
    }

    /**
     * @return the resourceData
     */
    public AbstractResourceData getResourceData() {
        return resourceData;
    }

    /**
     * @param resourceData
     *            the resourceData to set
     */
    public void setResourceData(AbstractResourceData resourceData) {
        this.resourceData = resourceData;
    }

    /**
     * Provides a mechanism for implementing capabilities to tell the resource a
     * capabilities data has changed and thus an action may be required (e.g.
     * redraw)
     */
    protected void capabilityChanged() {
        if (this.resourceData != null) {
            this.resourceData.fireChangeListeners(ChangeType.CAPABILITY, this);
        }
    }

    /**
     * @return the suppressingMenuItems
     */
    public boolean isSuppressingMenuItems() {
        return suppressingMenuItems;
    }

    /**
     * @param suppressingMenuItems
     *            the suppressingMenuItems to set
     */
    public void setSuppressingMenuItems(boolean suppressingMenuItems) {
        this.suppressingMenuItems = suppressingMenuItems;
    }

    public abstract AbstractCapability clone();
}
