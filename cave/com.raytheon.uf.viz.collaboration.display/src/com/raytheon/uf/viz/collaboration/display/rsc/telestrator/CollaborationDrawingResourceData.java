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
package com.raytheon.uf.viz.collaboration.display.rsc.telestrator;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.drawing.DrawingToolLayer.DrawMode;

/**
 * Resource data for the CollaborationDrawingResource
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class CollaborationDrawingResourceData extends AbstractResourceData {

    @XmlAttribute
    private String sessionId;

    @XmlAttribute
    private int displayId;

    @XmlElement
    private DrawMode resourceMode = DrawMode.NONE;

    @XmlAttribute
    private boolean locking = false;

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.rsc.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public CollaborationDrawingResource construct(
            LoadProperties loadProperties, IDescriptor descriptor)
            throws VizException {
        return new CollaborationDrawingResource(this, loadProperties);
    }

    /**
     * @return the sessionId
     */
    public String getSessionId() {
        return sessionId;
    }

    /**
     * @param sessionId
     *            the sessionId to set
     */
    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    /**
     * @return the displayId
     */
    public int getDisplayId() {
        return displayId;
    }

    /**
     * @param displayId
     *            the displayId to set
     */
    public void setDisplayId(int displayId) {
        this.displayId = displayId;
    }

    /**
     * @return the resourceMode
     */
    public DrawMode getResourceMode() {
        return resourceMode;
    }

    /**
     * @param resourceMode
     *            the resourceMode to set
     */
    public void setResourceMode(DrawMode resourceMode) {
        this.resourceMode = resourceMode;
    }

    /**
     * @return the locking
     */
    public boolean isLocking() {
        return locking;
    }

    /**
     * @param locking
     *            the locking to set
     */
    public void setLocking(boolean locking) {
        this.locking = locking;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
    @Override
    public void update(Object updateData) {

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
        return true;
    }

}
