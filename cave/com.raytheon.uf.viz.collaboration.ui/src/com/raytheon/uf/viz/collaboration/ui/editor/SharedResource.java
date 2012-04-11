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
package com.raytheon.uf.viz.collaboration.ui.editor;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IInitData;
import com.raytheon.uf.viz.core.drawables.ResourcePair;

/**
 * Shared resource object, specifies a resource shared by all users in the
 * venue. Can send command to add resource or remove
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 10, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class SharedResource implements ISerializableObject, IInitData {

    @XmlAttribute
    private boolean removeResource = false;

    @XmlElement
    private ResourcePair resource;

    /**
     * @return the resource
     */
    public ResourcePair getResource() {
        return resource;
    }

    /**
     * @param resource
     *            the resource to set
     */
    public void setResource(ResourcePair resource) {
        this.resource = resource;
    }

    /**
     * Returns true if the SharedResource represents a resource removal, false
     * if addition
     * 
     * @return the remove
     */
    public boolean isRemoveResource() {
        return removeResource;
    }

    /**
     * @param remove
     *            the remove to set
     */
    public void setRemoveResource(boolean remove) {
        this.removeResource = remove;
    }

}
