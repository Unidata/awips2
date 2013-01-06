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
package com.raytheon.uf.common.plugin.nwsauth.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Permission Element
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2012            mpduff     Initial creation.
 * Oct  2, 2012   1237     jpiatt     Allow for null description.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class PermissionXML implements ISerializableObject {

    @DynamicSerializeElement
    @XmlAttribute(name = "id")
    private String id;

    @DynamicSerializeElement
    @XmlElement(name = "description", type = String.class)
    private String description;

    /**
     * Get the permission id.
     * 
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * Set the permission id.
     * 
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * Get the permission description.
     * 
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    /**
     * Set the permission description.
     * 
     * @param description
     *            the description to set
     */
    public void setDescription(String description) {
        this.description = (description == null) ? null : description.trim();
    }
}
