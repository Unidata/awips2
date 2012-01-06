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
package com.raytheon.uf.viz.app.launcher.bundle;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Represents an element in a collection that is similar to path element;
 * the element has a value but no name. 
 *   
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2009 2081       mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0
 */
@XmlRootElement(name="entry")
@XmlAccessorType(XmlAccessType.NONE)
public class Entity implements IUpdatable,  ISerializableObject {
    /** the Entity's value */
    @XmlAttribute
    private String value;
    
    /**
     * Default constructor.
     */
    public Entity() {
        super();
    }
    /**
     * Constructor. Populates the object with the specified value.
     */
    public Entity(String value) {
        super();
        this.value = value;
    }
    /**
     * returns the object's value.
     */
    public String getValue() {
        return value;
    }
    /**
     * sets the object's value.
     */
    public void setValue(String value) {
        this.value = value;
    }
    /**
     * Returns a string representation of this class. The format of
     * the return value is "value".
     */
    @Override
    public String toString() {
        return value;
    }
    @Override
    public void updateData(String regex, String replacement) {
        if (value != null) {
            value = value.replaceAll(regex, replacement);
        }
    }
}
