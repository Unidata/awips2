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
 * Represents an item is a collection that is similar to an ANT property;
 * each element has both a name and a value.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2009 2081       mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0 
 */
@XmlRootElement(name="element")
@XmlAccessorType(XmlAccessType.NONE)
public class Element implements IUpdatable, ISerializableObject {
    /** the Element's name */
    @XmlAttribute
    private String name;
    
    /** the Element's value */
    @XmlAttribute
    private String value;
    
    /**
     * Default constructor.
     */
    public Element() {
        super();
    }
    /**
     * Constructor. Populates the object with the specified name
     * and value.
     */
    public Element(String name, String value) {
        super();
        this.name = name;
        this.value = value;
    }
    /**
     * returns the name of the object.
     */
    public String getName() {
        return name;
    }
    /**
     * sets the name of the object.
     */
    public void setName(String name) {
        this.name = name;
    }
    /**
     * returns the value of the object.
     */
    public String getValue() {
        return value;
    }
    /**
     * sets the value of the object.
     */
    public void setValue(String value) {
        this.value = value;
    }
    /**
     * Returns a string representation of this class. The format of
     * the return value is "name=value".
     */
    @Override
    public String toString() {
        return name + "=" + value;
    }
    /**
     * Updates the value attribute; replaces each occurrence of the
     * <em>token</em> with the <em>substitute</em> value provided..
     */
    @Override
    public void updateData(String regex, String replacement) {
        value = value.replaceAll(regex, replacement);
    }
}
