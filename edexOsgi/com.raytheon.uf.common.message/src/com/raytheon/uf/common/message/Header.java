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

package com.raytheon.uf.common.message;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents the message header in EDEX responses
 * 
 * SOFTWARE HISTORY
 * 
 * Date Ticket# Engineer Description ------------ ---------- -----------
 * -------------------------- 08-06-2006 chammack Initial creation. 27Apr2007
 * 208 MW Fegan Added getProperty() method.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Header implements ISerializableObject {

    @XmlElement
    @DynamicSerializeElement
    private Property[] properties;

    /**
     * Returns the value of the specified property.
     * 
     * @param name
     *            the name of the desired property
     * 
     * @return the value of the desired property
     */
    public String getProperty(String name) {
        String retVal = null;
        for (Property property : properties) {
            if (property.getName().equalsIgnoreCase(name)) {
                retVal = property.getValue();
                break;
            }
        }
        return retVal;
    }

    /**
     * @return the properties
     */
    public Property[] getProperties() {
        return properties;
    }

    /**
     * @param properties
     *            the properties to set
     */
    public void setProperties(Property[] properties) {
        this.properties = properties;
    }

}
