package com.raytheon.uf.common.datadelivery.registry;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.annotations.RegistryObjectDescription;
import com.raytheon.uf.common.registry.annotations.RegistryObjectName;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Data class for a <code>Parameter</code>'s level information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 14, 2012 #455       jspinks     Initial creation
 * 8/3/2012     724        bphillip    Added Registry annotations
 * Nov 19, 2012 1166       djohnson    Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 * 
 * @see Parameter
 */
@DynamicSerialize
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@RegistryObject({ "levelId", "levelValue" })
public class ParameterLevel {

    @RegistryObjectName
    @XmlAttribute
    @DynamicSerializeElement
    private int levelId;

    @RegistryObjectDescription
    @XmlAttribute
    @DynamicSerializeElement
    private double levelValue;

    /**
     * Set the value of the levelId attribute.
     * 
     * @param levelId
     *            The value to set the levelId attribute to.
     */
    public void setLevelId(int levelId) {
        this.levelId = levelId;
    }

    /**
     * Get the value of the levelId attribute.
     * 
     * @return The value of the levelId attribute.
     */
    public int getLevelId() {
        return levelId;
    }

    /**
     * Set the value of the levelValue attribute.
     * 
     * @param levelValue
     *            The value to set the levelValue attribute to.
     */
    public void setLevelValue(double levelValue) {
        this.levelValue = levelValue;
    }

    /**
     * Get the value of the levelValue attribute.
     * 
     * @return The value of the levelValue attribute.
     */
    public double getLevelValue() {
        return levelValue;
    }
}
