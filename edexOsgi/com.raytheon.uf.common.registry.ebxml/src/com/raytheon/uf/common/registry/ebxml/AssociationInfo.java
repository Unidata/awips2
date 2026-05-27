package com.raytheon.uf.common.registry.ebxml;

import java.io.Serializable;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A container Class for storing association information between to 
 * RegistryObjects.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2012 455        jspinks     Initial creation
 *
 * </pre>
 *
 * @author jspinks
 * @version 1.0
 */
@DynamicSerialize
public class AssociationInfo implements Serializable {
    
    private static final long serialVersionUID = -2668740314051844510L;

    @DynamicSerializeElement
    private String associationType;
    
    @DynamicSerializeElement
    private boolean required;
    
    /**
     * Public no argument constructor to support bean pattern.
     */
    public AssociationInfo() {}
    
    /**
     * Constructor with arguments.
     * 
     * @param type
     *        The type of the association.
     *        
     * @param required
     *        Whether or not the association is required.
     */
    public AssociationInfo(String type, boolean required) {
        this.associationType = type;
        this.required = required;
    }

    /**
     * Set the associationType attribute.
     * 
     * @param associationType
     *        The value to set the associationType attribute to.
     */
    public void setAssociationType(String associationType) {
        this.associationType = associationType;
    }
    
    /**
     * Get the value of the associationType attribute.
     * 
     * @return The value of the associationType attribute.
     */
    public String getAssociationType() {
        return associationType;
    }
    
    /**
     * Set the required attribute.
     * 
     * @param required
     *        The value to set the required attribute to.
     */
    public void setRequired(boolean required) {
        this.required = required;
    }

    /**
     * Get the value of the required attribute.
     * 
     * @return The value of the required attribute.
     */
    public boolean isRequired() {
        return required;
    }

}
