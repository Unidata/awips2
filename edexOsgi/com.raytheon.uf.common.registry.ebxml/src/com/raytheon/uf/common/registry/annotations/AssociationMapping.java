package com.raytheon.uf.common.registry.annotations;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;



/**
 * Annotation for specifying the relationship between to RegistyObject
 * classes.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2012            jspinks     Initial creation
 *
 * </pre>
 *
 * @author jspinks
 * @version 1.0
 * 
 * @see RegistryObject
 */
public @interface AssociationMapping {
    
    /**
     * The Class of the target RegitryObject type.
     * 
     * @return The Class to use to create the association with.
     */
    public Class<?> targetObject();
    
    /**
     * The names of the attributes that will be used to create the target
     * objects key information.
     * 
     * @return An array of field names.
     */
    public String[] keyFields();
    
    /**
     * The association type name for the association.  The type must be
     * the id of the Scheme
     * @return
     */
    public String associationType() default RegistryUtil.ASSOCIATION_CONTAINS;
   
    /**
     * Whether or not the association is required to exist.  If true, 
     * any attempt to store a RegistryObject with the required attribute
     * set to true where the target object cannot be found should result
     * in the store attempt failing.  
     * 
     * @return Whether or not the association is required.  
     */
    public boolean required() default false; 
}
