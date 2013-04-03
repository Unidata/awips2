package com.raytheon.uf.common.registry.ebxml;

import java.util.List;

/**
 * Interface for extracting a <code>List</code> of RegistryObjects from another
 * RegistryObject to support making associations between the RegistryObjects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2012 #455       jspinks     Initial creation
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
public interface RegistryObjectResolver {

    /**
     * Extract the <code>List</code> of Objects to be persisted.
     * 
     * @param associationObject
     *            The Object that holds a reference to the Object(s) to be
     *            stored.
     * 
     * @return A <code>List</code> of the Object(s) to store.
     * 
     * @throws IllegalArgumentException
     *             If the Class of the Objects contained in the
     *             associationObject are not annotated with
     *             <code>RegistryObject</code>.
     */
    List<Object> getRegistryObjects(Object associationObject)
            throws IllegalArgumentException;
}
