package com.raytheon.uf.common.registry.ebxml;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.registry.annotations.RegistryObject;



/**
 * An <code>RegistryObjectResolver</code> implementation for extracting a
 * RegistryObject from a single object reference.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 17, 2012 455        jspinks     Initial creation
 *
 * </pre>
 *
 * @author jspinks
 * @version 1.0
 */
public class DirectObjectResolver implements RegistryObjectResolver {

    /**
     * Extract a <code>List</code> of Objects from a single object reference.
     * 
     * @param referencedObject
     *        The Object that is a RegistryObject Class that
     *        an association should be built for.
     *        
     * @return A <code>List</code> containing the Object to store.
     * 
     * @throws IllegalArgumentException
     *         If the Class of the referencedObject is not annotated with <code>RegistryObject</code>.
     */
    @Override
    public List<Object> getRegistryObjects(Object referencedObject) throws IllegalArgumentException {
        List<Object> items = new ArrayList<Object>();
        // Since associations can ONLY be made between RegistryObjects,
        // ensure that the referenced Object is annotated with RegistryObject
        if (RegistryUtil.isRegistryObject(referencedObject)) {
            items.add(referencedObject);
        } else {
            throw new IllegalArgumentException("Referenced Object of type " + 
                    referencedObject.getClass().getName() + " is not annotated with the " + 
                    RegistryObject.class.getName() + " annotation.");
        }
        return items;
    }
}
