package com.raytheon.uf.common.registry.ebxml;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.registry.annotations.RegistryObject;



/**
 * ContainmentResolver implementation for extracting a <code>List</code> of RegistryObjects 
 * from a <code>Set</code>.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2012 455        jspinks     Initial creation
 *
 * </pre>
 *
 * @author jspinks
 * @version 1.0
 */
public class SetAssociationResolver implements RegistryObjectResolver {

    /**
     * Extract a <code>List</code> of Objects from a <code>Set</code>.
     * 
     * @param containingObject
     *        The container Object that holds references to the Objects to be stored. Will be
     *        runtime checked to ensure that it implements the <code>Set</code> interface.
     *        
     * @return A <code>List</code> of the Objects to store.
     * 
     * @throws IllegalArgumentException
     *         If the associatedObject is not a <code>Set</code> or the Object contained in associatedObject
     *         are not annotated with <code>@RegistryObject</code>.
     */
    @SuppressWarnings("rawtypes")
    @Override
    public List<Object> getRegistryObjects(Object containingObject) throws IllegalArgumentException {
        List<Object> items = new ArrayList<Object>();
        if (containingObject instanceof Set) {
            
            for (Object obj : (Set)containingObject) {
                // Since associations can ONLY be made between RegistryObjects,
                // ensure that the referenced Object is annotated with RegistryObject
                if (!RegistryUtil.isRegistryObject(obj)) {
                    throw new IllegalArgumentException("Referenced Object of type " + obj.getClass().getName() + " is not annotated with the " + 
                            RegistryObject.class.getName() + " annotation.");
                }
                items.add(obj);
            }
        } else {
            throw new IllegalArgumentException(SetAssociationResolver.class.getName() + " cannot be used to extract Objects from " +
                    containingObject.getClass().getName());
        }
        
        return items;
    }

}
