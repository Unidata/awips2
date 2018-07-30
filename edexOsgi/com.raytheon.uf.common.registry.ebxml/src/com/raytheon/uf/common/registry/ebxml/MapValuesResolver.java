package com.raytheon.uf.common.registry.ebxml;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.registry.annotations.RegistryObject;

/**
 * An <code>AssociationResolver</code> implementation for extracting a
 * collection of the value objects of a <code>Map</code>, and checking each to
 * make sure that they are annotated with RegistryObject.
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
 * 
 * @see java.util.Map#values()
 */
public class MapValuesResolver implements RegistryObjectResolver {

    /**
     * Extract a <code>List</code> of Objects from a <code>Set</code>.
     * 
     * @param containingObject
     *            The container Object that holds references to the Objects to
     *            be stored. Will be runtime checked to ensure that it
     *            implements the <code>Map</code> interface.
     * 
     * @return A <code>List</code> of the Objects to store.
     * 
     * @throws IllegalArgumentException
     *             If the associatedObject is not a <code>Set</code> or the
     *             Object contained in associatedObject are not annotated with
     *             <code>RegistryObject</code>.
     */
    @SuppressWarnings("rawtypes")
    @Override
    public List<Object> getRegistryObjects(Object containingObject)
            throws IllegalArgumentException {
        List<Object> items = new ArrayList<Object>();
        if (containingObject instanceof Map) {

            for (Object obj : ((Map) containingObject).values()) {
                // Since associations can ONLY be made between RegistryObjects,
                // ensure that the referenced Object is annotated with
                // RegistryObject
                if (obj.getClass().isAnnotationPresent(RegistryObject.class)) {
                    items.add(obj);
                } else {
                    throw new IllegalArgumentException(
                            "Referenced Object of type "
                                    + obj.getClass().getName()
                                    + " is not annotated with the "
                                    + RegistryObject.class.getName()
                                    + " annotation.");
                }
            }
        } else {
            throw new IllegalArgumentException(
                    MapValuesResolver.class.getName()
                            + " cannot be used to extract Objects from "
                            + containingObject.getClass().getName());
        }

        return items;
    }
}
