package com.raytheon.uf.common.datadelivery.registry;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.registry.ebxml.RegistryObjectResolver;

/**
 * An <code>AssociationResolver</code> implementation for extracting ParameterLevel
 * registry objects from a single Levels object reference.
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
public class ParameterLevelsResolver implements RegistryObjectResolver {

    /**
     * Extract a <code>List</code> of Objects from a single object reference.
     * 
     * @param referencedObject
     *        The Object that is a RegistryObject Class that
     *        an association should be built for.
     *        
     * @return A <code>List</code> containing the registry objects to store.
     * 
     * @throws IllegalArgumentException
     *         If the Class of the referencedObject is not annotated with <code>RegistryObject</code>.
     */
    @Override
    public List<Object> getRegistryObjects(Object referencedObject) throws IllegalArgumentException {
        List<Object> items = new ArrayList<Object>();
        
        // Transform the Levels Object passed in, into the corresponding
        // ParameterLevel Objects.
        
        if (referencedObject instanceof Levels) {
            Levels levels = (Levels)referencedObject;
            for (Double levelValue : levels.getLevel()) {
                ParameterLevel pl = new ParameterLevel();
                pl.setLevelId(levels.getLevelType());
                pl.setLevelValue(levelValue);
                items.add(pl);
            }
        } else {
            throw new IllegalArgumentException("Referenced Object of type " + 
                    referencedObject.getClass().getName() + 
                    " cannot be resolved by AssociationResolver class " + 
                    ParameterLevelsResolver.class.getName());
        }
        return items;
    }
}
