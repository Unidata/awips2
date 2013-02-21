package com.raytheon.uf.edex.ebxml.query.matcher;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

/**
 * Interface for containing the logic to match a query parameter's
 * value to a RegistryObject.
 * @author jsherida
 */
public interface IMatcher {
    
    /**
     * Does the specified value match the given RegistryObject?
     * @param obj The RegistryObject to check the value against.
     * @return true if the RegistryObject matches has the value.
     */
    public boolean matches(RegistryObjectType obj);
    
}
