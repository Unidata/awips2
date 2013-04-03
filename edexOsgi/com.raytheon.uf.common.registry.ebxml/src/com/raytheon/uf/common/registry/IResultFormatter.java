package com.raytheon.uf.common.registry;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import com.raytheon.uf.common.serialization.SerializationException;

/**
 * 
 * Defines an object that will custom format a result set.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2012            jspinks     Initial creation
 * Jun 21, 2012 736        djohnson    Add throws SerializationException.
 * Aug 15, 2012 0743       djohnson    Type-safe result formatters.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
public interface IResultFormatter<T> extends RegistryQuery<T> {
    
    T decodeObject(RegistryObjectType registryObjectType)
            throws SerializationException;
}
