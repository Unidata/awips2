/**
 * 
 */
package com.raytheon.uf.edex.ebxml.registry;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateActionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

/**
 * Registry interface with functions for manipulating and querying a registry.
 * 
 * @author jsherida
 */
public interface IRegistry {

    /**
     * Initialize the registry object.
     */
    public void init();

    /**
     * Store the given {@link RegistryObjectType}s in the registry.
     * 
     * @param objs
     *            The list of registry objects to store.
     * @return A {@link RegistryExceptionType} if there was a problem.
     */
    public RegistryExceptionType create(List<RegistryObjectType> objs);

    /**
     * Update the given {@link RegistryObjectType}s in the registry.
     * 
     * @param objs
     *            The list of registry objects to update.
     * @return A {@link RegistryExceptionType} if there was a problem.
     */
    public RegistryExceptionType replace(List<RegistryObjectType> objs);

    /**
     * Store new versions of the given {@link RegistryObjectType}s in the
     * registry.
     * 
     * @param objs
     *            The list of registry objects to store.
     * @return A {@link RegistryExceptionType} if there was a problem.
     */
    public RegistryExceptionType version(List<RegistryObjectType> objs);

    /**
     * Remove the given {@link ObjectRefType}s from the registry.
     * 
     * @param refs
     *            The list of registry objects to remove.
     * @return A {@link RegistryExceptionType} if there was a problem.
     */
    public RegistryExceptionType remove(List<ObjectRefType> refs);

    /**
     * Updates the give {@link ObjectRefType}s from the registry
     * 
     * @param refs
     *            The list of registry objects to update according to the update
     *            actions
     * @param updateActions
     *            The actions to perform on the registry objects provided
     * @param mode
     *            The {@link Mode} to use for updating data
     * @return A {@link RegistryExceptionType} if there was a problem
     */
    public RegistryExceptionType update(List<ObjectRefType> refs,
            List<UpdateActionType> updateActions, Mode mode);

    /**
     * Check the registry to see if it contains the given ID.
     * 
     * @param id
     *            The ID
     * @return True if the ID is in the registry.
     * @throws IOException
     *             if there was a problem.
     */
    public boolean containsId(String id) throws IOException;

    /**
     * Check the registry to see if it contains the given LID.
     * 
     * @param id
     *            The LID
     * @return True if the LID is in the registry.
     * @throws IOException
     *             if there was a problem.
     */
    public boolean containsLid(String lid) throws IOException;

    /**
     * Query the registry for Registry Objects matching the given parameters.
     * 
     * @param parameters
     *            A map of parameter name to a list containing one or more
     *            values for that parameter. If there is more than one value, it
     *            is assumed they will be matched with a logical AND.
     * @param matchAny
     *            If true, match each parameter to the object with logical OR.
     *            If false, use logical AND.
     * @return A non-null list of RegistryObjects matching the parameters.
     * @throws IOException
     *             if there is a problem retrieving information from registry
     */
    public List<RegistryObjectType> query(Map<String, List<Object>> parameters,
            boolean matchAny, long startIndex, long maxResults,
            long depth, boolean matchOlderVersions) throws IOException;

}
