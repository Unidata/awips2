package com.raytheon.uf.common.registry;

import java.util.List;

import com.raytheon.uf.common.registry.ebxml.AdhocRegistryQuery;
import com.raytheon.uf.common.registry.ebxml.IdQuery;

/**
 * Defines a class that can interact with the registry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2012 736        djohnson     Initial creation
 * Aug 02, 2012 955        djohnson     Type-safe registry query/responses.
 * Aug 20, 2012 0743       djohnson     Finish making registry type-safe.
 * Sep 14, 2012 1169       djohnson     Must call specific method to allow replace of an object.
 * 
 * </pre>
 * 
 * @author djohnson
 */
public interface RegistryHandler {

    /**
     * Retrieve registry objects that satisfy the RegistryQuery.
     * 
     * @param registryQuery
     *            A RegistryQuery to search the registry for objects.
     * 
     * @return A RegistryQueryResponse containing the status of the request, any
     *         registry objects that satisfied the RegistryQuery and any
     *         Exceptions generated from processing the RegistryQuery.
     * 
     * @see AdhocRegistryQuery
     * @see IdQuery
     */
    <T> RegistryQueryResponse<T> getObjects(RegistryQuery<T> query);
    
    /**
     * Remove objects from the registry that satisfy a RegistryQuery.
     * 
     * @param registryQuery
     *            A RegistryQuery to search the registry for objects. These
     *            Objects will be removed from the registry.
     * 
     * @return A RegistryResponse containing the status of the request, and any
     *         Exceptions generated from attempting to remove the Objects from
     *         the registry.
     * 
     * @see Object
     * 
     */
    <T> RegistryResponse<T> removeObjects(RegistryQuery<T> registryQuery);

    /**
     * Remove objects from the registry that satisfy a RegistryQuery.
     * 
     * @param username
     *            the username of the requester
     * @param registryQuery
     *            A RegistryQuery to search the registry for objects. These
     *            Objects will be removed from the registry.
     * 
     * @return A RegistryResponse containing the status of the request, and any
     *         Exceptions generated from attempting to remove the Objects from
     *         the registry.
     * 
     * @see Object
     * 
     */
    <T> RegistryResponse<T> removeObjects(String username,
            RegistryQuery<T> registryQuery);

    /**
     * Remove objects from the registry.
     * 
     * @param username
     *            The name of the user making the request.
     * 
     * @param registryObjects
     *            A List of Objects that are annotated with @Object. These
     *            Objects will be searched for in the registry and removed.
     * 
     * @return A RegistryResponse containing the status of the request, and any
     *         Exceptions generated from attempting to remove the Objects from
     *         the registry.
     * 
     * @see Object
     * 
     */
    <T> RegistryResponse<T> removeObjects(String username,
            List<T> registryObjects);

    /**
     * Store an Object to the registry.
     * 
     * @param object
     *            An Object whose Class is annotated with @Object. This Object
     *            will be stored in the registry.
     * 
     * @return A RegistryResponse containing the status of the request, and any
     *         Exceptions generated from attempting to store the Object into the
     *         registry.
     * 
     * @see Object
     * 
     */
    <T> RegistryResponse<T> storeObject(T object);

    /**
     * Store an Object to the registry, replacing any existing Object with the
     * same id.
     * 
     * @param object
     *            An Object whose Class is annotated with @Object. This Object
     *            will be stored in the registry.
     * 
     * @return A RegistryResponse containing the status of the request, and any
     *         Exceptions generated from attempting to store the Object into the
     *         registry.
     * 
     * @see Object
     * 
     */
    <T> RegistryResponse<T> storeOrReplaceObject(T object);

}