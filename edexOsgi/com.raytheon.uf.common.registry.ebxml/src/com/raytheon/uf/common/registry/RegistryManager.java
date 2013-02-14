package com.raytheon.uf.common.registry;

import java.util.List;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.ebxml.AdhocRegistryQuery;
import com.raytheon.uf.common.registry.ebxml.IdQuery;

/**
 * 
 * Convenience Class for storing and retrieving Objects from the ebXML registry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2012 356        jspinks     Initial creation
 * Apr 24, 2012 455        jspinks     Modified for storing associations between
 *                                       RegistryObjects.
 * Jun 21, 2012 736        djohnson    Change to utility class, inject instance with Spring.
 * Aug 02, 2012 955        djohnson    Type-safe registry query/responses.
 * Aug 20, 2012 0743       djohnson    Allow instance to be changed from {@link RegistryManagerTest}.
 * Sep 14, 2012 1169       djohnson    Add use of create only mode.
 * Nov 15, 2012 1322       djohnson    Use package-level constructor to control who can set the handler instance.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
public final class RegistryManager {

    private static RegistryHandler instance;

    private static final String REGISTRYMANAGER_DEBUG = "awips.registrymanager.debug";

    public static final boolean DEBUG = Boolean
            .getBoolean(REGISTRYMANAGER_DEBUG);

    /**
     * Package-level constructor for utility class, it allows Spring to set the
     * instance multiple times.
     */
    @VisibleForTesting
    RegistryManager(RegistryHandler instance) {
        RegistryManager.instance = instance;
    }

    /**
     * Factory method for getting specific implementation of RegistryHandler.
     * 
     * @return
     */
    private static RegistryHandler getInstance() {
        if (instance == null) {
            throw new IllegalStateException(
                    "Spring should have populated the singleton instance!");
        }

        return instance;
    }

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
     * @throws IllegalStateException
     *             when unable to connect to the registry
     * @see AdhocRegistryQuery
     * @see IdQuery
     */
    public static <T> RegistryQueryResponse<T> getRegistyObjects(
            RegistryQuery<T> registryQuery) {
        return getInstance().getObjects(registryQuery);
    }
    
    /**
     * Remove objects from the registry.
     * 
     * @param registryObjects
     *            A List of Objects that are annotated with @RegistryObject.
     *            These Objects will be searched for in the registry and
     *            removed.
     * 
     * @return A RegistryResponse containing the status of the request, and any
     *         Exceptions generated from attempting to remove the Objects from
     *         the registry.
     * 
     * @see RegistryObject
     * 
     */
    public static <T> RegistryResponse<T> removeRegistyObjects(
            List<T> registryObjects) {
        return removeRegistyObjects(null, registryObjects);
    }

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
     * @see RegistryObject
     * 
     */
    public static <T> RegistryResponse<T> removeRegistyObjects(
            RegistryQuery<T> registryQuery) {
        return getInstance().removeObjects(registryQuery);
    }

    /**
     * Remove objects from the registry that satisfy a RegistryQuery.
     * 
     * @param username
     *            the username requesting the removal
     * @param registryQuery
     *            A RegistryQuery to search the registry for objects. These
     *            Objects will be removed from the registry.
     * 
     * @return A RegistryResponse containing the status of the request, and any
     *         Exceptions generated from attempting to remove the Objects from
     *         the registry.
     * 
     * @see RegistryObject
     * 
     */
    public static <T> RegistryResponse<T> removeRegistyObjects(String username,
            RegistryQuery<T> registryQuery) {
        return getInstance().removeObjects(username, registryQuery);
    }

    /**
     * Remove objects from the registry.
     * 
     * @param username
     *        The name of the user making the request.
     *        
     * @param registryObjects
     *            A List of Objects that are annotated with @RegistryObject.
     *            These Objects will be searched for in the registry and
     *            removed.
     * 
     * @return A RegistryResponse containing the status of the request, and any
     *         Exceptions generated from attempting to remove the Objects from
     *         the registry.
     * 
     * @see RegistryObject
     * 
     */
    public static <T> RegistryResponse<T> removeRegistyObjects(String username,
            List<T> registryObjects) {
        return getInstance().removeObjects(username,
                registryObjects);
    }

    /**
     * Store an Object to the registry.
     * 
     * @param object
     *            An Object whose Class is annotated with @RegistryObject. This
     *            Object will be stored in the registry.
     * 
     * @return A RegistryResponse containing the status of the request, and any
     *         Exceptions generated from attempting to store the Object into the
     *         registry.
     * 
     * @see RegistryObject
     * 
     */
    public static <T> RegistryResponse<T> storeRegistryObject(T object) {
        return getInstance().storeObject(object);
    }

    /**
     * Store an Object to the registry, this method will replace any object
     * already existing with the same ID.
     * 
     * @param object
     *            An Object whose Class is annotated with @RegistryObject. This
     *            Object will be stored in the registry.
     * 
     * @return A RegistryResponse containing the status of the request, and any
     *         Exceptions generated from attempting to store the Object into the
     *         registry.
     * 
     * @see RegistryObject
     * 
     */
    public static <T> RegistryResponse<T> storeOrReplaceRegistryObject(T object) {
        return getInstance().storeOrReplaceObject(object);
    }
}
