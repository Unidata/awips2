/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.registry.handler;

import java.util.Collection;
import java.util.List;

import com.raytheon.uf.common.registry.annotations.RegistryObject;

/**
 * Used to interact with the registry via {@link RegistryObject} classes.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012 1169       djohnson     Initial creation
 * Sep 21, 2012 1187       djohnson     Add bulk delete operations.
 * Oct 05, 2012 1195       djohnson     Remove executeQuery method, add getById.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface IRegistryObjectHandler<T> {

    /**
     * Retrieve the object instance by its registry id.
     * 
     * @param id
     *            the registry id
     * @return the object, or null if not found
     * @throws RegistryHandlerException
     *             on unsuccessful response from the registry
     */
    T getById(String id) throws RegistryHandlerException;

    /**
     * Retrieve all of the objects.
     * 
     * @return the list of objects
     * @throws RegistryHandlerException
     *             on unsuccessful response from the registry
     */
    List<T> getAll() throws RegistryHandlerException;

    /**
     * Store a {@link RegistryObject}.
     * 
     * @param obj
     *            the object to store
     */
    void store(T obj) throws RegistryHandlerException;

    /**
     * Delete a {@link RegistryObject}.
     * 
     * @param obj
     *            the object to store
     * @throws RegistryHandlerException
     */
    void update(T obj) throws RegistryHandlerException;

    /**
     * Delete a {@link RegistryObject}.
     * 
     * @param obj
     *            the object to store
     * @throws RegistryHandlerException
     */
    void delete(T obj) throws RegistryHandlerException;

    /**
     * Delete a {@link RegistryObject} by its registry id.
     * 
     * @param username
     *            the username
     * @param registryId
     *            the registry id
     * @throws RegistryHandlerException
     */
    void deleteById(String username, String registryId)
            throws RegistryHandlerException;

    /**
     * Delete {@link RegistryObject}s by their registry ids.
     * 
     * @param username
     *            the username
     * @param registryIds
     *            the registry ids
     * @throws RegistryHandlerException
     */
    void deleteByIds(String username, List<String> registryIds)
            throws RegistryHandlerException;

    /**
     * Delete a {@link RegistryObject}.
     * 
     * @param username
     *            the username
     * @param obj
     *            the object to store
     * @throws RegistryHandlerException
     */
    void delete(String username, T obj) throws RegistryHandlerException;

    /**
     * Delete a {@link Collection} of the {@link RegistryObject}s.
     * 
     * @param objects
     *            the {@link Collection}
     * @throws RegistryHandlerException
     *             on an error to delete one or more of the objects
     */
    void delete(Collection<T> objects) throws RegistryHandlerException;

    /**
     * Delete a {@link Collection} of the {@link RegistryObject}s.
     * 
     * @param username
     *            the username
     * @param objects
     *            the {@link Collection}
     * @throws RegistryHandlerException
     *             on an error to delete one or more of the objects
     */
    void delete(String username, Collection<T> objects)
            throws RegistryHandlerException;
}
