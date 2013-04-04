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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import com.raytheon.uf.common.registry.OperationStatus;
import com.raytheon.uf.common.registry.RegistryManager;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.registry.RegistryResponse;
import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.registry.ebxml.AdhocRegistryQuery;
import com.raytheon.uf.common.registry.ebxml.IdQuery;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * Provides base implementation methods for {@link IRegistryObjectHandler}.
 * 
 * @param <T>
 *            the type of object the handler supports
 * @param <QUERY>
 *            the query used to retrieve objects of type T
 * 
 *            <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 17, 2012 1169       djohnson     Initial creation
 * Sep 21, 2012 1187       djohnson     Add bulk delete operations.
 * Oct 05, 2012 1195       djohnson     Remove executeQuery method, add getById.
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class BaseRegistryObjectHandler<T, QUERY extends AdhocRegistryQuery<T>>
        implements IRegistryObjectHandler<T> {

    /**
     * {@inheritDoc}
     */
    @Override
    public T getById(String id) throws RegistryHandlerException {
        IdQuery<T> query = new IdQuery<T>(getRegistryObjectClass());
        query.setID(id);

        RegistryQueryResponse<T> response = RegistryManager
                .getRegistyObjects(query);

        checkResponse(response, "getById");

        return response.getSingleResult();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getAll() throws RegistryHandlerException {
        QUERY query = getQuery();
        RegistryQueryResponse<T> response = RegistryManager
                .getRegistyObjects(query);

        checkResponse(response, "getAll");

        return response.getResults();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(T obj) throws RegistryHandlerException {
        RegistryResponse<T> response = RegistryManager.storeRegistryObject(obj);

        checkResponse(response, obj, "store");
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(T obj) throws RegistryHandlerException {
        RegistryResponse<T> response = RegistryManager
                .storeOrReplaceRegistryObject(obj);

        checkResponse(response, obj, "update");
    }

    /**
     * {@inheritDoc}
     * 
     */
    @Override
    public final void delete(T obj) throws RegistryHandlerException {
        delete(null, obj);
    }

    /**
     * {@inheritDoc}
     * 
     */
    @Override
    public final void delete(Collection<T> objects)
            throws RegistryHandlerException {
        delete(null, objects);
    }

    /**
     * {@inheritDoc}
     * 
     */
    @Override
    @SuppressWarnings("unchecked")
    public final void delete(String username, T obj)
            throws RegistryHandlerException {
        delete(username, Arrays.asList(obj));
    }

    /**
     * {@inheritDoc}
     * 
     */
    @Override
    public final void delete(String username, Collection<T> objects)
            throws RegistryHandlerException {
        // Note: Instead of dynamically serializing all of the objects up and
        // passing them server-side, it's much more performant to just get the
        // registry object IDs and delete them with an IdQuery
        List<String> ids = new ArrayList<String>(objects.size());
        for (T obj : objects) {
            ids.add(RegistryUtil.getRegistryObjectKey(obj));
        }

        deleteByIds(username, ids);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public final void deleteById(String username, String registryId)
            throws RegistryHandlerException {
        deleteByIds(username, Arrays.asList(registryId));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteByIds(String username, List<String> registryIds)
            throws RegistryHandlerException {
        IdQuery<T> registryQuery = new IdQuery<T>(getRegistryObjectClass());
        registryQuery.setIDs(registryIds);

        RegistryResponse<T> response = RegistryManager.removeRegistyObjects(
                username, registryQuery);

        checkResponse(response, "deleteByIds");

    }

    /**
     * Checks a registry response for successful status. If not successful,
     * throws an exception.
     * 
     * @param response
     *            the response
     * @param obj
     *            the object
     * @throws RegistryHandlerException
     */
    protected static <T> void checkResponse(RegistryResponse<?> response,
            T obj,
            String operation) throws RegistryHandlerException {
        if (response.getStatus() != OperationStatus.SUCCESS) {
            String message = "Unable to " + operation + " "
                    + obj.getClass().getName() + ".";

            List<Throwable> errors = response.getErrors();
            if (!CollectionUtil.isNullOrEmpty(errors)) {
                // For now, assume the important exception is the first one,
                // what should we do if more than one exception is provided?
                throw new RegistryHandlerException(message, errors.get(0));
            } else {
                throw new RegistryHandlerException(message);
            }
        }
    }

    /**
     * Checks a registry response for successful status. If not successful,
     * throws an exception.
     * 
     * @param response
     *            the response
     * @param obj
     *            the object
     * @throws RegistryHandlerException
     */
    protected void checkResponse(RegistryResponse<?> response, String operation)
            throws RegistryHandlerException {
        if (response.getStatus() != OperationStatus.SUCCESS) {
            String message = "Unable to " + operation + ".";

            List<Throwable> errors = response.getErrors();
            if (!CollectionUtil.isNullOrEmpty(errors)) {
                // For now, assume the important exception is the first one,
                // what should we do if more than one exception is provided?
                throw new RegistryHandlerException(message, errors.get(0));
            } else {
                throw new RegistryHandlerException(message);
            }
        }
    }

    /**
     * Return the {@link RegistryObject} annotated class object this handler
     * works with.
     * 
     * @return the registry object class
     */
    protected abstract Class<T> getRegistryObjectClass();

    /**
     * Retrieve the query implementation for the object type.
     * 
     * @return the query
     */
    protected abstract QUERY getQuery();
}
