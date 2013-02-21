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
package com.raytheon.uf.common.datadelivery.registry.handlers;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * Base memory registry object handler.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2012 0726       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class BaseMemoryRegistryObjectHandler<T>
        implements IRegistryObjectHandler<T> {

    private final Map<String, T> map = new HashMap<String, T>();

    /**
     * {@inheritDoc}
     */
    @Override
    public T getById(String id) throws RegistryHandlerException {
        return map.get(id);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getAll() throws RegistryHandlerException {
        return new ArrayList<T>(map.values());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(T obj) throws RegistryHandlerException {
        map.put(RegistryUtil.getRegistryObjectKey(obj), obj);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(T obj) throws RegistryHandlerException {
        store(obj);
    }

    /**
     * {@inheritDoc}
     * 
     */
    @Override
    @SuppressWarnings("unchecked")
    public final void delete(T obj) throws RegistryHandlerException {
        delete(Arrays.asList(obj));
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
        for (String id : registryIds) {
            map.remove(id);
        }
    }

    /**
     * Returns true if the attribute is null, or if the equals comparison
     * between attribute and value resolves as true.
     * 
     * @param <TYPE>
     *            the type of the attribute
     * @param attribute
     *            the attribute
     * @param value
     *            the value from the entity
     * @return true if the attribute is null, or if the equals comparison
     *         between attribute and value resolves as true.
     */
    protected <TYPE> boolean matches(TYPE attribute,
            TYPE value) {
        return attribute == null || attribute.equals(value);
    }
}
