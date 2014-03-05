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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * {@link IBaseSubscriptionHandler} in-memory implementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2012 0726       djohnson     Initial creation
 * Feb 20, 2013 1543       djohnson     Implement route filtering.
 * May 15, 2013 1040       mpduff       Office Id now a set.
 * May 28, 2013 1650       djohnson     Add getByNames.
 * Sep 11, 2013 2352       mpduff       Add siteId to getSubscribedToDataSetNames method.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class BaseMemorySubscriptionHandler<T extends Subscription> extends
        BaseMemoryRegistryObjectHandler<T> implements
        IBaseSubscriptionHandler<T> {
    /**
     * {@inheritDoc}
     */
    @Override
    public T getByName(String name) throws RegistryHandlerException {
        for (T obj : getAll()) {
            if (matches(name, obj.getName())) {
                return obj;
            }
        }

        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getByNames(Collection<String> names)
            throws RegistryHandlerException {
        List<T> retVal = new ArrayList<T>();
        for (T obj : getAll()) {
            if (names.contains(obj.getName())) {
                retVal.add(obj);
            }
        }

        return retVal;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getByOwner(String owner) throws RegistryHandlerException {
        List<T> retVal = new ArrayList<T>();
        for (T obj : getAll()) {
            if (matches(owner, obj.getOwner())) {
                retVal.add(obj);
            }
        }

        return retVal;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getByGroupName(String group) throws RegistryHandlerException {
        List<T> retVal = new ArrayList<T>();
        for (T obj : getAll()) {
            if (matches(group, obj.getGroupName())) {
                retVal.add(obj);
            }
        }

        return retVal;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getSubscribedToDataSetNames(String siteId)
            throws RegistryHandlerException {
        Set<String> retVal = new HashSet<String>();
        for (T obj : getAll()) {
            if (matches(siteId, obj.getOfficeIDs())) {
                retVal.add(obj.getDataSetName());
            }
        }

        return retVal;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getByFilters(String group, String officeIds)
            throws RegistryHandlerException {
        List<T> retVal = new ArrayList<T>();
        for (T obj : getAll()) {
            if (matches(group, obj.getGroupName())
                    && matches(officeIds, obj.getOfficeIDs())) {
                retVal.add(obj);
            }
        }

        return retVal;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getActive() throws RegistryHandlerException {
        List<T> retVal = new ArrayList<T>();
        for (T obj : getAll()) {
            if (obj.isActive()) {
                retVal.add(obj);
            }
        }

        return retVal;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getActiveForRoute(Network route)
            throws RegistryHandlerException {
        Map<Network, List<T>> subMap = getActiveForRoutes(new Network[] { route });
        return subMap.get(route);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<Network, List<T>> getActiveForRoutes(Network... routes)
            throws RegistryHandlerException {
        Map<Network, List<T>> retVal = new HashMap<Network, List<T>>();
        for (T obj : getActive()) {
            for (Network route : routes) {
                if (route == obj.getRoute()) {
                    if (retVal.get(route) == null) {
                        retVal.put(route, new ArrayList<T>());
                    }
                    retVal.get(route).add(obj);
                    break;
                }
            }
        }
        return retVal;
    }
}
