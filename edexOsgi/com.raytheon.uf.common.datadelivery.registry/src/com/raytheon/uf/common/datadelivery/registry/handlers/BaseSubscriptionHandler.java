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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.RecurringSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.ebxml.SubscriptionDataSetNameQuery;
import com.raytheon.uf.common.datadelivery.registry.ebxml.SubscriptionFilterableQuery;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.registry.handler.BaseRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlers;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Base subscription handler.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2012 1169       djohnson     Initial creation
 * Oct 03, 2012 1241       djohnson     More query methods.
 * Oct 10, 2012 0726       djohnson     Add {@link #getActive()}.
 * Feb 20, 2013 1543       djohnson     Add ability to filter on routes.
 * May 28, 2013 1650       djohnson     Add getByNames.
 * Jun 24, 2013 2106       djohnson     Now composes a registryHandler.
 * Jul 18, 2013 2193       mpduff       Changes for SubscriptionDataSetNameQuery.
 * Sep 11, 2013 2352       mpduff       Add siteId to getSubscribedToDataSetNames method.
 * Jan 14, 2014 2459       mpduff       Validate subs should be scheduled before returning them.
 * Jan 17, 2014 2459       mpduff       Persist the state of the expired subs.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class BaseSubscriptionHandler<T extends Subscription, QUERY extends SubscriptionFilterableQuery<T>>
        extends BaseRegistryObjectHandler<T, QUERY> implements
        IBaseSubscriptionHandler<T> {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BaseSubscriptionHandler.class);

    private final List<T> updateList = new ArrayList<T>();

    /**
     * {@inheritDoc}
     * 
     * @throws RegistryHandlerException
     */
    @Override
    public T getByName(String name) throws RegistryHandlerException {
        SubscriptionFilterableQuery<T> query = getQuery();
        query.setName(name);

        RegistryQueryResponse<T> response = registryHandler.getObjects(query);

        checkResponse(response, "getByName");

        return response.getSingleResult();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getByNames(Collection<String> names)
            throws RegistryHandlerException {
        SubscriptionFilterableQuery<T> query = getQuery();
        query.setNames(names);

        RegistryQueryResponse<T> response = registryHandler.getObjects(query);

        checkResponse(response, "getByNames");

        return response.getResults();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getByOwner(String owner) throws RegistryHandlerException {
        SubscriptionFilterableQuery<T> query = getQuery();
        query.setOwner(owner);

        RegistryQueryResponse<T> response = registryHandler.getObjects(query);

        checkResponse(response, "getByOwner");

        return response.getResults();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getByGroupName(String group) throws RegistryHandlerException {
        SubscriptionFilterableQuery<T> query = getQuery();
        query.setGroupName(group);

        RegistryQueryResponse<T> response = registryHandler.getObjects(query);

        checkResponse(response, "getByGroupName");

        return response.getResults();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getSubscribedToDataSetNames(String siteId)
            throws RegistryHandlerException {
        SubscriptionDataSetNameQuery query = new SubscriptionDataSetNameQuery();
        query.setRegistryObjectClass(getRegistryObjectClass().getName());
        query.setOfficeId(siteId);
        RegistryQueryResponse<String> response = registryHandler
                .getObjects(query);

        checkResponse(response, "getSubscribedToDataSetNames");

        return new HashSet<String>(response.getResults());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getByFilters(String group, String officeId)
            throws RegistryHandlerException {
        QUERY query = getQuery();
        if (group != null) {
            query.setGroupName(group);
        }

        if (officeId != null) {
            query.setOfficeId(officeId);
        }

        RegistryQueryResponse<T> response = registryHandler.getObjects(query);

        checkResponse(response, "getByFilters");

        return response.getResults();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getActive() throws RegistryHandlerException {
        SubscriptionFilterableQuery<T> query = getQuery();
        query.setActive(true);

        RegistryQueryResponse<T> response = registryHandler.getObjects(query);

        checkResponse(response, "getActive");

        return response.getResults();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getActiveForRoute(Network route)
            throws RegistryHandlerException {
        return getActiveForRoutes(route);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getActiveForRoutes(Network... routes)
            throws RegistryHandlerException {
        SubscriptionFilterableQuery<T> query = getQuery();
        query.setActive(true);
        query.setRoutes(Arrays.asList(routes));

        RegistryQueryResponse<T> response = registryHandler.getObjects(query);

        checkResponse(response, "getActiveForRoutes");

        List<T> returnList = new ArrayList<T>();
        for (T sub : response.getResults()) {
            if (((RecurringSubscription) sub).shouldSchedule()) {
                returnList.add(sub);
            } else if (((RecurringSubscription) sub).shouldUpdate()) {
                updateList.add(sub);
            }
        }

        // Save updated objects back to registry
        if (!updateList.isEmpty()) {
            ExecutorService executor = Executors.newFixedThreadPool(1);
            executor.execute(new Runnable() {
                @Override
                public void run() {
                    synchronized (updateList) {
                        ISubscriptionHandler sh = RegistryObjectHandlers
                                .get(ISubscriptionHandler.class);
                        for (T s : updateList) {
                            try {
                                sh.update(s);
                                statusHandler.info("Subscription "
                                        + s.getName() + " is expired.");
                            } catch (RegistryHandlerException e) {
                                statusHandler.handle(Priority.WARN,
                                        "Unable to set subscription to expired ["
                                                + s.getName() + "]", e);
                            }
                        }
                        updateList.clear();
                    }
                }
            });
        }

        return returnList;
    }
}
