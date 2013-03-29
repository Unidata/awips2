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
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.UserSubscription;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * {@link IRegistryObjectHandler} implementation for {@link Subscription}.
 * Currently only handles {@link UserSubscription} types.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2013 1841       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionHandler implements ISubscriptionHandler {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionHandler.class);

    private static final String SHARED_SUBSCRIPTIONS_ARE_NOT_CURRENTLY_SUPPORTED = "Shared subscriptions are not currently supported.";

    private final IUserSubscriptionHandler userSubscriptionHandler;

    /**
     * Constructor.
     * 
     * @param userSubscriptionHandler
     *            the user subscription handler
     */
    public SubscriptionHandler(IUserSubscriptionHandler userSubscriptionHandler) {
        this.userSubscriptionHandler = userSubscriptionHandler;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription getByPendingSubscription(PendingSubscription pending)
            throws RegistryHandlerException {
        return userSubscriptionHandler.getByPendingSubscription(pending);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription getByPendingSubscriptionId(final String id)
            throws RegistryHandlerException {
        return userSubscriptionHandler.getByPendingSubscriptionId(id);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteByIds(String username, List<String> ids)
            throws RegistryHandlerException {
        this.userSubscriptionHandler.deleteByIds(username, ids);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getActiveByDataSetAndProvider(String dataSetName,
            String providerName) throws RegistryHandlerException {
        return nullOrSubscriptionList(userSubscriptionHandler
                .getActiveByDataSetAndProvider(dataSetName, providerName));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription getByName(String name) throws RegistryHandlerException {
        return userSubscriptionHandler.getByName(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getByOwner(String owner)
            throws RegistryHandlerException {
        return nullOrSubscriptionList(userSubscriptionHandler.getByOwner(owner));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getByGroupName(String group)
            throws RegistryHandlerException {
        return nullOrSubscriptionList(userSubscriptionHandler
                .getByGroupName(group));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getByFilters(String group, String officeId)
            throws RegistryHandlerException {
        return nullOrSubscriptionList(userSubscriptionHandler.getByFilters(
                group, officeId));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getSubscribedToDataSetNames()
            throws RegistryHandlerException {
        return userSubscriptionHandler.getSubscribedToDataSetNames();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getActive() throws RegistryHandlerException {
        return nullOrSubscriptionList(userSubscriptionHandler.getActive());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getActiveForRoute(Network route)
            throws RegistryHandlerException {
        return nullOrSubscriptionList(userSubscriptionHandler
                .getActiveForRoute(route));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getActiveForRoutes(Network... routes)
            throws RegistryHandlerException {
        return nullOrSubscriptionList(userSubscriptionHandler
                .getActiveForRoutes(routes));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription getById(String id) throws RegistryHandlerException {
        return userSubscriptionHandler.getById(id);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getAll() throws RegistryHandlerException {
        return nullOrSubscriptionList(userSubscriptionHandler.getAll());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(Subscription obj) throws RegistryHandlerException {
        if (obj instanceof UserSubscription) {
            userSubscriptionHandler.store((UserSubscription) obj);
        } else {
            statusHandler
                    .info(SHARED_SUBSCRIPTIONS_ARE_NOT_CURRENTLY_SUPPORTED);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(Subscription obj) throws RegistryHandlerException {
        if (obj instanceof UserSubscription) {
            userSubscriptionHandler.update((UserSubscription) obj);
        } else {
            statusHandler
                    .info(SHARED_SUBSCRIPTIONS_ARE_NOT_CURRENTLY_SUPPORTED);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void delete(Subscription obj) throws RegistryHandlerException {
        if (obj instanceof UserSubscription) {
            userSubscriptionHandler.delete((UserSubscription) obj);
        } else {
            statusHandler
                    .info(SHARED_SUBSCRIPTIONS_ARE_NOT_CURRENTLY_SUPPORTED);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteById(String username, String registryId)
            throws RegistryHandlerException {
        userSubscriptionHandler.deleteById(username, registryId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void delete(String username, Subscription obj)
            throws RegistryHandlerException {
        if (obj instanceof UserSubscription) {
            userSubscriptionHandler.delete(username, (UserSubscription) obj);
        } else {
            statusHandler
                    .info(SHARED_SUBSCRIPTIONS_ARE_NOT_CURRENTLY_SUPPORTED);
        }
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public void delete(Collection<Subscription> objects)
            throws RegistryHandlerException {
        if (!CollectionUtil.isNullOrEmpty(objects)) {
            if (objects.iterator().next() instanceof UserSubscription) {
                final Collection asSubtype = objects;
                userSubscriptionHandler.delete(asSubtype);
            } else {
                statusHandler
                        .info(SHARED_SUBSCRIPTIONS_ARE_NOT_CURRENTLY_SUPPORTED);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public void delete(String username, Collection<Subscription> objects)
            throws RegistryHandlerException {
        if (!CollectionUtil.isNullOrEmpty(objects)) {
            if (objects.iterator().next() instanceof UserSubscription) {
                final Collection asSubtype = objects;
                userSubscriptionHandler.delete(username, asSubtype);
            } else {
                statusHandler
                        .info(SHARED_SUBSCRIPTIONS_ARE_NOT_CURRENTLY_SUPPORTED);
            }
        }
    }

    private List<Subscription> nullOrSubscriptionList(
            List<? extends Subscription> subscriptionList) {
        if (subscriptionList == null) {
            return null;
        } else {
            return new ArrayList<Subscription>(subscriptionList);
        }
    }
}
