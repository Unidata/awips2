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

import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingUserSubscription;
import com.raytheon.uf.common.datadelivery.registry.SharedSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.UserSubscription;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * {@link IRegistryObjectHandler} implementation for {@link Subscription}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2013 1841       djohnson     Initial creation
 * Apr 05, 2013 1841       djohnson     Add support for shared subscriptions.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SubscriptionHandler implements ISubscriptionHandler {

    private final IUserSubscriptionHandler userSubscriptionHandler;

    private final ISharedSubscriptionHandler sharedSubscriptionHandler;

    /**
     * Constructor.
     * 
     * @param userSubscriptionHandler
     *            the user subscription handler
     * @param sharedSubscriptionHandler
     *            the shared subscription handler
     */
    public SubscriptionHandler(
            IUserSubscriptionHandler userSubscriptionHandler,
            ISharedSubscriptionHandler sharedSubscriptionHandler) {
        this.userSubscriptionHandler = userSubscriptionHandler;
        this.sharedSubscriptionHandler = sharedSubscriptionHandler;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription getByPendingSubscription(PendingSubscription pending)
            throws RegistryHandlerException {
        if (pending instanceof PendingUserSubscription) {
            return userSubscriptionHandler.getByPendingSubscription(pending);
        } else {
            return sharedSubscriptionHandler.getByPendingSubscription(pending);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription getByPendingSubscriptionId(final String id)
            throws RegistryHandlerException {
        Subscription value = userSubscriptionHandler.getById(id);
        if (value == null) {
            value = sharedSubscriptionHandler.getById(id);
        }
        return value;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteByIds(String username, List<String> ids)
            throws RegistryHandlerException {
        userSubscriptionHandler.deleteByIds(username, ids);
        sharedSubscriptionHandler.deleteByIds(username, ids);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getActiveByDataSetAndProvider(String dataSetName,
            String providerName) throws RegistryHandlerException {
        List<Subscription> subs = Lists.newArrayList();
        subs.addAll(userSubscriptionHandler.getActiveByDataSetAndProvider(
                dataSetName, providerName));
        subs.addAll(sharedSubscriptionHandler.getActiveByDataSetAndProvider(
                dataSetName, providerName));

        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription getByName(String name) throws RegistryHandlerException {
        Subscription value = userSubscriptionHandler
                .getByName(name);
        if (value == null) {
            value = sharedSubscriptionHandler.getByName(name);
        }
        return value;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getByOwner(String owner)
            throws RegistryHandlerException {
        List<Subscription> subs = Lists.newArrayList();
        subs.addAll(userSubscriptionHandler.getByOwner(owner));
        subs.addAll(sharedSubscriptionHandler.getByOwner(owner));

        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getByGroupName(String group)
            throws RegistryHandlerException {
        List<Subscription> subs = Lists.newArrayList();
        subs.addAll(userSubscriptionHandler.getByGroupName(group));
        subs.addAll(sharedSubscriptionHandler.getByGroupName(group));

        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getByFilters(String group, String officeId)
            throws RegistryHandlerException {
        List<Subscription> subs = Lists.newArrayList();
        subs.addAll(userSubscriptionHandler.getByFilters(group, officeId));
        subs.addAll(sharedSubscriptionHandler.getByFilters(group, officeId));

        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getSubscribedToDataSetNames()
            throws RegistryHandlerException {
        Set<String> set = Sets.newHashSet();
        set.addAll(userSubscriptionHandler.getSubscribedToDataSetNames());
        set.addAll(sharedSubscriptionHandler.getSubscribedToDataSetNames());

        return set;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getActive() throws RegistryHandlerException {
        List<Subscription> subs = Lists.newArrayList();
        subs.addAll(userSubscriptionHandler.getActive());
        subs.addAll(sharedSubscriptionHandler.getActive());

        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getActiveForRoute(Network route)
            throws RegistryHandlerException {
        List<Subscription> subs = Lists.newArrayList();
        subs.addAll(userSubscriptionHandler.getActiveForRoute(route));
        subs.addAll(sharedSubscriptionHandler.getActiveForRoute(route));
        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getActiveForRoutes(Network... routes)
            throws RegistryHandlerException {
        List<Subscription> subs = Lists.newArrayList();
        subs.addAll(userSubscriptionHandler.getActiveForRoutes(routes));
        subs.addAll(sharedSubscriptionHandler.getActiveForRoutes(routes));
        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription getById(String id) throws RegistryHandlerException {
        Subscription value = userSubscriptionHandler.getById(id);
        if (value == null) {
            value = sharedSubscriptionHandler.getById(id);
        }
        return value;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Subscription> getAll() throws RegistryHandlerException {
        List<Subscription> subs = Lists.newArrayList();
        subs.addAll(userSubscriptionHandler.getAll());
        subs.addAll(sharedSubscriptionHandler.getAll());
        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(Subscription obj) throws RegistryHandlerException {
        if (obj instanceof UserSubscription) {
            userSubscriptionHandler.store((UserSubscription) obj);
        } else {
            sharedSubscriptionHandler.store((SharedSubscription) obj);
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
            sharedSubscriptionHandler.update((SharedSubscription) obj);
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
            sharedSubscriptionHandler.delete((SharedSubscription) obj);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteById(String username, String registryId)
            throws RegistryHandlerException {
        userSubscriptionHandler.deleteById(username, registryId);
        sharedSubscriptionHandler.deleteById(username, registryId);
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
            sharedSubscriptionHandler
                    .delete(username, (SharedSubscription) obj);
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
            final Collection asSubtype = objects;
            if (objects.iterator().next() instanceof UserSubscription) {
                userSubscriptionHandler.delete(asSubtype);
            } else {
                sharedSubscriptionHandler.delete(asSubtype);
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
            final Collection asSubtype = objects;
            if (objects.iterator().next() instanceof UserSubscription) {
                userSubscriptionHandler.delete(username, asSubtype);
            } else {
                sharedSubscriptionHandler.delete(username, asSubtype);
            }
        }
    }
}
