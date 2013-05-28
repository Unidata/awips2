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
import java.util.Collections;
import java.util.List;
import java.util.Set;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSharedSubscription;
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.InitialPendingSiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * {@link IRegistryObjectHandler} implementation for
 * {@link InitialPendingSubscription}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2012 1169       djohnson     Initial creation
 * Sep 24, 2012 1157       mpduff       Changed to use InitialPendingSubscription.
 * 4/9/2013     1802      bphillip   Using constant values from constants package instead of RegistryUtil
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class PendingSubscriptionHandler implements IPendingSubscriptionHandler {

    private final IPendingSiteSubscriptionHandler siteSubscriptionHandler;

    private final IPendingSharedSubscriptionHandler sharedSubscriptionHandler;

    /**
     * Constructor.
     * 
     * @param siteSubscriptionHandler
     *            the user subscription handler
     * @param sharedSubscriptionHandler
     *            the shared subscription handler
     */
    public PendingSubscriptionHandler(
            IPendingSiteSubscriptionHandler siteSubscriptionHandler,
            IPendingSharedSubscriptionHandler sharedSubscriptionHandler) {
        this.siteSubscriptionHandler = siteSubscriptionHandler;
        this.sharedSubscriptionHandler = sharedSubscriptionHandler;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InitialPendingSubscription getByName(String name)
            throws RegistryHandlerException {
        InitialPendingSubscription value = siteSubscriptionHandler
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
    public List<InitialPendingSubscription> getByOwner(String owner)
            throws RegistryHandlerException {
        List<InitialPendingSubscription> subs = Lists.newArrayList();
        subs.addAll(siteSubscriptionHandler.getByOwner(owner));
        subs.addAll(siteSubscriptionHandler.getByOwner(owner));
        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<InitialPendingSubscription> getByGroupName(String group)
            throws RegistryHandlerException {
        List<InitialPendingSubscription> subs = Lists.newArrayList();
        subs.addAll(siteSubscriptionHandler.getByGroupName(group));
        subs.addAll(sharedSubscriptionHandler.getByGroupName(group));
        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<InitialPendingSubscription> getByFilters(String group,
            String officeId) throws RegistryHandlerException {
        List<InitialPendingSubscription> subs = Lists.newArrayList();
        subs.addAll(siteSubscriptionHandler.getByFilters(group, officeId));
        subs.addAll(sharedSubscriptionHandler.getByFilters(group, officeId));
        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> getSubscribedToDataSetNames()
            throws RegistryHandlerException {
        Set<String> names = Sets.newHashSet();
        names.addAll(siteSubscriptionHandler.getSubscribedToDataSetNames());
        names.addAll(sharedSubscriptionHandler.getSubscribedToDataSetNames());
        return names;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<InitialPendingSubscription> getActive()
            throws RegistryHandlerException {
        List<InitialPendingSubscription> subs = Lists.newArrayList();
        subs.addAll(siteSubscriptionHandler.getActive());
        subs.addAll(sharedSubscriptionHandler.getActive());
        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<InitialPendingSubscription> getActiveForRoute(Network route)
            throws RegistryHandlerException {
        List<InitialPendingSubscription> subs = Lists.newArrayList();
        subs.addAll(siteSubscriptionHandler.getActiveForRoute(route));
        subs.addAll(sharedSubscriptionHandler.getActiveForRoute(route));
        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<InitialPendingSubscription> getActiveForRoutes(
            Network... routes) throws RegistryHandlerException {
        List<InitialPendingSubscription> subs = Lists.newArrayList();
        subs.addAll(siteSubscriptionHandler.getActiveForRoutes(routes));
        subs.addAll(sharedSubscriptionHandler.getActiveForRoutes(routes));
        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InitialPendingSubscription getById(String id)
            throws RegistryHandlerException {
        InitialPendingSubscription value = siteSubscriptionHandler.getById(id);
        if (value == null) {
            value = sharedSubscriptionHandler.getById(id);
        }
        return value;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<InitialPendingSubscription> getAll()
            throws RegistryHandlerException {
        List<InitialPendingSubscription> subs = Lists.newArrayList();
        subs.addAll(siteSubscriptionHandler.getAll());
        subs.addAll(sharedSubscriptionHandler.getAll());
        return subs;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(InitialPendingSubscription obj)
            throws RegistryHandlerException {
        if (obj instanceof InitialPendingSiteSubscription) {
            siteSubscriptionHandler.store((InitialPendingSiteSubscription) obj);
        } else {
            sharedSubscriptionHandler
                    .store((InitialPendingSharedSubscription) obj);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(InitialPendingSubscription obj)
            throws RegistryHandlerException {
        if (obj instanceof InitialPendingSiteSubscription) {
            siteSubscriptionHandler
                    .update((InitialPendingSiteSubscription) obj);
        } else {
            sharedSubscriptionHandler
                    .update((InitialPendingSharedSubscription) obj);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void delete(InitialPendingSubscription obj)
            throws RegistryHandlerException {
        if (obj instanceof InitialPendingSiteSubscription) {
            siteSubscriptionHandler
                    .delete((InitialPendingSiteSubscription) obj);
        } else {
            sharedSubscriptionHandler
                    .delete((InitialPendingSharedSubscription) obj);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteById(String username, String registryId)
            throws RegistryHandlerException {
        siteSubscriptionHandler.deleteById(username, registryId);
        sharedSubscriptionHandler.deleteById(username, registryId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteByIds(String username, List<String> registryIds)
            throws RegistryHandlerException {
        siteSubscriptionHandler.deleteByIds(username, registryIds);
        sharedSubscriptionHandler.deleteByIds(username, registryIds);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void delete(String username, InitialPendingSubscription obj)
            throws RegistryHandlerException {
        if (obj instanceof InitialPendingSiteSubscription) {
            siteSubscriptionHandler.delete(username,
                    (InitialPendingSiteSubscription) obj);
        } else {
            sharedSubscriptionHandler.delete(username,
                    (InitialPendingSharedSubscription) obj);
        }
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public void delete(Collection<InitialPendingSubscription> objects)
            throws RegistryHandlerException {
        if (!CollectionUtil.isNullOrEmpty(objects)) {
            final Collection asSubtype = objects;
            if (objects.iterator().next() instanceof InitialPendingSiteSubscription) {
                siteSubscriptionHandler.delete(asSubtype);
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
    public void delete(String username,
            Collection<InitialPendingSubscription> objects)
            throws RegistryHandlerException {
        if (!CollectionUtil.isNullOrEmpty(objects)) {
            final Collection asSubtype = objects;
            if (objects.iterator().next() instanceof SiteSubscription) {
                siteSubscriptionHandler.delete(username, asSubtype);
            } else {
                sharedSubscriptionHandler.delete(username, asSubtype);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InitialPendingSubscription getBySubscription(
            Subscription subscription) throws RegistryHandlerException {
        if (subscription instanceof SiteSubscription) {
            return siteSubscriptionHandler.getBySubscription(subscription);
        } else {
            return sharedSubscriptionHandler.getBySubscription(subscription);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InitialPendingSubscription getBySubscriptionId(String id)
            throws RegistryHandlerException {
        InitialPendingSubscription value = siteSubscriptionHandler.getById(id);
        if (value == null) {
            value = sharedSubscriptionHandler.getById(id);
        }
        return value;
    }

    /**
     * {@inheritDoc}
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    public List<InitialPendingSubscription> getBySubscriptions(
            Collection<Subscription> subscriptions)
            throws RegistryHandlerException {
        if (!CollectionUtil.isNullOrEmpty(subscriptions)) {
            final Collection asSubtype = subscriptions;
            if (subscriptions.iterator().next() instanceof SiteSubscription) {
                return nullOrSubscriptionList(siteSubscriptionHandler
                        .getBySubscriptions(asSubtype));
            } else {
                return nullOrSubscriptionList(sharedSubscriptionHandler
                        .getBySubscriptions(asSubtype));
            }
        }
        return Collections.emptyList();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<InitialPendingSubscription> getBySubscriptionIds(
            List<String> ids) throws RegistryHandlerException {
        List<InitialPendingSubscription> subs = Lists.newArrayList();
        subs.addAll(siteSubscriptionHandler.getBySubscriptionIds(ids));
        subs.addAll(sharedSubscriptionHandler.getBySubscriptionIds(ids));
        return subs;
    }

    private List<InitialPendingSubscription> nullOrSubscriptionList(
            List<? extends InitialPendingSubscription> subscriptionList) {
        if (subscriptionList == null) {
            return null;
        } else {
            return new ArrayList<InitialPendingSubscription>(subscriptionList);
        }
    }
}
