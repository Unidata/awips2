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
import java.util.Collections;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;



/**
 * {@link IPendingSubscriptionHandler} in-memory implementation.
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

public class MemoryPendingSubscriptionHandler extends
        BaseMemorySubscriptionHandler<InitialPendingSubscription> implements
        IPendingSubscriptionHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    public void store(InitialPendingSubscription obj)
            throws RegistryHandlerException {
        // TODO: Store an in-memory association to the subscription
        super.store(obj);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteByIds(String username, List<String> registryIds)
            throws RegistryHandlerException {
        // TODO: Delete in-memory association to the subscription
        super.deleteByIds(username, registryIds);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InitialPendingSubscription getBySubscription(
            Subscription subscription) throws RegistryHandlerException {
        return getBySubscriptionId(RegistryUtil
                .getRegistryObjectKey(subscription));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public InitialPendingSubscription getBySubscriptionId(String id)
            throws RegistryHandlerException {
        List<InitialPendingSubscription> results = getBySubscriptionIds(Arrays
                .asList(id));
        return (!results.isEmpty()) ? results.iterator().next() : null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<InitialPendingSubscription> getBySubscriptions(
            Collection<Subscription> subscriptions)
            throws RegistryHandlerException {
        List<String> ids = new ArrayList<String>(subscriptions.size());
        for (Subscription subscription : subscriptions) {
            ids.add(RegistryUtil.getRegistryObjectKey(subscription));
        }
        return getBySubscriptionIds(ids);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<InitialPendingSubscription> getBySubscriptionIds(
            List<String> ids) throws RegistryHandlerException {
        // TODO: Lookup via an in-memory association
        return Collections.emptyList();
    }
}
