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
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlers;
import com.raytheon.uf.common.util.CollectionUtil;

/**
 * Base {@link ISubscriptionTypeHandler} in-memory implementation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 29, 2013 1650       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class MemorySubscriptionTypeHandler<T extends Subscription>
        extends BaseMemorySubscriptionHandler<T> implements
        IBaseSubscriptionHandler<T>, ISubscriptionTypeHandler<T> {

    /**
     * {@inheritDoc}
     */
    @Override
    public T getByPendingSubscription(PendingSubscription pending)
            throws RegistryHandlerException {
        return getByPendingSubscriptionId(RegistryUtil
                .getRegistryObjectKey(pending));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public T getByPendingSubscriptionId(final String id)
            throws RegistryHandlerException {
        // TODO: lookup via in-memory association
        return null;
    }

    /**
     * Overridden because subscriptions must also have their
     * {@link PendingSubscription} object deleted.
     * 
     * @param username
     *            the username of the requester
     * @param ids
     *            the registry ids of the subscription objects
     */
    @Override
    public void deleteByIds(String username, List<String> ids)
            throws RegistryHandlerException {
        IPendingSubscriptionHandler handler = RegistryObjectHandlers
                .get(IPendingSubscriptionHandler.class);

        List<InitialPendingSubscription> pending = handler
                .getBySubscriptionIds(ids);
        if (!CollectionUtil.isNullOrEmpty(pending)) {
            handler.delete(username, pending);
        }

        super.deleteByIds(username, ids);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getActiveByDataSetAndProvider(String dataSetName,
            String providerName) throws RegistryHandlerException {
        List<T> retVal = new ArrayList<T>();

        for (T obj : getActive()) {
            if (matches(dataSetName, obj.getDataSetName())
                    && matches(providerName, obj.getProvider())) {
                retVal.add(obj);
            }
        }

        return retVal;
    }
}
