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
 * {@link ISubscriptionHandler} in-memory implementation.
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

public class MemorySubscriptionHandler extends
        BaseMemorySubscriptionHandler<Subscription> implements
        ISubscriptionHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription getByPendingSubscription(PendingSubscription pending)
            throws RegistryHandlerException {
        return getByPendingSubscriptionId(RegistryUtil
                .getRegistryObjectKey(pending));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Subscription getByPendingSubscriptionId(final String id)
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
    public List<Subscription> getActiveByDataSetAndProvider(String dataSetName,
            String providerName) throws RegistryHandlerException {
        List<Subscription> retVal = new ArrayList<Subscription>();

        for (Subscription obj : getActive()) {
            if (matches(dataSetName, obj.getDataSetName())
                    && matches(providerName, obj.getProvider())) {
                retVal.add(obj);
            }
        }

        return retVal;
    }
}
