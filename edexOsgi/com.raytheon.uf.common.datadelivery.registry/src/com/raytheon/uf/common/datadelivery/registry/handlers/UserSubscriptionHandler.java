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

import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.UserSubscription;
import com.raytheon.uf.common.datadelivery.registry.ebxml.UserSubscriptionQuery;
import com.raytheon.uf.common.registry.RegistryManager;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.registry.ebxml.AssociationQuery;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlers;
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
 * Sep 17, 2012 1169       djohnson     Initial creation.
 * Sep 24, 2012 1157       mpduff       Change to use InitialPendingSubscription.
 * Oct 17, 2012 0726       djohnson     Add {@link #getActiveByDataSetAndProvider}.
 * Mar 29, 2013 1841       djohnson     Renamed from SubscriptionHandler.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class UserSubscriptionHandler extends
        BaseSubscriptionHandler<UserSubscription, UserSubscriptionQuery>
        implements IUserSubscriptionHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    public UserSubscription getByPendingSubscription(PendingSubscription pending)
            throws RegistryHandlerException {
        return getByPendingSubscriptionId(RegistryUtil
                .getRegistryObjectKey(pending));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public UserSubscription getByPendingSubscriptionId(final String id)
            throws RegistryHandlerException {
        // Checks for the existence of the subscription
        AssociationQuery query = new AssociationQuery();
        query.setAssociationType(RegistryUtil.PATH_ASSOCIATION_RELATED_TO);
        query.setSourceObjectId(id);
        query.setReturnObjects(true);

        RegistryQueryResponse<Object> response = RegistryManager
                .getRegistyObjects(query);

        checkResponse(response, "getByPendingSubscriptionId");

        List<Object> results = response.getResults();
        // Currently only Subscriptions are associated to
        // PendingSubscriptions, but there could be other types of objects in
        // the future
        for (Object obj : results) {
            if (obj instanceof Subscription) {
                return UserSubscription.class.cast(obj);
            }
        }
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
    protected UserSubscriptionQuery getQuery() {
        return new UserSubscriptionQuery();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<UserSubscription> getRegistryObjectClass() {
        return UserSubscription.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<UserSubscription> getActiveByDataSetAndProvider(
            String dataSetName, String providerName)
            throws RegistryHandlerException {
        UserSubscriptionQuery query = getQuery();
        query.setDataSetName(dataSetName);
        query.setProviderName(providerName);
        query.setActive(true);

        RegistryQueryResponse<UserSubscription> response = RegistryManager
                .getRegistyObjects(query);

        checkResponse(response, "getActiveByDataSetAndProvider");

        return response.getResults();
    }
}
