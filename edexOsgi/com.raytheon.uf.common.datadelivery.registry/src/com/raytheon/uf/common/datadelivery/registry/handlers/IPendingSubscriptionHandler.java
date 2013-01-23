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

import com.raytheon.uf.common.datadelivery.registry.InitialPendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * The {@link IRegistryObjectHandler} interface for
 * {@link InitialPendingSubscription}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 18, 2012 1169       djohnson     Initial creation
 * Sep 24, 2012 1157       mpduff       Change to use InitialPendingSubscription.
 * Sep 28, 2012 1187       djohnson     Extend {@link IBaseSubscriptionHandler}.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IPendingSubscriptionHandler extends
        IBaseSubscriptionHandler<InitialPendingSubscription> {

    /**
     * Get the {@link InitialPendingSubscription} associated with a
     * {@link Subscription}.
     * 
     * @param subscription
     *            the subscription
     * @return the pending subscription
     * @throws RegistryHandlerException
     *             if an unsuccessful response is received from the registry
     */
    InitialPendingSubscription getBySubscription(Subscription subscription)
            throws RegistryHandlerException;

    /**
     * Get the {@link InitialPendingSubscription} associated with a
     * {@link Subscription} by the subscription id.
     * 
     * @param id
     *            the subscription's id
     * @return the pending subscription
     * @throws RegistryHandlerException
     *             if an unsuccessful response is received from the registry
     */
    InitialPendingSubscription getBySubscriptionId(String id)
            throws RegistryHandlerException;

    /**
     * Get the {@link InitialPendingSubscription}s associated with a collection
     * of {@link Subscription}s.
     * 
     * @param subscriptions
     *            the subscriptions
     * @return the pending subscriptions
     * @throws RegistryHandlerException
     *             if an unsuccessful response is received from the registry
     */
    List<InitialPendingSubscription> getBySubscriptions(
            Collection<Subscription> subscriptions)
            throws RegistryHandlerException;

    /**
     * Get the {@link InitialPendingSubscription}s associated with a collection
     * of {@link Subscription} ids.
     * 
     * @param ids
     *            the ids
     * @return the pending subscriptions
     * @throws RegistryHandlerException
     *             if an unsuccessful response is received from the registry
     */
    List<InitialPendingSubscription> getBySubscriptionIds(List<String> ids)
            throws RegistryHandlerException;
}
