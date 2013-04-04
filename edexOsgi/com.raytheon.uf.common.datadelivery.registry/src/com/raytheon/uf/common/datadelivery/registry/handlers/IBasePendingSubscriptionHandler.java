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
 * Apr 04, 2012 1841       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IBasePendingSubscriptionHandler<T extends InitialPendingSubscription>
        extends IBaseSubscriptionHandler<T> {

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
    T getBySubscription(Subscription subscription)
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
    T getBySubscriptionId(String id)
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
    List<T> getBySubscriptions(
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
    List<T> getBySubscriptionIds(List<String> ids)
            throws RegistryHandlerException;
}
