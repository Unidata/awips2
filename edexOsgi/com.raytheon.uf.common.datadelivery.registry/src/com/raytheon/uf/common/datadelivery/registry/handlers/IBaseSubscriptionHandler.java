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
import java.util.Set;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.registry.handler.IRegistryObjectHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * The {@link IRegistryObjectHandler} interface for classes that extend
 * {@link Subscription}.
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface IBaseSubscriptionHandler<T extends Subscription> extends
        IRegistryObjectHandler<T> {

    /**
     * Retrieve a subscription by name.
     * 
     * @param name
     *            the subscription name
     * @return the Subscription or null if none found
     * @throws RegistryHandlerException
     *             on unsuccessful response from the registry
     */
    T getByName(String name) throws RegistryHandlerException;

    /**
     * Retrieve a list of subscriptions by the owner.
     * 
     * @param owner
     *            the owner
     * @throws RegistryHandlerException
     *             on unsuccessful response from the registry
     */
    List<T> getByOwner(String owner) throws RegistryHandlerException;

    /**
     * Retrieve a list of subscriptions by the group name.
     * 
     * @param group
     *            the group name
     * @throws RegistryHandlerException
     *             on unsuccessful response from the registry
     */
    List<T> getByGroupName(String group) throws RegistryHandlerException;

    /**
     * Get a list of subscriptions by filters. Any field will allow null values.
     * 
     * @param group
     *            the group
     * @param officeId
     *            the officeId
     * @return the list of subscriptions meeting the criteria
     * @throws RegistryHandlerException
     *             on error
     */
    List<T> getByFilters(String group, String officeId)
            throws RegistryHandlerException;

    /**
     * Get the {@link Set} of data set names that are subscribed to.
     * 
     * @return the {@link Set} of data set names
     * @throws RegistryHandlerException
     *             on error
     */
    Set<String> getSubscribedToDataSetNames() throws RegistryHandlerException;

    /**
     * Retrieve all active subscriptions.
     * 
     * @return the list of subscriptions meeting the criteria
     * @throws RegistryHandlerException
     *             on error
     */
    List<T> getActive() throws RegistryHandlerException;
}
