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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * SiteSubscriptionHandler that performs no operations. Injected when site
 * subscriptions should be ignored. Used by the ncf registry since it does not
 * process site subscriptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 04, 2013  2545      bgonzale    Initial creation
 * Jan 29, 2014  2636      mpduff      Scheduling refactor.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class EmptySiteSubscriptionHandler implements ISiteSubscriptionHandler {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datadelivery.registry.handlers.
     * ISubscriptionTypeHandler
     * #getByPendingSubscription(com.raytheon.uf.common.datadelivery
     * .registry.PendingSubscription)
     */
    @Override
    public SiteSubscription getByPendingSubscription(PendingSubscription pending)
            throws RegistryHandlerException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datadelivery.registry.handlers.
     * ISubscriptionTypeHandler#getByPendingSubscriptionId(java.lang.String)
     */
    @Override
    public SiteSubscription getByPendingSubscriptionId(String id)
            throws RegistryHandlerException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datadelivery.registry.handlers.
     * ISubscriptionTypeHandler#getActiveByDataSetAndProvider(java.lang.String,
     * java.lang.String)
     */
    @Override
    public List<SiteSubscription> getActiveByDataSetAndProvider(
            String dataSetName, String providerName)
            throws RegistryHandlerException {
        return Collections.EMPTY_LIST;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datadelivery.registry.handlers.
     * IBaseSubscriptionHandler#getByName(java.lang.String)
     */
    @Override
    public SiteSubscription getByName(String name)
            throws RegistryHandlerException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datadelivery.registry.handlers.
     * IBaseSubscriptionHandler#getByNames(java.util.Collection)
     */
    @Override
    public List<SiteSubscription> getByNames(Collection<String> names)
            throws RegistryHandlerException {
        return Collections.EMPTY_LIST;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datadelivery.registry.handlers.
     * IBaseSubscriptionHandler#getByOwner(java.lang.String)
     */
    @Override
    public List<SiteSubscription> getByOwner(String owner)
            throws RegistryHandlerException {
        return Collections.EMPTY_LIST;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datadelivery.registry.handlers.
     * IBaseSubscriptionHandler#getByGroupName(java.lang.String)
     */
    @Override
    public List<SiteSubscription> getByGroupName(String group)
            throws RegistryHandlerException {
        return Collections.EMPTY_LIST;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datadelivery.registry.handlers.
     * IBaseSubscriptionHandler#getByFilters(java.lang.String, java.lang.String)
     */
    @Override
    public List<SiteSubscription> getByFilters(String group, String officeId)
            throws RegistryHandlerException {
        return Collections.EMPTY_LIST;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datadelivery.registry.handlers.
     * IBaseSubscriptionHandler#getSubscribedToDataSetNames(java.lang.String)
     */
    @Override
    public Set<String> getSubscribedToDataSetNames(String siteId)
            throws RegistryHandlerException {
        return Collections.EMPTY_SET;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datadelivery.registry.handlers.
     * IBaseSubscriptionHandler#getActive()
     */
    @Override
    public List<SiteSubscription> getActive() throws RegistryHandlerException {
        return Collections.EMPTY_LIST;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datadelivery.registry.handlers.
     * IBaseSubscriptionHandler
     * #getActiveForRoute(com.raytheon.uf.common.datadelivery.registry.Network)
     */
    @Override
    public List<SiteSubscription> getActiveForRoute(Network route)
            throws RegistryHandlerException {
        return Collections.EMPTY_LIST;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.datadelivery.registry.handlers.
     * IBaseSubscriptionHandler
     * #getActiveForRoutes(com.raytheon.uf.common.datadelivery
     * .registry.Network[])
     */
    @Override
    public Map<Network, List<SiteSubscription>> getActiveForRoutes(
            Network... routes) throws RegistryHandlerException {
        return new HashMap<Network, List<SiteSubscription>>(0);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.registry.handler.IRegistryObjectHandler#getById
     * (java.lang.String)
     */
    @Override
    public SiteSubscription getById(String id) throws RegistryHandlerException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.registry.handler.IRegistryObjectHandler#getAll()
     */
    @Override
    public List<SiteSubscription> getAll() throws RegistryHandlerException {
        return Collections.EMPTY_LIST;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.registry.handler.IRegistryObjectHandler#store(
     * java.lang.Object)
     */
    @Override
    public void store(SiteSubscription obj) throws RegistryHandlerException {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.registry.handler.IRegistryObjectHandler#update
     * (java.lang.Object)
     */
    @Override
    public void update(SiteSubscription obj) throws RegistryHandlerException {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.registry.handler.IRegistryObjectHandler#delete
     * (java.lang.Object)
     */
    @Override
    public void delete(SiteSubscription obj) throws RegistryHandlerException {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.registry.handler.IRegistryObjectHandler#deleteById
     * (java.lang.String, java.lang.String)
     */
    @Override
    public void deleteById(String username, String registryId)
            throws RegistryHandlerException {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.registry.handler.IRegistryObjectHandler#deleteByIds
     * (java.lang.String, java.util.List)
     */
    @Override
    public void deleteByIds(String username, List<String> registryIds)
            throws RegistryHandlerException {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.registry.handler.IRegistryObjectHandler#delete
     * (java.lang.String, java.lang.Object)
     */
    @Override
    public void delete(String username, SiteSubscription obj)
            throws RegistryHandlerException {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.registry.handler.IRegistryObjectHandler#delete
     * (java.util.Collection)
     */
    @Override
    public void delete(Collection<SiteSubscription> objects)
            throws RegistryHandlerException {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.registry.handler.IRegistryObjectHandler#delete
     * (java.lang.String, java.util.Collection)
     */
    @Override
    public void delete(String username, Collection<SiteSubscription> objects)
            throws RegistryHandlerException {
    }

}
