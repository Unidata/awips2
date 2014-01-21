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

import com.raytheon.uf.common.datadelivery.registry.AdhocSubscription;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * Empty implementation for adhoc subscriptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 20, 2014   2538     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class EmptyAdhocSubscriptionHandler implements IAdhocSubscriptionHandler {

    @Override
    public AdhocSubscription getByName(String name)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public List<AdhocSubscription> getByNames(Collection<String> names)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public List<AdhocSubscription> getByOwner(String owner)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public List<AdhocSubscription> getByGroupName(String group)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public List<AdhocSubscription> getByFilters(String group, String officeId)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Set<String> getSubscribedToDataSetNames(String siteId)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public List<AdhocSubscription> getActive() throws RegistryHandlerException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public List<AdhocSubscription> getActiveForRoute(Network route)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public List<AdhocSubscription> getActiveForRoutes(Network... routes)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public AdhocSubscription getById(String id) throws RegistryHandlerException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public List<AdhocSubscription> getAll() throws RegistryHandlerException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void store(AdhocSubscription obj) throws RegistryHandlerException {
        // TODO Auto-generated method stub

    }

    @Override
    public void update(AdhocSubscription obj) throws RegistryHandlerException {
        // TODO Auto-generated method stub

    }

    @Override
    public void delete(AdhocSubscription obj) throws RegistryHandlerException {
        // TODO Auto-generated method stub

    }

    @Override
    public void deleteById(String username, String registryId)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub

    }

    @Override
    public void deleteByIds(String username, List<String> registryIds)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub

    }

    @Override
    public void delete(String username, AdhocSubscription obj)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub

    }

    @Override
    public void delete(Collection<AdhocSubscription> objects)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub

    }

    @Override
    public void delete(String username, Collection<AdhocSubscription> objects)
            throws RegistryHandlerException {
        // TODO Auto-generated method stub

    }
}
