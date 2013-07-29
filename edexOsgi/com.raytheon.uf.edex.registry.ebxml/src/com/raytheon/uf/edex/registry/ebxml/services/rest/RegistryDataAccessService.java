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
package com.raytheon.uf.edex.registry.ebxml.services.rest;

import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DeliveryInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SubscriptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.RegistryException;
import com.raytheon.uf.common.registry.services.rest.IRegistryDataAccessService;
import com.raytheon.uf.common.registry.services.rest.response.RestCollectionResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;

/**
 * 
 * Implementation of the registry data access service interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/29/2013    2191        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Transactional
public class RegistryDataAccessService implements IRegistryDataAccessService {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryDataAccessService.class);

    /** Data access object for registry objects */
    private RegistryObjectDao registryObjectDao;

    /** Lifecyclemanager */
    private LifecycleManager lcm;

    @GET
    @Path("/rest/dataAccess/getRegistryObjectIds/{objectType}")
    public RestCollectionResponse<String> getRegistryObjectIdsOfType(
            @PathParam("objectType") String objectType) {
        statusHandler.info("Getting registry object ids of type [" + objectType
                + "]...");
        RestCollectionResponse<String> response = new RestCollectionResponse<String>();
        response.setPayload(registryObjectDao
                .getRegistryObjectIdsOfType(objectType));
        return response;
    }

    @GET
    @Path("/rest/dataAccess/removeSubscriptionsFor/{siteId}")
    public void removeSubscriptionsForSite(@PathParam("siteId") String siteId) {
        statusHandler.info("Removing subscriptions for: " + siteId);
        List<SubscriptionType> subscriptions = registryObjectDao
                .executeHQLQuery(
                        "from SubscriptionType sub where sub.owner=:siteId",
                        "siteId", siteId);
        if (subscriptions.isEmpty()) {
            statusHandler.info("No subscriptions present for site: " + siteId);
        } else {
            List<Integer> deliveryInfoKeys = new ArrayList<Integer>();
            ObjectRefListType refList = new ObjectRefListType();
            for (SubscriptionType sub : subscriptions) {
                refList.getObjectRef().add(new ObjectRefType(sub.getId()));
                for (DeliveryInfoType deliveryInfo : sub.getDeliveryInfo()) {
                    deliveryInfoKeys.add(deliveryInfo.getKey());
                }
            }
            RemoveObjectsRequest removeRequest = new RemoveObjectsRequest();
            removeRequest.setId("Remote subscription removal request for "
                    + siteId);
            removeRequest.setComment("Removal of remote subscriptions for "
                    + siteId);
            removeRequest.setObjectRefList(refList);
            try {
                RegistryResponseType response = lcm
                        .removeObjects(removeRequest);
                if (response.getStatus().equals(RegistryResponseStatus.SUCCESS)) {
                    registryObjectDao
                            .executeHQLStatement(
                                    "DELETE FROM DeliveryInfoType deliveryInfo where deliveryInfo.key in (:keys)",
                                    "keys", deliveryInfoKeys);
                    statusHandler
                            .info("Successfully removed subscriptions for site "
                                    + siteId);
                } else {
                    statusHandler
                            .info("Failed to remove subscriptions for site "
                                    + siteId);
                }
            } catch (Exception e) {
                throw new RegistryException(
                        "Error removing subscriptions for site " + siteId, e);
            }

        }
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setLcm(LifecycleManager lcm) {
        this.lcm = lcm;
    }

}
