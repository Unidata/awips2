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
package com.raytheon.uf.viz.datadelivery.services;

import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthService;
import com.raytheon.uf.common.datadelivery.service.IGroupDefinitionService;
import com.raytheon.uf.common.datadelivery.service.ISubscriptionNotificationService;
import com.raytheon.uf.common.datadelivery.service.subscription.ISubscriptionOverlapService;
import com.raytheon.uf.viz.datadelivery.subscription.IPermissionsService;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService;

/**
 * Provides various Spring injected implementations of services. This class
 * should only be used by "dynamically" constructed objects, such as GUI dialogs
 * that cannot be dependency injected with services via Spring.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 09, 2012 1286       djohnson     Initial creation
 * May 20, 2013 2000       djohnson     Add subscription overlap service.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class DataDeliveryServices {

    private static IBandwidthService bandwidthService;

    private static ISubscriptionService subscriptionService;

    private static ISubscriptionNotificationService subscriptionNotificationService;

    private static IPermissionsService permissionsService;

    private static IGroupDefinitionService groupDefinitionService;

    private static ISubscriptionOverlapService subscriptionOverlapService;

    /**
     * Spring only constructor. All access should be through static methods.
     */
    private DataDeliveryServices(IBandwidthService bandwidthService,
            ISubscriptionService subscriptionService,
            ISubscriptionNotificationService subscriptionNotificationService,
            IPermissionsService permissionsService,
            IGroupDefinitionService groupDefinitionService,
            ISubscriptionOverlapService subscriptionOverlapService) {
        DataDeliveryServices.bandwidthService = bandwidthService;
        DataDeliveryServices.subscriptionService = subscriptionService;
        DataDeliveryServices.subscriptionNotificationService = subscriptionNotificationService;
        DataDeliveryServices.permissionsService = permissionsService;
        DataDeliveryServices.groupDefinitionService = groupDefinitionService;
        DataDeliveryServices.subscriptionOverlapService = subscriptionOverlapService;
    }

    /**
     * Get the subscription service.
     * 
     * @return the subscriptionService the subscription service
     */
    public static ISubscriptionService getSubscriptionService() {
        return DataDeliveryServices.subscriptionService;
    }

    /**
     * Get the bandwidth service.
     * 
     * @return the bandwidthService
     */
    public static IBandwidthService getBandwidthService() {
        return DataDeliveryServices.bandwidthService;
    }

    /**
     * Get the subscription service.
     * 
     * @return the subscription notification service
     */
    public static ISubscriptionNotificationService getSubscriptionNotificationService() {
        return DataDeliveryServices.subscriptionNotificationService;
    }

    /**
     * Get the permissions service.
     * 
     * @return the permissions service
     */
    public static IPermissionsService getPermissionsService() {
        return DataDeliveryServices.permissionsService;
    }

    /**
     * Get the group definition service.
     * 
     * @return the groupDefinitionService
     */
    public static IGroupDefinitionService getGroupDefinitionService() {
        return DataDeliveryServices.groupDefinitionService;
    }

    /**
     * Get the subscription overlap service.
     * 
     * @return the subscriptionOverlapService
     */
    public static ISubscriptionOverlapService getSubscriptionOverlapService() {
        return DataDeliveryServices.subscriptionOverlapService;
    }
}
