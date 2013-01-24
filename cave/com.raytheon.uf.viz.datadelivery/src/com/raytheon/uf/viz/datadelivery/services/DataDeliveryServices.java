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
import com.raytheon.uf.viz.datadelivery.subscription.IPermissionsService;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionNotificationService;
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
 * Nov 9, 2012  1286      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class DataDeliveryServices {

    private static final DataDeliveryServices INSTANCE = new DataDeliveryServices();

    private IBandwidthService bandwidthService;

    private ISubscriptionService subscriptionService;

    private ISubscriptionNotificationService subscriptionNotificationService;

    private IPermissionsService permissionsService;

    /**
     * Disabled constructor.
     */
    private DataDeliveryServices() {
    }

    /**
     * Used by Spring to populate the various services. Non Spring code should
     * use the static getter methods to retrieve services.
     * 
     * @return the instance
     */
    public static DataDeliveryServices getInstance() {
        return INSTANCE;
    }

    /**
     * Get the subscription service.
     * 
     * @return the subscriptionService the subscription service
     */
    public static ISubscriptionService getSubscriptionService() {
        return INSTANCE.subscriptionService;
    }

    /**
     * Set the subscription service.
     * 
     * @param subscriptionService
     *            the subscriptionService to set
     */
    public void setSubscriptionService(ISubscriptionService subscriptionService) {
        this.subscriptionService = subscriptionService;
    }

    /**
     * Get the bandwidth service.
     * 
     * @return the bandwidthService
     */
    public static IBandwidthService getBandwidthService() {
        return INSTANCE.bandwidthService;
    }

    /**
     * Set the bandwidth service.
     * 
     * @param bandwidthService
     *            the bandwidthService to set
     */
    public void setBandwidthService(IBandwidthService bandwidthService) {
        this.bandwidthService = bandwidthService;
    }

    /**
     * Get the subscription service.
     * 
     * @return the subscription notification service
     */
    public static ISubscriptionNotificationService getSubscriptionNotificationService() {
        return INSTANCE.subscriptionNotificationService;
    }

    /**
     * Set the subscription notification service.
     * 
     * @param subscriptionNotificationService
     *            the subscription notification service to set
     */
    public void setSubscriptionNotificationService(
            ISubscriptionNotificationService subscriptionNotificationService) {
        this.subscriptionNotificationService = subscriptionNotificationService;
    }

    /**
     * Get the permissions service.
     * 
     * @return the permissions service
     */
    public static IPermissionsService getPermissionsService() {
        return INSTANCE.permissionsService;
    }

    /**
     * Set the permissions service.
     * 
     * @param permissionsService
     *            the permissionsService to set
     */
    public void setPermissionsService(IPermissionsService permissionsService) {
        this.permissionsService = permissionsService;
    }
}
