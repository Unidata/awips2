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
package com.raytheon.uf.viz.datadelivery.subscription;

import java.util.Collection;
import java.util.List;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * Provides filters to use with the SubscriptionManagerDlg.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 23, 2013 1650       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class SubscriptionManagerFilters {

    private static final ISubscriptionManagerFilter RETRIEVE_ALL = new ISubscriptionManagerFilter() {
        @Override
        public List<Subscription> getSubscriptions(
                ISubscriptionHandler subscriptionHandler)
                throws RegistryHandlerException {
            return subscriptionHandler.getAll();
        }
    };

    /**
     * Prevent construction.
     */
    private SubscriptionManagerFilters() {
    }

    public static ISubscriptionManagerFilter getAll() {
        return RETRIEVE_ALL;
    }

    /**
     * Filter that only accepts subscriptions with the specified names.
     * 
     * @param names
     *            the names
     * @return the filter
     */
    public static ISubscriptionManagerFilter getByNames(
            final Collection<String> names) {
        return new ISubscriptionManagerFilter() {
            @Override
            public List<Subscription> getSubscriptions(
                    ISubscriptionHandler subscriptionHandler)
                    throws RegistryHandlerException {
                return subscriptionHandler.getByNames(names);
            }
        };
    }

    /**
     * @param providerName
     * @param datasetName
     * @return
     */
    public static ISubscriptionManagerFilter getByProviderAndDataSet(
            final String providerName, final String datasetName) {
        return new ISubscriptionManagerFilter() {
            @Override
            public List<Subscription> getSubscriptions(
                    ISubscriptionHandler subscriptionHandler)
                    throws RegistryHandlerException {
                return subscriptionHandler.getActiveByDataSetAndProvider(
                        datasetName, providerName);
            }
        };
    }

}
