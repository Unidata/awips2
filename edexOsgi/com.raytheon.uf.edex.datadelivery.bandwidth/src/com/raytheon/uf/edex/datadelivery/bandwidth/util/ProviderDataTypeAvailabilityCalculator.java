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
package com.raytheon.uf.edex.datadelivery.bandwidth.util;

import java.util.concurrent.ExecutionException;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.datadelivery.registry.DataDeliveryRegistryObjectTypes;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.IProviderHandler;
import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.registry.event.RegistryEvent;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;

/**
 * Uses provider configured values for the availability delay.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 05, 2013 2038       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ProviderDataTypeAvailabilityCalculator implements
        IDataSetAvailablityCalculator {

    private static final Provider PROVIDER_NOT_FOUND = new Provider();

    private final LoadingCache<String, Provider> providerCache;

    /**
     * Constructor.
     */
    public ProviderDataTypeAvailabilityCalculator(
            final IProviderHandler providerHandler) {

        // TODO: This should probably be moved inside the registry handler
        // itself
        this.providerCache = CacheBuilder.newBuilder().build(
                new CacheLoader<String, Provider>() {
                    @Override
                    public Provider load(String key)
                            throws RegistryHandlerException {
                        Provider provider = providerHandler
                                .getByName(key);
                        if (provider == null) {
                            provider = PROVIDER_NOT_FOUND;
                        }
                        return provider;
                    }
                });

        EventBus.register(this);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getDataSetAvailablityDelay(Subscription subscription) {
        final String providerName = subscription.getProvider();
        final DataType dataType = subscription.getDataSetType();

        try {
            Provider provider = this.providerCache.get(providerName);

            if (provider == PROVIDER_NOT_FOUND) {
                throw new IllegalArgumentException(
                        "No availability delay registered for provider "
                                + providerName + " for data type " + dataType);
            }

            final ProviderType providerType = provider
                    .getProviderType(dataType);

            if (providerType == null) {
                throw new IllegalArgumentException(
                        "No availability delay registered for provider "
                                + providerName + " for data type " + dataType);
            }
            return providerType.getAvailabilityDelay();
        } catch (ExecutionException e) {
            throw new IllegalStateException(
                    "Exception querying for the provider!", e);
        }
    }

    @Subscribe
    public void registryEventListener(RegistryEvent re) {
        final String objectType = re.getObjectType();

        // If a provider event happens then expire the entire cache
        if (DataDeliveryRegistryObjectTypes.PROVIDER.equals(objectType)) {
            providerCache.invalidateAll();
        }
    }
}
