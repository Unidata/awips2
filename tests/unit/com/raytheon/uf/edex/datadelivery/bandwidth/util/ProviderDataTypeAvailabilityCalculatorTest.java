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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderType;
import com.raytheon.uf.common.datadelivery.registry.SiteSubscription;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.IProviderHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.util.registry.RegistryException;

/**
 * Test {@link ProviderDataTypeAvailabilityCalculator}.
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

public class ProviderDataTypeAvailabilityCalculatorTest {

    private static final int ONE_HUNDRED = 100;

    private static final String PROVIDER_NAME = "someProviderName";

    private static final Provider provider = new Provider();
    static {
        provider.setName(PROVIDER_NAME);
        provider.setProviderType(Arrays.<ProviderType> asList(new ProviderType(
                DataType.GRID, "grid", ONE_HUNDRED)));
    }

    private final IProviderHandler providerHandler = mock(IProviderHandler.class);

    private final ProviderDataTypeAvailabilityCalculator availabilityCalculator = new ProviderDataTypeAvailabilityCalculator(
            providerHandler);

    @Before
    public void setUp() throws RegistryException, RegistryHandlerException {
        when(providerHandler.getByName(PROVIDER_NAME)).thenReturn(provider);
    }

    @Test
    public void returnsConfiguredAvailabilityWhenRegistered() {
        Subscription subscription = new SiteSubscription();
        subscription.setProvider(PROVIDER_NAME);
        subscription.setDataSetType(DataType.GRID);

        assertThat(
                availabilityCalculator.getDataSetAvailablityDelay(subscription),
                is(equalTo(ONE_HUNDRED)));
    }

    @Test(expected = IllegalArgumentException.class)
    public void throwsIllegalArgumentExceptionWhenProviderNotRegistered() {
        Subscription subscription = new SiteSubscription();
        subscription.setProvider("someOtherProviderName");
        subscription.setDataSetType(DataType.GRID);

        availabilityCalculator.getDataSetAvailablityDelay(subscription);
    }

    @Test(expected = IllegalArgumentException.class)
    public void throwsIllegalArgumentExceptionWhenDataTypeNotRegistered() {
        Subscription subscription = new SiteSubscription();
        subscription.setProvider(PROVIDER_NAME);
        subscription.setDataSetType(DataType.POINT);

        availabilityCalculator.getDataSetAvailablityDelay(subscription);
    }

}
