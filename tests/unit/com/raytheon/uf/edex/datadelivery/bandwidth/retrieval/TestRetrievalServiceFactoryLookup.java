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
package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import org.junit.Ignore;

import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.edex.datadelivery.retrieval.IServiceFactoryLookup;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.opendap.MockOpenDapServiceFactory;

/**
 * Implementation of {@link IServiceFactoryLookup} that doesn't really go out to
 * the internet.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public class TestRetrievalServiceFactoryLookup implements IServiceFactoryLookup {
    /**
     * {@inheritDoc}
     */
    @Override
    public ServiceFactory getProviderServiceFactory(Provider provider) {
        final ServiceType serviceType = provider.getServiceType();
        switch (serviceType) {
        case OPENDAP:
            return new MockOpenDapServiceFactory(provider);
        default:
            throw new IllegalArgumentException(
                    "Don't know how to handle service [" + serviceType + "]");
        }

    }
}
