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
package com.raytheon.uf.edex.datadelivery.retrieval;

import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;
import com.raytheon.uf.edex.datadelivery.retrieval.opendap.OpenDapServiceFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.wcs.WcsServiceFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.wfs.WfsServiceFactory;
import com.raytheon.uf.edex.datadelivery.retrieval.wxxm.WxxmServiceFactory;

/**
 * Retrieve {@link ServiceType} specific implementations of interfaces.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2012 955        djohnson     Initial creation
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class ServiceTypeFactory {

    private ServiceTypeFactory() {

    }

    /**
     * Retrieve the {@link ServiceFactory} to handle a specific {@link Provider}
     * .
     * 
     * @param provider
     *            the provider
     * @return the factory
     */
    public static ServiceFactory retrieveServiceFactory(Provider provider) {
        final ServiceType serviceType = provider.getServiceType();
        switch (serviceType) {
        case OPENDAP:
            return new OpenDapServiceFactory(provider);
        case WCS:
            return new WcsServiceFactory();
        case WFS:
            return new WfsServiceFactory();
        case WXXM:
            return new WxxmServiceFactory();
        default:
            throw new IllegalArgumentException(String.format(
                    "No %s available to handle service type [%s]!",
                    ServiceFactory.class.getSimpleName(), serviceType));
        }
    }

    /**
     * Retrieve the {@link RetrievalAdapter} implementation for this service
     * type.
     * 
     * @param serviceType
     *            the service type
     * @return the retrieval adapter
     */
    public static RetrievalAdapter retrieveServiceRetrievalAdapter(
            ServiceType serviceType) {
        Provider provider = new Provider();
        provider.setServiceType(serviceType);
        return retrieveServiceFactory(provider).getRetrievalGenerator()
                .getServiceRetrievalAdapter();
    }
}
