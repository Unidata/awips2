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

import java.util.EnumMap;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.Provider.ServiceType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.ReflectionUtil;
import com.raytheon.uf.common.util.ServiceLoaderUtil;
import com.raytheon.uf.common.util.registry.GenericRegistry;
import com.raytheon.uf.common.util.registry.RegistryException;
import com.raytheon.uf.edex.datadelivery.retrieval.adapters.RetrievalAdapter;

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
 * Mar 21, 2013 1794       djohnson     ServiceLoaderUtil now requires the requesting class.
 * May 31, 2013 2038       djohnson     Plugin contributable registry.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class ServiceTypeFactory {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ServiceTypeFactory.class);

    private static class ServiceTypeRegistry extends
            GenericRegistry<ServiceType, Class<ServiceFactory>> {


        private ServiceTypeRegistry() {
            super(new EnumMap<ServiceType, Class<ServiceFactory>>(
                    ServiceType.class));
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Object register(ServiceType t, Class<ServiceFactory> s)
                throws RegistryException {

            Validate.notNull(t);
            Validate.notNull(s);

            statusHandler.info("Registered service type factory ["
                    + s.getName() + "] for service type [" + t + "]");

            return super.register(t, s);
        }
    };

    private static final ServiceTypeRegistry serviceTypeRegistry = new ServiceTypeRegistry();

    /**
     * Default {@link IServiceFactoryLookup} to be used in production code.
     */
    private static class ServiceTypeFactoryLookup implements
            IServiceFactoryLookup {
        @Override
        public ServiceFactory getProviderServiceFactory(Provider provider) {
            final ServiceType serviceType = provider.getServiceType();
            final Class<ServiceFactory> serviceFactoryClass = serviceTypeRegistry
                    .getRegisteredObject(serviceType);
            if (serviceFactoryClass == null) {
                throw new IllegalArgumentException(String.format(
                        "No %s available to handle service type [%s]!",
                        ServiceFactory.class.getSimpleName(), serviceType));
            }

            // Must create a new instance because the implementations are not
            // thread safe
            ServiceFactory serviceFactory = ReflectionUtil
                    .newInstanceOfAssignableType(ServiceFactory.class,
                            serviceFactoryClass);
            serviceFactory.setProvider(provider);
            return serviceFactory;
        }
    }

    private static final IServiceFactoryLookup SERVICE_FACTORY_LOOKUP = ServiceLoaderUtil
            .load(ServiceTypeFactory.class, IServiceFactoryLookup.class,
                    new ServiceTypeFactoryLookup());

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
        return SERVICE_FACTORY_LOOKUP.getProviderServiceFactory(provider);
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

    /**
     * Get the service type registry.
     * 
     * @return the registry
     */
    public static GenericRegistry<ServiceType, Class<ServiceFactory>> getServiceTypeRegistry() {
        return serviceTypeRegistry;
    }
}
