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
package com.raytheon.uf.edex.registry.ebxml.services.soap;

import org.apache.cxf.jaxws.JaxWsProxyFactoryBean;

import com.raytheon.uf.common.registry.services.RegistryServiceException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Cataloger;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Validator;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

/**
 *
 * Convenience class used for accessing the registry soap services
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 09, 2013  1802     bphillip  Initial implementation
 * Apr 24, 2013  1910     djohnson  RegistryResponseStatus is now an enum.
 * Aug 28, 2013  1538     bphillip  Removed caches, add http client preferences
 * Sep 05, 2013  1538     bphillip  Add HTTP header information
 * Oct 30, 2013  1538     bphillip  Made methods in this class non-static
 * Nov 20, 2013  2534     bphillip  Eliminated service caching
 * Jan 15, 2014  2613     bphillip  Eliminated service caching...again
 * Feb 19, 2014  2769     bphillip  Renamed getPort method
 * Jun 05, 2014  1712     bphillip  Moved configuration out to separate class.
 *                                  Added outbound interceptor
 * Jul 10, 2014  1717     bphillip  Added authorization policy
 * May 11, 2015  4448     bphillip  Separated EBXML Registry from Data Delivery
 * Jun 07, 2016  5589     tjensen   Remove static from createService so it can
 *                                  be overridden with a non-static
 * May 26, 2020  8165     bsteffen  Remove notification.
 * Jul 12, 2022  8891     tjensen   Change getLifecycleManagerServiceForURL to 
 *                                  pass in interface instead of Impl
 * Aug 02, 2022  8903     sharbison Change getLifecycleManagerServiceForURL to
 *                                  pass in interface instead of Impl
 *
 * </pre>
 *
 * @author bphillip
 */
public class RegistrySOAPServices {

    /** The logger */
    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistrySOAPServices.class);

    /** Path separator */
    protected static final String PATH_SEPARATOR = "/";

    /** WSDL suffix */
    protected static final String WSDL = "?wsdl";

    /** The name of the lifecycle manager service */
    protected static final String LIFECYCLE_MANAGER_SERVICE_NAME = "lifecycleManager";

    /** The name of the cataloger service */
    protected static final String CATALOGER_SERVICE_NAME = "cataloger";

    /** The name of the query service */
    protected static final String QUERY_SERVICE_NAME = "queryManager";

    /** The name of the validator service */
    protected static final String VALIDATOR_SERVICE_NAME = "validator";

    /**
     * Gets the lifecycle manager service for the given host
     *
     * @param host
     *            The host to get the lifecycle manager service for
     * @return The lifecycle manager service for the given host
     */
    public LifecycleManager getLifecycleManagerServiceForHost(
            final String host) {
        return getLifecycleManagerServiceForUrl(
                host + PATH_SEPARATOR + LIFECYCLE_MANAGER_SERVICE_NAME);
    }

    /**
     * Gets the lifecycle manager service for the given URL string
     *
     * @param url
     *            The service URL
     * @return The lifecycle manager service at the given URL string
     */
    public LifecycleManager getLifecycleManagerServiceForUrl(final String url) {
        return createService(url, LifecycleManager.class);
    }

    /**
     * Gets the cataloger service for the given host
     *
     * @param host
     *            The host to get the cataloger service for
     * @return The cataloger service at the given host
     */
    public Cataloger getCatalogerServiceForHost(final String host) {
        return getCatalogerServiceForUrl(
                host + PATH_SEPARATOR + CATALOGER_SERVICE_NAME);
    }

    /**
     * Gets the cataloger service for the given url string
     *
     * @param url
     *            the url string
     * @return The cataloger service
     */
    public Cataloger getCatalogerServiceForUrl(final String url) {
        return createService(url, Cataloger.class);
    }

    /**
     * Gets the query manager service at the given host
     *
     * @param host
     *            The host name
     * @return The query manager service
     */
    public QueryManager getQueryServiceForHost(final String host) {
        return getQueryServiceForUrl(
                host + PATH_SEPARATOR + QUERY_SERVICE_NAME);
    }

    /**
     * Gets the query manager service at the given url string
     *
     * @param serviceUrl
     *            The url string
     * @return The query manager service at the given url string
     */
    public QueryManager getQueryServiceForUrl(final String url) {
        return createService(url, QueryManager.class);
    }

    /**
     * Gets the validator service for the given host
     *
     * @param host
     *            The host
     * @return The validator service for the given host
     * @throws RegistryServiceException
     *             If errors occur creating the URL object
     */
    public Validator getValidatorServiceForHost(final String host) {
        return getValidatorServiceForUrl(
                host + PATH_SEPARATOR + VALIDATOR_SERVICE_NAME);
    }

    /**
     * Gets the validator service for the given url string
     *
     * @param serviceUrl
     *            The url string
     * @return The validator service for the given url string
     */
    public Validator getValidatorServiceForUrl(final String url)
            throws RegistryServiceException {
        return createService(url, Validator.class);
    }

    /**
     *
     * Sends a SubmitObjects request to the lifecycle manager service at the
     * given host
     *
     * @param request
     *            The request
     * @param host
     *            The host to send the request to
     * @throws RegistryServiceException
     *             If errors occur during request submission
     */
    public void sendSubmitObjectsRequest(SubmitObjectsRequest request,
            String host) throws RegistryServiceException {

        LifecycleManager lcm;
        try {
            lcm = getLifecycleManagerServiceForHost(host);
        } catch (RegistryServiceException e) {
            throw new RegistryServiceException(
                    "Error getting lifecyclemanager for host at [" + host + "]",
                    e);
        }
        RegistryResponseType response;
        try {
            response = lcm.submitObjects(request);
        } catch (MsgRegistryException e) {
            throw new RegistryServiceException("Error executing submitObjects!",
                    e);
        }
        RegistryResponseStatus status = response.getStatus();
        if (status.equals(RegistryResponseStatus.SUCCESS)) {
            statusHandler.info("Submit Objects request ["
                    + response.getRequestId() + "] successful");
        } else if (status.equals(RegistryResponseStatus.PARTIAL_SUCCESS)) {
            statusHandler.warn("Submit Objects request ["
                    + response.getRequestId() + "] partially successful");
        } else if (status.equals(RegistryResponseStatus.FAILURE)) {
            statusHandler.error("Submit Objects request ["
                    + response.getRequestId() + "] failed!");
            StringBuilder exceptionMessage = new StringBuilder();
            for (RegistryExceptionType exc : response.getException()) {
                exceptionMessage.append(exc).append("\n");
            }
            throw new RegistryServiceException(
                    "Submit Objects Request [" + response.getRequestId()
                            + "] failed with the following exceptions:"
                            + exceptionMessage.toString());
        }
    }

    /**
     * Creates a SOAP service proxy object
     *
     * @param serviceUrl
     *            The url hosting the service
     * @param serviceInterface
     *            The service interface
     * @return A proxy object encapsulating the service
     */
    @SuppressWarnings("unchecked")
    public <T extends Object> T createService(final String serviceUrl,
            final Class<?> serviceInterface) {
        JaxWsProxyFactoryBean proxyFactory = new JaxWsProxyFactoryBean();
        proxyFactory.setWsdlURL(serviceUrl + WSDL);
        proxyFactory.setAddress(serviceUrl);
        proxyFactory.setServiceClass(serviceInterface);
        T service = (T) proxyFactory.create();
        return service;
    }
}
