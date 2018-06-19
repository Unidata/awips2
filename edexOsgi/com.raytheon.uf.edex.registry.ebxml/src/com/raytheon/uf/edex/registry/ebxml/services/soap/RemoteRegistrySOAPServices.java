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

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.ws.wsaddressing.W3CEndpointReferenceBuilder;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Cataloger;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Validator;

import org.apache.cxf.endpoint.Client;
import org.apache.cxf.frontend.ClientProxy;
import org.apache.cxf.message.Message;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.ws.security.wss4j.WSS4JOutInterceptor;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.services.RegistryServiceConfiguration;
import com.raytheon.uf.common.registry.services.RegistryServiceException;
import com.raytheon.uf.edex.security.SecurityConfiguration;

/**
 * 
 * Convenience class used for accessing the registry soap services remotely.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jun 07, 2016  5589     tjensen   Initial creation
 * 
 * </pre>
 * 
 * @author tjensen
 * @version 1.0
 */
public class RemoteRegistrySOAPServices extends RegistrySOAPServices {

    protected WSS4JOutInterceptor securityInterceptor;

    protected RegistryServiceConfiguration serviceConfig;

    protected SecurityConfiguration securityConfig;

    @Override
    public NotificationListener getNotificationListenerServiceForUrl(
            final String url) throws RegistryServiceException {
        return createService(url, NotificationListener.class);
    }

    @Override
    public LifecycleManager getLifecycleManagerServiceForUrl(final String url) {
        return createService(url, LifecycleManager.class);
    }

    @Override
    public Cataloger getCatalogerServiceForUrl(final String url) {
        return createService(url, Cataloger.class);
    }

    @Override
    public QueryManager getQueryServiceForUrl(final String url) {
        return createService(url, QueryManager.class);
    }

    @Override
    public Validator getValidatorServiceForUrl(final String url)
            throws RegistryServiceException {
        return createService(url, Validator.class);
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T extends Object> T createService(String serviceUrl,
            Class<?> serviceInterface) throws RegistryServiceException {
        W3CEndpointReferenceBuilder endpointBuilder = new W3CEndpointReferenceBuilder();
        endpointBuilder.wsdlDocumentLocation(serviceUrl.toString() + WSDL);
        endpointBuilder.address(serviceUrl.toString());
        T port = (T) endpointBuilder.build().getPort(serviceInterface);
        Client client = ClientProxy.getClient(port);
        client.getOutInterceptors().add(this.securityInterceptor);
        HTTPConduit conduit = (HTTPConduit) client.getConduit();
        conduit.setClient(serviceConfig.getHttpClientPolicy());
        conduit.setTlsClientParameters(securityConfig.getTlsParams());
        conduit.setAuthorization(securityConfig.getAuthPolicy());

        // Create HTTP header containing the calling registry
        Map<String, List<String>> headers = new HashMap<>(2);
        headers.put(RegistryUtil.CALLING_REGISTRY_SOAP_HEADER_NAME,
                Arrays.asList(RegistryUtil.LOCAL_REGISTRY_ADDRESS));
        client.getRequestContext().put(Message.PROTOCOL_HEADERS, headers);

        return port;

    }

    public void setSecurityInterceptor(WSS4JOutInterceptor securityInterceptor) {
        this.securityInterceptor = securityInterceptor;
    }

    public void setServiceConfig(RegistryServiceConfiguration serviceConfig) {
        this.serviceConfig = serviceConfig;
    }

    public void setSecurityConfig(SecurityConfiguration securityConfig) {
        this.securityConfig = securityConfig;
    }
}
