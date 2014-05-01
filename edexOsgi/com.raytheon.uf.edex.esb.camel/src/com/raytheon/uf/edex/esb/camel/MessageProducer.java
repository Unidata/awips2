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
package com.raytheon.uf.edex.esb.camel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.camel.CamelContext;
import org.apache.camel.CamelExecutionException;
import org.apache.camel.ExchangePattern;
import org.apache.camel.ProducerTemplate;
import org.apache.camel.Route;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import com.raytheon.uf.common.message.IMessage;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.IMessageProducer;

/**
 * Sends message to endpoints programmatically.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2008            njensen     Initial creation
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class MessageProducer implements ApplicationContextAware,
        IMessageProducer {

    private static ApplicationContext springContext;

    private static List<CamelContext> camelContextList;

    private static Map<String, String> endpointIdUriMap = new HashMap<String, String>();

    private static Map<String, CamelContext> endpointContextMap = new HashMap<String, CamelContext>();

    private static Map<CamelContext, ProducerTemplate> contextProducerMap = new HashMap<CamelContext, ProducerTemplate>();

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.esb.camel.IMessageProducer#sendAsync(java.lang.String
     * , java.lang.Object)
     */
    public void sendAsync(String endpoint, Object message) throws EdexException {
        CamelContext camelContext = getCamelContext(endpoint);
        ProducerTemplate template = getProducerTemplate(camelContext);
        String uri = endpointIdUriMap.get(endpoint);
        Map<String, Object> headers = getHeaders(message);
        try {
            if (headers != null) {
                template.sendBodyAndHeaders(uri, ExchangePattern.InOnly,
                        message, headers);
            } else {
                template.sendBody(uri, ExchangePattern.InOnly, message);
            }
        } catch (CamelExecutionException e) {
            throw new EdexException("Error sending asynchronous message: "
                    + message + " to endpoint: " + uri, e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.esb.camel.IMessageProducer#sendSync(java.lang.String
     * , java.lang.Object)
     */
    public Object sendSync(String endpoint, Object message)
            throws EdexException {
        CamelContext camelContext = getCamelContext(endpoint);
        ProducerTemplate template = getProducerTemplate(camelContext);
        String uri = endpointIdUriMap.get(endpoint);
        Map<String, Object> headers = getHeaders(message);
        try {
            if (headers != null) {
                return template.sendBodyAndHeaders(uri, ExchangePattern.OutIn,
                        message, headers);
            } else {
                return template.sendBody(uri, ExchangePattern.OutIn, message);
            }
        } catch (CamelExecutionException e) {
            throw new EdexException("Error sending synchronous message: "
                    + message + " to uri: " + uri, e);
        }
    }

    private synchronized CamelContext getCamelContext(String endpointId)
            throws EdexException {
        CamelContext ctx = endpointContextMap.get(endpointId);
        if (ctx == null) {
            List<CamelContext> list = getCamelContextList();
            boolean found = false;
            for (CamelContext c : list) {
                List<Route> routes = c.getRoutes();
                for (Route r : routes) {
                    if (r.getProperties() != null
                            && endpointId.equals(r.getProperties().get(
                                    Route.ID_PROPERTY))) {
                        ctx = c;
                        endpointContextMap.put(endpointId, ctx);
                        endpointIdUriMap.put(endpointId, r.getEndpoint()
                                .getEndpointUri());
                        found = true;
                        break;
                    }
                }
                if (found) {
                    break;
                }
            }
            if (ctx == null) {
                throw new EdexException("Route id " + endpointId
                        + " not found.  Check loaded spring configurations.");
            }
        }

        return ctx;
    }

    private ProducerTemplate getProducerTemplate(CamelContext ctx) {
        ProducerTemplate tmp = contextProducerMap.get(ctx);
        if (tmp == null) {
            tmp = ctx.createProducerTemplate();
            contextProducerMap.put(ctx, tmp);
        }

        return tmp;
    }

    private synchronized List<CamelContext> getCamelContextList() {
        if (springContext == null) {
            springContext = EDEXUtil.getSpringContext();
        }

        if (springContext == null) {

            throw new IllegalStateException(
                    "Spring context has not been initialized on "
                            + MessageProducer.class.getName());
        }

        if (camelContextList == null) {
            camelContextList = new ArrayList<CamelContext>();
            String[] camelContexts = springContext
                    .getBeanNamesForType(CamelContext.class);
            for (String name : camelContexts) {
                CamelContext c = (CamelContext) springContext.getBean(name);
                camelContextList.add(c);
            }
        }
        return camelContextList;
    }

    @Override
    public void setApplicationContext(ApplicationContext context)
            throws BeansException {
        springContext = context;
    }

    @Override
    public void sendAsyncUri(String uri, Object message) throws EdexException {
        CamelContext ctx = getCamelContextList().get(0);
        ProducerTemplate template = getProducerTemplate(ctx);
        Map<String, Object> headers = getHeaders(message);
        try {
            if (headers != null) {
                template.sendBodyAndHeaders(uri, ExchangePattern.InOnly,
                        message, headers);
            } else {
                template.sendBody(uri, ExchangePattern.InOnly, message);
            }
        } catch (CamelExecutionException e) {
            throw new EdexException("Error sending asynchronous message: " + message
                    + " to uri: " + uri, e);
        }
    }

    private Map<String, Object> getHeaders(Object message) {
        Map<String, Object> headers = null;
        if (message instanceof IMessage) {
            headers = new HashMap<String, Object>();
            headers.put("JMSType", message.getClass().getName());
            headers.putAll(((IMessage) message).getHeaders());
        } else if (message instanceof List) {
            List<?> list = ((List<?>) message);
            if (list.size() > 0) {
                if (list.get(0) instanceof IMessage) {
                    headers = ((IMessage) list.get(0)).getHeaders();
                }
            }
        }
        return headers;
    }

}
