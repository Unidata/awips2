/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.soap;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Map;

import org.apache.cxf.common.util.StringUtils;
import org.apache.cxf.common.util.UrlUtils;
import org.apache.cxf.helpers.IOUtils;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageImpl;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.apache.cxf.transport.Conduit;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Provides custom WSDL for CXF endpoints
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2013            bclement     Initial creation
 * 5/2/2014     #3192      bhillip      Minor fix for CXF upgrade to 2.7.10
 * May 23, 2014 3199       bclement     moved to edex.soap from edex.ogc.common
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class CustomWsdlInterceptor extends AbstractPhaseInterceptor<Message> {

    private String wsdl;
    
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CustomWsdlInterceptor.class);

    /**
     * @param phase
     * @throws IOException
     */
    public CustomWsdlInterceptor(String wsdlLocation) throws IOException {
        super(Phase.READ);
        ClassLoader loader = this.getClass().getClassLoader();
        InputStream in = loader.getResourceAsStream(wsdlLocation);
        this.wsdl = IOUtils.readStringFromStream(in);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.cxf.interceptor.Interceptor#handleMessage(org.apache.cxf.message
     * .Message)
     */
    @Override
    public void handleMessage(Message msg) throws Fault {
        String method = (String) msg.get(Message.HTTP_REQUEST_METHOD);
        String query = (String) msg.get(Message.QUERY_STRING);
        if (!"GET".equals(method) || StringUtils.isEmpty(query)) {
            return;
        }
        String baseUrl = (String) msg.get(Message.REQUEST_URL);
        String host = getHostName(baseUrl);

        synchronized (msg.getExchange().getEndpoint()) {
            Map<String, String> args = UrlUtils.parseQueryString(query);
            if (args.containsKey("wsdl")) {
                
                OutputStream os = null;
                
                try {

                    Conduit c = msg.getExchange().getDestination()
                            .getBackChannel(msg, null, null);

                    Message mout = new MessageImpl();
                    mout.setExchange(msg.getExchange());
                    msg.getExchange().setOutMessage(mout);
                    mout.put(Message.CONTENT_TYPE, "text/xml");
                    c.prepare(mout);
                    os = mout.getContent(OutputStream.class);
                    OutputStreamWriter writer = new OutputStreamWriter(os);
                    writer.write(process(host));
                    writer.close();
                    msg.getInterceptorChain().abort();
                } catch (IOException e) {
                    throw new Fault(e);
                } finally {
                    if (os != null) {
                        try {
                            os.close();
                        } catch (Exception e) {
                            statusHandler.error("Unable to close CXF stream.", e);
                        }
                    }
                    msg.getExchange().setOutMessage(null);
                }
            }
        }
    }

    private String getHostName(String url) throws Fault {
        try {
            return new URL(url).getHost();
        } catch (MalformedURLException e) {
            throw new Fault(e);
        }
    }

    private String process(String host) {
        // TODO more robust processing
        return wsdl.replaceAll("0\\.0\\.0\\.0", host);
    }

}
