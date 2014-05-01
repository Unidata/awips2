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

package com.raytheon.edex.uengine.web;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URI;
import java.util.HashMap;
import java.util.Set;

import javax.servlet.ServletException;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager;
import org.apache.http.util.EntityUtils;
import org.w3c.dom.Document;

import com.raytheon.edex.msg.ResponseMessageURI;
import com.raytheon.edex.util.Util;
import com.raytheon.edex.util.XMLUtils;
import com.raytheon.uf.common.message.Body;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Interfaces with the ESB to get catalog and data information.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    10/30/06                  brockwoo    Initial Creation.
 *    07/26/07                  njensen     Added requestColormaps().
 *    8/20/08                   njensen     Converted to JAXB
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1
 */
public class RequestTestDriver {

    private static final Log theLogger = LogFactory
            .getLog(RequestTestDriver.class);

    private static final String ERROR = "ERROR";

    private static final String REQUEST = "Request";

    private static final String RESPONSE = "Response";

    private static final String PARSEERROR = "Service response could not be parsed, no output.";

    private static HttpClient client;

    private HashMap<String, String> responseStr;

    public RequestTestDriver() {
        responseStr = new HashMap<String, String>();
    }

    public HashMap<String, String> requestProduct(String actionXML,
            long timeout, boolean javascript) {

        if (actionXML != null) {
            responseStr.put(REQUEST, actionXML);
            // run action, return output

            String response = sendAndReceive(actionXML);

            if (responseStr.containsKey(ERROR)) {
                return responseStr;
            }
            responseStr.put(RESPONSE, response);
        }
        if (responseStr.get(RESPONSE).indexOf("responseMessageURI") != -1) {
            handleServiceResponse();
        } else if (responseStr.get(RESPONSE).indexOf("responseError") != -1) {
            responseStr.put("TYPE", "ERROR");
        } else if (responseStr.get(RESPONSE).indexOf("responseMessageGeneric") != -1) {
            handleAsciiResponse();
        }

        return responseStr;
    }

    public String[] requestCatalog(String parameter,
            HashMap<String, String> filter, long timeout) {
        String actionXML = assembleCatalogQuery(parameter, filter);
        String response = null;
        ResponseMessageCatalog servMsg = null;
        if (actionXML != null) {
            try {
                response = requestHTTP(actionXML);
            } catch (EdexException e) {
                e.printStackTrace();
            }
        }
        if (response == null)
            return null;
        Message msg = null;
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(Message.class,
                    ResponseMessageCatalog.class);
            Unmarshaller msh = jaxbContext.createUnmarshaller();
            StringReader reader = new StringReader(response);
            msg = (Message) msh.unmarshal(reader);
        } catch (Exception e) {
            theLogger.error("Error extracting catalog from message: ", e);
        }
        if (msg == null) {
            return null;
        }
        try {
            servMsg = (ResponseMessageCatalog) msg.getBody().getResponses()[0];
        } catch (Throwable e) {
            theLogger.error("Error extracting catalog from message: ", e);
        }
        return servMsg.getValues();
    }

    public String[] requestHibStats() {
        StringBuffer actionXML = new StringBuffer();
        actionXML.append("include(\"RetrieveHibStats.js\");\n");
        actionXML.append("var query = new HibernateStats();\n");
        actionXML.append("query.execute();");

        String response = null;
        ResponseMessageCatalog servMsg = null;
        if (actionXML != null) {
            response = sendAndReceive(actionXML.toString());
        }
        try {
            response = extractResponseFromMessage(response, "responseCatalog");
        } catch (Exception e) {
            theLogger.info("Error extracting catalog from message: "
                    + e.getMessage());
        }
        if (response == null) {
            return null;
        }
        // try {
        // IBindingFactory bfact = BindingDirectory
        // .getFactory(ResponseMessageCatalog.class);
        // IUnmarshallingContext uctx = bfact.createUnmarshallingContext();
        // servMsg = (ResponseMessageCatalog) uctx
        // .unmarshalDocument(new StringReader(response));
        // } catch (JiBXException e) {
        // e.printStackTrace();
        // } catch (Throwable e) {
        // e.printStackTrace();
        // }
        return servMsg.getValues();

    }

    public String[] requestColormaps(long timeout) {
        StringBuffer buf = new StringBuffer();
        buf.append("from com.raytheon.edex.uengine.tasks.query import ColormapQuery\n");
        buf.append("colormapQuery = ColormapQuery()\n");
        buf.append("return colormapQuery.execute()\n");
        String response = null;
        String action = buf.toString();
        ResponseMessageCatalog servMsg = null;
        if (action != null) {
            response = sendAndReceive(action);
        }
        Message msg = null;
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance(Message.class,
                    ResponseMessageCatalog.class);
            Unmarshaller msh = jaxbContext.createUnmarshaller();
            StringReader reader = new StringReader(response);
            msg = (Message) msh.unmarshal(reader);
        } catch (Exception e) {
            theLogger.info("Error extracting catalog from message: "
                    + e.getMessage());
            e.printStackTrace();
        }
        if (msg == null) {
            return null;
        }
        try {
            servMsg = (ResponseMessageCatalog) msg.getBody().getResponses()[0];
        } catch (Throwable e) {
            e.printStackTrace();
        }
        return servMsg.getValues();
    }

    public boolean purgeDataStore() {
        throw new UnsupportedOperationException("Not yet implemented");
        // try {
        // Connection connection = jms.createConnection();
        // Session session = jms.createSession(connection);
        // MessageProducer producer = jms.createProducer(session);
        // ObjectMessage purgeMessage = session
        // .createObjectMessage(PurgeSrv.DELETE_ALL_DATA);
        // producer.send(purgeMessage);
        // } catch (JMSException e) {
        // responseStr.put(ERROR, e.getMessage());
        // return false;
        // } catch (Exception e) {
        // responseStr.put(ERROR, e.getMessage());
        // return false;
        // }

        // return true;
    }

    public static String requestHTTP(String message) throws EdexException {
        if (client == null)
            client = new org.apache.http.impl.client.DefaultHttpClient(
                    new ThreadSafeClientConnManager());

        try {
            HttpPost put = new HttpPost(
                    "http://localhost:9581/services/pyproductjaxb");
            put.setEntity(new StringEntity(message, "text/xml", "ISO-8859-1"));

            HttpResponse resp = client.execute(put);
            int code = resp.getStatusLine().getStatusCode();

            if (code != 200) {
                throw new EdexException(
                        "Error reading server response.  Got error message: "
                                + EntityUtils.toString(resp.getEntity()));
            }

            ByteArrayOutputStream baos = null;
            InputStream is = null;
            try {
                is = resp.getEntity().getContent();
                baos = new ByteArrayOutputStream();
                int read = 0;
                do {
                    byte[] tmp = new byte[1024];
                    read = is.read(tmp);

                    if (read > 0)
                        baos.write(tmp, 0, read);
                } while (read > 0);

                return new String(baos.toByteArray());
            } finally {
                // It seems we do not need to do this with 4.1 closing the
                // input stream from the entity ( 'is' at the time of
                // writing ) should allow the connection te be released

                // if (put != null) {
                // put.releaseConnection();
                // }

                try {
                    if (resp != null && resp.getEntity() != null) {
                        EntityUtils.consume(resp.getEntity());
                    }
                } catch (IOException e) {
                    // if there was an error reading the input stream,
                    // notify but continue
                    theLogger.info(
                            "Error reading InputStream, assuming closed.", e);
                }

                try {
                    if (baos != null)
                        baos.close();
                } catch (IOException e1) {
                    // ignore
                }
                try {
                    if (is != null)
                        is.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        } catch (Exception e) {
            throw new EdexException("Unable to connect to server", e);
        }

    }

    private String sendAndReceive(String actionScript) {
        try {
            return requestHTTP(actionScript);
        } catch (Exception e) {
            responseStr.put(ERROR, e.getMessage());
        }

        return null;
    }

    /**
     * Handles a URI product response.
     * 
     * @param response
     *            the servlet response object
     * @param responseStr
     *            the EDEX response from JMS
     * 
     * @throws ServletException
     *             if a error occurs extracting information from the EDEX
     *             response
     * @throws IOException
     *             if an IO error occurs
     */
    private void handleServiceResponse() {
        responseStr.put("TYPE", "URI");
        String fullResponse = responseStr.get(RESPONSE);
        AbstractResponseMessage[] responses = null;
        ResponseMessageURI servMsg = null;
        try {
            responses = this.extractAllResponses(fullResponse, "responseURI");
        } catch (Exception e) {
            theLogger.error("Error extracting responses", e);
            e.printStackTrace();
        }

        int counter = 0;
        for (AbstractResponseMessage response : responses) {
            servMsg = (ResponseMessageURI) response;
            if (servMsg != null) {
                theLogger.info("processing URI response "
                        + servMsg.getDataURI() + ":" + servMsg.getValidTime()
                        + ":" + servMsg.getProductURI().length);
                URI[] uriArray = servMsg.getProductURI();
                responseStr.put("URI" + counter,
                        Util.getFileNameFromPath(uriArray[0].getPath()));
                responseStr.put("TYPE" + counter, servMsg.getFileType());
                responseStr.put("DATE" + counter, servMsg.getValidTime()
                        .toString());
            } else {
                responseStr.put(ERROR, PARSEERROR);
            }
            counter++;
        }
        responseStr.put("URICOUNT", Integer.toString(counter));

    }

    /**
     * Handles a ASCII product response.
     * 
     * @param response
     *            the servlet response object
     * @param responseStr
     *            the EDEX response from JMS
     * 
     * @throws ServletException
     *             if a error occurs extracting information from the EDEX
     *             response
     * @throws IOException
     *             if an IO error occurs
     */
    private void handleAsciiResponse() {
        responseStr.put("TYPE", "XML");
        String fullResponse = responseStr.get(RESPONSE);
        AbstractResponseMessage[] responses = null;
        try {
            responses = this.extractAllResponses(fullResponse, "contents");
        } catch (Exception e) {
            theLogger.error("Error extracting responses", e);
        }

        int counter = 0;
        JAXBContext jaxbContext;
        try {
            jaxbContext = JAXBContext.newInstance(Message.class,
                    ResponseMessageGeneric.class, ResponseMessageError.class);
            for (AbstractResponseMessage response : responses) {
                Marshaller msh = jaxbContext.createMarshaller();
                StringWriter writer = new StringWriter();
                msh.marshal(response, writer);
                String msg = writer.toString();
                responseStr.put("MESSAGE" + counter, msg);
                counter++;
            }
        } catch (JAXBException e) {
            theLogger.error("Error separating responses", e);
        }
        responseStr.put("XMLCOUNT", Integer.toString(counter));
    }

    private String assembleCatalogQuery(String fieldName,
            HashMap<String, String> queryTerms) {
        String pluginName = queryTerms.remove("plugin");
        StringBuffer query = new StringBuffer();
        // query.append("include(\"CatalogQuery.js\");\n");
        // query.append("var query = new CatalogQuery(\"" + pluginName +
        // "\");\n");
        // query.append("query.setDistinctField(\"" + fieldName + "\");\n");
        // Set<String> names = queryTerms.keySet();
        // for (String name : names) {
        // query.append("query.addConstraint(\"" + name + "\",\""
        // + queryTerms.get(name) + "\");\n");
        // }
        // query.append("query.execute();");
        query.append("import CatalogQuery\n");
        query.append("query = CatalogQuery.CatalogQuery(\"" + pluginName
                + "\")\n");
        query.append("query.setDistinctField(\"" + fieldName + "\")\n");
        Set<String> names = queryTerms.keySet();
        for (String name : names) {
            query.append("query.addConstraint(\"" + name + "\",\""
                    + queryTerms.get(name) + "\")\n");
        }
        query.append("return query.execute()");
        return query.toString();
    }

    /**
     * Extracts the response from the body of the message. The message is in
     * canonical message format.
     * 
     * @param message
     *            the canonical message
     * @param responseName
     *            the name of the response to extract
     * 
     * @return the requested response
     * 
     * @throws Exception
     *             if an error occurs
     */
    private String extractResponseFromMessage(String message,
            String responseName) {
        Document document;
        Document responseMsg;
        String parsedResponse = null;
        try {
            document = XMLUtils.scanXMLtoDOM(message);
            if (document == null) {
                return null;
            }
            responseMsg = XMLUtils.getSubDocument(document, responseName);
            parsedResponse = XMLUtils.transformXMLDocument(responseMsg);
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return parsedResponse;
    }

    /**
     * Extracts all response messages from the body of the message. The message
     * is in canonical message format.
     * 
     * @param message
     *            the canonical message
     * @param responseName
     *            the response to extract
     * 
     * @return array of responses as strings
     * 
     * @throws Exception
     *             if an error occurs.
     */
    private AbstractResponseMessage[] extractAllResponses(String message,
            String responseName) throws Exception {
        JAXBContext jaxbContext = JAXBContext.newInstance(Message.class,
                Body.class, Header.class, ResponseMessageCatalog.class,
                ResponseMessageURI.class, ResponseMessageGeneric.class,
                ResponseMessageError.class);
        Unmarshaller msh = jaxbContext.createUnmarshaller();
        StringReader reader = new StringReader(message);
        Message msg = (Message) msh.unmarshal(reader);
        return msg.getBody().getResponses();
    }
}
