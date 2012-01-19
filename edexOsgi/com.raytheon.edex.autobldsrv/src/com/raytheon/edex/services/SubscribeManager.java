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
package com.raytheon.edex.services;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.subscription.runners.ISubscribeRunner;
import com.raytheon.edex.subscription.runners.SubscribeRunner;
import com.raytheon.edex.subscription.util.Tools;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.serialization.SerializationUtil;

/**
 * The main class of the Subscription Manager. Receives and processes subscription
 * requests from a client. Returns a response based on the result of processing the
 * request. 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 15Dec2008    1709       MW Fegan    Initial Creation. Replaces SubscribeSrv.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class SubscribeManager {
    private transient Log logger = LogFactory.getLog(getClass());
    /**
     * Constructor.
     */
    public SubscribeManager() {
        super();
    }
    /**
     * Processes the subscription request.
     * 
     * @param requestXML XML containing the request
     * 
     * @return the response to the request
     */
    public Message processRequest(String requestXML) {
        String msg = "";
        logger.debug("Processing request: " + requestXML);
        List<Property> results = null;
        try {
            Message xml = parseMessage(requestXML);
            String oper = xml.getHeader().getProperty("operation");
            ISubscribeRunner runner = SubscribeRunner.getInstance(oper);
            if (null == runner) {
                msg = "Unable to get subscription runner for " + oper;
                logger.warn(msg);
                results = new ArrayList<Property>();
                results.add(new Property("STDERR",msg));
            } else {
                runner.initialize(xml);
                runner.execute();
                results = runner.getResults();
            }
        } catch (Exception e) {
            msg = "Unable to process message. " + e.toString();
            logger.error(msg, e);
            results = new ArrayList<Property>();
            results.add(new Property("STDERR",msg));
        }
        Message xmlMsg = createMessage(results);
        return xmlMsg;
    }
    /**
     * Creates a Message object containing the results of the the execution.
     * 
     * @param result the result to convert to a message
     * 
     * @return message object containing the result
     */
    private final Message createMessage(List<Property> result) {
        Message msg = new Message();
        Header h = new Header();
        h.setProperties(result.toArray(new Property[] {} ));
  
        msg.setHeader(h);

        return msg;
    }
    /**
     * 
     * @param message
     * @return
     * @throws Exception
     */
    private final Message parseMessage(String message) throws Exception {
        Object m = SerializationUtil.unmarshalFromXml(message);
        if (m instanceof Message) {
            for (Property property : ((Message) m).getHeader().getProperties()) {
                if (!"script".equalsIgnoreCase(property.getName())) {
                    property.setValue(Tools.hexToAscii(property.getValue()));
                }
            }
            return (Message)m;
        }
        return null;
    }

}
