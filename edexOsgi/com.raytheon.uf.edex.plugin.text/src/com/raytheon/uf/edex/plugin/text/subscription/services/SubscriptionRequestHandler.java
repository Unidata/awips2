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
package com.raytheon.uf.edex.plugin.text.subscription.services;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.text.request.SubscriptionRequest;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.plugin.text.subscription.runners.ISubscribeRunner;
import com.raytheon.uf.edex.plugin.text.subscription.runners.SubscribeRunner;

/**
 * Thrift handler for text subscription requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 5, 2014  2926      bclement     Initial creation, replaced SubscribeManager
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SubscriptionRequestHandler implements
        IRequestHandler<SubscriptionRequest> {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(SubscriptionRequestHandler.class);

    /**
     * 
     */
    public SubscriptionRequestHandler() {
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest(com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public Object handleRequest(SubscriptionRequest request) throws Exception {
        String msg = "";
        List<Property> results = null;
        try {
            Message requestMsg = request.getMessage();
            String oper = requestMsg.getHeader().getProperty("operation");
            ISubscribeRunner runner = SubscribeRunner.getInstance(oper);
            if (null == runner) {
                msg = "Unable to get subscription runner for " + oper;
                logger.warn(msg);
                results = new ArrayList<Property>();
                results.add(new Property("STDERR", msg));
            } else {
                runner.initialize(requestMsg);
                runner.execute();
                results = runner.getResults();
            }
        } catch (Exception e) {
            msg = "Unable to process message. " + e.toString();
            logger.error(msg, e);
            results = new ArrayList<Property>();
            results.add(new Property("STDERR", msg));
        }
        Message xmlMsg = createMessage(results);
        return xmlMsg;
    }

    /**
     * Creates a Message object containing the results of the the execution.
     * 
     * @param result
     *            the result to convert to a message
     * 
     * @return message object containing the result
     */
    private final Message createMessage(List<Property> result) {
        Message msg = new Message();
        Header h = new Header();
        h.setProperties(result.toArray(new Property[] {}));

        msg.setHeader(h);

        return msg;
    }

}
