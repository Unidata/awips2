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

package com.raytheon.uf.viz.core.comm;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;

import com.raytheon.uf.common.message.CatalogAttribute;
import com.raytheon.uf.common.message.CatalogItem;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.exception.VizServerSideException;

/**
 * Handle JiBX and message marshalling for EDEX Messaging
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date            Ticket#     Engineer      Description
 *    ------------    ----------  -----------   --------------------------
 *    Sep 11, 2006                chammack      Initial Creation.
 *    09May2007       TO7         njensen       Fixed XML responses.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class MessageMarshaller {

    /**
     * Decode a message into the Message object
     * 
     * @param monitor
     *            optional progress monitor (can set to null)
     * @param message
     *            the message to decode
     * @return the decoded message
     * @throws VizException
     * @throws SerializationException
     */
    public static Message decodeServiceMessage(IProgressMonitor monitor,
            byte[] message, String correlationID) throws VizException,
            SerializationException {
        if (message == null) {
            throw new VizException("Got null response from server");
        } else {
            Message servMsg = null;

            if (monitor != null) {
                monitor.subTask("Decoding response");
            }

            DynamicSerializationManager dsm = DynamicSerializationManager
                    .getManager(SerializationType.Thrift);

            Object rawMsg = dsm.deserialize(new ByteArrayInputStream(message));

            if (!(rawMsg instanceof Message))
                throw new VizException(
                        "Could not parse server result.  Expected type "
                                + Message.class.getName() + ".  Got: " + rawMsg);
            servMsg = (Message) rawMsg;

            if (monitor != null) {
                monitor.worked(1);
            }

            servMsg.setCorrelationID(correlationID);

            return servMsg;

        }
    }

    /**
     * Decode a message into the Message object
     * 
     * @param monitor
     *            optional progress monitor (can set to null)
     * @param message
     *            the message to decode
     * @return the decoded message
     * @throws VizException
     * @throws SerializationException
     */
    public static Message decodeServiceMessageStreaming(
            IProgressMonitor monitor, InputStream is, String correlationID)
            throws VizException, SerializationException {
        if (is == null) {
            throw new VizException("Got null response from server");
        } else {
            Message servMsg = null;

            if (monitor != null) {
                monitor.subTask("Decoding response");
            }

            DynamicSerializationManager dsm = DynamicSerializationManager
                    .getManager(SerializationType.Thrift);

            Object rawMsg = dsm.deserialize(is);

            if (!(rawMsg instanceof Message))
                throw new VizException(
                        "Could not parse server result.  Expected type "
                                + Message.class.getName() + ".  Got: " + rawMsg);
            servMsg = (Message) rawMsg;

            if (monitor != null) {
                monitor.worked(1);
            }

            servMsg.setCorrelationID(correlationID);

            return servMsg;

        }
    }

    /**
     * Localize a response (bring inline responses into uri space, etc)
     * 
     * @param message
     *            the message
     * @return the response uri message
     * @throws VizException
     */
    public static Object[] localizeResponse(Message message)
            throws VizException {
        if (message == null)
            return null;

        List<Object> responseList = new ArrayList<Object>();

        AbstractResponseMessage[] absresponses = message.getBody()
                .getResponses();

        for (AbstractResponseMessage response : absresponses) {

            if (response instanceof ResponseMessageError) {
                ResponseMessageError rme = (ResponseMessageError) response;
                throw reconstituteException(rme);
            } else if (response instanceof ResponseMessageGeneric) {
                Object dataRecord = ((ResponseMessageGeneric) response)
                        .getContents();
                responseList.add(dataRecord);
            } else if (response instanceof ResponseMessageCatalog) {
                ResponseMessageCatalog catalogMsg = (ResponseMessageCatalog) response;
                String[] vals = catalogMsg.getValues();
                if (vals != null && vals.length > 0) {
                    responseList.add(vals);
                } else {

                    CatalogItem[] item = catalogMsg.getItems();
                    List<String> valsList = new ArrayList<String>();
                    if (item != null && item.length > 0) {
                        for (CatalogItem i : item) {
                            if (i != null) {
                                CatalogAttribute[] attrib = i.getAttributes();
                                if (attrib != null) {
                                    for (CatalogAttribute a : attrib) {
                                        if (a.getValue() != null) {
                                            valsList.add(a.getValue());
                                        }
                                    }
                                }
                            }
                        }
                    }
                    responseList.add(valsList.toArray(new String[valsList
                            .size()]));
                }
            } else {
                throw new VizException("Recieved unknown message type: "
                        + response);
            }
        }

        return responseList.toArray(new Object[responseList.size()]);
    }

    private static VizServerSideException reconstituteException(
            ResponseMessageError rme) {
        StringBuffer sb = new StringBuffer();
        sb.append("Server-side exception: ");
        sb.append(rme.getErrorCause());
        sb.append(":: ");
        sb.append(rme.getErrorCause());
        sb.append("\n");
        if (rme.getErrorChain() != null) {
            for (String msg : rme.getErrorChain()) {
                sb.append(msg);
                sb.append("\n");
            }
        }
        String x = sb.toString();
        return new VizServerSideException(x);
    }

}
