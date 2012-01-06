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
package com.raytheon.edex.uengine.util;

import java.util.ArrayList;
import java.util.Calendar;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TextMessage;

import org.apache.activemq.command.ActiveMQQueue;

import com.raytheon.edex.util.Util;

/**
 * Contains utility methods that are specific to the Micro Engine. All
 * methods are {@code static}. All constants are {@code static final}.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 09Feb2007    TO5         MW Fegan    Initial Creation.
 * 02Oct2007    345         MW Fegan    Corrected correlation ID.
 * 16Oct2007    458         MW Fegan    Corrected logic in HandleTestMessage() to
 *                                       not attempt ACK when connection times out
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public class MEUtils {
    
    /**
     * Constructor.
     */
    public MEUtils() {
        //intentionally empty - nothing to initialize.
    }
    /**
     * Provides a simple interface for posting a message and awaiting
     * a reply. This overload sets up a non-durable queue that is 
     * non-transacted. 
     * <P>
     * Note that the message returned may be null.
     * 
     * @param text the text of the message to send
     * @param destinationQueue the queue to send the message to
     * @param responseQueue the queue to use for the response
     * @param timeOut time to wait for a response (msec)
     * @param correlationID correlation ID of the message, used to screen
     * @param clientID client ID for the session
     * @param url connection to the local JMS server
     * 
     * @return the return message from the queue
     * 
     * @throws JMSException if a JMS error occurred
     * @throws Exception for other errors
     */
    public static Message handleTextMessage(String  text,
                                            String  destinationQueue,
                                            String  responseQueue,
                                            long    timeOut,
                                            String  correlationID, 
                                            String  clientID, 
                                            String  url) 
    throws JMSException, Exception {
        return handleTextMessage(text,
                                 destinationQueue,
                                 responseQueue,
                                 timeOut,
                                 correlationID,
                                 clientID,
                                 url,
                                 false,
                                 false,
                                 false);
    }
    /**
     * Provides a simple interface for posting a message and awaiting
     * a reply. Creates and posts a JMS Text Message to the specified
     * JMS queue. Once the message has been dispatched, a connection 
     * to the return queue is established and the response is obtained.
     * <P>
     * Note that the message returned may be null.
     * 
     * @param text the text of the message to send
     * @param destinationQueue the queue to send the message to
     * @param responseQueue the queue to use for the response
     * @param timeOut time to wait for a response (msec)
     * @param correlationID correlation ID of the message, used to screen
     * @param clientID client ID for the session
     * @param url connection to the local JMS server
     * @param topic set true if the connection is a topic
     * @param transacted set true is the connection is transacted
     * @param durable set true if the connection is durable
     * 
     * @return the return message from the queue
     * 
     * @throws JMSException if a JMS error occurred
     * @throws Exception for other errors
     */
    public static Message handleTextMessage(String  text,
                                            String  destinationQueue,
                                            String  responseQueue,
                                            long    timeOut,
                                            String  correlationID, 
                                            String  clientID, 
                                            String  url,
                                            boolean topic,
                                            boolean transacted,
                                            boolean durable) 
    throws JMSException, Exception {
        /* create the JMS Support object */
        Connection connection = null;
        String selector =  "";
        Session session = null;
        Message response = null;
        JmsSupport jms = new JmsSupport(destinationQueue,
                                        topic,
                                        transacted,
                                        durable,
                                        clientID + "_producer");
        jms.setUrl(url);

        try {            
            /* create the JMS producer */
            connection = jms.createConnection();
            session = jms.createSession(connection);
            MessageProducer producer = jms.createProducer(session);
            
            /* create the message */
            TextMessage message = session.createTextMessage(text);
            
            /* sending the message */
            /* set the message correlation id */
            if (Util.isEmptyString(correlationID)) {
                correlationID = String.format("%1$tY%1$tm%1$td%1$tH%1$tM%1$tS", 
                                              Calendar.getInstance());
            }
            message.setJMSCorrelationID(correlationID);
            message.setStringProperty("MULE_CORRELATION_ID", correlationID);
            /* set the return queue */
            message.setJMSReplyTo(new ActiveMQQueue(responseQueue));
            
            /* send the message */
            producer.send(message);
            
            /* close the session */
            if (transacted) {
                session.commit();
            }
            jms.close(connection, session);
            
            /* receiving the return message */
            /* change JMS support for consumer */
            jms.setClientID(clientID + "_consumer");
            jms.setSubject(responseQueue);
            jms.setAckMode(Session.CLIENT_ACKNOWLEDGE);
            
            /* create the JMS consumer */
            selector = "JMSCorrelationID = '" + correlationID + "'";
            connection = jms.createConnection();
            session = jms.createSession(connection);
            MessageConsumer consumer = jms.createConsumer(session,selector);//jms.createConsumer(session);//jms.createConsumer(session,selector);
            
            /* fetch the message for the queue */
            response = consumer.receive(timeOut);
            if(response != null) {
                response.acknowledge();
            }
            
            return response;
        } finally {
            if (jms.isTransacted() && session != null) {
                session.commit();
            }
            jms.close(connection,session);
        }
    }
    /**
     * Converts the elements of an {@code ArrayList<String>} into a comma 
     * delineated string.
     * 
     * @param data contains the strings to convert
     * 
     * @return the comma delineated string.
     */
    public static String changeArrayListToString(ArrayList<Object> data) {
        StringBuffer retVal = new StringBuffer();
        boolean first = true;
        for (Object value : data) {
            if (!first) {
                retVal.append(",");
            }
            first = false;
            retVal.append(value.toString());
        }
        return retVal.toString();
    }
    /**
     * Filters the values in a byte array, setting to zero bytes less than the 
     * specified minimum or greater than the specified maximum.
     *  
     * @param bytes buffer of byte values to filter
     * @param low minimum value to retain
     * @param high maximum value to retain
     * 
     * @return the filtered byte buffer
     */
    public static byte[] byteHighLowFilterImage(byte[] bytes, int low, int high) {
        for (int i = 0; i < bytes.length; i++) {
            int temp = byteToInt(bytes[i]);
            if (temp < low) {
                bytes[i] = (byte)0;
            } else if (temp > high) {
                bytes[i] = (byte)0;
            }
        }
        return bytes;
    }
    /**
     * Safely converts a byte value to an integer.
     * 
     * @param val the byte to convert
     * 
     * @return the converted byte
     */
    private static int byteToInt(byte val) {
        return (val < 0)?255+val:val;
    }
}
