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
package com.raytheon.uf.viz.core.notification;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import javax.jms.BytesMessage;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.TextMessage;

import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.common.util.DataUnzipper;

/**
 * Encapsulation object for notification messages
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 2, 2008  1448       chammack     Initial creation
 * Oct 4, 2010  7193       cjeanbap     Added a new method, isNotExpired().
 * Feb 1, 2011  7193       cjeanbap     Added a new method, getPublishedTime().
 * Aug 6, 2013  2228       njensen      Use deserialize(byte[])
 * Aug 16, 2013 2169       bkowal       Unzip any gzipped information
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class NotificationMessage {

    /** The "raw" jms message */
    private Message jmsMessage;

    /** An unmarshalled object cached for future use */
    private Object unmarshalledObject;

    /**
     * Construct a notification message from a JMS message
     * 
     * @param message
     *            the jms message
     */
    public NotificationMessage(Message message) {
        this.jmsMessage = message;
        if (!(message instanceof TextMessage)
                && !(message instanceof BytesMessage))
            throw new IllegalArgumentException("Unsupported message type: "
                    + message);
    }

    /**
     * Return the unmarshalled object
     * 
     * NOTE: This returns the common message object. Do NOT modify this object.
     * If you need to modify it, make a copy first.
     * 
     * @return the unmarshalled object
     */
    public Object getMessagePayload() throws NotificationException {

        if (this.unmarshalledObject == null) {
            synchronized (jmsMessage) {
                try {
                    // Support messages serialized in binary or XML format
                    if (this.jmsMessage instanceof BytesMessage) {
                        BytesMessage bytesMessage = (BytesMessage) this.jmsMessage;
                        bytesMessage.reset();
                        long length = bytesMessage.getBodyLength();
                        byte[] data = new byte[(int) length];

                        int readLength = bytesMessage.readBytes(data);
                        // QC the data length
                        if (length != readLength)
                            throw new NotificationException(
                                    "Message payload terminated early.  Expected: "
                                            + length + ".  Got: " + readLength);
                        if (DataUnzipper.isGzipped(data)) {
                            data = new DataUnzipper().gunzip(data);
                        }
                        this.unmarshalledObject = DynamicSerializationManager
                                .getManager(SerializationType.Thrift)
                                .deserialize(data);
                    } else if (this.jmsMessage instanceof TextMessage) {
                        TextMessage textMessage = (TextMessage) this.jmsMessage;
                        this.unmarshalledObject = SerializationUtil
                                .unmarshalFromXml(textMessage.getText());
                    }
                } catch (Exception e) {
                    throw new NotificationException(
                            "Error deserializing message payload", e);
                }
            }
        }
        return this.unmarshalledObject;
    }

    /**
     * Return a list of properties
     * 
     * @return a list of properties
     * @throws VizException
     */
    public List<String> getProperties() throws NotificationException {
        try {
            Enumeration<?> enumer = this.jmsMessage.getPropertyNames();

            List<String> retVal = new ArrayList<String>();
            while (enumer.hasMoreElements()) {
                retVal.add((String) enumer.nextElement());
            }

            return retVal;
        } catch (JMSException e) {
            throw new NotificationException("Error reading property names", e);
        }
    }

    /**
     * Get a property by name
     * 
     * @param propertyName
     *            the property name
     * @return the property value
     * @throws VizException
     */
    public Object getProperty(String propertyName) throws NotificationException {
        try {
            return this.jmsMessage.getObjectProperty(propertyName);
        } catch (JMSException e) {
            throw new NotificationException("Error reading property", e);
        }
    }

    /**
     * Return the source information (where the message came from)
     * 
     * @return the source information
     * @throws VizException
     */
    public String getSource() throws NotificationException {
        try {
            return this.jmsMessage.getJMSDestination().toString();
        } catch (JMSException e) {
            throw new NotificationException(
                    "Error retrieving source information", e);
        }
    }

    /**
     * Returns true, if the message has not expired set by the time-to-live
     * value on the Message object; otherwise false.
     * 
     * @return true, if the message has not expired.
     * @throws NotificationException
     */
    public boolean isNotExpired() throws NotificationException {
        try {
            long currentTime = System.currentTimeMillis();
            return (currentTime < this.jmsMessage.getJMSExpiration() ? true
                    : false);
        } catch (JMSException e) {
            throw new NotificationException(
                    "Error retrieving source information", e);
        }
    }

    /**
     * Returns the time as a long when the message was handled off to the
     * provider to be sent.
     * 
     * @return long, the time as in milliseconds.
     * @throws NotificationException
     */
    public long getPublishedTime() throws NotificationException {
        try {
            return this.jmsMessage.getJMSTimestamp();
        } catch (JMSException e) {
            throw new NotificationException(
                    "Error retrieving source information", e);
        }
    }
}
