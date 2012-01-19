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

package com.raytheon.uf.common.localization.msgs;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;

/**
 * Utility class for converting and constructing localization messages
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2007            chammack    Initial Creation.
 * Aug 22, 2008 1448       chammack    Added Thrift Binary Support 	
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class UtilityMessageMarshaller {

    /**
     * Hide constructor for utility class
     */
    private UtilityMessageMarshaller() {

    }

    /**
     * Convert localization xml response to the value object
     * 
     * @param message
     *            the xml message
     * @return the value object
     * @throws EdexException
     */
    public static UtilityResponseMessage deserializeResponse(String message)
            throws LocalizationException {
        return (UtilityResponseMessage) deserialize(message,
                UtilityResponseMessage.class);
    }

    /**
     * Convert localization binary response to the value object
     * 
     * @param message
     *            the binary message
     * @return the value object
     * @throws EdexException
     */
    public static UtilityResponseMessage deserializeResponseBinary(
            byte[] message) throws LocalizationException {
        return (UtilityResponseMessage) deserializeBinary(message,
                UtilityResponseMessage.class);
    }

    /**
     * Convert localization xml request to the value object
     * 
     * @param message
     *            the xml message
     * @return the value object
     * @throws EdexException
     */
    public static UtilityRequestMessage deserializeRequest(String message)
            throws LocalizationException {
        return (UtilityRequestMessage) deserialize(message,
                UtilityRequestMessage.class);
    }

    /**
     * Convert localization binary message request to the value object
     * 
     * @param message
     *            the binary message
     * @return the value object
     * @throws EdexException
     */
    public static UtilityRequestMessage deserializeRequest(byte[] message)
            throws LocalizationException {
        return (UtilityRequestMessage) deserializeBinary(message,
                UtilityRequestMessage.class);
    }

    /**
     * Convert a request value object to XML
     * 
     * @param message
     *            the value object message
     * @return xml string
     * @throws EdexException
     */
    public static String serializeRequest(UtilityRequestMessage message)
            throws LocalizationException {
        return serialize(message, UtilityRequestMessage.class);
    }

    /**
     * Convert a request value object to Binary
     * 
     * @param message
     *            the value object message
     * @return binary message
     * @throws EdexException
     */
    public static byte[] serializeRequestBinary(UtilityRequestMessage message)
            throws LocalizationException {
        return serializeBinary(message, UtilityRequestMessage.class);
    }

    /**
     * Convert a response value object to XML
     * 
     * @param message
     *            the value object message
     * @return xml string
     * @throws EdexException
     */
    public static String serializeResponse(UtilityResponseMessage message)
            throws LocalizationException {
        return serialize(message, UtilityResponseMessage.class);
    }

    /**
     * Convert a response value object to binary
     * 
     * @param message
     *            the value object message
     * @return binary message
     * @throws EdexException
     */
    public static byte[] serializeResponseBinary(UtilityResponseMessage message)
            throws LocalizationException {
        return serializeBinary(message, UtilityResponseMessage.class);
    }

    private static Object deserialize(String message, Class<?> c)
            throws LocalizationException {
        try {
            return SerializationUtil.unmarshalFromXml(message);
        } catch (JAXBException e) {
            throw new LocalizationException("Unable to deserialize: ", e);
        }
    }

    private static String serialize(Object o, Class<?> c)
            throws LocalizationException {
        try {
            return SerializationUtil.marshalToXml(o);
        } catch (JAXBException e) {
            throw new LocalizationException("Unable to serialize: ", e);
        }
    }

    private static Object deserializeBinary(byte[] message, Class<?> c)
            throws LocalizationException {
        ByteArrayInputStream bais = null;
        try {
            DynamicSerializationManager mgr = DynamicSerializationManager
                    .getManager(SerializationType.Thrift);
            bais = new ByteArrayInputStream(message);

            return mgr.deserialize(bais);
        } catch (SerializationException e) {
            throw new LocalizationException("Unable to deserialize: ", e);
        } finally {
            try {
                if (bais != null)
                    bais.close();
            } catch (IOException e) {
                // ignore
            }
        }
    }

    private static byte[] serializeBinary(Object o, Class<?> c)
            throws LocalizationException {
        try {
            DynamicSerializationManager mgr = DynamicSerializationManager
                    .getManager(SerializationType.Thrift);
            return mgr.serialize(o);
        } catch (SerializationException e) {
            throw new LocalizationException("Unable to serialize: ", e);
        }
    }

}
