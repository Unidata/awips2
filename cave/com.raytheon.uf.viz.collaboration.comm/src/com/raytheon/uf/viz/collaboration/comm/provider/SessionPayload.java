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
package com.raytheon.uf.viz.collaboration.comm.provider;

import org.jivesoftware.smack.packet.PacketExtension;
import org.jivesoftware.smack.util.Base64;

import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;

/**
 * XMPP packet extension for collaboration session data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2013 2562       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SessionPayload implements PacketExtension {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(SessionPayload.class);

    public static enum PayloadType {
        Config, Command, Invitation;
    };
    
    public static final String XMLNS = "urn:uf:viz:collaboration";

    public static final String ELEMENT_NAME = "SessionData"; 

    public static final String TYPE_ATTRIBUTE = "payloadtype";

    public static final String ENCODING_ATTRIBUTE = "encoding";

    private final PayloadType payloadType;

    private final SerializationMode mode;

    private final Object data;

    /**
     * @param type
     *            type of message
     * @param mode
     *            encoding for message
     * @param data
     *            message object
     */
    public SessionPayload(PayloadType type, SerializationMode mode, Object data) {
        this.payloadType = type;
        this.mode = mode;
        this.data = data;
    }

    /**
     * Serialization mode will be determine by reflection on object
     * 
     * @param type
     *            type of message
     * @param data
     *            message object
     */
    public SessionPayload(PayloadType type, Object data) {
        this(type, SerializationMode.getMode(data), data);
    }

    /**
     * Create collaboration extension payload XML
     * 
     * @param type
     *            type of message
     * @param mode
     *            encoding for message
     * @param data
     *            message object
     * @return XML packet fragment
     * @throws CollaborationException
     */
    public static String createXml(PayloadType type, SerializationMode mode,
            Object data) throws CollaborationException {
        StringBuilder builder = new StringBuilder();
        builder = new StringBuilder();
        builder.append("<").append(ELEMENT_NAME).append(" ");
        appendAttribute(builder, "xmlns", XMLNS);
        appendAttribute(builder, TYPE_ATTRIBUTE, type.name());
        appendAttribute(builder, ENCODING_ATTRIBUTE, mode.name());
        builder.append(">");
        switch (mode) {
        case THRIFT:
            try {
                byte[] arr = SerializationUtil.transformToThrift(data);
                builder.append(Base64.encodeBytes(arr));
            } catch (Exception e) {
                throw new CollaborationException(
                        "[THRIFT] Could not serialize object", e);
            }
            break;
        case JAXB:
            try {
                CollaborationXmlManager jaxb = CollaborationXmlManager
                        .getInstance();
                String xml = jaxb.marshalToFragment(data);
                builder.append(xml);
            } catch (Exception je) {
                throw new CollaborationException(
                        "[JAXB] Could not serialize object", je);
            }
            break;
        case STRING:
            builder.append(data.toString());
            break;
        case NONE:
            throw new CollaborationException("Serialization of "
                    + data.getClass().getName() + " not supported");
        case ISNULL:
            break;
        }
        builder.append("</").append(ELEMENT_NAME).append(">");
        return builder.toString();
    }

    /**
     * Format XML attribute name/value pair and append to string builder
     * 
     * @param sb
     * @param name
     * @param value
     */
    private static void appendAttribute(StringBuilder sb, String name,
            String value) {
        sb.append(name).append("='").append(value).append("' ");
    }


    /**
     * @return the payloadType
     */
    public PayloadType getPayloadType() {
        return payloadType;
    }

    /**
     * @return the data encoding mode
     */
    public SerializationMode getMode() {
        return mode;
    }

    /**
     * @return the data object
     */
    public Object getData() {
        return data;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.jivesoftware.smack.packet.PacketExtension#getElementName()
     */
    @Override
    public String getElementName() {
        return ELEMENT_NAME;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.jivesoftware.smack.packet.PacketExtension#getNamespace()
     */
    @Override
    public String getNamespace() {
        return XMLNS;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.jivesoftware.smack.packet.PacketExtension#toXML()
     */
    @Override
    public String toXML() {
        try {
            return createXml(payloadType, mode, data);
        } catch (CollaborationException e) {
            log.error("Unable to marshall payload to XML", e);
            // this method will be called by smack and the result appended to a
            // string buffer, this will result in an empty packet which will be
            // ignored
            return "";
        }
    }


}
