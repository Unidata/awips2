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

import java.io.IOException;

import javax.xml.bind.JAXBException;

import org.apache.commons.lang.StringUtils;
import org.jivesoftware.smack.packet.PacketExtension;
import org.jivesoftware.smack.provider.PacketExtensionProvider;
import org.jivesoftware.smack.util.Base64;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.provider.SessionPayload.PayloadType;

/**
 * XMPP packet extension parsing provider for collaboration session data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2013 2562       bclement     Initial creation
 * Feb 12, 2014 2793       bclement     improved error handling
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class SessionPayloadProvider implements PacketExtensionProvider {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(SessionPayloadProvider.class);


    /* (non-Javadoc)
     * @see org.jivesoftware.smack.provider.PacketExtensionProvider#parseExtension(org.xmlpull.v1.XmlPullParser)
     */
    @Override
    public PacketExtension parseExtension(XmlPullParser parser)
            throws Exception {
        try {
            return parseInternal(parser);
        } catch (CollaborationException e) {
            // collaboration exceptions are only thrown for problems with our own format
            log.error("Unable to parse collaboration packet", e);
            return new SessionPayload(PayloadType.Command,
                    SerializationMode.ISNULL, null);
        } finally {
            // ensure that we are at the end of the packet so we don't corrupt
            // stream
            while (!atEndOfPacket(parser)) {
                parser.next();
            }
        }
    }

    /**
     * @param parser
     * @return true if parser is at the end tag of the payload packet
     * @throws XmlPullParserException
     */
    private static boolean atEndOfPacket(XmlPullParser parser)
            throws XmlPullParserException {
        return parser.getEventType() == XmlPullParser.END_TAG
                && parser.getName().equals(SessionPayload.ELEMENT_NAME);
    }

    /**
     * Parse contents of packet extension from XMPP stream.
     * 
     * @param parser
     * @return
     * @throws CollaborationException
     *             when error occurs with collaboration format
     * @throws XmlPullParserException
     *             when error occurs with XMPP stream
     * @throws IOException
     *             when error occurs with XMPP stream
     */
    private static PacketExtension parseInternal(XmlPullParser parser)
            throws CollaborationException, XmlPullParserException, IOException {
        String typeString = parser.getAttributeValue(null,
                SessionPayload.TYPE_ATTRIBUTE);
        String modeString = parser.getAttributeValue(null,
                SessionPayload.ENCODING_ATTRIBUTE);
        checkAttribute(SessionPayload.TYPE_ATTRIBUTE, typeString);
        checkAttribute(SessionPayload.ENCODING_ATTRIBUTE, modeString);

        SerializationMode mode;
        try {
            mode = SerializationMode.valueOf(modeString);
        } catch (IllegalArgumentException e) {
            throw new CollaborationException("Unsupported payload encoding: "
                    + modeString, e);
        }
        PayloadType t;
        try {
            t = PayloadType.valueOf(typeString);
        } catch (IllegalArgumentException e) {
            throw new CollaborationException("Unsupported IQ payload type: "
                    + typeString, e);
        }
        Object data;
        switch (mode) {
        case THRIFT:
            String text = getText(parser);
            data = unmarshalThrift(text);
            break;
        case JAXB:
            data = unmarshalJaxb(parser);
            break;
        case STRING:
            data = getText(parser);
            break;
        default:
            throw new CollaborationException("Could not deserialize object");
        }
        return new SessionPayload(t, mode, data);
    }

    /**
     * Unmarshal base64 encoded thrift data
     * 
     * @param data
     * @return
     * @throws CollaborationException
     */
    public static Object unmarshalThrift(String data)
            throws CollaborationException {
        try {
            byte[] b = Base64.decode(data);
            return SerializationUtil.transformFromThrift(Object.class, b);
        } catch (SerializationException e) {
            throw new CollaborationException("Could not deserialize object", e);
        }
    }

    /**
     * Unmarshal XML sub tags using JAXB
     * 
     * @param parser
     * @return
     * @throws XmlPullParserException
     * @throws IOException
     * @throws CollaborationException
     * @throws JAXBException
     */
    private static Object unmarshalJaxb(XmlPullParser parser)
            throws XmlPullParserException, IOException, CollaborationException {
        int tag = parser.next();
        if (tag != XmlPullParser.START_TAG) {
            throw new CollaborationException(
                    "Encountered JAXB payload without XML as data");
        }
        CollaborationXmlManager manager = CollaborationXmlManager.getInstance();
        return manager.unmarshalFromXPP(parser);
    }

    /**
     * Get any text elements under current tag
     * 
     * @param parser
     * @return
     * @throws XmlPullParserException
     * @throws IOException
     */
    private static String getText(XmlPullParser parser)
            throws XmlPullParserException, IOException {
        boolean done = false;
        StringBuilder payloadText = new StringBuilder();
        while (!done) {
            if (atEndOfPacket(parser)) {
                done = true;
                continue;
            } else if (parser.getEventType() == XmlPullParser.TEXT) {
                payloadText.append(parser.getText());
            }
            parser.next();
        }
        return payloadText.toString();
    }


    /**
     * Assert that value is not null or empty
     * 
     * @param name
     *            used for error message
     * @param value
     * @throws CollaborationException
     *             if value is null or empty
     */
    public static void checkAttribute(String name, String value)
            throws CollaborationException {
        if (StringUtils.isBlank(value)) {
            throw new CollaborationException("Missing attribute: " + name);
        }
    }

}
