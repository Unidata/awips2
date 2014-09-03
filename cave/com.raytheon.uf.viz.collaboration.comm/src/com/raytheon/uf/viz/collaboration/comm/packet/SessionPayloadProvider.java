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
package com.raytheon.uf.viz.collaboration.comm.packet;

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
import com.raytheon.uf.common.xmpp.BaseProvider;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.packet.SessionPayload.PayloadType;
import com.raytheon.uf.viz.collaboration.comm.provider.CollaborationXmlManager;
import com.raytheon.uf.viz.collaboration.comm.provider.SerializationMode;

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
 * Feb 27, 2013 2756       bclement     extends BaseProvider
 * Jun 12, 2013 2903       bclement     added support for jaxb xml in base64
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class SessionPayloadProvider extends BaseProvider<SessionPayload>
        implements PacketExtensionProvider {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(SessionPayloadProvider.class);

    public SessionPayloadProvider() {
        super(SessionPayload.ELEMENT_NAME);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.jivesoftware.smack.provider.PacketExtensionProvider#parseExtension
     * (org.xmlpull.v1.XmlPullParser)
     */
    @Override
    public PacketExtension parseExtension(XmlPullParser parser)
            throws Exception {
        return parse(parser);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.xmpp.BaseProvider#parseInternal(org.xmlpull.v1
     * .XmlPullParser)
     */
    @Override
    protected SessionPayload parseInternal(XmlPullParser parser)
            throws XmlPullParserException, IOException {
        try {
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
                throw new CollaborationException(
                        "Unsupported payload encoding: " + modeString, e);
            }
            PayloadType t;
            try {
                t = PayloadType.valueOf(typeString);
            } catch (IllegalArgumentException e) {
                throw new CollaborationException(
                        "Unsupported IQ payload type: " + typeString, e);
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
        } catch (CollaborationException e) {
            // collaboration exceptions are only thrown for problems with our
            // own format
            log.error("Unable to parse collaboration packet", e);
            return new SessionPayload(PayloadType.Command,
                    SerializationMode.ISNULL, null);
        }
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
        CollaborationXmlManager manager = CollaborationXmlManager.getInstance();
        int tag = parser.next();
        Object rval;
        if (tag == XmlPullParser.TEXT) {
            /*
             * default behavior is to wrap XML in base64 to avoid problems with
             * openfire handling complex XML leading to disconnect
             */
            byte[] xmlBytes = Base64.decode(parser.getText());
            String xml = new String(xmlBytes, SessionPayload.XML_ENCODING);
            try {
                rval = manager.unmarshalFromXml(xml);
            } catch (JAXBException e) {
                throw new CollaborationException(
                        "Unable to parse base64 encoded XML payload: " + xml, e);
            }
        } else if (tag == XmlPullParser.START_TAG) {
            /* JAXB payload is more XML, attempt to unmarshal it */
            rval = manager.unmarshalFromXPP(parser);
        } else {
            throw new CollaborationException(
                    "Unexpected parser state after JAXB tag: " + tag);
        }
        return rval;
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
