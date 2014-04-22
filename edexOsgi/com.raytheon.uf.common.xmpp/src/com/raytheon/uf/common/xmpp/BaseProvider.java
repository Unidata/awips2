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
package com.raytheon.uf.common.xmpp;

import java.io.IOException;
import java.util.Iterator;

import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smackx.ServiceDiscoveryManager;
import org.jivesoftware.smackx.packet.DiscoverInfo;
import org.jivesoftware.smackx.packet.DiscoverInfo.Feature;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

/**
 * Base class for XMPP IQ and extension providers which are used to parse XMPP
 * packets.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2014 2756       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class BaseProvider<T> {

    protected final String tagName;

    /**
     * @param tagName
     *            packet tag name, used to determine when provider is done
     *            parsing
     */
    public BaseProvider(String tagName) {
        this.tagName = tagName;
    }

    /**
     * Parse contents of packet from XMPP stream.
     * 
     * @param parser
     * @return
     * @throws XmlPullParserException
     * @throws IOException
     *             when errors occurs with XMPP stream
     */
    abstract protected T parseInternal(XmlPullParser parser)
            throws XmlPullParserException, IOException;

    /**
     * Parse contents of packet from XMPP stream. Ensures that parser is left in
     * a good state so the stream isn't corrupted.
     * 
     * @param parser
     * @return
     * @throws XmlPullParserException
     * @throws IOException
     */
    protected T parse(XmlPullParser parser) throws XmlPullParserException,
            IOException {
        try {
            return parseInternal(parser);
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
     * @return true if parser is at the end tag of the packet
     * @throws XmlPullParserException
     */
    protected boolean atEndOfPacket(XmlPullParser parser)
            throws XmlPullParserException {
        return atEndOfTag(parser, tagName);
    }

    /**
     * @param parser
     * @param tag
     * @return true if parser is at the end of the specified tag
     * @throws XmlPullParserException
     */
    protected boolean atEndOfTag(XmlPullParser parser, String tag)
            throws XmlPullParserException {
        return parser.getEventType() == XmlPullParser.END_TAG
                && parser.getName().equals(tag);
    }

    /**
     * Get any text elements under current tag
     * 
     * @param parser
     * @return
     * @throws XmlPullParserException
     * @throws IOException
     */
    protected String getText(XmlPullParser parser)
            throws XmlPullParserException, IOException {
        if (parser.getEventType() == XmlPullParser.TEXT) {
            return parser.getText();
        } else if (parser.getEventType() != XmlPullParser.START_TAG) {
            return null;
        }
        String tag = parser.getName();
        StringBuilder allText = new StringBuilder();
        while (!atEndOfTag(parser, tag)) {
            if (parser.getEventType() == XmlPullParser.TEXT) {
                allText.append(parser.getText());
            }
            parser.next();
        }
        return allText.toString();
    }

    /**
     * @param conn
     * @return true if server has feature listed in supported features
     * @throws XMPPException
     */
    public static boolean serverSupportsFeature(XMPPConnection conn,
            String feature) throws XMPPException {
        ServiceDiscoveryManager sdm = ServiceDiscoveryManager
                .getInstanceFor(conn);
        boolean rval = false;
        DiscoverInfo discoverInfo = sdm.discoverInfo(null);
        Iterator<Feature> iter = discoverInfo.getFeatures();
        while (iter.hasNext()) {
            String supportedFeature = iter.next().getVar();
            if (supportedFeature.equals(feature)) {
                rval = true;
                break;
            }
        }
        return rval;
    }

}
