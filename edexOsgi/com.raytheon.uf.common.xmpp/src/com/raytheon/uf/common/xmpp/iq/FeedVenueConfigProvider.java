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
package com.raytheon.uf.common.xmpp.iq;

import java.io.IOException;

import org.jivesoftware.smack.packet.IQ;
import org.jivesoftware.smack.provider.IQProvider;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import com.raytheon.uf.common.xmpp.BaseProvider;
import com.raytheon.uf.common.xmpp.PacketConstants;
import com.raytheon.uf.common.xmpp.iq.FeedVenueConfig.VenueType;

/**
 * Info Query parsing support for feed venue configuration packets
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 16, 2014 3288       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class FeedVenueConfigProvider extends BaseProvider<FeedVenueConfig>
        implements IQProvider {

    /**
     */
    public FeedVenueConfigProvider() {
        super(PacketConstants.QUERY_ELEMENT_NAME);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.jivesoftware.smack.provider.IQProvider#parseIQ(org.xmlpull.v1.
     * XmlPullParser)
     */
    @Override
    public IQ parseIQ(XmlPullParser parser) throws Exception {
        String subdomain = null;
        String name = null;
        VenueType type = null;

        do {
            String tagName = parser.getName();
            switch (parser.getEventType()) {
            case XmlPullParser.START_TAG:
                if (PacketConstants.QUERY_ELEMENT_NAME.equals(tagName)) {
                    subdomain = parser.getAttributeValue(null,
                            FeedVenueConfig.FEED_VENUE_SUBDOMAIN_ATTRIBUTE);
                    name = parser.getAttributeValue(null,
                            FeedVenueConfig.FEED_VENUE_NAME_ATTRIBUTE);
                    String typeStr = parser.getAttributeValue(null,
                            FeedVenueConfig.FEED_VENUE_TYPE_ATTRIBUTE);
                    if (typeStr != null && !typeStr.trim().isEmpty()) {
                        type = VenueType.valueOf(typeStr.trim().toUpperCase());
                    }
                }
                break;
            }
            parser.next();
        } while (!atEndOfPacket(parser));
        return new FeedVenueConfig(subdomain, name, type);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.xmpp.BaseProvider#parseInternal(org.xmlpull.v1
     * .XmlPullParser)
     */
    @Override
    protected FeedVenueConfig parseInternal(XmlPullParser parser)
            throws XmlPullParserException, IOException {
        return super.parse(parser);
    }

}
