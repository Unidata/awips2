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
package com.raytheon.openfire.plugin.configuration.collaboration.iq;

import java.util.Arrays;

import org.dom4j.Element;
import org.dom4j.tree.DefaultAttribute;
import org.jivesoftware.openfire.IQHandlerInfo;
import org.jivesoftware.openfire.XMPPServer;
import org.jivesoftware.openfire.auth.UnauthorizedException;
import org.jivesoftware.openfire.muc.MUCRoom;
import org.jivesoftware.openfire.muc.MultiUserChatManager;
import org.jivesoftware.openfire.muc.MultiUserChatService;
import org.jivesoftware.util.JiveGlobals;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xmpp.packet.IQ;

/**
 * Handler for feed venue configuration queries
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
public class FeedVenueConfigHandler extends AbstractConfigHandler {

    private static final Logger log = LoggerFactory
            .getLogger(FeedVenueConfigHandler.class);

    public static enum VenueType {
        /* SINGLE is a normal room, AGGREGATE is multiple rooms viewed as one */
        SINGLE, AGGREGATE;
    }

    /* XML constants */

    public static final String FEED_QUERY_XMLNS = "urn:uf:viz:collaboration:iq:feed";

    public static final String FEED_VENUE_SUBDOMAIN_ATTRIBUTE = "subdomain";

    public static final String FEED_VENUE_NAME_ATTRIBUTE = "name";

    public static final String FEED_VENUE_TYPE_ATTRIBUTE = "type";

    /* config storage constants */

    public static final String FEED_VENUE_SUBDOMAIN_KEY = "plugin.collaboration.feed.venue.subdomain";

    public static final String FEED_VENUE_SUBDOMAIN_DEFAULT = "conference";

    public static final String FEED_VENUE_NAME_KEY = "plugin.collaboration.feed.venue.name";

    public static final String FEED_VENUE_NAME_DEFAULT = "nws-collaboration";

    public static final String FEED_VENUE_TYPE_KEY = "plugin.collaboration.feed.venue.type";

    public static final VenueType FEED_VENUE_TYPE_DEFAULT = VenueType.SINGLE;


    /**
     * 
     */
    public FeedVenueConfigHandler() {
        super("Collaboration Dataserver Feed Venue Configuration Handler",
                Arrays.asList(FEED_QUERY_XMLNS), new IQHandlerInfo(
                        QUERY_ELEMENT_NAME, FEED_QUERY_XMLNS));
    }

    /* (non-Javadoc)
     * @see com.raytheon.openfire.plugin.configuration.collaboration.iq.AbstractConfigHandler#handleGet(org.xmpp.packet.IQ)
     */
    @Override
    protected IQ handleGet(IQ packet) throws UnauthorizedException {
        String subdomain = getFeedVenueSubdomain();
        String name = getFeedVenueName();
        /* always default until we implement aggregate */
        VenueType type = FEED_VENUE_TYPE_DEFAULT;

        Element query = packet.getChildElement();
        query.setParent(null);
        query.add(new DefaultAttribute(FEED_VENUE_SUBDOMAIN_ATTRIBUTE,
                subdomain));
        query.add(new DefaultAttribute(FEED_VENUE_NAME_ATTRIBUTE, name));
        query.add(new DefaultAttribute(FEED_VENUE_TYPE_ATTRIBUTE, type
                .toString()));
        IQ rval = IQ.createResultIQ(packet);
        rval.setChildElement(query);
        return rval;
    }

    /* (non-Javadoc)
     * @see com.raytheon.openfire.plugin.configuration.collaboration.iq.AbstractConfigHandler#handleSet(org.xmpp.packet.IQ)
     */
    @Override
    protected IQ handleSet(IQ packet) throws UnauthorizedException {
        log.debug("Received unsupported packet type: " + packet.getType());
        throw new UnauthorizedException(
                "Feed Venue configuration can only be queried");
    }


    /**
     * @param subdomain
     * @param name
     * @return false if subdomain or name do not exist on server
     */
    public static boolean setFeedVenueConfig(String subdomain, String name) {
        XMPPServer server = XMPPServer.getInstance();
        MultiUserChatManager mucManager = server.getMultiUserChatManager();
        MultiUserChatService service = mucManager
                .getMultiUserChatService(subdomain);
        boolean rval = false;
        if (service != null) {
            MUCRoom chatRoom = service.getChatRoom(name);
            if (chatRoom != null) {
                JiveGlobals.setProperty(FEED_VENUE_SUBDOMAIN_KEY, subdomain);
                JiveGlobals.setProperty(FEED_VENUE_NAME_KEY, name);
                rval = true;
            }
        }
        return rval;
    }

    /**
     * @return
     */
    public static String getFeedVenueSubdomain() {
        return JiveGlobals.getProperty(FEED_VENUE_SUBDOMAIN_KEY,
                FEED_VENUE_SUBDOMAIN_DEFAULT);
    }

    /**
     * @return
     */
    public static String getFeedVenueName() {
        return JiveGlobals.getProperty(FEED_VENUE_NAME_KEY,
                FEED_VENUE_NAME_DEFAULT);
    }

}
