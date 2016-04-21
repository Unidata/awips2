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
package com.raytheon.uf.viz.collaboration.comm.provider.session;

import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.IQ;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.util.SyncPacketSend;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.xmpp.BaseProvider;
import com.raytheon.uf.common.xmpp.iq.FeedVenueConfig;
import com.raytheon.uf.common.xmpp.iq.FeedVenueConfig.VenueType;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;

/**
 * Utility responsible for retrieving feed venue configuration from XMPP server
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
public class FeedVenueConfigManager {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(FeedVenueConfigManager.class);

    private static FeedVenueConfig feedVenueConfig = null;

    private static final String FEED_VENUE_FALLBACK_SUBDOMAIN = "conference";

    private static final String FEED_VENUE_FALLBACK_NAME = "nws-collaboration";

    private static final VenueType FEED_VENUE_FALLBACK_TYPE = VenueType.SINGLE;

    /**
     * Get feed venue configuration from server. Results from server are cached.
     * 
     * @return defaults if configuration cannot be retrieved from server
     */
    public static synchronized FeedVenueConfig getConfig(){
        FeedVenueConfig rval;
        if ( feedVenueConfig == null){
            XMPPConnection conn = CollaborationConnection.getConnection()
                    .getXmppConnection();
            rval = new FeedVenueConfig(FEED_VENUE_FALLBACK_SUBDOMAIN,
                    FEED_VENUE_FALLBACK_NAME, FEED_VENUE_FALLBACK_TYPE);
            if (canQueryForConfig(conn)) {
                IQ query = FeedVenueConfig.createGet();
                try {
                    Packet reply = SyncPacketSend.getReply(conn, query);
                    if (reply instanceof FeedVenueConfig) {
                        rval = feedVenueConfig = (FeedVenueConfig) reply;
                    } else {
                        log.error("Unexpected feed venue config return type: "
                                + reply.getClass());
                    }
                } catch (XMPPException e) {
                    log.error("Problem querying for feed venue configuration",
                            e);
                }
            }
        } else {
            rval = feedVenueConfig;
        }
        return rval;
    }

    /**
     * @param conn
     * @return true if server returns that it supports feed venue configuration
     */
    private static boolean canQueryForConfig(XMPPConnection conn) {
        boolean rval = false;
        try {
            rval = BaseProvider.serverSupportsFeature(conn,
                    FeedVenueConfig.FEED_QUERY_XMLNS);
        } catch (XMPPException e) {
            log.error("Unable to determine if server supports "
                    + "feed venue configuration queries", e);
        }
        return rval;
    }

    /**
     * reset internal state
     */
    public static synchronized void reset() {
        feedVenueConfig = null;
    }

}
