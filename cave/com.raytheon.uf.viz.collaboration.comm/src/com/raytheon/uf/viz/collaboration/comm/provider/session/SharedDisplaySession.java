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

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Message.Type;
import org.jivesoftware.smack.packet.PacketExtension;
import org.jivesoftware.smackx.packet.DiscoverInfo;
import org.jivesoftware.smackx.pubsub.AccessModel;
import org.jivesoftware.smackx.pubsub.Affiliation;
import org.jivesoftware.smackx.pubsub.ConfigureForm;
import org.jivesoftware.smackx.pubsub.FormType;
import org.jivesoftware.smackx.pubsub.Item;
import org.jivesoftware.smackx.pubsub.ItemDeleteEvent;
import org.jivesoftware.smackx.pubsub.ItemPublishEvent;
import org.jivesoftware.smackx.pubsub.LeafNode;
import org.jivesoftware.smackx.pubsub.NodeType;
import org.jivesoftware.smackx.pubsub.PayloadItem;
import org.jivesoftware.smackx.pubsub.PubSubManager;
import org.jivesoftware.smackx.pubsub.PublishModel;
import org.jivesoftware.smackx.pubsub.Subscription;
import org.jivesoftware.smackx.pubsub.Subscription.State;
import org.jivesoftware.smackx.pubsub.listener.ItemDeleteListener;
import org.jivesoftware.smackx.pubsub.listener.ItemEventListener;
import org.jivesoftware.smackx.pubsub.packet.SyncPacketSend;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.provider.SessionPayload;
import com.raytheon.uf.viz.collaboration.comm.provider.SessionPayload.PayloadType;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Chat room with shared display
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 18, 2012            njensen     Initial creation
 * Dec 18, 2013 2562       bclement    moved data to packet extension
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SharedDisplaySession extends VenueSession implements
        ISharedDisplaySession, ItemEventListener<Item>, ItemDeleteListener {

    private static final transient IUFStatusHandler log = UFStatus
            .getHandler(SharedDisplaySession.class);

    private UserId sessionLeader = null;

    private UserId dataProvider = null;

    private PubSubManager pubsubMgr;

    private LeafNode topic;

    private XMPPConnection conn;

    public SharedDisplaySession(EventBus externalBus,
            CollaborationConnection manager) throws CollaborationException {
        super(externalBus, manager);
        init();
    }

    public SharedDisplaySession(EventBus externalBus,
            CollaborationConnection manager, String sessionId)
            throws CollaborationException {
        super(externalBus, manager, sessionId);
        init();
    }

    /**
     * perform setup/management tasks before connecting to venue
     */
    private void init() {
        this.conn = getConnection().getXmppConnection();
        pubsubMgr = new PubSubManager(conn);
        cleanupOldTopics();
    }

    /**
     * Delete orphaned topic from sessions this user started that are not
     * currently running on this client
     */
    private void cleanupOldTopics() {
        Collection<ISession> sessions = getConnection().getSessions();
        Set<String> sessionIds = new HashSet<String>(sessions.size());
        for ( ISession s : sessions){
            sessionIds.add(s.getSessionId());
        }
        List<Affiliation> affiliations;
        try {
            affiliations = pubsubMgr.getAffiliations();
        } catch (XMPPException e) {
            // openfire returns a 404 if there are no topic affiliations for
            // this user. Smack turns that into an exception.
            log.debug("Unable to get affiliations from server: "
                    + e.getLocalizedMessage());
            return;
        }
        for (Affiliation aff : affiliations) {
            if (aff.getType().equals(
                    org.jivesoftware.smackx.pubsub.Affiliation.Type.owner)) {
                if (!sessionIds.contains(aff.getNodeId())) {
                    // TODO should we check to see if the topic is a certain age
                    // before cleanup?
                    log.info("Deleting old topic: " + aff.getNodeId());
                    try {
                        pubsubMgr.deleteNode(aff.getNodeId());
                    } catch (XMPPException e) {
                        log.error(
                                "Problem cleaning up old topic: "
                                        + aff.getNodeId(), e);
                    }
                }
            }
        }
    }


    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession
     * #sendObjectToVenue(java.lang.Object)
     */
    @Override
    public void sendObjectToVenue(Object obj) throws CollaborationException {
        if (obj == null) {
            return;
        }
        SessionPayload payload = new SessionPayload(PayloadType.Command, obj);
        topic.publish(new PayloadItem<SessionPayload>(payload));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession
     * #sendObjectToPeer
     * (com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID,
     * java.lang.Object)
     */
    @Override
    public void sendObjectToPeer(
            com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID participant,
            Object obj) throws CollaborationException {
        // TODO should only send to CAVE clients
        if (obj == null) {
            return;
        }
        SessionPayload payload = new SessionPayload(PayloadType.Command, obj);
        Message msg = new Message(participant.getFQName(), Type.normal);
        msg.addExtension(payload);
        msg.setFrom(conn.getUser());
        msg.setProperty(Tools.PROP_SESSION_ID, getSessionId());
        conn.sendPacket(msg);
    }

    /**
     * Get the identification of the user who is the DataProvider.
     * 
     * @return The DataProvider user identification.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#getCurrentDataProvider()
     */
    @Override
    public UserId getCurrentDataProvider() {
        return dataProvider;
    }

    /**
     * Get the identification of the user who is the Session Leader.
     * 
     * @return The Session Leader user identification.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#getCurrentSessionLeader()
     */
    @Override
    public UserId getCurrentSessionLeader() {
        return sessionLeader;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#hasRole(com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole)
     */
    @Override
    public boolean hasRole(SharedDisplayRole role) {
        boolean result = true;
        if (role.equals(SharedDisplayRole.DATA_PROVIDER)
                && !this.getUserID().equals(this.getCurrentDataProvider())) {
            result = false;
        } else if (role.equals(SharedDisplayRole.SESSION_LEADER)
                && !this.getUserID().equals(this.getCurrentSessionLeader())) {
            result = false;
        }
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession
     * #setCurrentSessionLeader
     * (com.raytheon.uf.viz.collaboration.comm.provider.user.UserId)
     */
    @Override
    public void setCurrentSessionLeader(UserId id) {
        sessionLeader = id;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession
     * #setCurrentDataProvider
     * (com.raytheon.uf.viz.collaboration.comm.provider.user.UserId)
     */
    @Override
    public void setCurrentDataProvider(UserId id) {
        dataProvider = id;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.provider.session.VenueSession#
     * configureVenue(java.lang.String)
     */
    @Override
    protected IVenueInfo configureVenue(String venueName)
            throws CollaborationException {
        IVenueInfo rval = super.configureVenue(venueName);
        try {
            configureSubscription();
        } catch (XMPPException e) {
            throw new CollaborationException(
                    "Unable to configure subscription", e);
        }
        return rval;
    }

    /**
     * Setup data subscription for this client
     * 
     * @throws XMPPException
     * @throws CollaborationException
     */
    private void configureSubscription() throws XMPPException,
            CollaborationException {
        topic = pubsubMgr.getNode(getSessionId());
        topic.addItemEventListener(this);
        topic.addItemDeleteListener(this);
        Subscription sub = findSubscription(getUserID());
        if (sub == null) {
            sub = topic.subscribe(conn.getUser());
        }
        if ( !sub.getState().equals(State.subscribed)){
            String msg = String.format(
                    "Problem subscribing to topic %s. Current state: %s",
                    topic.getId(), sub.getState().toString());
            throw new CollaborationException(msg);
        }
    }

    /**
     * Find subscriptions this user has on the current topic
     * 
     * @param user
     * @return null if none found
     * @throws XMPPException
     */
    protected Subscription findSubscription(UserId user) throws XMPPException {
        Subscription rval = null;
        for (Subscription sub : topic.getSubscriptions()) {
            if (user.isSameUser(sub.getJid())) {
                log.info("found sub: " + sub.toXML());
                rval = sub;
            }
        }
        return rval;
    }


    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.provider.session.VenueSession#
     * createVenue(java.lang.String, java.lang.String)
     */
    @Override
    protected IVenueInfo createVenue(String venueName, String subject)
            throws CollaborationException {
        IVenueInfo rval = super.createVenue(venueName, subject);
        try {
            createNode(getSessionId());
        } catch (XMPPException e) {
            throw new CollaborationException("Unable to create topic", e);
        }
        try {
            configureSubscription();
        } catch (XMPPException e) {
            throw new CollaborationException(
                    "Unable to configure subscription", e);
        }
        return rval;
    }

    /**
     * Create and configure new topic for data subscriptions
     * 
     * @param topicName
     * @throws XMPPException
     */
    private void createNode(String topicName) throws XMPPException {
        ConfigureForm form = new ConfigureForm(FormType.submit);
        form.setAccessModel(AccessModel.open);
        form.setDeliverPayloads(true);
        form.setNodeType(NodeType.leaf);
        form.setTitle(topicName);
        form.setSubscribe(true);
        form.setPublishModel(PublishModel.open);
        form.setPresenceBasedDelivery(false);
        form.setPersistentItems(false);
        form.setNotifyDelete(true);
        form.setNotifyConfig(true);
        form.setNotifyRetract(true);
        form.setMaxPayloadSize(Short.MAX_VALUE);
        pubsubMgr.createNode(topicName, form);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.jivesoftware.smackx.pubsub.listener.ItemEventListener#
     * handlePublishedItems(org.jivesoftware.smackx.pubsub.ItemPublishEvent)
     */
    @Override
    public void handlePublishedItems(ItemPublishEvent<Item> items) {
        for (Item i : items.getItems()) {
            if (i instanceof PayloadItem<?>) {
                PacketExtension payload = ((PayloadItem<?>) i).getPayload();
                handlePayload(payload);
            }
        }
    }

    /**
     * Process session data payload
     * 
     * @param payload
     */
    private void handlePayload(PacketExtension payload) {
        if (payload instanceof SessionPayload) {
            SessionPayload sp = (SessionPayload) payload;
            Object obj = sp.getData();
            postEvent(obj);
        }
    }

    /**
     * @return true if session data topic exists
     */
    private boolean topicExists() {
        try {
            DiscoverInfo info = new DiscoverInfo();
            info.setTo("pubsub." + conn.getServiceName());
            info.setNode(topic.getId());
            SyncPacketSend.getReply(conn, info);
            return true;
        } catch (XMPPException e) {
            return false;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.provider.session.VenueSession#
     * close()
     */
    @Override
    public void close() {
        super.close();
        if (pubsubMgr == null || topic == null || !topicExists()) {
            return;
        }
        try {
            Subscription sub = findSubscription(getUserID());
            if (sub == null) {
                return;
            }
            topic.unsubscribe(sub.getJid(), sub.getId());
            topic.removeItemDeleteListener(this);
            topic.removeItemEventListener(this);
            if (hasRole(SharedDisplayRole.SESSION_LEADER)) {
                pubsubMgr.deleteNode(topic.getId());
            }
            topic = null;
            pubsubMgr = null;
        } catch (XMPPException e) {
            log.error("Unable to close subscription", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.jivesoftware.smackx.pubsub.listener.ItemDeleteListener#handleDeletedItems
     * (org.jivesoftware.smackx.pubsub.ItemDeleteEvent)
     */
    @Override
    public void handleDeletedItems(ItemDeleteEvent items) {
        // TODO this method should be able to tell the client if the topic is
        // deleted, either smack or openfire doesn't support that feature, so we
        // use topicExists() instead
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.jivesoftware.smackx.pubsub.listener.ItemDeleteListener#handlePurge()
     */
    @Override
    public void handlePurge() {
        // we don't persist data packets to topics
    }

}
