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

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.http.client.methods.HttpDelete;
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
import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.comm.HttpClient.HttpClientResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.xmpp.ext.ChangeAffiliationExtension;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.packet.SessionPayload;
import com.raytheon.uf.viz.collaboration.comm.packet.SessionPayload.PayloadType;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.LeaderChangeEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;

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
 * Jan 28, 2014 2698       bclement    removed venue info
 * Jan 30, 2014 2698       bclement    changed UserId to VenueParticipant
 *                                     changed args to create/configure venue
 * Feb 12, 2014 2793       bclement    added additional null check to sendObjectToVenue
 * Feb 13, 2014 2751       bclement    VenueParticipant refactor
 * Feb 13, 2014 2751       njensen     Added changeLeader()
 * Feb 18, 2014 2751       bclement    implemented room and pubsub ownership transfer
 * Feb 19, 2014 2751       bclement    added isClosed()
 * Feb 24, 2014 2751       bclement    added validation for change leader event
 * Feb 28, 2014 2756       bclement    added cleanUpHttpStorage()
 * Mar 06, 2014 2751       bclement    added calls to getParticipantUserid()
 * Mar 07, 2014 2848       bclement    moved pubsub close logic to closePubSub()
 *                                      ensure that subscription is setup before joining room
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

    private VenueParticipant sessionLeader = null;

    private VenueParticipant dataProvider = null;

    private PubSubManager pubsubMgr;

    private LeafNode topic;

    private XMPPConnection conn;

    private boolean closed = false;

    public SharedDisplaySession(EventBus externalBus,
            CollaborationConnection manager, String venueName, String handle,
            String sessionId) {
        super(externalBus, manager, venueName, handle, sessionId);
        init();
    }

    public SharedDisplaySession(EventBus externalBus,
            CollaborationConnection manager, CreateSessionData data) {
        super(externalBus, manager, data);
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
        for (ISession s : sessions) {
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
                        cleanUpHttpStorage(aff.getNodeId());
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
    public void sendObjectToVenue(Object obj){
        if (obj == null) {
            return;
        }
        if (topic == null) {
            log.warn("Attempted to send object when topic not configured: "
                    + obj);
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
    public void sendObjectToPeer(VenueParticipant participant, Object obj) {
        // TODO should only send to CAVE clients
        if (obj == null) {
            return;
        }
        // TODO should we use MUC private chat for this?
        UserId userid = getVenue().getParticipantUserid(participant);
        if (userid == null) {
            log.warn("Attempted to send object to peer when actual userid is unknown");
            return;
        }
        SessionPayload payload = new SessionPayload(PayloadType.Command, obj);
        Message msg = new Message(userid.getFQName(), Type.normal);
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
    public VenueParticipant getCurrentDataProvider() {
        return dataProvider;
    }

    /**
     * Get the identification of the user who is the Session Leader.
     * 
     * @return The Session Leader user identification.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#getCurrentSessionLeader()
     */
    @Override
    public VenueParticipant getCurrentSessionLeader() {
        return sessionLeader;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#hasRole(com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole)
     */
    @Override
    public boolean hasRole(SharedDisplayRole role) {
        boolean result = true;
        if (role.equals(SharedDisplayRole.DATA_PROVIDER)
                && !this.getUserID().isSameUser(this.getCurrentDataProvider())) {
            result = false;
        } else if (role.equals(SharedDisplayRole.SESSION_LEADER)
                && !this.getUserID().isSameUser(this.getCurrentSessionLeader())) {
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
    public void setCurrentSessionLeader(VenueParticipant id) {
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
    public void setCurrentDataProvider(VenueParticipant id) {
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
    public void configureVenue()
            throws CollaborationException {
        try {
            configureSubscription();
        } catch (XMPPException e) {
            closePubSub();
            throw new CollaborationException(
                    "Unable to configure subscription", e);
        }
        try {
            super.configureVenue();
        } catch (CollaborationException e) {
            closePubSub();
            throw e;
        }
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
        Subscription sub = findSubscription(getAccount());
        if (sub == null) {
            sub = topic.subscribe(conn.getUser());
        }
        if (!sub.getState().equals(State.subscribed)) {
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
    public void createVenue(CreateSessionData data)
            throws CollaborationException {
        boolean topicCreated = false;
        try {
            createNode(getSessionId());
            topicCreated = true;
            configureSubscription();
            super.createVenue(data);
        } catch (XMPPException e) {
            closePubSub();
            String action = topicCreated ? "configure" : "create";
            throw new CollaborationException("Unable to " + action
                    + " session topic", e);
        } catch (CollaborationException e) {
            closePubSub();
            throw e;
        }
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
            boolean validEvent = true;
            if (obj instanceof LeaderChangeEvent) {
                validEvent = handleLeaderChange((LeaderChangeEvent) obj);
            }
            /*
             * TODO create white list of events that non-leaders can send to
             * topic (ie telestration). Ignore all other events that are not
             * from leader.
             */
            if (validEvent) {
                postEvent(obj);
            }
        }
    }

    /**
     * Apply leadership change event to session
     * 
     * @param event
     * @return true if the leader change event is valid
     */
    private boolean handleLeaderChange(LeaderChangeEvent event) {
        VenueParticipant newLeader = event.getNewLeader();
        boolean rval;
        if (!isRoomOwner(newLeader)) {
            log.info("Invalid leader change event: " + newLeader
                    + " is not an owner of the room");
            rval = false;
        } else {
            setCurrentDataProvider(newLeader);
            setCurrentSessionLeader(newLeader);
            rval = true;
        }
        return rval;
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
        closePubSub();
    }

    private void closePubSub() {
        try {
            if (pubsubMgr == null || topic == null || !topicExists()) {
                return;
            }
            Subscription sub = findSubscription(getAccount());
            if (sub == null) {
                return;
            }
            topic.unsubscribe(sub.getJid(), sub.getId());
            topic.removeItemDeleteListener(this);
            topic.removeItemEventListener(this);
            if (hasRole(SharedDisplayRole.SESSION_LEADER)) {
                cleanUpHttpStorage(topic.getId());
                pubsubMgr.deleteNode(topic.getId());
            }
            topic = null;
            pubsubMgr = null;
        } catch (XMPPException e) {
            log.error("Unable to close subscription", e);
        } finally {
            // if an error happens, we still want to advertise that we were
            // closed
            closed = true;
        }
    }

    /**
     * Delete session directory on HTTP server. This should only be called if
     * this client is the owner of the topic associated with the session and
     * only after the session is closed.
     * 
     * TODO this will not be needed if the xmpp server takes care of shared
     * display session lifecycle (create, transfer ownership, destroy, etc)
     * 
     * @param sessionid
     */
    private void cleanUpHttpStorage(String sessionid) {
        try {
            String userid = getAccount().getNormalizedId();
            String url = PeerToPeerCommHelper.getCollaborationHttpServer();
            url += sessionid + "/";
            URI uri = new URI(url);
            HttpDelete delete = new HttpDelete(uri);
            ClientAuthManager authManager = getConnection().getAuthManager();
            authManager.signRequest(delete, userid, uri);
            HttpClientResponse response = HttpClient.getInstance()
                    .executeRequest(delete);
            if (!response.isSuccess() && !response.isNotExists()) {
                throw new CollaborationException("Session deletion failed for "
                        + uri + ": " + new String(response.data));
            }
        } catch (Exception e) {
            log.error(
                    "Problem cleaning up old HTTP storage objects for session: "
                            + sessionid, e);
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

    /**
     * Make user with id an owner of the pubsub topic
     * 
     * @param id
     * @throws XMPPException
     */
    private void grantTopicOwnership(String id) throws XMPPException {
        sendAffiliationPacket(id, Affiliation.Type.owner);
    }

    /**
     * Make user with id a publisher of the pubsub topic
     * 
     * @param id
     * @throws XMPPException
     */
    private void revokeTopicOwnership(String id) throws XMPPException {
        sendAffiliationPacket(id, Affiliation.Type.publisher);
    }

    /**
     * Change affiliation of user with id
     * 
     * @param id
     * @param type
     * @throws XMPPException
     */
    private void sendAffiliationPacket(String id, Affiliation.Type type)
            throws XMPPException {
        ChangeAffiliationExtension affiliation = new ChangeAffiliationExtension(
                topic, id, type);
        PubSubOperations.sendAffiliationPacket(conn, affiliation);
    }

    @Override
    public void changeLeader(VenueParticipant newLeader)
            throws CollaborationException {
        final VenueParticipant oldLeader = getCurrentDataProvider();
        if (!oldLeader.isSameUser(getUserID())) {
            throw new CollaborationException(
                    "Only the leader can transfer leadership");
        }
        UserId actualId = getVenue().getParticipantUserid(newLeader);
        if (actualId == null) {
            throw new CollaborationException(
                    "Unable to grant ownership because new leader's actual userid is not known");
        }

        final String newLeaderId = actualId.getNormalizedId();

        boolean topicOwnershipGranted = false;
        boolean roomOwnershipGranted = false;
        boolean othersNotified = false;
        String revokeTarget = null;
        try {
            // was formerly the data provider, so hand off pubsub ownership
            grantTopicOwnership(newLeaderId);
            topicOwnershipGranted = true;

            // change the room's ownership cause the leader needs to know
            // participants' jids to properly complete some leader actions
            muc.grantOwnership(newLeaderId);
            roomOwnershipGranted = true;

            // TODO if we get private chat within a chat room working, we
            // shouldn't need to let everyone know the leader's jid
            LeaderChangeEvent event = new LeaderChangeEvent(newLeader,
                    oldLeader);
            this.sendObjectToVenue(event);
            othersNotified = true;

            // revoke our own ownership, last action in case other parts
            // of the transfer fail
            revokeTarget = "topic";
            UserId account = getAccount();
            revokeTopicOwnership(account.getNormalizedId());
            // we revoke admin instead of ownership because it sets back to
            // 'member' instead of just down to 'admin'
            revokeTarget = "room";
            muc.revokeAdmin(account.getNormalizedId());
        } catch (XMPPException e) {
            if (!othersNotified) {
                // transaction, attempt to roll back the ownership changes
                // because other participants didn't hear about it
                if (roomOwnershipGranted) {
                    try {
                        muc.revokeAdmin(newLeaderId);
                    } catch (XMPPException e1) {
                        log.error(
                                "Problem rolling back room ownership transfer",
                                e1);
                    }
                }
                if (topicOwnershipGranted) {
                    try{
                        revokeTopicOwnership(newLeaderId);
                    } catch (XMPPException e1) {
                        log.error(
                                "Problem rolling back topic ownership transfer",
                                e1);
                    }
                }
                throw new CollaborationException(
                        "Error transferring leadership", e);
            } else {
                log.warn("Problem releasing ownership of " + revokeTarget
                        + ". " + e.getLocalizedMessage());
            }
        } 
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession
     * #isClosed()
     */
    @Override
    public boolean isClosed() {
        return closed;
    }

}
