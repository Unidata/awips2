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
package com.raytheon.uf.viz.collaboration.comm.xmpp.internal;

import java.io.IOException;
import java.util.HashMap;

import org.eclipse.ecf.core.ContainerConnectException;
import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.identity.IDCreateException;
import org.eclipse.ecf.core.identity.IDFactory;
import org.eclipse.ecf.core.identity.Namespace;
import org.eclipse.ecf.core.security.IConnectContext;
import org.eclipse.ecf.core.sharedobject.SharedObjectAddException;
import org.eclipse.ecf.internal.provider.xmpp.events.ChatMembershipEvent;
import org.eclipse.ecf.internal.provider.xmpp.events.IQEvent;
import org.eclipse.ecf.internal.provider.xmpp.events.MessageEvent;
import org.eclipse.ecf.internal.provider.xmpp.events.PresenceEvent;
import org.eclipse.ecf.presence.IIMMessageListener;
import org.eclipse.ecf.presence.chatroom.IChatRoomParticipantListener;
import org.eclipse.ecf.provider.generic.SOWrapper;
import org.jivesoftware.smack.packet.IQ;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.viz.collaboration.comm.xmpp.XMPPContainer;
import com.raytheon.uf.viz.collaboration.comm.xmpp.internal.smack.ECFConnection;

/**
 * Override the ECF XMPPChatRoomContainer but provide a custom viz
 * XMPPChatRoomContainerHelper
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 20, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

@SuppressWarnings("restriction")
public class XMPPChatRoomContainer extends
        org.eclipse.ecf.internal.provider.xmpp.XMPPChatRoomContainer {

    private static final String CONTAINER_HELPER_ID = XMPPContainer.class
            .getName() + ".xmppgroupchathandler"; //$NON-NLS-1$

    private ID containerHelperID;

    /**
     * Use viz version of chatRoomContainerHelper.
     */
    private XMPPChatRoomContainerHelper containerHelper;

    public XMPPChatRoomContainer(ECFConnection conn, Namespace usernamespace)
            throws IDCreateException {
        super(conn, usernamespace);
        this.containerHelperID = IDFactory.getDefault().createStringID(
                CONTAINER_HELPER_ID);
        this.containerHelper = new XMPPChatRoomContainerHelper(usernamespace,
                getXMPPConnection());
    }

    /**
     * Overridden to use custom containerHelper.
     */
    @Override
    public void addChatRoomParticipantListener(
            IChatRoomParticipantListener participantListener) {
        if (containerHelper != null) {
            containerHelper.addChatParticipantListener(participantListener);
        }
    }

    /**
     * Overridden to use custom containerHelper.
     */
    @Override
    public void addMessageListener(IIMMessageListener listener) {
        containerHelper.addChatRoomMessageListener(listener);
    }

    /**
     * Overridden to use custom containerHelper.
     */
    @Override
    protected void addSharedObjectToContainer(ID remote)
            throws SharedObjectAddException {
        getSharedObjectManager().addSharedObject(containerHelperID,
                containerHelper, new HashMap());
    }

    /**
     * Overridden to clean up custom containerHelper.
     */
    @Override
    protected void cleanUpConnectFail() {
        super.cleanUpConnectFail();
        if (containerHelper != null) {
            getSharedObjectManager().removeSharedObject(containerHelperID);
            containerHelper = null;
            containerHelperID = null;
        }
    }

    /**
     * Overridden to connect custom containerHelper.
     */
    @Override
    public void connect(ID remote, IConnectContext connectContext)
            throws ContainerConnectException {
        super.connect(remote, connectContext);
        containerHelper.setRoomID(remoteServerID);
    }

    /**
     * Overridden to disconnect custom containerHelper.
     */
    @Override
    public void disconnect() {
        super.disconnect();
        if (containerHelper != null)
            containerHelper.disconnect();
    }

    /**
     * Overridden to dispose custom containerHelper.
     */
    @Override
    public void dispose() {
        if (containerHelperID != null) {
            getSharedObjectManager().removeSharedObject(containerHelperID);
            containerHelperID = null;
        }
        super.dispose();
        if (containerHelper != null)
            containerHelper.dispose(getID());
        containerHelper = null;
    }

    /**
     * Overridden to use custom containerHelper.
     */
    @Override
    public ID[] getChatRoomParticipants() {
        return containerHelper.getChatRoomParticipants();
    }

    /**
     * Overridden to use custom containerHelper.
     */
    @Override
    public void removeChatRoomParticipantListener(
            IChatRoomParticipantListener participantListener) {
        if (containerHelper != null) {
            containerHelper.removeChatParticipantListener(participantListener);
        }
    }

    /**
     * Overridden to use custom containerHelper.
     */
    @Override
    public void removeMessageListener(IIMMessageListener listener) {
        containerHelper.removeChatRoomMessageListener(listener);
    }

    /**
     * Overridden to use custom containerHelperID.
     */
    @Override
    protected void handleChatMessage(Message mess) throws IOException {
        final SOWrapper wrap = getSharedObjectWrapper(containerHelperID);
        if (wrap != null) {
            wrap.deliverEvent(new MessageEvent(mess));
        }
    }

    /**
     * Overridden to use custom containerHelperID.
     */
    @Override
    protected void handleIQMessage(IQ mess) throws IOException {
        final SOWrapper wrap = getSharedObjectWrapper(containerHelperID);
        if (wrap != null) {
            wrap.deliverEvent(new IQEvent(mess));
        }
    }

    /**
     * Overridden to use custom containerHelperID.
     */
    @Override
    protected void handlePresenceMessage(Presence mess) throws IOException {
        final SOWrapper wrap = getSharedObjectWrapper(containerHelperID);
        if (wrap != null) {
            wrap.deliverEvent(new PresenceEvent(mess));
        }
    }

    /**
     * Overridden to use custom containerHelperID.
     */
    @Override
    protected void handleChatMembershipEvent(String from, boolean add) {
        final SOWrapper wrap = getSharedObjectWrapper(containerHelperID);
        if (wrap != null) {
            wrap.deliverEvent(new ChatMembershipEvent(from, add));
        }
    }

}
