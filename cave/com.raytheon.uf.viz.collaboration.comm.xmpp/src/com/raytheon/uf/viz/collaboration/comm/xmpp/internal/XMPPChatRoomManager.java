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

import java.util.Map;

import org.eclipse.ecf.core.ContainerCreateException;
import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.identity.IDCreateException;
import org.eclipse.ecf.core.identity.Namespace;
import org.eclipse.ecf.internal.provider.xmpp.Messages;
import org.eclipse.ecf.presence.chatroom.ChatRoomCreateException;
import org.eclipse.ecf.presence.chatroom.IChatRoomContainer;
import org.eclipse.ecf.presence.chatroom.IChatRoomInfo;
import org.eclipse.ecf.provider.xmpp.identity.XMPPRoomID;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smackx.Form;
import org.jivesoftware.smackx.muc.MultiUserChat;
import org.jivesoftware.smackx.muc.RoomInfo;

import com.raytheon.uf.viz.collaboration.comm.xmpp.internal.smack.ECFConnection;

/**
 * Extend the ECF XMPPChatRoomManager but add the ability to use a custom viz
 * chat room container.
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
public class XMPPChatRoomManager extends
        org.eclipse.ecf.internal.provider.xmpp.XMPPChatRoomManager {

    private static final String PROP_XMPP_SUBJECT = "subject";

    /**
     * Super has ecfConnection but it is private and we need it.
     */
    private ECFConnection ecfConnection = null;

    /**
     * Super has connectNamespace but it is private and we need it.
     */
    private Namespace connectNamespace = null;

    public XMPPChatRoomManager(ID containerID) {
        super(containerID);
    }

    /**
     * Overriden to so we can grab the ecfConnection and namespace
     */
    public void setConnection(Namespace connectNamespace, ID connectedID,
            ECFConnection connection) {
        super.setConnection(connectNamespace, connectedID, connection);
        this.connectNamespace = connectNamespace;
        this.ecfConnection = connection;
    }

    /**
     * Overriden to wrap the roomInfo in a custom roomInfo.
     */
    @Override
    protected IChatRoomInfo getChatRoomInfo(ID roomID) {
        IChatRoomInfo result = super.getChatRoomInfo(roomID);
        if (result != null) {
            result = new ECFRoomInfo(result);
        }
        return result;
    }

    /**
     * Overriden to wrap the roomInfo in a custom roomInfo.
     */
    @Override
    public IChatRoomInfo getChatRoomInfo(String roomname) {
        IChatRoomInfo result = super.getChatRoomInfo(roomname);
        if (result != null) {
            result = new ECFRoomInfo(result);
        } else if (ecfConnection != null) {
            // If the result is null then it is possible that ecf silently
            // disgarded an exception from xmpp, we would like some way to be
            // able to capture that exception so this code will
            // attempt to find the room itself and log errors.
            Exception exception = null;
            String mucName = null;
            try {
                XMPPConnection conn = ecfConnection.getXMPPConnection();
                XMPPRoomID roomID = new XMPPRoomID(connectNamespace, conn,
                        roomname);
                mucName = roomID.getMucString();
                RoomInfo info = MultiUserChat.getRoomInfo(conn, mucName);
                if (info != null) {
                    // Theoretically this will never hit but if it does there is
                    // no way of getting a super.ECFRoomInfo even
                    // though we know the room exists, so just log it.
                    System.err
                            .println("XMPPChatRoomManager cannot find info for room "
                                    + roomname + " but it exists.");
                }
            } catch (XMPPException e) {
                if (e.getXMPPError().getCode() != 404) {
                    // 404 is considered normal, everything else is bad
                    exception = e;
                }
            } catch (final Exception e) {
                exception = e;
            }
            if (exception != null) {
                System.err
                        .println("XMPPChatRoomManager cannot find info for room "
                                + roomname
                                + " when looking for "
                                + String.valueOf(mucName));
                exception.printStackTrace();
            }
        }
        return result;
    }

    /**
     * Overriden to use the same nickname that is used when joining.
     */
    @Override
    public IChatRoomInfo createChatRoom(String roomname, Map properties)
            throws ChatRoomCreateException {
        if (roomname == null)
            throw new ChatRoomCreateException(roomname,
                    Messages.XMPPChatRoomManager_EXCEPTION_ROOM_CANNOT_BE_NULL);
        try {
            String nickname = ecfConnection.getXMPPConnection().getUser();
            final String server = ecfConnection.getXMPPConnection().getHost();
            final String domain = (properties == null) ? XMPPRoomID.DOMAIN_DEFAULT
                    : (String) properties.get(PROP_XMPP_CONFERENCE);
            final String conference = XMPPRoomID.fixConferenceDomain(domain,
                    server);
            final String roomID = roomname + XMPPRoomID.AT_SIGN + conference;
            // create proxy to the room
            final MultiUserChat muc = new MultiUserChat(
                    ecfConnection.getXMPPConnection(), roomID);

            if (!checkRoom(conference, roomID)) {
                // otherwise create a new one

                /**
                 * This is the reason we override super.createChatRoom, when we
                 * join it only uses the username, not then host so we must do
                 * the same here or a user departed event is triggered for
                 * user@host when user arrives.
                 */
                if (nickname.contains("@")) {
                    nickname = nickname.split("@")[0];
                }
                muc.create(nickname);
                muc.sendConfigurationForm(new Form(Form.TYPE_SUBMIT));
                final String subject = (properties == null) ? null
                        : (String) properties.get(PROP_XMPP_SUBJECT);
                if (subject != null)
                    muc.changeSubject(subject);
            }

        } catch (final XMPPException e) {
            throw new ChatRoomCreateException(roomname, e.getMessage(), e);
        }
        return getChatRoomInfo(roomname);
    }

    /**
     * Custom roomInfo that wraps another roomInfo and overrides
     * createChatRoomContainer to return viz container.
     */
    class ECFRoomInfo implements IChatRoomInfo {

        IChatRoomInfo realInfo;

        public ECFRoomInfo(IChatRoomInfo realInfo) {
            this.realInfo = realInfo;
        }

        public String getDescription() {
            return realInfo.getDescription();
        }

        public String getSubject() {
            return realInfo.getSubject();
        }

        public ID getRoomID() {
            return realInfo.getRoomID();
        }

        public int getParticipantsCount() {
            return realInfo.getParticipantsCount();
        }

        public String getName() {
            return realInfo.getName();
        }

        public boolean isPersistent() {
            return realInfo.isPersistent();
        }

        public boolean requiresPassword() {
            return realInfo.requiresPassword();
        }

        public boolean isModerated() {
            return realInfo.isModerated();
        }

        public ID getConnectedID() {
            return realInfo.getConnectedID();
        }

        public Object getAdapter(Class adapter) {
            return realInfo.getAdapter(adapter);
        }

        /**
         * Adapted from the ECF version of ECFRoomInfo but changed to return our
         * custom container.
         */
        public IChatRoomContainer createChatRoomContainer()
                throws ContainerCreateException {
            XMPPChatRoomContainer chatContainer = null;
            if (ecfConnection == null)
                throw new ContainerCreateException(
                        Messages.XMPPChatRoomManager_EXCEPTION_CONTAINER_DISCONNECTED);
            try {
                chatContainer = new XMPPChatRoomContainer(ecfConnection,
                        connectNamespace);
                addChat(chatContainer);
                return chatContainer;
            } catch (final IDCreateException e) {
                throw new ContainerCreateException(
                        Messages.XMPPChatRoomManager_EXCEPTION_CREATING_CHAT_CONTAINER,
                        e);
            }
        }

        public String toString() {
            return realInfo.toString();
        }
    }

}
