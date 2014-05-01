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
package com.raytheon.uf.viz.collaboration.comm.xmpp;

import java.io.IOException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.ecf.core.ContainerConnectException;
import org.eclipse.ecf.core.events.ContainerConnectedEvent;
import org.eclipse.ecf.core.events.ContainerConnectingEvent;
import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.security.IConnectContext;
import org.eclipse.ecf.core.sharedobject.SharedObjectAddException;
import org.eclipse.ecf.core.user.User;
import org.eclipse.ecf.core.util.ECFException;
import org.eclipse.ecf.internal.provider.xmpp.Messages;
import org.eclipse.ecf.internal.provider.xmpp.XmppPlugin;
import org.eclipse.ecf.internal.provider.xmpp.smack.ECFConnectionObjectPacketEvent;
import org.eclipse.ecf.presence.chatroom.IChatRoomContainer;
import org.eclipse.ecf.presence.chatroom.IChatRoomManager;
import org.eclipse.ecf.presence.im.IChatManager;
import org.eclipse.ecf.presence.roster.IRosterManager;
import org.eclipse.ecf.provider.comm.AsynchEvent;
import org.eclipse.ecf.provider.comm.ConnectionCreateException;
import org.eclipse.ecf.provider.comm.ISynchAsynchConnection;
import org.eclipse.ecf.provider.generic.ContainerMessage;
import org.eclipse.osgi.util.NLS;
import org.jivesoftware.smack.XMPPException;

import com.raytheon.uf.viz.collaboration.comm.xmpp.internal.XMPPChatRoomContainer;
import com.raytheon.uf.viz.collaboration.comm.xmpp.internal.XMPPChatRoomManager;
import com.raytheon.uf.viz.collaboration.comm.xmpp.internal.XMPPContainerPresenceHelper;
import com.raytheon.uf.viz.collaboration.comm.xmpp.internal.smack.ECFConnection;

/**
 * Extends the ECF XMPPContainer but overrides any methods which use the
 * chatRoomManager or containerPresenceHelper to use viz specific instances.
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
public class XMPPContainer extends org.eclipse.ecf.provider.xmpp.XMPPContainer {

    /**
     * custom chat room manager.
     */
    XMPPChatRoomManager chatRoomManager = null;

    XMPPContainerPresenceHelper presenceHelper = null;

    public XMPPContainer() throws Exception {
        this(DEFAULT_KEEPALIVE);
    }

    public XMPPContainer(int ka) throws Exception {
        super(ka);
        chatRoomManager = new XMPPChatRoomManager(getID());
        presenceHelper = new XMPPContainerPresenceHelper(this);
    }

    public XMPPContainer(String userhost, int ka) throws Exception {
        super(userhost, ka);
        chatRoomManager = new XMPPChatRoomManager(getID());
        presenceHelper = new XMPPContainerPresenceHelper(this);
    }

    /**
     * Overridden to return custom chatRoomManager
     */
    @Override
    public IChatRoomManager getChatRoomManager() {
        return chatRoomManager;
    }

    /**
     * Overridden to use rosterManager from custom presenceHelper
     */
    @Override
    public IRosterManager getRosterManager() {
        return presenceHelper.getRosterManager();
    }

    /**
     * Overridden to use chatManager from custom presenceHelper
     */
    @Override
    public IChatManager getChatManager() {
        return presenceHelper.getChatManager();
    }

    /**
     * Overridden to disconnect custom presenceHelper and connect our own
     * instead. Also takes the functionality from the connect method in
     * ClientSOContainer so that we can complete all the necessary functionality
     * of the super's super method in our own method. The super method
     * originally called super with a different presence helper id and that
     * would break part of our functionality
     */
    @Override
    public void connect(ID remote, IConnectContext joinContext)
            throws ContainerConnectException {
        try {
            getSharedObjectManager().addSharedObject(presenceHelperID,
                    presenceHelper, null);

            // following taken from
            // ClientSOContainer.connect(ID,IConnectContext);
            try {
                if (isClosing)
                    throw new IllegalStateException("Container closing"); //$NON-NLS-1$
                if (remote == null)
                    throw new ContainerConnectException(
                            "targetID cannot be null"); //$NON-NLS-1$
                Object response = null;
                synchronized (getConnectLock()) {
                    // Throw if already connected
                    if (isConnected())
                        throw new IllegalStateException(
                                "Container already connected connectedID=" + getConnectedID()); //$NON-NLS-1$
                    // Throw if connecting
                    if (isConnecting())
                        throw new IllegalStateException("Container connecting"); //$NON-NLS-1$
                    // else we're entering connecting state
                    // first notify synchonously
                    final ISynchAsynchConnection aConnection = createConnection(
                            remote, joinContext);
                    setStateConnecting(aConnection);

                    fireContainerEvent(new ContainerConnectingEvent(
                            this.getID(), remote, joinContext));

                    final Object connectData = getConnectData(remote,
                            joinContext);
                    final int connectTimeout = getConnectTimeout();

                    synchronized (aConnection) {

                        try {
                            // Make connect call
                            response = aConnection.connect(remote, connectData,
                                    connectTimeout);
                        } catch (final ECFException e) {
                            if (getConnection() != aConnection)
                                disconnect(aConnection);
                            else
                                setStateDisconnected(aConnection);
                            throw e;
                        }
                        // If not in correct state, disconnect and return
                        if (getConnection() != aConnection) {
                            disconnect(aConnection);
                            throw new IllegalStateException(
                                    "Container connect failed because not in correct state"); //$NON-NLS-1$
                        }
                        ID serverID = null;
                        try {
                            serverID = handleConnectResponse(remote, response);
                        } catch (final Exception e) {
                            setStateDisconnected(aConnection);
                            throw e;
                        }
                        setStateConnected(serverID, aConnection);
                        // notify listeners
                        fireContainerEvent(new ContainerConnectedEvent(
                                this.getID(), remoteServerID));
                        aConnection.start();
                    }
                }
            } catch (XMPPException e) {
                throw new ContainerConnectException(e.getMessage(), e);
            } catch (final ECFException e) {
                final IStatus s = e.getStatus();
                throw new ContainerConnectException(s.getMessage(),
                        s.getException());
            } catch (final Exception e) {
                throw new ContainerConnectException(e.getLocalizedMessage(), e);
            }

            XmppPlugin.getDefault().registerService(this);
        } catch (final ContainerConnectException e) {
            disconnect();
            throw e;
        } catch (final SharedObjectAddException e1) {
            disconnect();
            throw new ContainerConnectException(NLS.bind(
                    Messages.XMPPContainer_EXCEPTION_ADDING_SHARED_OBJECT,
                    presenceHelperID), e1);
        }
        // end ClientSOContainer code

        getSharedObjectManager().removeSharedObject(presenceHelperID);
        try {
            getSharedObjectManager().addSharedObject(presenceHelperID,
                    presenceHelper, null);
        } catch (SharedObjectAddException e) {
            disconnect();
            throw new ContainerConnectException(NLS.bind(
                    Messages.XMPPContainer_EXCEPTION_ADDING_SHARED_OBJECT,
                    presenceHelperID), e);
        }
    }

    /**
     * Need to override this method from ClientSOContainer so that our connect
     * method can set the connection and the connectionState and the
     * remoteServerID
     * 
     * Taken from ECF.
     * 
     * @param serverID
     * @param conn
     */
    private void setStateConnected(ID serverID, ISynchAsynchConnection conn) {
        connectionState = CONNECTED;
        connection = conn;
        remoteServerID = serverID;
    }

    /**
     * Need to override this method out of ClientSOContainer so that our connect
     * method can set the connectionState and disconnect the connection
     * 
     * Taken from ECF
     * 
     */
    private void setStateDisconnected(ISynchAsynchConnection conn) {
        disconnect(conn);
        connectionState = DISCONNECTED;
        connection = null;
        remoteServerID = null;
    }

    /**
     * Need to override this method out of ClientSOContainer so that our connect
     * method can set the connectionState and the connection
     * 
     * Taken from ECF
     * 
     * @param conn
     */
    private void setStateConnecting(ISynchAsynchConnection conn) {
        connectionState = CONNECTING;
        connection = conn;
    }

    /**
     * Overridden to disconnect custom chatRoomManager and custom presenceHelper
     */
    @Override
    public void disconnect() {
        super.disconnect();
        chatRoomManager.setConnection(null, null, null);
        presenceHelper.disconnect();
    }

    /**
     * Overridden to return custom chatRoomManager
     */
    @Override
    public void dispose() {
        super.dispose();
        chatRoomManager.dispose();
    }

    /**
     * Overriden to set connection in custom chatRoomManager and to set user in
     * custom presenceHelper
     */
    @Override
    protected ID handleConnectResponse(ID originalTarget, Object serverData)
            throws Exception {
        ID result = super.handleConnectResponse(originalTarget, serverData);
        chatRoomManager.setConnection(getConnectNamespace(), originalTarget,
                getECFConnection());
        presenceHelper.setUser(new User(originalTarget));
        return result;
    }

    /**
     * Overriden to cast ECFConnection to viz implementation
     */
    public ECFConnection getECFConnection() {
        return (ECFConnection) super.getConnection();
    }

    /**
     * Overriden to create viz ECFConnection
     */
    @Override
    protected ISynchAsynchConnection createConnection(ID remoteSpace,
            Object data) throws ConnectionCreateException {
        final boolean google = isGoogle(remoteSpace);
        return new ECFConnection(google, getConnectNamespace(), receiver);
    }

    /**
     * Adapted from super.processAsynch to use custom chatRoomManager for chat
     * events.
     */
    @Override
    protected void processAsynch(AsynchEvent e) {
        try {
            if (e instanceof ECFConnectionObjectPacketEvent) {
                // It's an ECF object message
                final ECFConnectionObjectPacketEvent evt = (ECFConnectionObjectPacketEvent) e;
                final Object obj = evt.getObjectValue();
                // this should be a ContainerMessage
                final Object cm = deserializeContainerMessage((byte[]) obj);
                if (cm == null)
                    super.processAsynch(e);
                final ContainerMessage contMessage = (ContainerMessage) cm;
                final IChatRoomContainer chat = chatRoomManager
                        .findReceiverChatRoom(contMessage.getToContainerID());
                if (chat != null && chat instanceof XMPPChatRoomContainer) {
                    final XMPPChatRoomContainer cont = (XMPPChatRoomContainer) chat;
                    cont.handleContainerMessage(contMessage);
                    return;
                }
                super.processAsynch(e);
            }
        } catch (final IOException except) {
            log(NLS.bind(Messages.XMPPContainer_EXCEPTION_HANDLING_ASYCH_EVENT,
                    e), except);
        }
        super.processAsynch(e);
    }

}
