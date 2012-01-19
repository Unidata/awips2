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

package com.raytheon.viz.drawing.collaboration;

import java.util.Iterator;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.PlatformUI;
import org.jivesoftware.smack.ConnectionListener;
import org.jivesoftware.smack.PacketCollector;
import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.filter.PacketTypeFilter;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Mode;
import org.jivesoftware.smackx.muc.MultiUserChat;

import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.drawing.DrawingIO;
import com.raytheon.viz.drawing.DrawingLayer;
import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Contains collaboration communication code
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date         Ticket#     Engineer    Description
 *     ------------ ----------  ----------- --------------------------
 *     Nov 21, 2006 66          chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */

public class CollaborationManager extends Job {
    /** The tag which indicates whether a message is a drawing object */
    private static final String DRAWING_MESSAGE = "CAVE-DRAWING";

    /** The instance of the collaboration manager */
    private static CollaborationManager instance;

    /** The username */
    private final String username;

    /** The connection to the server */
    private final XMPPConnection connection;

    /** The multi user chat room */
    private final MultiUserChat groupChat;

    /** The drawing layer associated with collaboration */
    private final DrawingLayer layer;

    /** Whether to keep fielding messages */
    private boolean runEventLoop;

    /**
     * Private constructor
     * 
     * @param hostname
     * @param username
     * @param password
     */
    private CollaborationManager(DrawingLayer layer, String hostname,
            String username, String password) throws XMPPException {
        super("CollaborationJob");

        this.username = username;
        this.layer = layer;

        connection = new XMPPConnection(hostname);
        connection.connect();
        connection.login(username, password);

        groupChat = new MultiUserChat(connection, "collaboration@conference."
                + hostname);

        groupChat.join(username);

        groupChat.changeAvailabilityStatus("Available", Mode.available);

    }

    /**
     * Get the list of users
     * 
     * @return
     */
    public String[] getUsers() {

        String[] users = new String[groupChat.getOccupantsCount()];
        Iterator<String> iterator = groupChat.getOccupants();
        int i = 0;
        while (iterator.hasNext()) {
            Object o = iterator.next();
            users[i] = (String) o;
            i++;
        }

        return users;
    }

    /**
     * Get the running instance
     * 
     * @return
     */
    public static CollaborationManager getInstance() {
        return instance;
    }

    /**
     * Returns true if the collaboration manager is running
     * 
     * @return
     */
    public static boolean isRunning() {
        if (instance == null)
            return false;
        return true;
    }

    /**
     * Connects to the collaboration server
     * 
     * @param hostname
     * @param username
     * @param password
     * @return
     */
    public static CollaborationManager connect(DrawingLayer layer,
            String hostname, String username, String password)
            throws XMPPException {
        if (instance != null) {
            instance.disconnect();
        }

        instance = new CollaborationManager(layer, hostname, username, password);
        instance.setSystem(true);
        instance.schedule();

        IPerspectiveDescriptor desc = PlatformUI
                .getWorkbench()
                .getPerspectiveRegistry()
                .findPerspectiveWithId(
                        "com.raytheon.viz.drawing.collaboration.CollaborationPerspective");

        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
                .setPerspective(desc);

        ChatWindow.getInstance().updateUsers();

        return instance;
    }

    /**
     * Create an account on the server
     * 
     * @param hostname
     * @param username
     * @param password
     * @throws XMPPException
     */
    public static void createAccount(String hostname, String username,
            String password) throws XMPPException {
        XMPPConnection connection = new XMPPConnection(hostname);
        connection.connect();
        connection.getAccountManager().createAccount(username, password);
        connection.disconnect();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
     * IProgressMonitor)
     */
    @Override
    protected IStatus run(IProgressMonitor monitor) {
        runEventLoop = true;
        connection.addConnectionListener(new ConnectionListener() {

            public void connectionClosed() {
                // don't care
            }

            public void connectionClosedOnError(final Exception arg0) {
                VizApp.runAsync(new Runnable() {

                    public void run() {
                        MessageBox mb = new MessageBox(VizWorkbenchManager
                                .getInstance().getCurrentWindow().getShell(),
                                SWT.ICON_ERROR);
                        mb.setMessage("Collaboration Connection Closed: "
                                + arg0.getMessage());
                        mb.open();
                    }

                });

            }

            public void reconnectingIn(int arg0) {
                // TODO Auto-generated method stub

            }

            public void reconnectionFailed(Exception arg0) {
                // TODO Auto-generated method stub

            }

            public void reconnectionSuccessful() {
                // TODO Auto-generated method stub

            }

        });

        groupChat.addParticipantListener(new PacketListener() {

            public void processPacket(final Packet arg0) {
                VizApp.runAsync(new Runnable() {
                    public void run() {
                        Presence p = (Presence) arg0;

                        if (p.getType() == Presence.Type.available) {
                            ChatWindow.getInstance()
                                    .addParticipant(p.getFrom());
                        } else if (p.getType() == Presence.Type.unavailable) {
                            ChatWindow.getInstance().removeParticipant(
                                    p.getFrom());
                        }
                    }

                });
            }

        });

        groupChat.addMessageListener(new PacketListener() {

            public void processPacket(final Packet arg0) {
                VizApp.runAsync(new Runnable() {
                    public void run() {
                        Message msg = (Message) arg0;
                        if (msg.getFrom().endsWith(username)) {
                            return;
                        }

                        Boolean bool = (Boolean) msg
                                .getProperty(DRAWING_MESSAGE);

                        if (bool == null || bool == false) {
                            ChatWindow.getInstance().addChatData(msg.getFrom(),
                                    msg.getBody());
                            return;
                        }

                        String txt = msg.getBody();
                        DrawingIO.handleDrawingChange(layer, txt);
                    }

                });
            }

        });

        PacketTypeFilter filter = new PacketTypeFilter(Message.class);
        PacketCollector collector = connection.createPacketCollector(filter);
        while (connection.isConnected() && runEventLoop) {
            final Packet p = collector.nextResult();
            if (p instanceof Message) {
                VizApp.runAsync(new Runnable() {

                    public void run() {
                        Message m = (Message) p;
                        if (m.getType() == Message.Type.chat) {
                            MessageBox mb = new MessageBox(VizWorkbenchManager
                                    .getInstance().getCurrentWindow()
                                    .getShell());
                            mb.setMessage(m.getFrom() + ":: " + m.getBody());
                            mb.open();
                        }
                    }
                });
            }
        }

        return Status.OK_STATUS;
    }

    /**
     * Send an XML collaboration message
     * 
     * @param msg
     *            the xml message
     * @throws XMPPException
     */
    public void sendCollabMessage(String msg) throws XMPPException {
        Message message = groupChat.createMessage();
        message.setProperty(DRAWING_MESSAGE, true);
        message.setBody(msg);
        groupChat.sendMessage(message);

    }

    /**
     * Send a text chat message
     * 
     * @param msg
     *            message to send
     * @throws XMPPException
     */
    public void sendChatMessage(String msg) throws XMPPException {
        Message message = groupChat.createMessage();
        message.setProperty(DRAWING_MESSAGE, false);
        message.setBody(msg);
        groupChat.sendMessage(message);

    }

    /**
     * Leave the collaboration (but stay connected)
     * 
     */
    public void leave() {
        groupChat.leave();
    }

    /**
     * Check to see if still connected
     * 
     * @return
     */
    public boolean isConnected() {
        return connection.isConnected();
    }

    /**
     * Disconnect from the chat session
     * 
     */
    public void disconnect() {
        if (groupChat != null) {
            groupChat.leave();
        }

        connection.disconnect();

    }

}
