package com.raytheon.uf.viz.collaboration.ui.session;

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

import java.util.List;

import org.eclipse.ecf.presence.IPresence.Type;
import org.eclipse.ecf.presence.roster.IRosterEntry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IPeerToPeer;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageListener;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class PeerToPeerView extends AbstractSessionView {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PeerToPeerView.class);

    private static final String PEER_TO_PEER_IMAGE_NAME = "chats.gif";

    public static final String ID = "com.raytheon.uf.viz.collaboration.PeerToPeerView";

    private static Color userColor = null;

    private static Color chatterColor = null;

    private static Color black = null;

    protected IMessageListener messageListener;

    private IQualifiedID peer;

    private boolean online = true;

    public PeerToPeerView() {
        super();
        userColor = Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE);
        chatterColor = Display.getCurrent().getSystemColor(SWT.COLOR_RED);
        black = Display.getCurrent().getSystemColor(SWT.COLOR_BLACK);
        CollaborationConnection.getConnection().registerEventHandler(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#dispose
     * ()
     */
    @Override
    public void dispose() {
        CollaborationConnection conn = CollaborationConnection.getConnection();
        if (conn != null) {
            conn.unregisterEventHandler(this);
        }
        super.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * populateSashForm(org.eclipse.swt.custom.SashForm)
     */
    protected void populateSashForm(SashForm sashForm) {
        super.populateSashForm(sashForm);
        sashForm.setWeights(new int[] { 20, 5 });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * setMessageLabel(org.eclipse.swt.widgets.Label)
     */
    protected void setMessageLabel(Composite comp) {
        // no message needed as there is no subject and we know that it is
        // private based on the fact that there are no participants
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#sendMessage
     * ()
     */
    public void sendMessage() {
        String message = getComposedMessage();
        if (message.length() > 0) {
            try {
                CollaborationConnection connection = CollaborationConnection
                        .getConnection();
                if (online) {
                    appendMessage(connection.getUser(),
                            System.currentTimeMillis(), message);
                    IPeerToPeer p2p = (IPeerToPeer) connection
                            .getPeerToPeerSession();
                    p2p.sendPeerToPeer(peer, message);
                } else {
                    appendMessage(connection.getUser(),
                            System.currentTimeMillis(), message);
                    StringBuilder builder = new StringBuilder();
                    builder.append("Unable to send message. User is not online.");
                    sendErrorMessage(builder);
                }
            } catch (CollaborationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to send message to " + peer.getName(), e);
            }
        }
    }

    protected void styleAndAppendText(StringBuilder sb, int offset,
            String name, UserId userId, List<StyleRange> ranges) {
        Color color = null;
        if (userId == null) {
            color = black;
        } else if (!userId.equals(CollaborationConnection.getConnection()
                .getUser())) {
            color = chatterColor;
        } else {
            color = userColor;
        }
        styleAndAppendText(sb, offset, name, userId, ranges, color);
    };

    public void styleAndAppendText(StringBuilder sb, int offset, String name,
            UserId userId, List<StyleRange> ranges, Color color) {
        StyleRange range = new StyleRange(messagesText.getCharCount(), offset,
                color, null, SWT.NORMAL);
        ranges.add(range);
        if (userId != null) {
            range = new StyleRange(messagesText.getCharCount() + offset,
                    name.length() + 1, color, null, SWT.BOLD);
        } else {
            range = new StyleRange(messagesText.getCharCount() + offset,
                    sb.length() - offset, color, null, SWT.BOLD);
        }
        ranges.add(range);
        messagesText.append(sb.toString());
        for (StyleRange newRange : ranges) {
            messagesText.setStyleRange(newRange);
        }
        messagesText.setTopIndex(messagesText.getLineCount() - 1);
    }

    protected String getSessionImageName() {
        return PEER_TO_PEER_IMAGE_NAME;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * getSessionName()
     */
    @Override
    protected String getSessionName() {
        for (UserId id : getUserIds()) {
            if (id.equals(peer)) {
                return id.getAlias();
            }
        }
        return getViewSite().getSecondaryId();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * getMessageArchive()
     */
    @Override
    protected SessionMsgArchive getMessageArchive() {
        UserId me = CollaborationConnection.getConnection().getUser();
        return new SessionMsgArchive(me.getHost(), me.getName(), peer.getName());
    }

    public void setPeer(IQualifiedID peer) {
        this.peer = peer;
    }

    public IQualifiedID getPeer() {
        return peer;
    }

    @Subscribe
    public void handleModifiedPresence(IRosterEntry entry) {
        UserId id = IDConverter.convertFrom(entry.getUser());
        if (id.equals(peer)) {
            if (entry.getPresence().getType() == Type.UNAVAILABLE) {
                online = false;
            } else {
                online = true;
            }
        }
    }
}
