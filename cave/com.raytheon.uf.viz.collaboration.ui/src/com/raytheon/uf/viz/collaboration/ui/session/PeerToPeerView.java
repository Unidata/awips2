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

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.List;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.jivesoftware.smack.packet.Presence.Type;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IPeerToPeer;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.RosterItem;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.actions.PrintLogActionContributionItem;

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
public class PeerToPeerView extends AbstractSessionView implements
        IPrintableView {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PeerToPeerView.class);

    private static final String PEER_TO_PEER_IMAGE_NAME = "chats.gif";

    public static final String ID = "com.raytheon.uf.viz.collaboration.PeerToPeerView";

    private static Color userColor = null;

    private static Color chatterColor = null;

    private static Color black = null;

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
    @Override
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
    @Override
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
    @Override
    public void sendMessage() {
        String message = getComposedMessage();
        if (message.length() > 0) {
            try {
                CollaborationConnection connection = CollaborationConnection
                        .getConnection();
                appendMessage(connection.getUser(), System.currentTimeMillis(),
                        message, null);
                if (online) {
                    IPeerToPeer p2p = (IPeerToPeer) connection
                            .getPeerToPeerSession();
                    p2p.sendPeerToPeer(peer, message);
                } else {
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

    @Override
    protected void styleAndAppendText(StringBuilder sb, int offset,
            String name, UserId userId, String subject, List<StyleRange> ranges) {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection == null) {
            return;
        }
        Color color = null;
        if (userId == null) {
            color = black;
        } else if (!userId.equals(connection.getUser())) {
            color = chatterColor;
        } else {
            color = userColor;
        }
        styleAndAppendText(sb, offset, name, userId, ranges, color);
    };

    @Override
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

    @Override
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
        if (peer == null) {
            return getViewSite().getSecondaryId();
        } else if (peer instanceof UserId) {
            return CollaborationConnection.getConnection().getContactsManager()
                    .getDisplayName((UserId) peer);
        } else {
            return peer.getFQName();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * getMessageArchive()
     */
    @Override
    protected SessionMsgArchive createMessageArchive() {
        UserId me = CollaborationConnection.getConnection().getUser();
        return new SessionMsgArchive(me.getHost(), me.getName(), peer.getName());
    }

    public void setPeer(IQualifiedID peer) {
        this.peer = peer;
        setPartName(getSessionName());
        initMessageArchive();
    }

    public IQualifiedID getPeer() {
        return peer;
    }

    @Subscribe
    public void handleModifiedPresence(RosterItem entry) {
        UserId id = entry.getId();
        if (id.equals(peer)) {
            if (entry.getPresence().getType() == Type.unavailable) {
                online = false;
            } else {
                online = true;
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * initComponents(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected void initComponents(Composite parent) {
        super.initComponents(parent);

        // unfortunately this code cannot be a part of createToolbarButton
        // because I cannot instantiate the ACI until after the messagesText
        // widget is instantiated which happens in initComponents
        IContributionItem printAction = new PrintLogActionContributionItem(this);
        ToolBarManager mgr = (ToolBarManager) getViewSite().getActionBars()
                .getToolBarManager();
        mgr.insert(mgr.getSize() - 1, printAction);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.IPrintableView#getStyledText
     * ()
     */
    @Override
    public StyledText getStyledText() {
        return messagesText;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.session.IPrintableView#getHeaderText
     * ()
     */
    @Override
    public String getHeaderText() {
        DateFormat dateFormatter = new SimpleDateFormat("dd MMM yyyy");
        return "Conversation session with user " + getSessionName()
                + ", Date: "
                + dateFormatter.format(msgArchive.getCreationTime());
    }
}
