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
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ColumnViewerToolTipSupport;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.window.ToolTip;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.jivesoftware.smack.packet.Presence;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ParticipantEventType;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue;
import com.raytheon.uf.viz.collaboration.comm.provider.event.UserNicknameChangedEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.event.VenueUserEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.session.VenueSession;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.collaboration.display.data.SessionColorManager;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.actions.PrintLogActionContributionItem;
import com.raytheon.uf.viz.collaboration.ui.prefs.CollabPrefConstants;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.sounds.SoundUtil;
import com.raytheon.viz.ui.views.CaveWorkbenchPageManager;

/**
 * The ViewPart of a text only room, contains methods that are used by the
 * shared display rooms and the feed room
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            rferrel     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * Dec 19, 2013 2563       bclement    reworked participant event logic
 * Jan 28, 2014 2698       bclement    removed venue info
 * Feb 13, 2014 2751       bclement    VenueParticipant refactor
 * Feb 18, 2014 2631       mpduff      Add processJoinAlert()
 * Feb 24, 2014 2632       mpduff      Move playSound to CollaborationUtils
 * Mar 05, 2014 2798       mpduff      Moved processJoinAlert() call from participantHandler
 *                                         to participantArrived.
 * Mar 06, 2014 2751       bclement    moved users table refresh logic to refreshParticipantList()
 * Mar 06, 2014 2848       bclement    get venueName directly from session
 * Mar 11, 2014 #2865      lvenable    Added null checks in threads
 * Mar 28, 2014 #2960      lvenable    Added check to make sure the SashForm is not getting
 *                                     negative weights - set to zero if negative.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public class SessionView extends AbstractSessionView<VenueParticipant>
        implements IPrintableView {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SessionView.class);

    private static final String SESSION_IMAGE_NAME = "chats.gif";

    public static final String ID = "com.raytheon.uf.viz.collaboration.SessionView";

    protected TableViewer usersTable;

    protected CLabel participantsLabel;

    protected String sessionId;

    protected IVenueSession session;

    private Image downArrow;

    private Image rightArrow;

    private Image highlightedRightArrow;

    private Image highlightedDownArrow;

    protected Action chatAction;

    protected SessionColorManager colorManager;

    protected Map<RGB, Color> mappedColors;

    public SessionView() {
        super();
    }

    @Override
    public void createPartControl(Composite parent) {
        super.createPartControl(parent);
        createActions();
        createContextMenu();
        mappedColors = new HashMap<RGB, Color>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * initComponents(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected void initComponents(Composite parent) {
        initColorManager();
        super.initComponents(parent);

        // unfortunately this code cannot be a part of createToolbarButton
        // because I cannot instantiate the ACI until after the messagesText
        // widget is instantiated which happens in initComponents
        IContributionItem printAction = new PrintLogActionContributionItem(this);
        ToolBarManager mgr = (ToolBarManager) getViewSite().getActionBars()
                .getToolBarManager();
        mgr.add(printAction);
    }

    @Override
    protected void populateSashForm(SashForm sashForm) {
        createArrows();
        createUsersComp(sashForm);
        super.populateSashForm(sashForm);
        sashForm.setWeights(new int[] { 1, 20, 5 });
    }

    protected void createActions() {
        chatAction = new Action("Chat") {
            @Override
            public void run() {
                try {
                    ISession session = CollaborationConnection.getConnection()
                            .getPeerToPeerSession();
                    CaveWorkbenchPageManager.getActiveInstance().showView(
                            PeerToPeerView.ID, session.getSessionId(),
                            IWorkbenchPage.VIEW_ACTIVATE);
                } catch (PartInitException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to open chat", e);
                } catch (CollaborationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        };
    }

    /**
     * 
     */
    private void createContextMenu() {
        MenuManager menuManager = new MenuManager();
        menuManager.setRemoveAllWhenShown(true);
        menuManager.addMenuListener(new IMenuListener() {
            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.jface.action.IMenuListener#menuAboutToShow(org.eclipse
             * .jface.action.IMenuManager)
             */
            @Override
            public void menuAboutToShow(IMenuManager manager) {
                fillContextMenu(manager);
            }
        });
        Menu menu = menuManager.createContextMenu(usersTable.getControl());
        usersTable.getControl().setMenu(menu);
        getSite().registerContextMenu(menuManager, usersTable);
    }

    protected void fillContextMenu(IMenuManager manager) {
    }

    @Subscribe
    public void handleMessage(IMessage message) {
        final IMessage msg = message;

        boolean isHistory = isHistory(msg);
        // so not to have delay, going to handle messages from yourself
        // separately
        // unless it is history, then you want to show them
        if (isSelf(msg, isHistory)) {
            return;
        }
        appendMessage(msg);

    }

    protected void initColorManager() {
        colorManager = new SessionColorManager();
    }

    protected void createUsersComp(final Composite parent) {
        Composite comp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        comp.setLayout(layout);
        comp.setLayoutData(data);

        participantsLabel = new CLabel(comp, SWT.NONE);
        layout = new GridLayout(1, false);
        data = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        participantsLabel.setLayout(layout);
        participantsLabel.setLayoutData(data);
        participantsLabel.setText("Participants");
        participantsLabel.setImage(rightArrow);
        participantsLabel.setToolTipText("Select to show participants...");

        final Composite usersComp = new Composite(comp, SWT.NONE);
        layout = new GridLayout(1, false);
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        usersComp.setVisible(false);
        layout.marginWidth = 0;
        usersComp.setLayout(layout);
        usersComp.setLayoutData(data);

        participantsLabel.addMouseTrackListener(new MouseTrackAdapter() {
            @Override
            public void mouseEnter(MouseEvent e) {
                if (usersComp.getVisible()) {
                    participantsLabel.setImage(highlightedDownArrow);
                } else {
                    participantsLabel.setImage(highlightedRightArrow);
                }
            }

            @Override
            public void mouseExit(MouseEvent e) {
                if (usersComp.getVisible()) {
                    participantsLabel.setImage(downArrow);
                } else {
                    participantsLabel.setImage(rightArrow);
                }
            }
        });
        participantsLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                GridData data = ((GridData) usersComp.getLayoutData());
                data.exclude = !data.exclude;
                usersComp.setVisible(!data.exclude);

                usersComp.layout();
                int[] weights = ((SashForm) parent).getWeights();
                if (!usersComp.getVisible()) {
                    int val = weights[0] + weights[1] + weights[2];
                    val = (int) Math.ceil((val / 26.0));
                    weights[1] = weights[0] + weights[1] - 1;
                    weights[0] = val;
                    participantsLabel.setImage(rightArrow);
                    participantsLabel
                            .setToolTipText("Select to show participants...");
                } else {
                    // fix this to make up for possible negative values TODO XXX
                    int val = usersComp.computeSize(SWT.DEFAULT, SWT.DEFAULT).y
                            + participantsLabel.getBounds().height;
                    double percentage = ((double) val)
                            / (double) parent.getSize().y;
                    // not greater than 50% of view when popping out
                    if (percentage > 0.5) {
                        percentage = 0.5;
                    }
                    int weight = weights[0] + weights[1] + weights[2];
                    double tmp = weight * percentage;
                    weights[1] = (int) (weights[1] - tmp);
                    weights[0] = (int) (weights[0] + tmp);
                    participantsLabel.setImage(downArrow);
                    participantsLabel
                            .setToolTipText("Select to hide participants...");
                }

                // If the weight value is negative then make it zero since it
                // cannot have a negative number.
                for (int i = 0; i < weights.length; i++) {
                    if (weights[i] < 0) {
                        weights[i] = 0;
                    }
                }

                ((SashForm) parent).setWeights(weights);
                parent.layout();
            }
        });

        usersTable = new TableViewer(usersComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL | SWT.H_SCROLL);
        layout = new GridLayout(1, false);
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        usersTable.getTable().setLayout(layout);
        usersTable.getTable().setLayoutData(data);

        ParticipantsLabelProvider labelProvider = new ParticipantsLabelProvider();
        setParticipantValues(labelProvider);
        usersTable.setContentProvider(ArrayContentProvider.getInstance());

        usersTable.setLabelProvider(labelProvider);
        usersTable.setSorter(new ViewerSorter() {
            @Override
            public int compare(Viewer viewer, Object e1, Object e2) {
                VenueParticipant c1 = (VenueParticipant) e1;
                VenueParticipant c2 = (VenueParticipant) e1;

                return c1.getHandle().compareTo(c2.getHandle());
            }
        });

        ColumnViewerToolTipSupport.enableFor(usersTable, ToolTip.RECREATE);
        // TODO this needs to be a private chat through the muc
        // usersTable.addDoubleClickListener(new IDoubleClickListener() {
        // @Override
        // public void doubleClick(DoubleClickEvent event) {
        // StructuredSelection selection = (StructuredSelection) usersTable
        // .getSelection();
        //
        // Object o = selection.getFirstElement();
        // if (o instanceof UserId) {
        // new PeerToPeerChatAction((UserId) o).run();
        // }
        // }
        // });

        if (session != null) {
            refreshParticipantList();
        } else {
            // session was null, why this would happen we don't know but this
            // will somewhat gracefully let the user know a problem occurred and
            // will not let them do anything in this view
            statusHandler.handle(Priority.PROBLEM,
                    "Session was null, which means a major problem occurred");
            participantsLabel.setEnabled(false);
            participantsLabel.setForeground(Display.getCurrent()
                    .getSystemColor(SWT.COLOR_DARK_GRAY));
            comp.setEnabled(false);
        }
        ((GridData) usersComp.getLayoutData()).exclude = true;
    }

    protected void setParticipantValues(ParticipantsLabelProvider labelProvider) {
        labelProvider.setSessionId(sessionId);
        labelProvider.setManager(colorManager);
    }

    @Override
    public void dispose() {
        // dispose of the images first
        disposeArrow(highlightedDownArrow);
        disposeArrow(highlightedRightArrow);
        disposeArrow(downArrow);
        disposeArrow(rightArrow);

        if (mappedColors != null) {
            for (Color col : mappedColors.values()) {
                col.dispose();
            }
            mappedColors.clear();
        }
        if (colorManager != null) {
            colorManager.clearColors();
        }

        // clean up event handlers
        session.unregisterEventHandler(this);
        session.close();
        CollaborationConnection conn = CollaborationConnection.getConnection();
        if (conn != null) {
            conn.unregisterEventHandler(this);
        }

        super.dispose();
    }

    private void disposeArrow(Image image) {
        if (image != null && !image.isDisposed()) {
            image.dispose();
        }
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
                appendMessage(session.getUserID(), System.currentTimeMillis(),
                        message, null);
                session.sendChatMessage(message);
            } catch (CollaborationException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * styleAndAppendText(java.lang.StringBuilder, int, java.lang.String,
     * java.lang.String, java.util.List)
     */
    @Override
    protected void styleAndAppendText(StringBuilder sb, int offset,
            String name, VenueParticipant userId, String subject,
            List<StyleRange> ranges) {
        RGB rgb = colorManager.getColorForUser(userId);
        if (mappedColors.get(rgb) == null) {
            Color col = new Color(Display.getCurrent(), rgb);
            mappedColors.put(rgb, col);
        }
        styleAndAppendText(sb, offset, name, userId, ranges,
                mappedColors.get(rgb));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * styleAndAppendText(java.lang.StringBuilder, int, java.lang.String,
     * com.raytheon.uf.viz.collaboration.comm.provider.user.UserId,
     * java.util.List, org.eclipse.swt.graphics.Color)
     */
    @Override
    protected void styleAndAppendText(StringBuilder sb, int offset,
            String name, VenueParticipant userId, List<StyleRange> ranges,
            Color color) {
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

    public String getRoom() {
        return sessionId;
    }

    @Override
    protected String getSessionImageName() {
        return SESSION_IMAGE_NAME;
    }

    private void createArrows() {
        int imgWidth = 11;
        int imgHeight = 11;

        rightArrow = new Image(Display.getCurrent(), imgWidth, imgHeight);
        downArrow = new Image(Display.getCurrent(), imgWidth, imgHeight);
        highlightedRightArrow = new Image(Display.getCurrent(), imgWidth,
                imgHeight);
        highlightedDownArrow = new Image(Display.getCurrent(), imgWidth,
                imgHeight);

        // the right arrow
        GC gc = new GC(rightArrow);
        drawArrowImage(gc, imgWidth, imgHeight, false, false);

        // the down arrow
        gc = new GC(downArrow);
        drawArrowImage(gc, imgWidth, imgHeight, true, false);

        // the down arrow
        gc = new GC(highlightedRightArrow);
        drawArrowImage(gc, imgWidth, imgHeight, false, true);

        // the down arrow
        gc = new GC(highlightedDownArrow);
        drawArrowImage(gc, imgWidth, imgHeight, true, true);

        gc.dispose();
    }

    private void drawArrowImage(GC gc, int imgWidth, int imgHeight,
            boolean down, boolean fill) {
        gc.setAntialias(SWT.ON);
        // "Erase" the canvas by filling it in with a rectangle.
        gc.setBackground(Display.getDefault().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
        gc.fillRectangle(0, 0, imgWidth, imgHeight);
        gc.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_BLACK));
        int[] polyArray = null;
        if (down) {
            polyArray = new int[] { 2, 3, 5, 6, 8, 3 };
        } else {
            polyArray = new int[] { 3, 2, 6, 5, 3, 8 };
        }
        if (fill) {
            gc.fillPolygon(polyArray);
        } else {
            gc.drawPolygon(polyArray);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * setMessageLabel(org.eclipse.swt.widgets.Label)
     */
    @Override
    protected void setMessageLabel(Composite comp) {
        Label label = new Label(comp, SWT.WRAP);
        GridData data = new GridData(SWT.FILL, SWT.NONE, true, false);
        label.setLayoutData(data);
        StringBuilder labelInfo = new StringBuilder();
        if (session != null) {
            String subject = session.getVenue().getSubject();
            labelInfo.append(subject);
            label.setToolTipText(subject);
        }
        label.setText(labelInfo.toString());
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite)
     */
    @Override
    public void init(IViewSite site) throws PartInitException {
        super.init(site);
        this.sessionId = site.getSecondaryId();
        this.session = (IVenueSession) CollaborationConnection.getConnection()
                .getSession(this.sessionId);
        initMessageArchive();

        CollaborationConnection.getConnection().registerEventHandler(this);

        session.registerEventHandler(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * getSessionName()
     */
    @Override
    protected String getSessionName() {
        if (session == null) {
            return sessionId;
        }
        return session.getVenueName();
    }

    @Subscribe
    public void userEventHandler(VenueUserEvent event) {
        sendSystemMessage(new StringBuilder(event.getMessage()));
    }

    @Subscribe
    public void participantHandler(IVenueParticipantEvent event)
            throws Exception {

        final ParticipantEventType type = event.getEventType();
        final Presence presence = event.getPresence();
        final VenueParticipant participant = event.getParticipant();
        final String description = event.getEventDescription();
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (!session.isConnected()
                        || usersTable.getTable().isDisposed()) {
                    return;
                }
                switch (type) {
                case ARRIVED:
                    participantArrived(participant, description);
                    break;
                case DEPARTED:
                    participantDeparted(participant, description);
                    break;
                case PRESENCE_UPDATED:
                    participantPresenceUpdated(participant, presence);
                    break;
                case UPDATED:
                    if (usersTable.getTable().isDisposed() == false) {
                        usersTable.refresh();
                    }
                    if (description != null) {
                        sendParticipantSystemMessage(participant, description);
                    }
                    break;
                default:
                    System.err.println("Unknown Event type");
                }
            }
        });
    }

    /**
     * get an updated list of participants from session and refresh usersTable
     */
    protected void refreshParticipantList() {
        IVenue venue = session.getVenue();
        Collection<VenueParticipant> participants = venue.getParticipants();
        if (session.isAdmin()) {
            for (VenueParticipant p : participants) {
                if (!p.hasActualUserId()) {
                    p.setUserid(venue.getParticipantUserid(p));
                }
            }
        }
        usersTable.setInput(participants);
        usersTable.refresh();
    }

    @Subscribe
    public void userNicknameChanged(UserNicknameChangedEvent e) {
        refreshParticipantList();
    }

    /**
     * Update participant list and notify user that new participant joined chat
     * 
     * @param participant
     */
    protected void participantArrived(VenueParticipant participant,
            String description) {
        refreshParticipantList();
        String message = description != null ? description
                : "has entered the room.";
        sendParticipantSystemMessage(participant, message);
        processJoinAlert();
    }

    /**
     * Update participant list and notify user that new participant left chat
     * 
     * @param participant
     */
    protected void participantDeparted(VenueParticipant participant,
            String description) {
        refreshParticipantList();
        String message = description != null ? description
                : "has left the room.";
        sendParticipantSystemMessage(participant, message);
    }

    /**
     * Send message about about participant. Message is in the form of a
     * statement pertaining to the participant. For example, to get the output
     * "Susan was kicked", you would provide Susan's UserId and the message
     * "was kicked".
     * 
     * @param participant
     * @param message
     */
    protected void sendParticipantSystemMessage(VenueParticipant participant,
            String message) {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection != null) {
            String name = getDisplayName(participant);

            StringBuilder builder = new StringBuilder(name);
            builder.append(" ").append(message);
            sendSystemMessage(builder);
        }
    }

    /**
     * @param participant
     * @param presence
     */
    protected void participantPresenceUpdated(VenueParticipant participant,
            Presence presence) {
        usersTable.refresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * getMessageArchive()
     */
    @Override
    protected SessionMsgArchive createMessageArchive() {
        String sessionName = getSessionName();
        UserId me = CollaborationConnection.getConnection().getUser();
        return new SessionMsgArchive(me.getHost(), me.getName(), sessionName);
    }

    protected boolean isHistory(IMessage message) {
        return VenueSession.SEND_HISTORY.equals(message.getStatus());
    }

    protected boolean isSelf(IMessage message, boolean isHistory) {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        boolean self = message.getFrom().equals(connection.getUser())
                && !isHistory;
        return self;
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
        return "Conversation session from room " + getSessionName()
                + ", Date: "
                + dateFormatter.format(msgArchive.getCreationTime());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * getDisplayName
     * (com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID)
     */
    @Override
    protected String getDisplayName(VenueParticipant userId) {
        return userId.getHandle();
    }

    /**
     * Process a room join alert.
     */
    protected void processJoinAlert() {
        boolean enabled = Activator
                .getDefault()
                .getPreferenceStore()
                .getBoolean(
                        CollabPrefConstants.ENABLE_JOIN_EVENTS_FIELD_EDITOR_ID);
        if (enabled) {
            SoundUtil.playSound(getJoinFile());
        }
    }
}
