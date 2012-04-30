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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ColumnViewerToolTipSupport;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.window.ToolTip;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyleRange;
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
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ParticipantEventType;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.IRosterEntry;
import com.raytheon.uf.viz.collaboration.comm.provider.roster.RosterEntry;
import com.raytheon.uf.viz.collaboration.comm.provider.roster.RosterItem;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.ui.SessionColorManager;
import com.raytheon.uf.viz.core.VizApp;

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
public class SessionView extends AbstractSessionView {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SessionView.class);

    private static final String SESSION_IMAGE_NAME = "chats.gif";

    public static final String ID = "com.raytheon.uf.viz.collaboration.SessionView";

    protected TableViewer usersTable;

    protected String sessionId;

    private IVenueSession session;

    private Image downArrow;

    private Image rightArrow;

    private Image highlightedRightArrow;

    private Image highlightedDownArrow;

    protected Action chatAction;

    protected SessionColorManager manager;

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
        CollaborationDataManager.getInstance().getCollaborationConnection()
                .getEventPublisher().register(this);
        super.initComponents(parent);
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
                    CollaborationConnection sessionManager = CollaborationDataManager
                            .getInstance().getCollaborationConnection();
                    ISession session = sessionManager.getPeerToPeerSession();
                    PlatformUI
                            .getWorkbench()
                            .getActiveWorkbenchWindow()
                            .getActivePage()
                            .showView(PeerToPeerView.ID,
                                    session.getSessionId(),
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
        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
                .getActivePart().getSite()
                .registerContextMenu(menuManager, usersTable);
        usersTable.getTable().setMenu(menu);
    }

    protected void fillContextMenu(IMenuManager manager) {
        IStructuredSelection selection = (IStructuredSelection) usersTable
                .getSelection();
        // TODO do something here!
        Object ob = selection.getFirstElement();
    }

    @Subscribe
    public void handleMessage(IMessage message) {
        final IMessage msg = message;
        // so not to have delay, going to handle messages from yourself
        // separately
        if (message.getFrom().equals(
                CollaborationDataManager.getInstance()
                        .getCollaborationConnection().getAccount())) {
            return;
        }
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                appendMessage(msg);
            }
        });
    }

    @Subscribe
    public void handleModifiedPresence(IRosterEntry rosterEntry) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (!usersTable.getTable().isDisposed()) {
                    usersTable.refresh();
                }
            }
        });
    }

    @Subscribe
    public void updateUserAlias(UserId id) {
        List<IRosterEntry> entries = (List<IRosterEntry>) usersTable.getInput();
        for (IRosterEntry entry : entries) {
            if (entry.getUser().getFQName().equals(id.getFQName())) {
                ((RosterItem) entry).setName(id.getAlias());
                break;
            }
        }
        usersTable.refresh();
    }

    protected void initColorManager() {
        manager = new SessionColorManager();
    }

    protected void createUsersComp(final Composite parent) {
        Composite comp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        comp.setLayout(layout);
        comp.setLayoutData(data);

        final CLabel participantsLabel = new CLabel(comp, SWT.NONE);
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
                    val = (int) Math.ceil(((double) val / 26.0));
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

        ParticipantsContentProvider contentProvider = new ParticipantsContentProvider();
        ParticipantsLabelProvider labelProvider = new ParticipantsLabelProvider();
        labelProvider.setSessionId(sessionId);
        labelProvider.setManager(manager);
        usersTable.setContentProvider(contentProvider);

        usersTable.setLabelProvider(labelProvider);
        usersTable.setSorter(new ViewerSorter() {
            public int compare(Viewer viewer, Object e1, Object e2) {
                IRosterEntry c1 = (IRosterEntry) e1;
                IRosterEntry c2 = (IRosterEntry) e1;
                return c1.getUser().getFQName()
                        .compareTo(c2.getUser().getFQName());
            }
        });

        ColumnViewerToolTipSupport.enableFor(usersTable, ToolTip.RECREATE);
        List<IRosterEntry> users = new ArrayList<IRosterEntry>();
        if (session != null) {
            Map<UserId, IRosterEntry> usersMap = session.getConnection()
                    .getContactsManager().getUsersMap();
            for (UserId participant : session.getVenue().getParticipants()) {
                IRosterEntry entry = usersMap.get(participant);
                // if it is yourself, add it
                if (participant.equals(session.getUserID())) {
                    RosterEntry rEntry = new RosterEntry(participant);
                    rEntry.setPresence(CollaborationDataManager.getInstance()
                            .getCollaborationConnection().getPresence());
                    users.add(rEntry);
                } else if (entry != null) {
                    users.add(usersMap.get(participant));
                }
            }
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
        usersTable.setInput(users);
        ((GridData) usersComp.getLayoutData()).exclude = true;
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
        if (manager != null) {
            manager.clearColors();
        }

        // clean up event handlers
        session.unRegisterEventHandler(this);
        CollaborationDataManager mgr = CollaborationDataManager.getInstance();
        mgr.closeSession(sessionId);
        mgr.getCollaborationConnection().getEventPublisher().unregister(this);

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
    public void sendMessage() {
        String message = getComposedMessage();
        if (message.length() > 0) {
            try {
                UserId id = CollaborationDataManager.getInstance()
                        .getCollaborationConnection().getAccount();
                appendMessage(id, System.currentTimeMillis(), message);
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
            String name, UserId userId, List<StyleRange> ranges) {
        RGB rgb = manager.getColorFromUser(userId);
        if (mappedColors.get(rgb) == null) {
            if (rgb == null) {
                rgb = new RGB(0, 0, 0);
            }
            Color col = new Color(Display.getCurrent(), rgb);
            mappedColors.put(rgb, col);
        }
        StyleRange range = new StyleRange(messagesText.getCharCount(), offset,
                mappedColors.get(rgb), null, SWT.NORMAL);
        ranges.add(range);
        range = new StyleRange(messagesText.getCharCount() + offset,
                name.length() + 1, mappedColors.get(rgb), null, SWT.BOLD);
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
        StringBuilder labelInfo = new StringBuilder();
        if (session != null) {
            IVenueInfo info = session.getVenue().getInfo();
            labelInfo.append(info.getVenueSubject());
            label.setToolTipText(info.getVenueSubject());
        }
        label.setText(labelInfo.toString());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.session.AbstractSessionView#
     * getSessionName()
     */
    @Override
    protected String getSessionName() {
        setSession(getViewSite().getSecondaryId());
        if (session == null) {
            return sessionId;
        }
        return session.getVenue().getInfo().getVenueDescription();
    }

    protected void setSession(String sessionId) {
        this.sessionId = sessionId;
        this.session = CollaborationDataManager.getInstance().getSession(
                this.sessionId);
    }

    @Subscribe
    public void participantHandler(IVenueParticipantEvent event)
            throws Exception {
        final ParticipantEventType type = event.getEventType();
        final UserId participant = event.getParticipant();
        final IPresence presence = event.getPresence();
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                switch (type) {
                case ARRIVED:
                    participantArrived(participant);
                    break;
                case DEPARTED:
                    participantDeparted(participant);
                    break;
                case PRESENCE_UPDATED:
                    participantPresenceUpdated(participant, presence);
                    break;
                case UPDATED:
                    // TODO ?
                    break;
                default:
                    System.err.println("Unknown Event type");
                }
            }
        });
    }

    @SuppressWarnings("unchecked")
    private void participantArrived(UserId participant) {
        List<IRosterEntry> users = (List<IRosterEntry>) usersTable.getInput();
        IRosterEntry user = session.getConnection().getContactsManager()
                .getUsersMap().get(participant);
        users.add(user);
        usersTable.refresh();
    }

    @SuppressWarnings("unchecked")
    private void participantDeparted(UserId participant) {
        List<IRosterEntry> users = (List<IRosterEntry>) usersTable.getInput();
        for (int i = 0; i < users.size(); ++i) {
            if (users.get(i) == null
                    || participant.equals(users.get(i).getUser())) {
                users.remove(i);
                usersTable.refresh();
                break;
            }
        }
    }

    /**
     * @param participant
     * @param presence
     */
    @SuppressWarnings("unchecked")
    private void participantPresenceUpdated(UserId participant,
            IPresence presence) {
        // Ignore the presence's mode/type. May not be the same as the user's.
        // TODO Keep as a place holder for now since it may be needed to set
        // leader/provider roles.
        List<IRosterEntry> users = (List<IRosterEntry>) usersTable.getInput();
    }
}
