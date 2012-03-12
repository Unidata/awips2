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
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageFilter;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageListener;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IVenueParticipantListener;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.CollaborationKeywords;
import com.raytheon.uf.viz.collaboration.data.CollaborationUser;
import com.raytheon.uf.viz.collaboration.data.DataUser.RoleType;
import com.raytheon.uf.viz.collaboration.ui.CollaborationUtils;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

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
public class SessionView extends ViewPart implements IPartListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SessionView.class);

    protected static final String SESSION_IMAGE_KEY = "sessionId.key";

    private static final String SESSION_IMAGE_NAME = "chats.gif";

    protected Map<String, Image> imageMap;

    public static final String ID = "com.raytheon.uf.viz.collaboration.SessionView";

    private static int SASH_WIDTH = 5;

    private static int SASH_COLOR = SWT.COLOR_DARK_GRAY;

    private TableViewer usersTable;

    private StyledText messagesText;

    protected StyledText composeText;

    protected String sessionId;

    private Image downArrow;

    private Image rightArrow;

    private Image highlightedRightArrow;

    private Image highlightedDownArrow;

    private Action chatAction;

    protected IVenueParticipantListener participantListener;

    protected IMessageListener messageListener;

    public SessionView() {
        imageMap = new HashMap<String, Image>();
    }

    @Override
    public void createPartControl(Composite parent) {
        setTitleImage(getImage());
        initComponents(parent);
        createActions();
        createContextMenu();
    }

    @Override
    public void setFocus() {
        composeText.setFocus();
    }

    private void initComponents(Composite parent) {
        Composite sashComp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        sashComp.setLayout(layout);
        sashComp.setLayoutData(data);

        Color sashColor = Display.getCurrent().getSystemColor(SASH_COLOR);

        SashForm sashForm = new SashForm(sashComp, SWT.VERTICAL);
        layout = new GridLayout(1, false);
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        sashForm.setLayout(layout);
        sashForm.setLayoutData(data);
        sashForm.setBackground(sashColor);
        sashForm.setSashWidth(SASH_WIDTH);

        createListeners();
        createArrows();
        createUsersComp(sashForm);
        createMessagesComp(sashForm);
        createComposeComp(sashForm);
        sashForm.setWeights(new int[] { 1, 20, 5 });

    }

    protected void createActions() {
        chatAction = new Action("Chat") {
            @Override
            public void run() {
                try {
                    CollaborationDataManager dataManager = CollaborationDataManager
                            .getInstance();
                    CollaborationUser user = (CollaborationUser) ((IStructuredSelection) usersTable
                            .getSelection()).getFirstElement();
                    String session = dataManager.createCollaborationSession(
                            user.getId(), "Chatting...");
                    PlatformUI
                            .getWorkbench()
                            .getActiveWorkbenchWindow()
                            .getActivePage()
                            .showView(CollaborationSessionView.ID, session,
                                    IWorkbenchPage.VIEW_ACTIVATE);
                    // }
                } catch (PartInitException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to open chat", e);
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
        // do something here!
        Object ob = selection.getFirstElement();
        System.out.println(ob.toString());
        manager.add(chatAction);
        manager.add(new Separator());
    }

    /**
     * Ties the view to a session.
     * 
     * @param sessionId
     */
    private void createListeners() {
        this.getViewSite().getWorkbenchWindow().getPartService()
                .addPartListener(this);

        sessionId = getViewSite().getSecondaryId();
        IVenueSession session = CollaborationDataManager.getInstance()
                .getSession(sessionId);
        if (session != null) {
            setPartName(session.getVenue().getInfo().getVenueDescription());
            messageListener = new IMessageListener() {

                @Override
                public void processMessage(final IMessage message) {
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            addMessage(message.getFrom().getName(),
                                    message.getTimeStamp(), message.getBody());
                        }
                    });
                }
            };
            session.addMessageListener(messageListener, new IMessageFilter() {

                @Override
                public boolean filter(IMessage message) {
                    return true;
                }
            });

            participantListener = new IVenueParticipantListener() {
                @Override
                public void handleUpdated(IVenueParticipant participant) {
                    System.out.println("updated");
                }

                @Override
                public void handlePresenceUpdated(IVenueParticipant fromID,
                        IPresence presence) {
                    // not the best way to do it, should just be adding the
                    // new
                    // user instead of requerying for participants
                    Collection<IVenueParticipant> participants = CollaborationDataManager
                            .getInstance().getSession(sessionId).getVenue()
                            .getParticipants();
                    final List<CollaborationUser> users = new ArrayList<CollaborationUser>();
                    for (IVenueParticipant part : participants) {
                        CollaborationUser user = new CollaborationUser(
                                part.getName());
                        user.setStatus(presence.getMode());
                        user.setText(user.getId());
                        users.add(user);
                    }
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            usersTable.setInput(users
                                    .toArray(new CollaborationUser[users.size()]));
                        }
                    });
                }

                @Override
                public void handleDeparted(IVenueParticipant participant) {
                    System.out.println("goodbye");

                }

                @Override
                public void handleArrived(IVenueParticipant participant) {
                    System.out.println("you've got mail");
                }
            };
            session.addVenueParticipantListener(participantListener);

            getViewSite().getWorkbenchWindow().getWorkbench()
                    .addWorkbenchListener(new IWorkbenchListener() {

                        @Override
                        public boolean preShutdown(IWorkbench workbench,
                                boolean forced) {
                            return false;
                        }

                        @Override
                        public void postShutdown(IWorkbench workbench) {
                            System.out.println("Shutting down");
                        }
                    });
        }
    }

    private void createUsersComp(final Composite parent) {
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
        usersTable.setContentProvider(contentProvider);

        usersTable.setLabelProvider(labelProvider);
        usersTable.setSorter(new ViewerSorter() {
            public int compare(Viewer viewer, Object e1, Object e2) {
                return super.compare(viewer, e1, e2);
            }
        });

        IVenueSession session = CollaborationDataManager.getInstance()
                .getSession(sessionId);
        List<CollaborationUser> users = new ArrayList<CollaborationUser>();
        if (session != null) {
            for (IVenueParticipant part : session.getVenue().getParticipants()) {
                CollaborationUser user = new CollaborationUser(part.getName());
                RoleType[] roles = user.getRoles(sessionId);
                for (RoleType role : roles) {
                    user.addRole(role);
                }
                user.setText(part.getName());
                users.add(user);
            }
        } else {
            participantsLabel.setEnabled(false);
            participantsLabel.setForeground(Display.getCurrent()
                    .getSystemColor(SWT.COLOR_DARK_GRAY));
            comp.setEnabled(false);
        }
        usersTable.setInput(users.toArray(new CollaborationUser[users.size()]));
        ((GridData) usersComp.getLayoutData()).exclude = true;
    }

    private void createMessagesComp(Composite parent) {
        Composite messagesComp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        messagesComp.setLayout(layout);
        // TODO, wrap label in view
        Label label = new Label(messagesComp, SWT.WRAP);

        StringBuilder labelInfo = new StringBuilder();
        IVenueSession session = CollaborationDataManager.getInstance()
                .getSession(sessionId);
        if (session != null) {
            IVenueInfo info = session.getVenue().getInfo();
            labelInfo.append(info.getVenueSubject());
            label.setToolTipText(info.getVenueSubject());
        }
        messagesText = new StyledText(messagesComp, SWT.MULTI | SWT.WRAP
                | SWT.READ_ONLY | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        messagesText.setLayoutData(new GridData(GridData.FILL_BOTH));

        if (session == null) {
            labelInfo.append("There is no active session.");
            label.setEnabled(false);
            messagesText.setEnabled(false);
        }

        label.setText(labelInfo.toString());
    }

    private void createComposeComp(Composite parent) {
        Composite composeComp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        composeComp.setLayout(layout);

        Label label = new Label(composeComp, SWT.NONE);
        label.setText("Compose:");
        composeText = new StyledText(composeComp, SWT.MULTI | SWT.WRAP
                | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        composeText.setLayoutData(new GridData(GridData.FILL_BOTH));
        composeText.setToolTipText("Enter message here");
        composeText.addKeyListener(new KeyListener() {
            private boolean keyPressed;

            @Override
            public void keyReleased(KeyEvent e) {
                if (e.keyCode == SWT.SHIFT) {
                    keyPressed = false;
                }
                // do nothing, all done on key pressed
            }

            @Override
            public void keyPressed(KeyEvent e) {
                if (!keyPressed
                        && (e.keyCode == SWT.CR || e.keyCode == SWT.KEYPAD_CR)) {
                    sendMessage();
                }
                if (e.keyCode == SWT.SHIFT) {
                    keyPressed = true;
                }
            }
        });

        composeText.addFocusListener(new FocusListener() {

            @Override
            public void focusLost(FocusEvent e) {
                // Restore other perspective's key bindings.
                VizPerspectiveListener.getCurrentPerspectiveManager()
                        .activateContexts();
            }

            @Override
            public void focusGained(FocusEvent e) {
                // Remove other perspective's key bindings.
                VizPerspectiveListener.getCurrentPerspectiveManager()
                        .deactivateContexts();
            }
        });

        IVenueSession session = CollaborationDataManager.getInstance()
                .getSession(sessionId);
        if (session == null) {
            composeComp.setEnabled(false);
            composeText.setEnabled(false);
            label.setEnabled(false);
        }
    }

    private Image getImage() {
        Image image = imageMap.get(SESSION_IMAGE_KEY);
        if (image == null) {
            image = CollaborationUtils
                    .getImageDescriptor(getSessionImageName()).createImage();
            if (image != null) {
                imageMap.put(SESSION_IMAGE_KEY, image);
            }
        }
        return image;
    }

    @Override
    public void dispose() {
        if (messageListener != null) {
            CollaborationDataManager.getInstance().getSession(sessionId)
                    .removeMessageListener(messageListener);
        }
        if (participantListener != null) {
            CollaborationDataManager.getInstance().getSession(sessionId)
                    .removeVenueParticipantListener(participantListener);
        }
        for (Image im : imageMap.values()) {
            im.dispose();
        }

        imageMap.clear();
        imageMap = null;

        // dispose of the images first
        disposeArrow(highlightedDownArrow);
        disposeArrow(highlightedRightArrow);
        disposeArrow(downArrow);
        disposeArrow(rightArrow);

        super.dispose();
    }

    private void disposeArrow(Image image) {
        if (image != null && !image.isDisposed()) {
            image.dispose();
        }
    }

    public void addUsers(java.util.List<CollaborationUser> users) {
        for (CollaborationUser user : users) {
            addUser(user);
        }
    }

    public void addUser(CollaborationUser user) {
        usersTable.add(user);
    }

    public void clearUsers() {
        usersTable.getTable().removeAll();
    }

    public void removeUser(CollaborationUser user) {
        usersTable.remove(user);
    }

    public void addMessage(String user, long timestamp, String message) {
        Calendar cal = Calendar.getInstance();
        cal.setTimeInMillis(timestamp);
        String time = String.format("%1$tI:%1$tM:%1$tS %1$Tp", cal);
        StringBuilder sb = new StringBuilder();
        if (messagesText.getCharCount() != 0) {
            sb.append("\n");
        }
        int offset = 0;
        sb.append("(").append(time).append(") ");
        offset = sb.length();

        sb.append(user).append(": ").append(message);

        // here is the place to put the font and color changes for keywords
        // read in localization file once and then don't read in again, per chat
        // room?
        List<String> keywords = CollaborationKeywords.parseKeywords();
        List<StyleRange> ranges = new ArrayList<StyleRange>();
        if (keywords != null) {
            for (String keyword : keywords) {
                if (sb.toString().toLowerCase().contains(keyword.toLowerCase())) {
                    StyleRange keywordRange = new StyleRange(
                            messagesText.getCharCount()
                                    + sb.toString().toLowerCase()
                                            .indexOf(keyword.toLowerCase()),
                            keyword.length(), null, null, SWT.BOLD | SWT.ITALIC);
                    ranges.add(keywordRange);
                }
            }
        }

        // XXX determine from the user data
        // get self
        List<RoleType> type = new ArrayList<RoleType>();
        type.add(RoleType.LEADER);
        Color color = SessionColorAdvisor.getColor(type, false);
        StyleRange range = new StyleRange(messagesText.getCharCount() + offset,
                user.length() + 1, color, null, SWT.BOLD);
        messagesText.append(sb.toString());
        messagesText.setStyleRange(range);
        for (StyleRange newRange : ranges) {
            messagesText.setStyleRange(newRange);
        }
        messagesText.setTopIndex(messagesText.getLineCount() - 1);

        // room for other fun things here, such as sounds and such
        executeSightsSounds();
    }

    /**
     * 
     */
    private void executeSightsSounds() {
        // TODO Auto-generated method stub
        // placeholder for future things
    }

    public void sendMessage() {
        String message = null;
        message = composeText.getText().trim();
        composeText.setText("");
        composeText.setCaretOffset(0);
        if (message.length() == 0) {
            // Do not send empty messages.
            return;
        }
        CollaborationDataManager.getInstance().getSession(sessionId)
                .sendTextMessage(message);

    }

    public String getRoom() {
        return sessionId;
    }

    protected String getSessionImageName() {
        return SESSION_IMAGE_NAME;
    }

    @Override
    public void partActivated(IWorkbenchPart part) {
        // nothing to do
    }

    @Override
    public void partBroughtToTop(IWorkbenchPart part) {
        // TODO
        // if link with editor is on, need to activate the editor
    }

    @Override
    public void partClosed(IWorkbenchPart part) {
        // TODO
        // here you need to end a session that is a temporary session
        IVenueSession session = CollaborationDataManager.getInstance()
                .getSession(sessionId);
        if (session != null) {
            session.removeMessageListener(messageListener);
            for (IMessageListener list : session.getMessageListeners()) {
                session.removeMessageListener(list);
            }
            session.removeVenueParticipantListener(participantListener);
        }
        this.getViewSite().getWorkbenchWindow().getPartService()
                .removePartListener(this);
    }

    @Override
    public void partDeactivated(IWorkbenchPart part) {
        // nothing to do
    }

    @Override
    public void partOpened(IWorkbenchPart part) {
        // nothing to do
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
        gc.setBackground(Display.getCurrent().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
        gc.fillRectangle(0, 0, imgWidth, imgHeight);
        gc.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_BLACK));
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
}
