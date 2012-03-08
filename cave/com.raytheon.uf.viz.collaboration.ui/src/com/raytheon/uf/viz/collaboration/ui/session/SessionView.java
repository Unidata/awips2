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
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ExpandEvent;
import org.eclipse.swt.events.ExpandListener;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.ExpandBar;
import org.eclipse.swt.widgets.ExpandItem;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IPartListener;
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
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageFilter;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageListener;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IPresenceListener;
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

    private Action sendMessageAction;

    private Action chatAction;

    protected IPresenceListener presListener;

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
        Composite view = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        layout.marginWidth = 0;
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        view.setLayout(layout);
        view.setData(data);

        Color sashColor = Display.getCurrent().getSystemColor(SASH_COLOR);

        SashForm sashForm = new SashForm(view, SWT.VERTICAL);
        layout = new GridLayout(1, false);
        sashForm.setLayout(layout);
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        sashForm.setLayoutData(data);
        sashForm.setBackground(sashColor);
        sashForm.setSashWidth(SASH_WIDTH);

        createListeners();
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
                    PlatformUI
                            .getWorkbench()
                            .getActiveWorkbenchWindow()
                            .getActivePage()
                            .showView(CollaborationSessionView.ID, null,
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
        manager.add(chatAction);
        manager.add(new Separator());
    }

    /**
     * Ties the view to a session.
     * 
     * @param sessionId
     */
    private void createListeners() {
        sessionId = getViewSite().getSecondaryId();
        if (CollaborationDataManager.getInstance().getSession(sessionId) != null) {
            setPartName(CollaborationDataManager.getInstance()
                    .getSession(sessionId).getVenue().getInfo().getVenueName());
            // Attach desired listeners to the session
            ISession session = CollaborationDataManager.getInstance()
                    .getSession(sessionId);
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

            presListener = new IPresenceListener() {

                @Override
                public void notifyPresence(IPresence presence) {
                    // not the best way to do it, should just be adding the new
                    // user instead of requerying for participants
                    Collection<IVenueParticipant> participants = CollaborationDataManager
                            .getInstance().getSession(sessionId).getVenue()
                            .getParticipants();
                    final List<CollaborationUser> users = new ArrayList<CollaborationUser>();
                    for (IVenueParticipant part : participants) {
                        CollaborationUser user = new CollaborationUser(
                                part.getName());
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
                };
            };
            session.addPresenceListener(presListener, new IMessageFilter() {
                @Override
                public boolean filter(IMessage message) {
                    return true;
                }

            });

        }
    }

    private void createUsersComp(Composite parent) {
        Composite comp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        comp.setLayout(layout);
        comp.setLayoutData(data);

        ExpandBar usersBar = new ExpandBar(comp, SWT.NONE);
        layout = new GridLayout(1, false);
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        usersBar.setLayout(layout);
        usersBar.setLayoutData(data);
        Image usersImage = Display.getCurrent().getSystemImage(SWT.ICON_SEARCH);

        final Composite usersComp = new Composite(usersBar, SWT.NONE);
        layout = new GridLayout(1, false);
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        usersComp.setLayout(layout);
        usersComp.setLayoutData(data);

        ExpandItem usersItem = new ExpandItem(usersBar, SWT.NONE);
        usersItem.setText("Participants");
        usersItem.setImage(usersImage);

        usersBar.addExpandListener(new ExpandListener() {
            @Override
            public void itemExpanded(ExpandEvent e) {
                usersComp.setSize(usersTable.getTable().computeSize(
                        SWT.DEFAULT, 500));
                ((SashForm) usersComp.getParent().getParent().getParent())
                        .layout();
            }

            @Override
            public void itemCollapsed(ExpandEvent e) {
                usersComp.setSize(usersTable.getTable().computeSize(
                        SWT.DEFAULT, 100));

                ((SashForm) usersComp.getParent().getParent().getParent())
                        .layout();
            }
        });

        usersTable = new TableViewer(usersComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL | SWT.H_SCROLL);
        layout = new GridLayout(1, false);
        data = new GridData(SWT.FILL, SWT.FILL, true, true);
        data.heightHint = 100;
        usersTable.getTable().setLayout(layout);
        usersTable.getTable().setLayoutData(data);

        usersItem.setHeight(usersComp.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
        usersItem.setControl(usersComp);

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
        for (IVenueParticipant part : session.getVenue().getParticipants()) {
            CollaborationUser user = new CollaborationUser(part.getName());
            user.setText(part.getName());
            users.add(user);
        }
        usersTable.setInput(users.toArray(new CollaborationUser[users.size()]));
    }

    private void createMessagesComp(Composite parent) {
        Composite messagesComp = new Composite(parent, SWT.NONE);
        GridLayout layout = new GridLayout(1, false);
        messagesComp.setLayout(layout);
        Label label = new Label(messagesComp, SWT.NONE);

        StringBuilder labelInfo = new StringBuilder();
        IVenueInfo info = CollaborationDataManager.getInstance()
                .getSession(sessionId).getVenue().getInfo();
        labelInfo.append(info.getVenueDescription());
        label.setToolTipText(info.getVenueSubject());
        if (info.getVenueSubject() != null && !info.getVenueSubject().isEmpty()) {
            labelInfo.append(":");
            labelInfo.append(info.getVenueSubject());
        }
        label.setText(labelInfo.toString());
        messagesText = new StyledText(messagesComp, SWT.MULTI | SWT.WRAP
                | SWT.READ_ONLY | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
        messagesText.setLayoutData(new GridData(GridData.FILL_BOTH));
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
        if (presListener != null) {
            CollaborationDataManager.getInstance().getSession(sessionId)
                    .removePresenceListener(presListener);
        }
        for (String key : imageMap.keySet()) {
            imageMap.get(key).dispose();
        }
        imageMap.clear();
        imageMap = null;
        super.dispose();
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
    }

    @Override
    public void partDeactivated(IWorkbenchPart part) {
        // nothing to do
    }

    @Override
    public void partOpened(IWorkbenchPart part) {
        // nothing to do
    }
}
