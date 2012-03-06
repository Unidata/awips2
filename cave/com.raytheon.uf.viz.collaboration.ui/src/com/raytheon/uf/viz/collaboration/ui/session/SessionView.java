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

import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
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
import org.eclipse.swt.widgets.ExpandBar;
import org.eclipse.swt.widgets.ExpandItem;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.part.ViewPart;

import com.raytheon.uf.viz.collaboration.comm.identity.IMessage;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.ISession;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageFilter;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageListener;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IPresenceListener;
import com.raytheon.uf.viz.collaboration.data.CollaborationDataManager;
import com.raytheon.uf.viz.collaboration.data.CollaborationNode;
import com.raytheon.uf.viz.collaboration.data.CollaborationUser;
import com.raytheon.uf.viz.collaboration.data.DataUser;
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
public class SessionView extends ViewPart {

    protected static final String SESSION_IMAGE_KEY = "sessionId.key";

    private static final String SESSION_IMAGE_NAME = "chats.gif";

    protected Map<String, Image> imageMap;

    public static final String ID = "com.raytheon.uf.viz.collaboration.SessionView";

    protected Composite view;

    private static int SASH_WIDTH = 3;

    private static int SASH_COLOR = SWT.COLOR_DARK_GRAY;

    private TableViewer usersList;

    private StyledText messagesText;

    protected StyledText composeText;

    protected String sessionId;

    private int usersCollapsedHeight = -1;

    private boolean usersExpanding = true;

    private int usersBarExpandedHeight = -1;

    private int[] usersExpandedWeights;

    private Action sendMessageAction;

    public SessionView() {
        imageMap = new HashMap<String, Image>();
    }

    @Override
    public void createPartControl(Composite parent) {
        setTitleImage(getImage());
        initComponents(parent);
        createToolBar();
    }

    @Override
    public void setFocus() {
        composeText.setFocus();
    }

    private void initComponents(Composite parent) {
        view = new Composite(parent, SWT.NONE);
        Color sashColor = view.getParent().getDisplay()
                .getSystemColor(SASH_COLOR);
        view.setLayout(new GridLayout(1, false));
        view.setLayoutData(new GridData(GridData.FILL_BOTH));
        SashForm bar = new SashForm(view, SWT.VERTICAL);
        bar.setLayoutData(new GridData(GridData.FILL_BOTH));
        bar.setBackground(sashColor);
        bar.setSashWidth(SASH_WIDTH);
        bar.setLayoutData(new GridData(GridData.FILL_BOTH));

        createUsersComp(bar);
        createMessagesComp(bar);
        createComposeComp(bar);
        bar.setWeights(new int[] { 5, 85, 10 });
        createListeners();
    }

    /**
     * Ties the view to a session.
     * 
     * @param sessionId
     */
    private void createListeners() {
        sessionId = getViewSite().getSecondaryId();
        setPartName(CollaborationDataManager.getInstance()
                .getSession(sessionId).getVenue().getInfo().getVenueName());

        // Attach desired listeners to the session
        ISession session = CollaborationDataManager.getInstance().getSession(
                sessionId);
        session.addMessageListener(new IMessageListener() {

            @Override
            public void processMessage(final IMessage message) {
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        addMessage(message.getFrom().getFQName(),
                                message.getTimeStamp(), message.getBody());
                    }
                });
            }
        }, new IMessageFilter() {

            @Override
            public boolean filter(IMessage message) {
                return true;
            }
        });

        // TODO populate participants table
        session.addPresenceListener(new IPresenceListener() {

            @Override
            public void notifyPresence(IPresence presence) {
                System.out.println(presence.getMode());
                System.out.println(presence.getType());

            }

        }, new IMessageFilter() {
            @Override
            public boolean filter(IMessage message) {
                return true;
            }

        });

        // for (String id : users) {
        // CollaborationUser user = new CollaborationUser(id, sessionId);
        // usersList.add(user);
        // }
    }

    private void setUsersHeight(ExpandBar bar) {
        ExpandItem item = bar.getItem(0);

        // Composite comp = (Composite) item.getControl();
        // GridLayout gl = (GridLayout) comp.getLayout();
        int y = bar.getSize().y;
        // y -= usersCollapsedHeight + 0 * gl.verticalSpacing + gl.marginBottom;
        y -= usersCollapsedHeight;
        if (y < usersCollapsedHeight) {
            y = usersCollapsedHeight;
        }
        item.setHeight(y);
    }

    private void createUsersComp(Composite parent) {
        Composite comp = new Composite(parent, SWT.NONE);
        comp.setLayout(new GridLayout(1, false));
        comp.setLayoutData(new GridData(GridData.FILL_BOTH));
        comp.addControlListener(new ControlListener() {

            @Override
            public void controlResized(ControlEvent e) {
                if (usersExpanding) {
                    // Expand control listener is adjusting the height.
                    usersExpanding = false;
                } else {
                    Composite comp = (Composite) e.getSource();
                    ExpandBar bar = (ExpandBar) comp.getChildren()[0];
                    usersBarExpandedHeight = comp.getSize().y;
                    bar.setSize(bar.getSize().x, usersBarExpandedHeight);
                    setUsersHeight(bar);
                }
            }

            @Override
            public void controlMoved(ControlEvent e) {
            }
        });

        ExpandBar usersBar = new ExpandBar(comp, SWT.BORDER | SWT.V_SCROLL);
        usersBar.setLayoutData(new GridData(GridData.FILL_BOTH));
        Image usersImage = parent.getParent().getDisplay()
                .getSystemImage(SWT.ICON_SEARCH);

        Composite usersComp = new Composite(usersBar, SWT.NONE);
        usersComp.setLayout(new GridLayout(1, false));
        usersComp.addControlListener(new ControlListener() {

            @Override
            public void controlResized(ControlEvent e) {
                System.err.println("controlResized: " + e.toString());
            }

            @Override
            public void controlMoved(ControlEvent e) {
                System.err.println("controlMoved: " + e.toString());
            }
        });

        ExpandItem usersItem = new ExpandItem(usersBar, SWT.DEFAULT);
        usersItem.setText("Participants");
        usersItem.setControl(usersComp);
        usersItem.setImage(usersImage);

        usersBar.addExpandListener(new ExpandListener() {

            @Override
            public void itemExpanded(ExpandEvent e) {
                usersExpanding = true;
                ExpandBar bar = (ExpandBar) e.getSource();
                SashForm form = (SashForm) bar.getParent().getParent();
                form.setWeights(usersExpandedWeights);
                bar.setSize(bar.getSize().x, usersBarExpandedHeight);
                setUsersHeight(bar);
            }

            @Override
            public void itemCollapsed(ExpandEvent e) {
                usersExpanding = true;
                ExpandBar bar = (ExpandBar) e.getSource();
                SashForm form = (SashForm) bar.getParent().getParent();
                ExpandItem item = bar.getItem(0);
                usersExpandedWeights = form.getWeights();
                Composite c = (Composite) item.getControl();
                GridLayout gl = (GridLayout) c.getLayout();
                int y = usersCollapsedHeight + 6 * gl.verticalSpacing
                        + gl.marginBottom;

                item.setHeight(y);
                int[] wts = usersExpandedWeights.clone();
                wts[0] = y;
                wts[1] += usersExpandedWeights[0] - y;
                form.setWeights(wts);
            }
        });

        usersList = new TableViewer(usersComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL | SWT.H_SCROLL);
        usersList.setContentProvider(new IStructuredContentProvider() {

            @Override
            public void inputChanged(Viewer viewer, Object oldInput,
                    Object newInput) {
            }

            @Override
            public void dispose() {
            }

            @Override
            public Object[] getElements(Object inputElement) {

                return (CollaborationUser[]) inputElement;
            }
        });

        usersList.setLabelProvider(new LabelProvider() {
            public String getText(Object element) {
                CollaborationUser user = (CollaborationUser) element;
                DataUser.RoleType[] roles = user.getRoles(sessionId);
                StringBuilder sb = new StringBuilder();
                if (roles.length > 0
                        && roles[0] != DataUser.RoleType.PARTICIPANT) {
                    sb.append("[");
                    for (DataUser.RoleType r : roles) {
                        switch (r) {
                        case DATA_PROVIDER:
                            sb.append("D");
                            break;
                        case LEADER:
                            sb.append("L");
                            break;
                        default:
                            sb.append("?");
                            break;
                        }
                    }
                    sb.append("] - ");
                }
                sb.append(user.getId());
                return sb.toString();
            }

            public Image getImage(Object element) {
                Image image = null;
                if (element instanceof CollaborationNode) {
                    CollaborationNode node = (CollaborationNode) element;
                    String key = node.getImageKey();
                    if (key != null) {
                        image = imageMap.get(key);
                        if (image == null) {
                            image = CollaborationUtils.getNodeImage(node);
                            imageMap.put(key, image);
                        }
                    }
                }
                return image;
            }
        });

        usersList.setSorter(new ViewerSorter() {
            public int compare(Viewer viewer, Object e1, Object e2) {
                // return ((CollaborationUser) e1)
                // .compareTo((CollaborationUser) e1);
                return super.compare(viewer, e1, e2);
            }
        });

        usersList.getTable().setLayoutData(new GridData(GridData.FILL_BOTH));
        usersCollapsedHeight = usersComp.computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
        usersItem.setHeight(usersCollapsedHeight);
    }

    private void createMessagesComp(Composite parent) {
        Composite messagesComp = new Composite(parent, SWT.BORDER);
        messagesComp.setLayout(new GridLayout(1, false));
        Label label = new Label(messagesComp, SWT.NONE);
        label.setText("Messages");
        messagesText = new StyledText(messagesComp, SWT.MULTI | SWT.WRAP
                | SWT.READ_ONLY | SWT.H_SCROLL | SWT.V_SCROLL);
        messagesText.setLayoutData(new GridData(GridData.FILL_BOTH));
    }

    private void createComposeComp(Composite parent) {
        Composite composeComp = new Composite(parent, SWT.BORDER);
        composeComp.setLayout(new GridLayout(1, false));
        Label label = new Label(composeComp, SWT.NONE);
        label.setText("Compose");
        composeText = new StyledText(composeComp, SWT.MULTI | SWT.WRAP
                | SWT.H_SCROLL | SWT.V_SCROLL);
        composeText.setLayoutData(new GridData(GridData.FILL_BOTH));
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
                if (!keyPressed && e.keyCode == SWT.CR) {
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
        usersList.add(user);
    }

    public void clearUsers() {
        usersList.getTable().removeAll();
    }

    public void removeUser(CollaborationUser user) {
        usersList.remove(user);
    }

    public void addMessage(String user, long timestamp, String message) {
        Calendar cal = Calendar.getInstance();
        cal.setTimeInMillis(timestamp);
        String time = String.format("%1$tI:%1$tM:%1$tS %1$Tp", cal);

        StringBuilder sb = new StringBuilder("\n");
        sb.append(user).append(" (").append(time).append(") ==> ")
                .append(message);
        messagesText.append(sb.toString());
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

    public void setRoom(String room) {
        this.sessionId = room;
        setPartName(room);
        // setTitleImage(getImage());
    }

    protected String getSessionImageName() {
        return SESSION_IMAGE_NAME;
    }

    public void sendMessageAction() {
        sendMessageAction = new Action() {
            public void run() {
                sendMessage();
            }
        };
        // TODO this the image we want?
        sendMessageAction.setImageDescriptor(CollaborationUtils
                .getImageDescriptor("icon_show_advanced_prop.png"));
        sendMessageAction.setToolTipText("Send Message");
    }

    protected void createToolBar() {
        IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
        sendMessageAction();
        // switchDataProviderAction();
        // switchLeaderAction();
        // ActionContributionItem item = null;
        // item = new ActionContributionItem(switchDataProviderAction);
        // item.setMode(ActionContributionItem.MODE_FORCE_TEXT);
        // mgr.add(item);
        // item = new ActionContributionItem(switchLeaderAction);
        // item.setMode(ActionContributionItem.MODE_FORCE_TEXT);
        // mgr.add(switchLeaderAction);
        // // item = new ActionContributionItem(sendMessageAction);
        // // item.setMode(ActionContributionItem.MODE_FORCE_TEXT);
        mgr.add(sendMessageAction);
    }
}
