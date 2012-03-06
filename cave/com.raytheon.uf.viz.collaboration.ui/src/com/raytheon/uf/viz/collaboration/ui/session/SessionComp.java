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

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.collaboration.data.CollaborationUser;
import com.raytheon.uf.viz.collaboration.data.DataUser;

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
public class SessionComp extends Composite {
    protected static Image image;

    private static int SASH_WIDTH = 3;

    private static int SASH_COLOR = SWT.COLOR_DARK_GRAY;

    private ListViewer usersList;

    private Text chatMessages;

    private Text messageToSend;

    private String session;

    public SessionComp(Composite parent) {
        this(parent, "UNKNOWN");
    }

    public SessionComp(Composite parent, String session) {
        super(parent, SWT.NONE);
        this.session = session;
        initComponents();
    }

    private void initComponents() {
        Color sashColor = getParent().getDisplay().getSystemColor(SASH_COLOR);
        setLayout(new GridLayout(1, false));
        setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        SashForm bar = new SashForm(this, SWT.HORIZONTAL);
        bar.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        bar.setBackground(sashColor);
        bar.setSashWidth(SASH_WIDTH);

        Composite usersComp = new Composite(bar, SWT.NONE);
        bar.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        Composite messagesComp = new Composite(bar, SWT.NONE);
        bar.setWeights(new int[] { 20, 80 });

        usersComp.setLayout(new GridLayout(1, false));
        Label label = null;
        label = new Label(usersComp, SWT.NONE);
        label.setText("Participants");

        usersList = new ListViewer(usersComp, SWT.MULTI | SWT.V_SCROLL
                | SWT.H_SCROLL);
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
                DataUser.RoleType[] roles = user.getRoles(session);
                StringBuilder sb = new StringBuilder();
                if (roles.length > 0) {
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
                            sb.append("P");
                            break;
                        }
                    }
                    sb.append("] - ");
                }
                sb.append(user.getId());
                return sb.toString();
            }
        });

        usersList.setSorter(new ViewerSorter() {
            public int compare(Viewer viewer, Object e1, Object e2) {
                return super.compare(viewer, e1, e2);
            }
        });

        usersList.getList().setLayoutData(new GridData(GridData.FILL_BOTH));

        CollaborationUser u = new CollaborationUser("OAX_user2");
        u.addRole(DataUser.RoleType.DATA_PROVIDER);
        u.addRole(DataUser.RoleType.LEADER);
        u.setStatus(DataUser.StatusType.AVAILABLE);
        usersList.add(u);
        u = new CollaborationUser("OAX_user1");
        u.addRole(DataUser.RoleType.PARTICIPANT);
        u.setStatus(DataUser.StatusType.MEETING);
        usersList.add(u);

        messagesComp.setLayout(new GridLayout(1, false));

        SashForm messagesSash = new SashForm(messagesComp, SWT.VERTICAL);
        messagesSash
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        messagesSash.setBackground(sashColor);
        messagesSash.setSashWidth(SASH_WIDTH);
        Composite chatComp = new Composite(messagesSash, SWT.BORDER
        /* | SWT.H_SCROLL | SWT.V_SCROLL */);
        Composite sendComp = new Composite(messagesSash, SWT.NONE);
        messagesSash.setWeights(new int[] { 80, 20 });

        chatComp.setLayout(new GridLayout(1, false));
        label = new Label(chatComp, SWT.NONE);
        label.setText("Messages");
        chatMessages = new Text(chatComp, SWT.MULTI | SWT.READ_ONLY);
        chatMessages
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        chatMessages.setText("Chat messages\n go here.");

        sendComp.setLayout(new GridLayout(1, false));
        label = new Label(sendComp, SWT.NONE);
        label.setText("Compose");
        messageToSend = new Text(sendComp, SWT.MULTI);
        messageToSend
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        messageToSend.setText("send messages go here");
    }

    @Override
    public void dispose() {
        if (image != null) {
            image.dispose();
        }
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
        usersList.getList().removeAll();
    }

    public void removeUser(CollaborationUser user) {
        usersList.remove(user);
    }

    public void addMessage(CollaborationUser user, String timestamp,
            String message) {
        StringBuilder sb = new StringBuilder("\n");
        sb.append(user.getId()).append(" (").append(timestamp).append(") ==> ")
                .append(message);
    }

    public void sendMessage() {
        System.out.println("Get and send message here");
    }

    public String getRoom() {
        return session;
    }

    public void setRoom(String room) {
        this.session = room;
    }

    public String getRoomLabel() {
        return "(P) - " + getRoom();
    }

    public Image getRoomImage() {
        return image;
    }

    public void grabFocus() {
        messageToSend.setFocus();
    }
}
