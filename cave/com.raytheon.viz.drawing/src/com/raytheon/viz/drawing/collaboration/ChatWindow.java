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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.part.ViewPart;
import org.jivesoftware.smack.XMPPException;

import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Defines a chat window
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

public class ChatWindow extends ViewPart {

    private static ChatWindow instance;

    /** The table viewer used to display the logged in users */
    private TableViewer tableViewer;

    /** The set of people currently logged in */
    private Set<String> people;

    /** The chat area */
    private StyledText chatArea;

    /** The chat history as a stringbuffer */
    private StringBuffer chatBuffer;

    /** The last chat objects */
    private List<String> history;

    /** The input area */
    private Text inputLine;

    /**
     * Create a chat window
     * 
     */
    public ChatWindow() {
        super();
        instance = this;
        people = new HashSet<String>();
    }

    /**
     * Get the running chat window instance
     * 
     * @return a chat window
     */
    public static ChatWindow getInstance() {
        return instance;
    }

    /**
     * Add a participant to the user list
     * 
     * @param participant
     *            the participants name
     */
    public void addParticipant(String participant) {
        String strippedName = participant
                .substring(participant.indexOf('/') + 1);
        if (!people.contains(strippedName)) {
            people.add(strippedName);
            tableViewer.add(strippedName);
        }
    }

    /**
     * Remove a participiant from the user list
     * 
     * @param participant
     *            the participant name
     */
    public void removeParticipant(String participant) {
        String strippedName = participant
                .substring(participant.indexOf('/') + 1);
        tableViewer.remove(strippedName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    public void createPartControl(Composite parent) {
        Composite c = new Composite(parent, SWT.None);
        c.setLayout(new GridLayout(1, false));
        Label participants = new Label(c, SWT.BOLD | SWT.CENTER);
        participants.setText("Participants:");
        participants.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        tableViewer = new TableViewer(c, SWT.V_SCROLL | SWT.BORDER);
        tableViewer.getTable()
                .setLayoutData(
                        new GridData(GridData.GRAB_HORIZONTAL
                                | GridData.HORIZONTAL_ALIGN_FILL
                                | GridData.GRAB_VERTICAL
                                | GridData.VERTICAL_ALIGN_FILL));

        Label chatLabel = new Label(c, SWT.BOLD | SWT.CENTER);
        chatLabel.setText("Text Chat:");
        chatLabel.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        chatArea = new StyledText(c, SWT.MULTI | SWT.READ_ONLY | SWT.WRAP
                | SWT.BORDER | SWT.V_SCROLL);
        chatArea.setSize(300, 600);
        chatArea.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_VERTICAL
                | GridData.VERTICAL_ALIGN_FILL));
        inputLine = new Text(c, SWT.SINGLE | SWT.BORDER);
        inputLine.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        inputLine.addKeyListener(new KeyListener() {

            public void keyPressed(KeyEvent e) {
                if (e.character == '\r') {
                    CollaborationManager mgr = CollaborationManager
                            .getInstance();
                    if (mgr == null || !mgr.isConnected()) {
                        MessageBox mb = new MessageBox(VizWorkbenchManager
                                .getInstance().getCurrentWindow().getShell(),
                                SWT.ICON_ERROR);
                        mb.setMessage("Not Connected to Collaboration");
                        mb.open();
                    } else {
                        try {
                            mgr.sendChatMessage(inputLine.getText());
                            addChatData(null, inputLine.getText());
                            inputLine.setText("");
                        } catch (XMPPException e1) {
                            // TODO Auto-generated catch block
                            e1.printStackTrace();
                        }
                    }
                }
            }

            public void keyReleased(KeyEvent e) {

            }

        });
        history = new java.util.LinkedList<String>();
        updateUsers();
    }

    /**
     * Update the logged in users
     * 
     */
    public void updateUsers() {
        tableViewer.getTable().removeAll();

        CollaborationManager mgr = CollaborationManager.getInstance();
        if (mgr == null)
            return;

        String[] users = mgr.getUsers();
        for (String u : users) {
            addParticipant(u);
        }
    }

    /**
     * Add chat data
     * 
     * @param username
     *            the username who sent the message
     * @param msg
     *            the message
     */
    public void addChatData(String username, String msg) {

        if (username == null) {
            history.add("---> " + msg + "\n");
        } else {
            String preparedUsername = username
                    .substring(username.indexOf('/') + 1);
            history.add("<" + preparedUsername + ">  " + msg + "\n");
        }
        if (history.size() > 100) {
            history.remove(0);
        }

        chatBuffer = new StringBuffer();
        for (String str : history) {
            chatBuffer.append(str);
        }
        String str = chatBuffer.toString();
        chatArea.setText(str);
        chatArea.setSelection(str.length() - 1);
    }

    /**
     * Clear all chat buffers
     * 
     */
    public void clear() {
        tableViewer.getTable().removeAll();
        history.clear();
        people.clear();
        chatArea.setText("");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        // TODO Auto-generated method stub

    }

}
