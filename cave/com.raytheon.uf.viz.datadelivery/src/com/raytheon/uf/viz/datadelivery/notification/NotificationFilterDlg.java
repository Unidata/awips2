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
package com.raytheon.uf.viz.datadelivery.notification;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.datadelivery.common.ui.ITableChange;
import com.raytheon.uf.viz.datadelivery.notification.PriorityImages.Priority;
import com.raytheon.uf.viz.datadelivery.notification.xml.MessageLoadXML;
import com.raytheon.uf.viz.datadelivery.notification.xml.NotificationFilterXML;
import com.raytheon.uf.viz.datadelivery.notification.xml.UserFilterXML;
import com.raytheon.uf.viz.datadelivery.utils.NotificationHandler;
import com.raytheon.viz.ui.dialogs.CaveSWTDialogBase;
import com.raytheon.viz.ui.widgets.duallist.DualList;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;

/**
 * Filter table dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb  6, 2012            mpduff     Initial creation.
 * Mar 20, 2012   240      jpiatt     Updates to filter notification table data.
 * Jun  1, 2012   645      jpiatt     Added tooltips.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class NotificationFilterDlg extends CaveSWTDialogBase {

    /** Always include notifications checkbox */
    private Button alwaysIncludeMeBtn;

    /** Filter By Priority checkboxes */
    private Button[] priorityBtns;

    /** DualList object */
    private DualList userDualList;

    /** DualList object */
    private DualList subDualList;

    /** Notification handler */
    private final NotificationHandler handler;

    /** Currently logged in user */
    String currentUser = LocalizationManager.getInstance().getCurrentUser();

    /** Instance of the configManager */
    NotificationConfigManager configManager = NotificationConfigManager
            .getInstance();

    /** Callback to NotificationDialog */
    private final ITableChange callback;

    /**
     * Constructor.
     * 
     * @param shell
     *            Parent shell.
     * @param callback
     *            ITableChange callback
     */
    public NotificationFilterDlg(Shell shell, ITableChange callback) {
        super(shell, CAVE.INDEPENDENT_SHELL);
        setText("Notification Filter Settings");
        handler = new NotificationHandler();
        this.callback = callback;

        LocalizationManager.getInstance().getCurrentUser();
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {

        createUserGroup();
        createSubscriptionGroup();
        createPriorityGroup();

        createButtons();

        filterByPriority();
    }

    /**
     * Create the Filter by User group.
     */
    private void createUserGroup() {

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        Group userGroup = new Group(shell, SWT.NONE);
        userGroup.setLayout(gl);
        userGroup.setText(" Filter by User ");
        userGroup.setToolTipText("Select users to be visible in the table");

        gl = new GridLayout(1, false);
        Composite topComp = new Composite(userGroup, SWT.NONE);
        topComp.setLayout(gl);
        topComp.setLayoutData(gd);

        boolean selfInclude = configManager.getFilterXml().getUserFilterXml()
                .isSelfInclude();

        // Always include currently logged in user check box
        alwaysIncludeMeBtn = new Button(topComp, SWT.CHECK);
        alwaysIncludeMeBtn.setText("Always include my notifications");
        alwaysIncludeMeBtn.setSelection(selfInclude);
        alwaysIncludeMeBtn
                .setToolTipText("Select to maintain current user name in Selected list");
        alwaysIncludeMeBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (alwaysIncludeMeBtn.getSelection()) {
                    userDualList.getConfig().getIncludeList().clear();
                    userDualList.getConfig().getIncludeList().add(currentUser);

                    // Available and Selected Filter Lists
                    String[] available = userDualList.getAvailableListItems();
                    String[] select = userDualList.getSelectedListItems();
                    List<String> arr = new ArrayList<String>();
                    ArrayList<String> arr2 = new ArrayList<String>();

                    for (String s : available) {

                        // Check if current user is in the available list
                        if (s.equals(currentUser)) {

                            arr.add(s);

                        } else {
                            // Add to Available List
                            arr2.add(s);
                        }

                    }
                    // Add current user to Selected Users
                    for (String s3 : select) {

                        arr.add(s3);
                    }

                    if (!arr.contains(currentUser)
                            && alwaysIncludeMeBtn.getSelection()) {
                        arr.add(currentUser);
                    }

                    String[] select2 = arr.toArray(new String[arr.size()]);
                    userDualList.setSelectedItems(select2);
                    userDualList.setAvailableItems(arr2);

                } else {
                    userDualList.getConfig().getIncludeList().clear();
                }

            }
        });

        gl = new GridLayout(3, false);

        List<NotificationRecord> notificationList = null;

        // Retrieve data from db
        MessageLoadXML messageLoad = new MessageLoadXML();
        messageLoad.setLoadAllMessages(true);
        notificationList = handler.intialLoad(messageLoad, null);

        if (notificationList == null) {
            return;
        }

        ArrayList<String> fullList = new ArrayList<String>();

        // Grab the usernames of each record in the table and add them to the
        // fullList
        for (NotificationRecord record : notificationList) {

            if (!fullList.contains(record.getUsername())
                    && record.getUsername() != null
                    && !record.getUsername().equals("")) {
                fullList.add(record.getUsername());
            }

        }

        if (!fullList.contains(currentUser)) {
            fullList.add(currentUser);
        }

        NotificationFilterXML xml = configManager.getFilterXml();

        ArrayList<String> selectedList = xml.getUserFilterXml().getUserList();
        ArrayList<String> selectedListFinal = new ArrayList<String>();

        // Check if everything in the selected list is still in the db
        for (String s : selectedList) {

            if (fullList.toString().contains(s)) {
                selectedListFinal.add(s);
            }

        }

        HashSet<String> h = new HashSet<String>();

        if (selfInclude) {

            h.add(currentUser);
        }

        // Create the user dual list
        DualListConfig dualConfig = new DualListConfig();
        dualConfig.setListHeight(120);
        dualConfig.setListWidth(125);
        dualConfig.setShowUpDownBtns(false);
        dualConfig.setIncludeList(h);
        dualConfig.setAvailableListLabel("Available Users:");
        dualConfig.setSelectedListLabel("Selected Users:");

        if (alwaysIncludeMeBtn.getSelection()
                && !selectedListFinal.contains(currentUser)) {
            selectedListFinal.add(currentUser);
        }

        // Set the available and selected lists
        dualConfig.setFullList(fullList);
        dualConfig.setSelectedList(selectedListFinal);

        userDualList = new DualList(userGroup, SWT.NONE, dualConfig);

    }

    /**
     * Create the Filter by Priority group.
     */
    private void createPriorityGroup() {
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);

        Group priorityGroup = new Group(shell, SWT.NONE);
        priorityGroup.setText(" Filter by Priority ");
        priorityGroup.setLayout(gl);
        priorityGroup.setLayoutData(gd);
        priorityGroup
                .setToolTipText("Select priorities to be visible in the table");

        priorityBtns = new Button[Priority.values().length];

        for (int i = 0; i < priorityBtns.length; i++) {
            Button b = new Button(priorityGroup, SWT.CHECK);
            b.setSelection(false);

            Priority pri = Priority.values()[i];
            b.setText(pri.getPriorityNum() + " - " + pri.getPriorityName());
            b.setData(pri);
            if (i == 0) {
                b.setEnabled(false);
            }
            priorityBtns[i] = b;
        }
    }

    /**
     * Create the Filter by Subscription group.
     */
    private void createSubscriptionGroup() {

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        Group subGroup = new Group(shell, SWT.NONE);
        subGroup.setLayout(gl);
        subGroup.setText(" Filter by Subscription ");
        subGroup.setToolTipText("Subscriptions moved to the Selected list will be visible");

        gl = new GridLayout(1, false);
        Composite topComp = new Composite(subGroup, SWT.NONE);
        topComp.setLayout(gl);
        topComp.setLayoutData(gd);

        gl = new GridLayout(3, false);

        List<NotificationRecord> notificationList = null;

        // Retrieve data from db
        MessageLoadXML messageLoad = new MessageLoadXML();
        messageLoad.setLoadAllMessages(true);
        notificationList = handler.intialLoad(messageLoad, null);

        if (notificationList == null) {
            return;
        }

        ArrayList<String> fullSubList = new ArrayList<String>();

        // Grab the usernames of each record in the table and add them to the
        // fullList
        for (NotificationRecord record : notificationList) {

            if (!fullSubList.contains(record.getCategory())
                    && record.getCategory() != null
                    && !record.getCategory().equals("")) {
                fullSubList.add(record.getCategory());
            }

        }

        NotificationFilterXML xml = configManager.getFilterXml();

        ArrayList<String> selectedSubList = xml.getSubscriptionList();
        ArrayList<String> selectedSubListFinal = new ArrayList<String>();

        // Check if everything in the selected list is still in the db
        for (String s : selectedSubList) {

            if (fullSubList.toString().contains(s)) {
                selectedSubListFinal.add(s);
            }

        }

        DualListConfig dualConfig = new DualListConfig();
        dualConfig.setListHeight(120);
        dualConfig.setListWidth(125);
        dualConfig.setShowUpDownBtns(false);
        dualConfig.setAvailableListLabel("Available Subscriptions:");
        dualConfig.setSelectedListLabel("Selected Subscriptions:");

        // Set the available and selected lists
        dualConfig.setFullList(fullSubList);
        dualConfig.setSelectedList(selectedSubListFinal);

        subDualList = new DualList(subGroup, SWT.NONE, dualConfig);

    }

    /**
     * Create buttons.
     */
    private void createButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(6, false);

        Composite bottomComp = new Composite(shell, SWT.NONE);
        bottomComp.setLayout(gl);
        bottomComp.setLayoutData(gd);

        // OK button
        int buttonWidth = 75;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button okBtn = new Button(bottomComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(btnData);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleOK();
            }
        });

        // Cancel button
        Button closeBtn = new Button(bottomComp, SWT.PUSH);
        closeBtn.setText("Cancel");
        closeBtn.setLayoutData(btnData);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(false);
                close();
            }
        });
    }

    /**
     * Handle the OK button action.
     * 
     */
    private void handleOK() {

        setReturnValue(true);

        NotificationFilterXML xml = configManager.getFilterXml();

        // Filter By User Info
        UserFilterXML userFilter = xml.getUserFilterXml();
        if (userFilter == null) {
            userFilter = new UserFilterXML();
        }
        userFilter.clearUsers();
        userFilter.setSelfInclude(alwaysIncludeMeBtn.getSelection());

        String[] selectedList = userDualList.getSelectedListItems();
        if (selectedList.length > 0) {
            for (String s : selectedList) {
                userFilter.addUser(s);
            }
        }

        xml.setUserFilterXml(userFilter);

        // Filter By Subscription Info
        ArrayList<String> subFilter = xml.getSubscriptionList();

        if (subFilter == null) {
            subFilter = new ArrayList<String>();
        }
        subFilter.clear();

        String[] selectedSubList = subDualList.getSelectedListItems();
        if (selectedSubList.length > 0) {
            for (String s : selectedSubList) {
                subFilter.add(s);
            }
        }

        xml.setsubscriptionList(subFilter);

        // Filter By User Info
        xml.clearPriorityList();
        for (Button b : priorityBtns) {
            if (b.getSelection()) {
                xml.addPriority((Priority) b.getData());
            }
        }

        configManager.setFilterXml(xml);
        configManager.saveXml();

        callback.tableChanged();
        close();
    }

    /**
     * Filter table by selected priorities.
     * 
     */
    private void filterByPriority() {

        NotificationFilterXML xml = configManager.getFilterXml();
        ArrayList<Priority> priorityList = xml.getPriorityList();
        for (int i = 0; i < priorityBtns.length; i++) {
            Button b = priorityBtns[i];
            for (Priority p : priorityList) {
                if (i == 0) {
                    b.setSelection(true);
                    break;
                }

                if (((Priority) b.getData()) == p) {
                    b.setSelection(true);
                    break;
                }
            }
        }

        UserFilterXML userXml = xml.getUserFilterXml();

        this.alwaysIncludeMeBtn.setSelection(userXml.isSelfInclude());
    }

}
