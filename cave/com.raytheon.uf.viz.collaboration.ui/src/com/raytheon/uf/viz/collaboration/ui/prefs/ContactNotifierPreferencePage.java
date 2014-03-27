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
package com.raytheon.uf.viz.collaboration.ui.prefs;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.RosterGroup;

import com.google.common.collect.Lists;
import com.raytheon.uf.viz.collaboration.comm.provider.session.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.ContactsManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.SharedGroup;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.ui.Activator;
import com.raytheon.uf.viz.collaboration.ui.notifier.NotifierTask;
import com.raytheon.uf.viz.collaboration.ui.notifier.NotifierTools;
import com.raytheon.uf.viz.collaboration.ui.session.AddNotifierDlg;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * Contact Notifier preferences page.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2014   2632     mpduff      Initial creation
 * Mar 27, 2014   2632     mpduff      Corrected the OK, Apply, Cancel actions
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ContactNotifierPreferencePage extends PreferencePage implements
        IWorkbenchPreferencePage {
    /** The notifier list control */
    private org.eclipse.swt.widgets.List notifierList;

    /** Delete notifier button */
    private Button deleteBtn;

    /** Edit notifier button */
    private Button editBtn;

    /** New notifier button */
    private Button newBtn;

    /** Notifier task list */
    private java.util.List<NotifierTask> taskList = NotifierTools
            .getNotifierTasks();

    /** Data map backing the notifier list */
    private Map<String, NotifierTask> dataMap = new HashMap<String, NotifierTask>();

    /**
     * {@inheritDoc}
     */
    @Override
    public void init(IWorkbench workbench) {
        setPreferenceStore(Activator.getDefault().getPreferenceStore());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Control createContents(Composite parent) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.horizontalSpan = 3;
        gd.heightHint = 150;

        notifierList = new org.eclipse.swt.widgets.List(parent, SWT.BORDER
                | SWT.V_SCROLL);
        notifierList.setLayoutData(gd);
        notifierList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setButtonState();
            }
        });

        Composite buttonComp = new Composite(parent, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        gd = new GridData(SWT.RIGHT, SWT.FILL, true, true);
        gd.horizontalSpan = 3;
        buttonComp.setLayoutData(gd);

        gd = new GridData(75, SWT.DEFAULT);
        editBtn = new Button(buttonComp, SWT.PUSH);
        editBtn.setText("Edit...");
        editBtn.setLayoutData(gd);
        editBtn.setEnabled(false);
        editBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                editNotifierTask();
            }
        });

        gd = new GridData(75, SWT.DEFAULT);
        newBtn = new Button(buttonComp, SWT.PUSH);
        newBtn.setText("New...");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                newNotifierTask();
            }
        });

        gd = new GridData(75, SWT.DEFAULT);
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.setEnabled(false);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                deleteNotifierTask();
            }
        });

        populate(true);

        return null;
    }

    /**
     * Populate the list.
     */
    private void populate(boolean reload) {
        if (reload) {
            taskList = NotifierTools.getNotifierTasks();
        } else {
            taskList.clear();
            taskList.addAll(dataMap.values());
        }
        for (NotifierTask task : taskList) {
            dataMap.put(task.getUserName(), task);
        }

        Collection<String> items = dataMap.keySet();
        List<String> dataList = Lists.newArrayList(items);
        Collections.sort(dataList);
        notifierList.setItems(dataList.toArray(new String[0]));
    }

    /**
     * Edit the notifier task.
     */
    private void editNotifierTask() {
        if (notifierList.getSelectionIndex() == -1) {
            MessageBox messageDialog = new MessageBox(this.getShell(), SWT.OK);
            messageDialog.setText("Select User");
            messageDialog.setMessage("Please select a user to edit.");
            messageDialog.open();
            return;
        }
        String user = notifierList.getItem(notifierList.getSelectionIndex());
        NotifierTask task = dataMap.get(user);
        if (task != null) {
            ICloseCallback callback = new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    updatePrefs((Map<String, NotifierTask>) returnValue);
                }
            };

            AddNotifierDlg dlg = new AddNotifierDlg(getShell(),
                    new String[] { task.getUserName() }, callback);
            dlg.open();
        }
    }

    /**
     * Delete the notifier task.
     */
    private void deleteNotifierTask() {
        if (notifierList.getSelectionIndex() == -1) {
            MessageBox messageDialog = new MessageBox(this.getShell(), SWT.OK);
            messageDialog.setText("Select User");
            messageDialog.setMessage("Please select a user to delete.");
            messageDialog.open();
            return;
        }
        String user = notifierList.getItem(notifierList.getSelectionIndex());
        NotifierTask task = dataMap.remove(user);
        if (task != null) {
            taskList.remove(task);
            notifierList.remove(notifierList.getSelectionIndex());
        }
        setButtonState();
    }

    /**
     * Create a new notifier task.
     */
    private void newNotifierTask() {
        CollaborationConnection conn = CollaborationConnection.getConnection();
        if (conn != null) {
            String[] contacts = getContacts();
            ICloseCallback callback = new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    updatePrefs((Map<String, NotifierTask>) returnValue);
                    setButtonState();
                }
            };
            AddNotifierDlg dlg = new AddNotifierDlg(getShell(), contacts,
                    callback);
            dlg.open();
        } else {
            MessageBox messageDialog = new MessageBox(this.getShell(), SWT.OK);
            messageDialog.setText("Must Log In");
            messageDialog
                    .setMessage("User must be logged in to Collaboration to add contact notifiers.");
            messageDialog.open();
        }
    }

    /**
     * Set the state of the buttons.
     */
    private void setButtonState() {
        if (notifierList.getSelectionCount() > 0) {
            deleteBtn.setEnabled(true);
            editBtn.setEnabled(true);
        } else {
            deleteBtn.setEnabled(false);
            editBtn.setEnabled(false);
        }
    }

    /**
     * Get the contacts.
     * 
     * @return String[] of contacts
     */
    private String[] getContacts() {
        Set<String> users = new HashSet<String>();
        ContactsManager contactsManager = CollaborationConnection
                .getConnection().getContactsManager();
        for (RosterGroup rg : contactsManager.getGroups()) {
            for (RosterEntry re : rg.getEntries()) {
                UserId userId = IDConverter.convertFrom(re);
                users.add(userId.getName());
            }
        }

        for (SharedGroup rg : contactsManager.getSharedGroups()) {
            for (RosterEntry re : rg.getEntries()) {
                UserId userId = IDConverter.convertFrom(re);
                users.add(userId.getName());
            }
        }

        for (RosterEntry re : contactsManager.getNonGroupedContacts()) {
            UserId userId = IDConverter.convertFrom(re);
            users.add(userId.getName());
        }

        return users.toArray(new String[users.size()]);
    }

    /**
     * Update with the new settings.
     */
    private void updatePrefs(Map<String, NotifierTask> dataMap) {
        this.dataMap = dataMap;
        populate(false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performOk()
     */
    @Override
    public boolean performOk() {
        performApply();
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.PreferencePage#performApply()
     */
    @Override
    protected void performApply() {
        taskList.clear();
        taskList.addAll(dataMap.values());
        NotifierTools.saveNotifiers(taskList);
        setButtonState();
    }
}
