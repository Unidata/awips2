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
package com.raytheon.viz.gfe.dialogs;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.UIFormat;
import com.raytheon.viz.gfe.core.UIFormat.FilterType;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The break lock dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2008            ebabin       Initial Creation
 * Jun 30, 2008            ebabin       Updates for status of break lock.
 * Sep 20, 2012  #1190     dgilling     Use new WsId.getHostName() method.
 * Nov 17, 2015  #5129     dgilling     Support new IFPClient.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class BreakLockDialog extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(BreakLockDialog.class);

    private static final int BOX_LIMIT = 12;

    private static final int BREAK_LOCK = 45;

    private static final int SELECT_ALL_ORPHANS = 46;

    private static final int CLEAR_ALL = 47;

    private static final int CANCEL = IDialogConstants.CANCEL_ID;

    private UIFormat uiFormat;

    private Composite top = null;

    private List<Lock> clientLocks;

    private List<Lock> orphanedLocks;

    private CheckboxTableViewer clientViewer;

    private CheckboxTableViewer orphanedViewer;

    private DataManager dataManager;

    private static final SimpleDateFormat dateFormatter = new SimpleDateFormat(
            "MMMdd HH:mmz");
    static {
        dateFormatter.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    public BreakLockDialog(Shell parent, DataManager dataManager) {
        super(parent);
        this.dataManager = dataManager;

        this.uiFormat = new UIFormat(this.dataManager.getParmManager(),
                FilterType.MUTABLE, FilterType.MUTABLE);

        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
        try {
            initLockData();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve lock data", e);
        }
    }

    protected void initLockData() {
        List<LockTable> lockTables = dataManager.getParmOp()
                .mutableDbLockTable();

        ServerResponse<List<String>> sr = dataManager.getClient()
                .getClientList();
        List<String> clients;
        if (!sr.isOkay()) {
            statusHandler
                    .error(String
                            .format("Unable to retrieve client info: %s. All locks will appear as orphans",
                                    sr.message()));
            clients = Collections.emptyList();
        } else {
            clients = sr.getPayload();
        }

        this.clientLocks = new ArrayList<Lock>();
        this.orphanedLocks = new ArrayList<Lock>();
        for (LockTable lockTable : lockTables) {
            // Look for locks owned by others
            for (Lock lock : lockTable.getLocks()) {
                if (!lock.getWsId().equals(dataManager.getWsId())) {
                    boolean orphaned = true;
                    for (String clientSessionName : clients) {
                        if (clientSessionName.startsWith(lock.getWsId()
                                .toString())) {
                            this.clientLocks.add(lock);
                            orphaned = false;
                            break;
                        }
                    }
                    if (orphaned) {
                        this.orphanedLocks.add(lock);
                    }
                }
            }
        }
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        top.setLayout(new GridLayout());

        Label lab = new Label(top, SWT.NONE);
        if (this.clientLocks.size() != 0 || this.orphanedLocks.size() != 0) {
            lab.setText("Select the locks to be broken:");
        } else {
            lab.setText("      No locks to break      ");
        }
        GridData data = new GridData(SWT.CENTER, SWT.TOP, true, false);
        lab.setLayoutData(data);

        if (clientLocks.size() > 0) {
            Label clientLabel = new Label(top, SWT.NONE);
            clientLabel.setText("Locks Owned By Active Clients:");
            Label warningLabel = new Label(top, SWT.NONE);
            warningLabel
                    .setText("Warning: Before Breaking These Locks, Please coordinate with the other users!!!");
            warningLabel.setForeground(Display.getCurrent().getSystemColor(
                    SWT.COLOR_RED));
            clientViewer = addLocks(clientLocks);
        }

        if (orphanedLocks.size() > 0) {
            Label orphanLabel = new Label(top, SWT.NONE);
            orphanLabel.setText("Orphaned Locks:");
            orphanedViewer = addLocks(orphanedLocks);
        }

        return top;
    }

    private CheckboxTableViewer addLocks(List<Lock> lockList) {
        // Make a CheckboxTableViewer for the locks
        int size = Math.min(lockList.size(), BOX_LIMIT);

        final CheckboxTableViewer ctv = CheckboxTableViewer.newCheckList(top,
                SWT.BORDER);
        ctv.setContentProvider(new ArrayContentProvider());
        ctv.setLabelProvider(new LabelProvider() {
            @Override
            public String getText(Object element) {
                return lockLabel((Lock) element);
            }
        });
        ctv.setInput(lockList);
        GridData data = new GridData(SWT.FILL, SWT.TOP, true, false);
        data.heightHint = ctv.getTable().getItemHeight() * size;
        ctv.getTable().setLayoutData(data);

        return ctv;
    }

    private String lockLabel(Lock lock) {
        ParmID lockParm = lock.getParmId();
        StringBuffer buf = new StringBuffer();

        buf.append(uiFormat.uiParmID(lockParm));

        buf.append(" [")
                .append(dateFormatter.format(lock.getTimeRange().getStart())
                        .replace("GMT", "z")).append(" -> ");
        buf.append(
                dateFormatter.format(lock.getTimeRange().getEnd()).replace(
                        "GMT", "z")).append("] owned by: ");
        buf.append(lock.getWsId().getUserName()).append(" (")
                .append(lock.getWsId().getProgName()).append(")")
                .append(" on: ").append(lock.getWsId().getHostName());
        return buf.toString();
    }

    private void clearAll() {
        if (clientViewer != null) {
            clientViewer.setAllChecked(false);
        }

        if (orphanedViewer != null) {
            orphanedViewer.setAllChecked(false);
        }
    }

    private void selectAllOrphans() {
        orphanedViewer.setAllChecked(true);
    }

    private void breakLocks(Object[] locks) {
        for (Object obj : locks) {
            Lock lock = (Lock) obj;
            dataManager.getParmOp().breakLock(lock.getParmId(),
                    lock.getTimeRange());
        }
    }

    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == CLEAR_ALL) {
            clearAll();

        } else if (buttonId == SELECT_ALL_ORPHANS) {
            selectAllOrphans();

        } else if (buttonId == BREAK_LOCK) {
            if (clientViewer != null) {
                breakLocks(clientViewer.getCheckedElements());
            }
            if (orphanedViewer != null) {
                breakLocks(orphanedViewer.getCheckedElements());
            }
            close();

        } else {
            super.buttonPressed(buttonId);
        }
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        if (clientLocks.size() > 0 || orphanedLocks.size() > 0) {
            super.createButton(parent, BREAK_LOCK, "BreakLock(s)", false);

            if (orphanedLocks.size() > 0) {
                super.createButton(parent, SELECT_ALL_ORPHANS,
                        "Select All Orphans", false);
            }
            super.createButton(parent, CLEAR_ALL, "Clear All", false);
        }
        super.createButton(parent, CANCEL, "Cancel", true);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Break Lock(s)");
    }
}
