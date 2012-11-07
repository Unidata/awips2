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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.request.CommitGridRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.ISelectTimeRangeManager;
import com.raytheon.viz.gfe.core.msgs.ISCSendStatusChangedMsg;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.parm.ParmOp;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * The publish to official dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Mar 6, 2008            Eric Babin  Initial Creation
 * Sep 01, 2009      #1370 randerso    Completely reworked
 * Aug 05, 2010 6698       mpduff      Moved Publish work to its own thread.
 * Oct 25, 2012 1287       rferrel     Code cleanup for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */
public class PublishDialog extends CaveJFACEDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PublishDialog.class);

    private final int MAX_LIST_HEIGHT = 10;

    private final PythonPreferenceStore prefs = Activator.getDefault()
            .getPreferenceStore();

    private static Boolean IscStateP;

    private DataManager dataManager;

    private Composite top;

    private IParmManager parmMgr;

    private ParmID[] availableParms;

    private ParmID[] displayedParms;

    private ParmID[] selectedParms;

    private String[] timePd;

    private ParmOp parmOp;

    private DatabaseID mutableDb;

    private ToggleSelectList weListbox;

    private org.eclipse.swt.widgets.List tpListbox;

    public PublishDialog(Shell parent, DataManager dataManager) {
        // Constructor for the Publish Dialog

        super(parent);
        this.dataManager = dataManager;
        parmMgr = dataManager.getParmManager();
        parmOp = dataManager.getParmOp();
        mutableDb = parmMgr.getMutableDatabase();
        ParmID[] availableParms = parmMgr.getAvailableParms(mutableDb);
        DatabaseID officialDb = parmMgr.getProductDB();
        ParmID[] officialParms = parmMgr.getAvailableParms(officialDb);

        // eliminate any parms in availableParms that isn't in officialParms
        ArrayList<ParmID> temp = new ArrayList<ParmID>();
        for (ParmID p : availableParms) {
            for (ParmID o : officialParms) {
                if (p.getParmName().equals(o.getParmName())
                        && p.getParmLevel().equals(o.getParmLevel())) {
                    temp.add(p);
                    break;
                }
            }
        }

        Collections.sort(temp);
        this.availableParms = temp.toArray(new ParmID[temp.size()]);
        String initialBundleGrp = prefs
                .getString("PublishDialogInitialWEGroup");
        if (initialBundleGrp.isEmpty()) {
            displayedParms = parmMgr.getParmIDs(parmMgr.getDisplayedParms());
        } else {
            displayedParms = this.dataManager.getWEGroupManager().getParmIDs(
                    initialBundleGrp, this.availableParms);
        }
        selectedParms = parmMgr.getParmIDs(parmMgr.getSelectedParms());

        timePd = prefs.getStringArray("PublishTimes");
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

        shell.setText("Publish to Official");
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, "Publish", true);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout(2, true);
        top.setLayout(layout);

        // make the ISC note if needed and pack it
        addISCNote();

        // make the Weather Element part of the dialog
        makeWEBox();

        // make the Time Period part of the dialog
        makeTimePeriodBox();

        // make another button box
        makeAllButtonBox();

        return top;
    }

    protected void makeAllButtonBox() {
        final Composite btnComp = new Composite(top, SWT.NONE);
        GridLayout layout = new GridLayout(4, true);
        btnComp.setLayout(layout);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.horizontalSpan = 2;
        btnComp.setLayoutData(layoutData);

        Button setAllBtn = new Button(btnComp, SWT.PUSH);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        setAllBtn.setLayoutData(layoutData);
        setAllBtn.setText("Set All");
        setAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setAll();
            }
        });

        Button setSelBtn = new Button(btnComp, SWT.PUSH);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        setSelBtn.setLayoutData(layoutData);
        setSelBtn.setText("Set Selected");
        setSelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setSelected();
            }
        });

        final Button groupBtn = new Button(btnComp, SWT.PUSH);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        groupBtn.setLayoutData(layoutData);
        groupBtn.setText("&Groups");
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        groupBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = groupBtn.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = btnComp.toDisplay(pt);
                Menu menu = makeWxGroupMenu();
                menu.setLocation(pt.x, pt.y);
                menu.setVisible(true);
            }
        });

        Button clearAllBtn = new Button(btnComp, SWT.PUSH);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        clearAllBtn.setLayoutData(layoutData);
        clearAllBtn.setText("Clear All");
        clearAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                clearAll();
            }
        });
    }

    protected Menu makeWxGroupMenu() {
        // Get the list of Weather Element Groups
        List<String> groupInv = dataManager.getWEGroupManager().getInventory();

        ArrayList<String> filtGroupInv = new ArrayList<String>();
        for (String group : groupInv) {
            // verify that there are valid entries in it
            if (dataManager.getWEGroupManager().getParmIDs(group,
                    availableParms).length > 0) {
                filtGroupInv.add(group);
            }
        }

        Menu menu = new Menu(getShell(), SWT.POP_UP);
        for (String group : filtGroupInv) {
            MenuItem item = new MenuItem(menu, SWT.PUSH);
            item.setData(group);
            group = group.replace("&", "&&");
            item.setText(group);
            item.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    openBundleCB((String) ((MenuItem) e.widget).getData());
                }
            });
        }
        return menu;
    }

    private void addISCNote() {
        // get new state
        ISCSendStatusChangedMsg msg = Message
                .inquireLastMessage(ISCSendStatusChangedMsg.class);
        boolean newState = msg.isEnabled();
        if (IscStateP != null && (IscStateP.booleanValue() == newState)) {
            return; // no message needed
        }
        IscStateP = newState; // save new state
        String t;
        if (this.dataManager.sendIscOnPublish()) {
            if (newState) {
                // enabled
                t = "ISC Send on Publish is now ENABLED. These published\n"
                        + "grids will be sent via ISC.";
            } else {
                t = "ISC Send on Publish is now DISABLED. These published\n"
                        + "grids will be not sent via ISC. If desired, use the\n"
                        + "Consistency->ISC Send Enable menu before publishing grids.";
            }
            // Tkinter.Label(master, foreground='red', background='white',
            // text="Notice: " + t).pack(side=Tkinter.TOP)
            Label notice = new Label(top, SWT.BORDER);
            notice.setText("Notice: " + t);
            notice.setBackground(notice.getDisplay().getSystemColor(
                    SWT.COLOR_WHITE));
            notice.setForeground(notice.getDisplay().getSystemColor(
                    SWT.COLOR_RED));
            GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true,
                    false, 2, 1);
            notice.setLayoutData(layoutData);
        }
    }

    private void openBundleCB(String bundleName) {
        // Callback to open the selected Bundle
        // Get the list of ParmIDs, mutable db
        ParmID[] parmIDs = dataManager.getWEGroupManager().getParmIDs(
                bundleName, availableParms);

        // reset all entries (weather elements) to off
        weListbox.deselectAll();
        for (int index = 0; index < availableParms.length; index++) {
            if (Arrays.asList(parmIDs).contains(availableParms[index])) {
                weListbox.select(index);
            }
        }
    }

    protected void makeWEBox() {
        // make the frame for the Weather Element
        Group weGroup = new Group(top, SWT.SHADOW_NONE);
        weGroup.setText("Weather Elements");
        GridLayout layout = new GridLayout(1, false);
        weGroup.setLayout(layout);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        weGroup.setLayoutData(layoutData);

        weListbox = new ToggleSelectList(weGroup, SWT.MULTI | SWT.V_SCROLL);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.heightHint = weListbox.getItemHeight() * MAX_LIST_HEIGHT;
        weListbox.setLayoutData(layoutData);

        String[] elementList = new String[availableParms.length];
        int i = 0;
        for (ParmID parmID : availableParms) {
            elementList[i++] = parmID.compositeNameUI();
        }
        weListbox.setItems(elementList);

        int cnt = 0;
        for (ParmID parmID : availableParms) {
            if (Arrays.asList(displayedParms).contains(parmID)) {
                weListbox.select(cnt);
            }
            cnt += 1;
        }

    }

    protected void makeTimePeriodBox() {
        // make the frame for the Weather Element
        Group tpGroup = new Group(top, SWT.SHADOW_NONE);
        tpGroup.setText("Time Period");
        GridLayout layout = new GridLayout(1, false);
        tpGroup.setLayout(layout);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        tpGroup.setLayoutData(layoutData);

        tpListbox = new org.eclipse.swt.widgets.List(tpGroup, SWT.SINGLE
                | SWT.V_SCROLL);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.heightHint = tpListbox.getItemHeight() * MAX_LIST_HEIGHT;
        tpListbox.setLayoutData(layoutData);

        ArrayList<String> elementList = new ArrayList<String>();
        elementList.add("All Grids");
        elementList.add("Selected Time");

        // get list from config for which time periods should be displayed
        if (timePd != null) {
            ISelectTimeRangeManager selectTRmgr = dataManager
                    .getSelectTimeRangeManager();
            for (String period : timePd) {
                if (selectTRmgr.getRange(period) != null) {
                    elementList.add(period);
                }
            }
        }

        tpListbox.setItems(elementList.toArray(new String[elementList.size()]));
        tpListbox.select(0);
    }

    private List<ParmID> getParms() {
        // make a list of the parms for which the check button is selected
        ArrayList<ParmID> parmsIDs = new ArrayList<ParmID>();
        for (int i = 0; i < availableParms.length; i++) {
            if (weListbox.isSelected(i)) {
                parmsIDs.add(availableParms[i]);
            }
        }
        return parmsIDs;
    }

    private void publishCB() {
        final Cursor origCursor = getShell().getCursor();
        getShell().setCursor(
                getShell().getDisplay().getSystemCursor(SWT.CURSOR_WAIT));

        // get the selected parms
        List<ParmID> parmsIDs = getParms();
        final List<CommitGridRequest> requests = new ArrayList<CommitGridRequest>(
                parmsIDs.size());

        // make a CommitGridRequest for each parm
        for (ParmID parm : parmsIDs) {
            // get the time range for each parm
            TimeRange tr = getTR(parm);

            // add the commit request
            requests.add(new CommitGridRequest(parm, tr, dataManager
                    .clientISCSendStatus()));
        }

        Job publishJob = new Job("Publish Job") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                long t0 = System.currentTimeMillis();

                try {
                    // publish the data by calling the dataManager
                    // publish function
                    statusHandler.handle(Priority.EVENTA, "PUBLISH: "
                            + requests);
                    parmOp.publish(requests);
                } catch (Exception e) {
                    statusHandler.handle(Priority.ERROR,
                            "Error occurred publishing parms " + requests, e);
                }

                // done with the dialog
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        PublishDialog.this.getShell().setCursor(origCursor);
                        PublishDialog.super.okPressed();
                    }
                });

                long t1 = System.currentTimeMillis();
                System.out.println("GFE Publish took " + (t1 - t0) + " ms");

                return Status.OK_STATUS;
            }

        };
        publishJob.setSystem(true);
        publishJob.schedule();
    }

    protected TimeRange getTR(ParmID parm) {
        // get the selected time range
        String selTR = tpListbox.getSelection()[0];

        TimeRange tr;
        if (selTR.equals("All Grids")) {
            tr = TimeRange.allTimes();

        } else if (selTR.equals("Selected Time")) {
            tr = parmOp.getSelectionTimeRange();

        } else { // time range
            tr = dataManager.getSelectTimeRangeManager().getRange(selTR)
                    .toTimeRange();
        }

        return tr;
    }

    private void setSelected() {
        weListbox.deselectAll();
        for (ParmID parmID : selectedParms) {
            int idx = Arrays.asList(availableParms).indexOf(parmID);
            weListbox.select(idx);
        }
        tpListbox.setSelection(1); // Selected Time
    }

    private void setAll() {
        tpListbox.setSelection(0);
        weListbox.selectAll();
    }

    private void clearAll() {
        tpListbox.setSelection(0);
        weListbox.deselectAll();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#okPressed()
     */
    @Override
    protected void okPressed() {
        this.getShell().setEnabled(false);
        publishCB();
    }
}
