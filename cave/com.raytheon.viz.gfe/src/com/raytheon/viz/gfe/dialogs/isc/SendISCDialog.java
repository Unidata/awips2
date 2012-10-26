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

package com.raytheon.viz.gfe.dialogs.isc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SendISCRequest;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.ISelectTimeRangeManager;
import com.raytheon.viz.gfe.core.parm.ParmOp;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.widgets.ToggleSelectList;

/**
 * Send Intersite grids dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/20/09      1995       lvenable    Initial creation
 * 09/02/09          #1370  randerso    Make the same as PublishDialog
 * 10/26/2012   1287        rferrel    Code cleanup for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class SendISCDialog extends CaveJFACEDialog {

    private final int MAX_LIST_HEIGHT = 10;

    private final PythonPreferenceStore prefs = Activator.getDefault()
            .getPreferenceStore();

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

    private List tpListbox;

    private Button autoBtn;

    private Button manBtn;

    public SendISCDialog(Shell parent, DataManager dataManager) {
        // Constructor for the SendISCDialog

        super(parent);
        this.dataManager = dataManager;
        this.parmMgr = dataManager.getParmManager();
        this.parmOp = dataManager.getParmOp();
        this.mutableDb = parmMgr.getMutableDatabase();

        // this next section ensures that these weather elements exist
        // in the ifpServer (official database), thus we eliminate weather
        // elements that are temporary
        ParmID[] availableParms = parmMgr.getAvailableParms(this.mutableDb);
        DatabaseID officialDb = parmMgr.getProductDB();
        ParmID[] officialParms = parmMgr.getAvailableParms(officialDb);

        // eliminate any parms in availableParms that aren't in officialParms
        // eliminate any parms in availableParms in the ISC_neverSendParms list
        java.util.List<String> limitedParms = Arrays.asList(prefs
                .getStringArray("ISC_neverSendParms"));
        ArrayList<ParmID> temp = new ArrayList<ParmID>();
        for (ParmID p : availableParms) {
            for (ParmID o : officialParms) {
                if (p.getParmName().equals(o.getParmName())
                        && p.getParmLevel().equals(o.getParmLevel())
                        && !limitedParms.contains(p.getParmName())) {
                    temp.add(p);
                    break;
                }
            }
        }

        Collections.sort(temp);
        this.availableParms = temp.toArray(new ParmID[temp.size()]);
        String initialBundleGrp = prefs
                .getString("SendISCGridDialogInitialWEGroup");
        if (initialBundleGrp.isEmpty()) {
            this.displayedParms = this.parmMgr.getParmIDs(this.parmMgr
                    .getDisplayedParms());
        } else {
            this.displayedParms = this.dataManager.getWEGroupManager()
                    .getParmIDs(initialBundleGrp, this.availableParms);
        }
        this.selectedParms = this.parmMgr.getParmIDs(this.parmMgr
                .getSelectedParms());

        // get list from config for which time periods should be displayed
        this.timePd = prefs.getStringArray("SendISCTimes");
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

        shell.setText("Send Intersite Grids");
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, "SendISCGrids", true);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);

        // make the auto/manual button pane
        makeAutoManBox(comp);

        top = new Composite(comp, SWT.NONE);
        GridLayout layout = new GridLayout(2, true);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        top.setLayout(layout);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        top.setLayoutData(layoutData);

        // make the Weather Element part of the dialog
        makeWEBox();

        // make the Time Period part of the dialog
        makeTimePeriodBox();

        // make another button box
        makeAllButtonBox();

        modeChanged();

        return top;
    }

    protected void modeChanged() {
        boolean manual = manBtn.getSelection();
        ((GridData) top.getLayoutData()).exclude = !manual;
        top.setVisible(manual);
        getShell().pack();
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
        final Menu menu = makeWxGroupMenu();
        groupBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = groupBtn.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = btnComp.toDisplay(pt);
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
        java.util.List<String> groupInv = dataManager.getWEGroupManager()
                .getInventory();

        ArrayList<String> filtGroupInv = new ArrayList<String>();
        for (String group : groupInv) {
            // verify that there are valid entries in it
            if (dataManager.getWEGroupManager().getParmIDs(group,
                    this.availableParms).length > 0) {
                filtGroupInv.add(group);
            }
        }

        Menu menu = new Menu(getShell(), SWT.POP_UP);
        for (String group : filtGroupInv) {
            MenuItem item = new MenuItem(menu, SWT.PUSH);
            item.setText(group);
            item.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    openBundleCB(((MenuItem) e.widget).getText());
                }
            });
        }
        return menu;
    }

    private void openBundleCB(String bundleName) {
        // Callback to open the selected Bundle
        // Get the list of ParmIDs, mutable db
        ParmID[] parmIDs = this.dataManager.getWEGroupManager().getParmIDs(
                bundleName, this.availableParms);

        // reset all entries (weather elements) to off
        this.weListbox.deselectAll();
        for (int index = 0; index < this.availableParms.length; index++) {
            if (Arrays.asList(parmIDs).contains(this.availableParms[index])) {
                this.weListbox.select(index);
            }
        }
    }

    protected void makeAutoManBox(Composite comp) {
        Group autoManGroup = new Group(comp, SWT.SHADOW_NONE);
        GridLayout layout = new GridLayout(1, false);
        layout.verticalSpacing = 0;
        autoManGroup.setLayout(layout);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.horizontalSpan = 2;
        autoManGroup.setLayoutData(layoutData);

        autoBtn = new Button(autoManGroup, SWT.RADIO);
        autoBtn.setText("Auto: Sends all grids saved but not yet sent");
        autoBtn.setSelection(true);
        autoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                modeChanged();
            }
        });

        manBtn = new Button(autoManGroup, SWT.RADIO);
        manBtn.setText("Manual: Sends only what is selected below");
        manBtn.setSelection(false);
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

        tpListbox = new List(tpGroup, SWT.SINGLE | SWT.V_SCROLL);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.heightHint = tpListbox.getItemHeight() * MAX_LIST_HEIGHT;
        tpListbox.setLayoutData(layoutData);

        ArrayList<String> elementList = new ArrayList<String>();
        elementList.add("All Grids");
        elementList.add("Selected Time");

        // get list from config for which time periods should be displayed
        if (this.timePd != null) {
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

    private java.util.List<ParmID> getParms() {
        // make a list of the parms for which the check button is selected
        ArrayList<ParmID> parmsIDs = new ArrayList<ParmID>();
        for (int i = 0; i < availableParms.length; i++) {
            if (weListbox.isSelected(i)) {
                parmsIDs.add(availableParms[i]);
            }
        }
        return parmsIDs;
    }

    private void sendISCCB() {
        java.util.List<SendISCRequest> requests = new ArrayList<SendISCRequest>();

        // manual configuration
        if (manBtn.getSelection()) {
            // get the selected parms
            java.util.List<ParmID> parmsIDs = getParms();

            // make a SendISCRequest for each parm
            for (ParmID parm : parmsIDs) {
                // get the time range for each parm
                TimeRange tr = getTR(parm);

                // make and append the SendISCRequest
                requests.add(new SendISCRequest(parm, tr));
            }
        }
        // auto configuration - send 1 empty request
        else {
            requests.add(new SendISCRequest());
        }

        // LogStream.logUse("Send ISC: ", requests);

        // send the data by calling the parm op command
        parmOp.sendISC(requests);
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
        for (ParmID parmID : this.selectedParms) {
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
        sendISCCB();
        super.okPressed();
    }
}
