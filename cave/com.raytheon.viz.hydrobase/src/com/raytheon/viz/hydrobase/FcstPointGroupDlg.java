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
package com.raytheon.viz.hydrobase;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrobase.listeners.IForecastGroupAssignmentListener;
import com.raytheon.viz.hydrocommon.data.RPFFcstGroupData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Forecast Point Group dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008				lvenable	Initial creation
 * Jan 7, 2008  1802        askripsk    Connect to DB
 * Jun 27,2013  2088        rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class FcstPointGroupDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FcstPointGroupDlg.class);

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Group list control.
     */
    private List groupList;

    /**
     * Cache of forecast groups
     */
    private java.util.List<RPFFcstGroupData> fcstGroups;

    /**
     * Previously selected group
     */
    private String previousGroup;

    /**
     * List of listeners to group assignment changes
     */
    private java.util.List<IForecastGroupAssignmentListener> fcstGroupListeners;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public FcstPointGroupDlg(Shell parent, String previousGroup) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Fcst Point Group Assignment");

        this.previousGroup = previousGroup;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        controlFont.dispose();
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
        setReturnValue(false);
        // Initialize all of the controls and layouts
        initializeComponents();

        getDialogData();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createListControl();

        // Add a horizontal separator.
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomButtons();
    }

    /**
     * Create the group label and list control.
     */
    private void createListControl() {
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));

        Label groupLbl = new Label(listComp, SWT.NONE);
        groupLbl.setText("Group");

        GridData gd = new GridData(325, 175);
        groupList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        groupList.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 90;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                fireUpdateEvent();
                close();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button clearBtn = new Button(buttonComp, SWT.PUSH);
        clearBtn.setText("Clear");
        clearBtn.setLayoutData(gd);
        clearBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                groupList.deselectAll();
                fireUpdateEvent();
                close();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Retrieves the Forecast Groups from the database.
     */
    private void getDialogData() {
        try {
            RPFFcstGroupData seedData = new RPFFcstGroupData();
            seedData.setOrderByStatement("ORDER BY group_id");

            fcstGroups = HydroDBDataManager.getInstance().getData(seedData);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to load RPF Forecast Group Data: ", e);
        }

        updateDisplay();
    }

    /**
     * Populates the display with the data from the database
     */
    private void updateDisplay() {
        groupList.removeAll();

        if (fcstGroups != null) {
            for (RPFFcstGroupData currGroup : fcstGroups) {
                groupList.add(currGroup.getGroupID());
            }

            // Select the old group, if there is one
            for (int i = 0; i < groupList.getItemCount(); i++) {
                if (groupList.getItem(i).equals(previousGroup)) {
                    groupList.select(i);
                    break;
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setMessage("No forecast groups have been defined under \"Setup\"");
            mb.open();
        }
    }

    /**
     * @return The currently selected group or NULL if a group is not selected.
     */
    private RPFFcstGroupData getSelectedFcstGroup() {
        RPFFcstGroupData selectedGroup = null;

        if (groupList.getSelectionCount() > 0) {
            selectedGroup = fcstGroups.get(groupList.getSelectionIndex());
        }

        return selectedGroup;
    }

    /**
     * Notifies the listeners of the fcst group change
     */
    private void fireUpdateEvent() {
        for (IForecastGroupAssignmentListener currListener : fcstGroupListeners) {
            currListener.notifyUpdate(getSelectedFcstGroup());
        }
    }

    /**
     * Adds a listener for fcst group changes
     * 
     * @param listener
     */
    public void addListener(IForecastGroupAssignmentListener listener) {
        if (fcstGroupListeners == null) {
            fcstGroupListeners = new ArrayList<IForecastGroupAssignmentListener>();
        }

        fcstGroupListeners.add(listener);
    }

    /**
     * Removes a listener for fcst group changes
     * 
     * @param listener
     */
    public void removeListener(IForecastGroupAssignmentListener listener) {
        fcstGroupListeners.remove(listener);
    }
}
