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
package com.raytheon.viz.hydrobase.dialogs;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrobase.listeners.ICountyStateListener;
import com.raytheon.viz.hydrocommon.data.CountiesData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the County/State dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Sep 2008             lvenable    Initial creation.
 * Dec 5, 2008  1744       askripsk    Connect to DB
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class CountyStateDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * County/State list control.
     */
    private List countyStateList;

    /**
     * Hold the county/state data
     */
    private ArrayList<CountiesData> countiesData;

    /**
     * List of listeners for changes
     */
    private ArrayList<ICountyStateListener> countyStateListeners;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public CountyStateDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("County/State");
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(true);

        // Initialize all of the controls and layouts
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createCountyStateControls();

        // Add a separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomButtons();

        // Load data
        getDialogData();
    }

    /**
     * Open method used to display the dialog.
     * 
     * @return True/False.
     */
    public Object open(CountiesData selectedData) {
        open();

        setSelection(selectedData);

        Display display = getDisplay();

        // Manually block here
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return getReturnValue();
    }

    /**
     * Create the county and state controls.
     */
    private void createCountyStateControls() {
        Composite countyStateComp = new Composite(shell, SWT.NONE);
        countyStateComp.setLayout(new GridLayout(1, false));
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        countyStateComp.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 4;
        Label countyStateLbl = new Label(countyStateComp, SWT.NONE);
        countyStateLbl.setText(getCountyStateListLabelText());
        countyStateLbl.setFont(controlFont);
        countyStateLbl.setLayoutData(gd);

        gd = new GridData(400, 200);
        countyStateList = new List(countyStateComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        countyStateList.setLayoutData(gd);
        countyStateList.setFont(controlFont);
        countyStateList.deselectAll();
    }

    /**
     * Create the bottom buttons.
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

                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button clearBtn = new Button(buttonComp, SWT.PUSH);
        clearBtn.setText("Clear");
        clearBtn.setLayoutData(gd);
        clearBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                countyStateList.deselectAll();
                fireUpdateEvent();
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Get the county/state list label text.
     * 
     * @return List label text.
     */
    private String getCountyStateListLabelText() {
        String format = "%S                               %S";

        String labelStr = String.format(format, "County", "State");

        return labelStr;
    }

    /**
     * Retrieves the county/state data from the db
     */
    private void getDialogData() {
        try {
            countiesData = HydroDBDataManager.getInstance().getData(
                    CountiesData.class);

            // Display the data
            updateDialogDisplay();
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * Displays the county/state data
     */
    private void updateDialogDisplay() {
        countyStateList.removeAll();

        for (CountiesData currCounty : countiesData) {
            countyStateList.add(String.format("%-20s %-2s", currCounty
                    .getCounty(), currCounty.getState()));
        }
    }

    /**
     * Gets the currently selected county/state data
     * 
     * @return
     */
    private CountiesData getSelectedCountyState() {
        CountiesData rval;

        if (countyStateList.getSelectionCount() > 0) {
            rval = countiesData.get(countyStateList.getSelectionIndex());
        } else {
            rval = new CountiesData();
            rval.setCounty("XXXXXXXXXXXXXXXXXXXX");
            rval.setState("XX");
        }

        return rval;
    }

    /**
     * Notifies the listeners of the county/state change
     */
    private void fireUpdateEvent() {
        for (ICountyStateListener currListener : countyStateListeners) {
            currListener.notifyUpdate(getSelectedCountyState());
        }
    }

    /**
     * Adds a listener for county/state changes
     * 
     * @param listener
     */
    public void addListener(ICountyStateListener listener) {
        if (countyStateListeners == null) {
            countyStateListeners = new ArrayList<ICountyStateListener>();
        }

        countyStateListeners.add(listener);
    }

    /**
     * Removes a listener for county/state changes
     * 
     * @param listener
     */
    public void removeListener(ICountyStateListener listener) {
        countyStateListeners.remove(listener);
    }

    /**
     * Sets the currently selected county/state.
     * 
     * @param selectedData
     */
    public void setSelection(CountiesData selectedData) {
        if (selectedData == null) {
            return;
        }
        String currRow;
        String currCounty;
        String currState;
        String[] countyState;

        // Clear selection
        countyStateList.deselectAll();

        // Find the county/state and select it
        for (int i = 0; i < countyStateList.getItemCount(); i++) {
            currRow = countyStateList.getItem(i);

            // Split on whitespace
            countyState = currRow.split("\\s+");

            // Save the county and state
            currCounty = countyState[0];
            currState = countyState[1];

            // compare
            if (selectedData.getCounty().equalsIgnoreCase(currCounty)
                    && selectedData.getState().equalsIgnoreCase(currState)) {
                countyStateList.setSelection(i);
                break;
            }
        }
    }
}
