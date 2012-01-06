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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrobase.PreferencesData;
import com.raytheon.viz.hydrobase.PreferencesDataManager;
import com.raytheon.viz.hydrobase.PreferencesData.SortCriteria;
import com.raytheon.viz.hydrobase.listeners.IPreferencesListener;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class shows the preferences dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 5, 2008				lvenable	Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class PreferencesDlg extends CaveSWTDialog {

    /**
     * Handbook V identifier check box.
     */
    private Button handbookVIdChk;

    /**
     * Location name check box.
     */
    private Button locationNameChk;

    /**
     * State/County check box.
     */
    private Button stateCountyChk;

    /**
     * Basin check box.
     */
    private Button basinChk;

    /**
     * River/Stream check box.
     */
    private Button riverStreamChk;

    /**
     * Latitude/Longitude check box.
     */
    private Button latLonChk;

    /**
     * Station radio button.
     */
    private Button stationRdo;

    /**
     * Name radio button.
     */
    private Button nameRdo;

    /**
     * State/County radio button.
     */
    private Button stateCountyRdo;

    /**
     * Checked button count.
     */
    private int checkedBtns = 3;

    /**
     * Field included array.
     */
    private ArrayList<Button> fieldIncArray;

    /**
     * Maximum number of checked check boxes.
     */
    private int maxChecked = 3;

    /**
     * Listeners to notify main HB Dialog of settings
     */
    private ArrayList<IPreferencesListener> preferencesListeners;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public PreferencesDlg(Shell parent) {
        super(parent);
        setText("Preferences");
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        createWinTitleGroupControls();
        createFieldSortGroupControls();
        createBottomButtons();

        loadPreferences();
    }

    /**
     * Create Window Title group and controls.
     */
    private void createWinTitleGroupControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group winTitleGroup = new Group(shell, SWT.NONE);
        winTitleGroup.setLayout(new GridLayout(2, false));
        winTitleGroup.setLayoutData(gd);
        winTitleGroup.setText(" Window Title String ");

        gd = new GridData(200, SWT.DEFAULT);
        handbookVIdChk = new Button(winTitleGroup, SWT.CHECK);
        handbookVIdChk.setText("Handbook V Identifier");
        handbookVIdChk.setLayoutData(gd);

        locationNameChk = new Button(winTitleGroup, SWT.CHECK);
        locationNameChk.setText("Location Name");
    }

    /**
     * Create the Field Sort group and controls.
     */
    private void createFieldSortGroupControls() {
        fieldIncArray = new ArrayList<Button>();

        // ----------------------------------------------------
        // Create the main composite that will contain the
        // Fields Included & Sort Criteria groups
        // ----------------------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(2, false));
        mainComp.setLayoutData(gd);

        // ----------------------------------------------
        // Create the Fields Included group
        // ----------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group fieldsIncludedGroup = new Group(mainComp, SWT.NONE);
        fieldsIncludedGroup.setLayout(new GridLayout(1, false));
        fieldsIncludedGroup.setLayoutData(gd);
        fieldsIncludedGroup.setText(" Fields Included (Select 3) ");

        stateCountyChk = new Button(fieldsIncludedGroup, SWT.CHECK);
        stateCountyChk.setText("State,County");
        stateCountyChk.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                updateIncludedControls(stateCountyChk.getSelection());
            }
        });
        fieldIncArray.add(stateCountyChk);

        basinChk = new Button(fieldsIncludedGroup, SWT.CHECK);
        basinChk.setText("Basin");
        basinChk.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                updateIncludedControls(basinChk.getSelection());
            }
        });
        fieldIncArray.add(basinChk);

        riverStreamChk = new Button(fieldsIncludedGroup, SWT.CHECK);
        riverStreamChk.setText("River/Stream");
        riverStreamChk.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                updateIncludedControls(riverStreamChk.getSelection());
            }
        });
        fieldIncArray.add(riverStreamChk);

        latLonChk = new Button(fieldsIncludedGroup, SWT.CHECK);
        latLonChk.setText("Latitude/Longitude");
        latLonChk.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                updateIncludedControls(latLonChk.getSelection());
            }
        });
        fieldIncArray.add(latLonChk);

        // ----------------------------------------------
        // Create the Sort Criteria group
        // ----------------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group sortCriteriaGroup = new Group(mainComp, SWT.NONE);
        sortCriteriaGroup.setLayout(new GridLayout(1, false));
        sortCriteriaGroup.setLayoutData(gd);
        sortCriteriaGroup.setText(" Sort Criteria ");

        stationRdo = new Button(sortCriteriaGroup, SWT.RADIO);
        stationRdo.setText("Station");
        stationRdo.setSelection(true);

        nameRdo = new Button(sortCriteriaGroup, SWT.RADIO);
        nameRdo.setText("Name");

        stateCountyRdo = new Button(sortCriteriaGroup, SWT.RADIO);
        stateCountyRdo.setText("State,County");
    }

    /**
     * Create the buttons at the bottom of hte display.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                savePreferences();
                fireUpdateEvent();
                shell.dispose();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
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
     * Update the field included controls.
     * 
     * @param isSelected
     *            Flag indicating if a check box is selected.
     */
    private void updateIncludedControls(boolean isSelected) {
        /*
         * Check if a check box was unselected
         */
        if (isSelected == false) {
            /*
             * If the number of checked check boxes are equal to the maximum
             * number of checked check boxes allowed then enable all of the
             * check boxes.
             */
            if (checkedBtns == maxChecked) {
                for (Button chkBtn : fieldIncArray) {
                    chkBtn.setEnabled(true);
                }
            }
            --checkedBtns;
        } else {
            ++checkedBtns;
        }

        /*
         * If the number of selected check boxes equal the maximum number of
         * checked check boxes allowed then disable the remaining unselected
         * check boxes.
         */
        if (checkedBtns == maxChecked) {
            for (Button chkBtn : fieldIncArray) {
                if (chkBtn.getSelection() == false) {
                    chkBtn.setEnabled(false);
                }
            }
        }
    }

    /**
     * Adds the listener
     * 
     * @param prefListener
     *            The listener to add
     */
    public void addListener(IPreferencesListener prefListener) {
        if (preferencesListeners == null)
            preferencesListeners = new ArrayList<IPreferencesListener>();

        preferencesListeners.add(prefListener);
    }

    /**
     * Removes the listener
     */
    public void removeListener(IPreferencesListener prefListener) {
        preferencesListeners.remove(prefListener);
    }

    private void loadPreferences() {
        PreferencesDataManager dm = PreferencesDataManager.getInstance();

        int columnsToDisplay = dm.getDisplayColumns();
        SortCriteria selectedSort = dm.getSortCriteria();

        // Load the window title settings
        handbookVIdChk.setSelection(dm.getShowLID());
        locationNameChk.setSelection(dm.getShowName());

        // Load the displayed columns
        checkedBtns = 0;

        if ((columnsToDisplay & PreferencesData.SHOW_BASIN) != 0) {
            basinChk.setSelection(true);
            checkedBtns++;
        } else {
            basinChk.setSelection(false);
        }

        if ((columnsToDisplay & PreferencesData.SHOW_STATE_COUNTY) != 0) {
            stateCountyChk.setSelection(true);
            checkedBtns++;
        } else {
            stateCountyChk.setSelection(false);
        }

        if ((columnsToDisplay & PreferencesData.SHOW_RIVER_STREAM) != 0) {
            riverStreamChk.setSelection(true);
            checkedBtns++;
        } else {
            riverStreamChk.setSelection(false);
        }

        if ((columnsToDisplay & PreferencesData.SHOW_LAT_LON) != 0) {
            latLonChk.setSelection(true);
            checkedBtns++;
        } else {
            latLonChk.setSelection(false);
        }

        if (checkedBtns == maxChecked) {
            for (Button chkBtn : fieldIncArray) {
                if (chkBtn.getSelection() == false) {
                    chkBtn.setEnabled(false);
                }
            }
        }

        // Load the sort criteria
        nameRdo.setSelection(selectedSort == SortCriteria.NAME);
        stationRdo.setSelection(selectedSort == SortCriteria.STATION);
        stateCountyRdo.setSelection(selectedSort == SortCriteria.STATE_COUNTY);
    }

    /**
     * Saves preferences
     */
    private void savePreferences() {
        PreferencesDataManager dm = PreferencesDataManager.getInstance();

        // Save display columns
        dm.setSelectedColumns(stateCountyChk.getSelection(), basinChk
                .getSelection(), riverStreamChk.getSelection(), latLonChk
                .getSelection());

        // Save sort preference
        if (stationRdo.getSelection())
            dm.setSortCriteria(SortCriteria.STATION);
        else if (nameRdo.getSelection())
            dm.setSortCriteria(SortCriteria.NAME);
        else if (stateCountyRdo.getSelection())
            dm.setSortCriteria(SortCriteria.STATE_COUNTY);

        // Save dialog preferences
        dm.setTitleString(handbookVIdChk.getSelection(), locationNameChk
                .getSelection());

        try {
            dm.savePreferences();
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Notifies the listeners of the preference changes
     */
    private void fireUpdateEvent() {
        for (IPreferencesListener currListener : preferencesListeners) {
            currListener.notifyUpdate();
        }
    }
}
