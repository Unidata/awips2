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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.CountiesData;
import com.raytheon.viz.hydrocommon.data.CountyInfoData;
import com.raytheon.viz.hydrocommon.data.EligZoneData;
import com.raytheon.viz.hydrocommon.data.ZoneInfoData;
import com.raytheon.viz.hydrocommon.datamanager.CountyZoneUgcDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the County Zone UGC dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Sep 2008             lvenable    Initial creation.
 * 30 Dec 2008  1802       askripsk    Connect to database.
 * 04 Dec 2012  15522      wkwock      Fix incorrect zones and not able to add
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class CountyZoneUgcDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Available County/Zone list control.
     */
    private List availableList;

    /**
     * Selected County/Zone list control.
     */
    private List selectedList;

    /**
     * County/Zone selection combo box.
     */
    private Combo selectionCbo;

    /**
     * Add button.
     */
    private Button addBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Clear button.
     */
    private Button clearBtn;

    /**
     * for keep track whether selected zones are initialized.  false is not initialized.
     */
    private boolean zonesFlag = false;
    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public CountyZoneUgcDlg(Shell parent, String titleInfo, String lid) {
        super(parent);
        setText("County/Zone UGC" + titleInfo);

        CountyZoneUgcDataManager.getInstance().setLid(lid);
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
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createSelectionControl();
        addSeparator();
        createListAndControls();
        addSeparator();
        createBottomButtons();

        getDialogData();
    }

    /**
     * Create the county/zone label and selection control.
     */
    private void createSelectionControl() {
        Composite selectionComp = new Composite(shell, SWT.NONE);
        selectionComp.setLayout(new GridLayout(2, false));

        Label selectionLbl = new Label(selectionComp, SWT.NONE);
        selectionLbl.setText("Selection: ");

        selectionCbo = new Combo(selectionComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        selectionCbo.add("Counties");
        selectionCbo.add("Zones");
        selectionCbo.select(0);
        selectionCbo.setLayoutData(new GridData(120, SWT.DEFAULT));
        selectionCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
				if (!zonesFlag) {
					getZoneData();
				}
                updateDisplay();
            }
        });
    }

    /**
     * Create the available and selected list controls and labels.
     */
    private void createListAndControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(3, false));
        listComp.setLayoutData(gd);

        Label availableLbl = new Label(listComp, SWT.NONE);
        availableLbl.setText("Available");

        // Filler label
        new Label(listComp, SWT.NONE);

        Label selectedLbl = new Label(listComp, SWT.NONE);
        selectedLbl.setText("Selected");

        // --------------------------------------
        // Create the Available list
        // --------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 350;
        gd.heightHint = 300;
        availableList = new List(listComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        availableList.setLayoutData(gd);
        availableList.setFont(controlFont);

        // --------------------------------------
        // Create the list controls
        // --------------------------------------
        Composite buttonComp = new Composite(listComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(new GridData(SWT.DEFAULT, SWT.FILL, false,
                true));

        gd = new GridData(SWT.DEFAULT, SWT.BOTTOM, false, true);
        gd.widthHint = 100;
        addBtn = new Button(buttonComp, SWT.PUSH);
        addBtn.setText("Add");
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                addRecord();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.widthHint = 100;
        clearBtn = new Button(buttonComp, SWT.PUSH);
        clearBtn.setText("Clear");
        clearBtn.setLayoutData(gd);
        clearBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                clearRecords();
            }
        });

        // --------------------------------------
        // Create the Selected list
        // --------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 350;
        gd.heightHint = 300;
        selectedList = new List(listComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        selectedList.setLayoutData(gd);
        selectedList.setFont(controlFont);
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        Composite buttonComp = new Composite(mainButtonComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(100, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveRecords();
                shell.dispose();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveRecords();
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
     * Add a horizontal separator to the main display.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Retrieves data via the Data Manager and updates the display.
     */
    private void getDialogData() {
        if (countiesDisplayed()) {
            getCountyData();
        } else {
            getZoneData();
        }

        updateDisplay();
    }

    /**
     * Retrieves the County data via the Data Manager.
     */
    private void getCountyData() {
        try {
            CountyZoneUgcDataManager.getInstance().getCountiesSelected(true);
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * Retrieves the Zone data via the Data Manager.
     */
    private void getZoneData() {
        try {
            CountyZoneUgcDataManager.getInstance().getZonesSelected(true);
            zonesFlag=true;
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * Refreshed the data being displayed
     */
    private void updateDisplay() {
        availableList.removeAll();
        selectedList.removeAll();

        if (countiesDisplayed()) {
            updateCountyDisplay();
        } else {
            updateZoneDisplay();
        }
    }

    /**
     * Displays the current County data.
     */
    private void updateCountyDisplay() {
        String countyFormat = "%3s   %-20s   %2s";

        try {
            for (CountiesData availCounty : CountyZoneUgcDataManager
                    .getInstance().getCountiesAvailable()) {
                availableList.add(String.format(countyFormat, availCounty
                        .getCountyNumber(), availCounty.getCounty(),
                        availCounty.getState()));
            }

            for (CountyInfoData selectedCounty : CountyZoneUgcDataManager
                    .getInstance().getCountiesSelected()) {
                selectedList.add(String.format(countyFormat, selectedCounty
                        .getCountyNumber(), selectedCounty.getCounty(),
                        selectedCounty.getState()));
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * Displayes the current Zone data.
     */
    private void updateZoneDisplay() {
        String zoneFormat = "%3s   %-20s   %2s";

        try {
            for (EligZoneData availZone : CountyZoneUgcDataManager
                    .getInstance().getZonesAvailable()) {
                availableList.add(String.format(zoneFormat, availZone
                        .getZoneNumber(), availZone.getDescription(), availZone
                        .getState()));
            }

            for (ZoneInfoData selectedZone : CountyZoneUgcDataManager
                    .getInstance().getZonesSelected()) {
                selectedList.add(String.format(zoneFormat, selectedZone
                        .getZoneNumber(), selectedZone.getDescription(),
                        selectedZone.getState()));
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * Determines if the dialog is displaying Counties or Zones
     * 
     * @return True if counties are currently being displayed; False for zones.
     */
    private boolean countiesDisplayed() {
        return selectionCbo.getItem(selectionCbo.getSelectionIndex()).equals(
                "Counties");
    }

    private void addRecord() {
        int selectedIndex = availableList.getSelectionIndex();

        if (selectedIndex >= 0) {
            if (countiesDisplayed()) {
                CountyZoneUgcDataManager.getInstance().addSelectedCounty(
                        selectedIndex);
            } else {
                CountyZoneUgcDataManager.getInstance().addSelectedZone(
                        selectedIndex);
            }
        }

        updateDisplay();
    }

    private void deleteRecord() {
        int selectedIndex = selectedList.getSelectionIndex();

        if (selectedIndex >= 0) {
            if (countiesDisplayed()) {
                CountyZoneUgcDataManager.getInstance().removeSelectedCounty(
                        selectedIndex);
            } else {
                CountyZoneUgcDataManager.getInstance().removeSelectedZone(
                        selectedIndex);
            }
        }

        updateDisplay();
    }

    private void clearRecords() {
        if (countiesDisplayed()) {
            CountyZoneUgcDataManager.getInstance().clearSelectedCounties();
        } else {
            CountyZoneUgcDataManager.getInstance().clearSelectedZones();
        }

        updateDisplay();
    }

    private void saveRecords() {
        try {
            if (countiesDisplayed()) {
                CountyZoneUgcDataManager.getInstance().saveCounties();
            } else {
                CountyZoneUgcDataManager.getInstance().saveZones();
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        getDialogData();
    }
}
