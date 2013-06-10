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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.RadarLocData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Radar Locations dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 8, 2008				lvenable	Initial creation
 * Dec 30, 2008 1802        askripsk    Connect to database.
 * Apr 26, 2013 1790        rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class RadarLocationsDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarLocationsDlg.class);

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Active check box.
     */
    private Button activeChk;

    /**
     * ID text control.
     */
    private Text idTF;

    /**
     * Number text control.
     */
    private Text numberTF;

    /**
     * Prefix text control.
     */
    private Text prefixTF;

    /**
     * Name text control.
     */
    private Text nameTF;

    /**
     * State text control.
     */
    private Text stateTF;

    /**
     * Bias Source text control.
     */
    private Text biasTF;

    /**
     * Latitude text control.
     */
    private Text latTF;

    /**
     * Longitude text control.
     */
    private Text lonTF;

    /**
     * Elevation text control.
     */
    private Text elevationTF;

    /**
     * Tower Height text control.
     */
    private Text towerHeightTF;

    /**
     * Add button.
     */
    private Button addBtn;

    /**
     * Update button.
     */
    private Button updateBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Cache of Radar Locations
     */
    private java.util.List<RadarLocData> radarData;

    /**
     * States of the dialog
     */
    private enum DialogStates {
        NO_DATA, DATA_AVAILABLE
    }

    /**
     * Non-blocking Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public RadarLocationsDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Radar Locations");
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
     * shell.dispos (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createDataListControl();
        createParametersGroup();
        createCloseButton();

        getDialogData();
    }

    /**
     * Create data label and list control;
     */
    private void createDataListControl() {
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));

        GridData gd = new GridData();
        gd.horizontalIndent = 4;
        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText(getListLabelText());
        listLbl.setFont(controlFont);
        listLbl.setLayoutData(gd);

        gd = new GridData(925, 150);
        dataList = new List(listComp, SWT.BORDER | SWT.V_SCROLL);
        dataList.setFont(controlFont);
        dataList.setLayoutData(gd);
        dataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateInformation();
            }
        });
    }

    /**
     * Create the parameters for selected radar group and controls.
     */
    private void createParametersGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group paramGroup = new Group(shell, SWT.NONE);
        paramGroup.setLayout(new GridLayout(1, false));
        paramGroup.setLayoutData(gd);
        paramGroup.setText(" Parameters for Selected Radar ");

        // -----------------------------------------------
        // Create text controls
        // -----------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite textComp = new Composite(paramGroup, SWT.NONE);
        textComp.setLayout(new GridLayout(6, false));
        textComp.setLayoutData(gd);

        // Identifier
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label idLbl = new Label(textComp, SWT.RIGHT);
        idLbl.setText("Identifier:");
        idLbl.setLayoutData(gd);

        gd = new GridData(40, SWT.DEFAULT);
        idTF = new Text(textComp, SWT.BORDER);
        idTF.setLayoutData(gd);
        idTF.setTextLimit(3);

        // Active check box
        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.horizontalIndent = 10;
        activeChk = new Button(textComp, SWT.CHECK);
        activeChk.setText("Active");
        activeChk.setLayoutData(gd);

        // Latitude
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label latLbl = new Label(textComp, SWT.RIGHT);
        latLbl.setText("Latitude:");
        latLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        latTF = new Text(textComp, SWT.BORDER);
        latTF.setLayoutData(gd);

        // Number
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label numberLbl = new Label(textComp, SWT.RIGHT);
        numberLbl.setText("Number:");
        numberLbl.setLayoutData(gd);

        gd = new GridData(40, SWT.DEFAULT);
        numberTF = new Text(textComp, SWT.BORDER);
        numberTF.setLayoutData(gd);

        // Prefix
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalIndent = 30;
        Label prefixLbl = new Label(textComp, SWT.RIGHT);
        prefixLbl.setText("Prefix:");
        prefixLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        prefixTF = new Text(textComp, SWT.BORDER);
        prefixTF.setLayoutData(gd);
        prefixTF.setTextLimit(1);

        // Longitude
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label lonLbl = new Label(textComp, SWT.RIGHT);
        lonLbl.setText("Longitude:");
        lonLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        lonTF = new Text(textComp, SWT.BORDER);
        lonTF.setLayoutData(gd);

        // Name
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label nameLbl = new Label(textComp, SWT.RIGHT);
        nameLbl.setText("Name:");
        nameLbl.setLayoutData(gd);

        gd = new GridData(300, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        nameTF = new Text(textComp, SWT.BORDER);
        nameTF.setLayoutData(gd);
        nameTF.setTextLimit(20);

        // Elevation
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label elevationLbl = new Label(textComp, SWT.RIGHT);
        elevationLbl.setText("Elevation:");
        elevationLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        elevationTF = new Text(textComp, SWT.BORDER);
        elevationTF.setLayoutData(gd);

        // State
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label stateLbl = new Label(textComp, SWT.RIGHT);
        stateLbl.setText("State:");
        stateLbl.setLayoutData(gd);

        gd = new GridData(30, SWT.DEFAULT);
        stateTF = new Text(textComp, SWT.BORDER);
        stateTF.setLayoutData(gd);
        stateTF.setTextLimit(2);

        // Bias Source
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label biasLbl = new Label(textComp, SWT.RIGHT);
        biasLbl.setText("Bias Source:");
        biasLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        biasTF = new Text(textComp, SWT.BORDER);
        biasTF.setLayoutData(gd);
        biasTF.setTextLimit(5);

        // Tower Height
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label towerHeightLbl = new Label(textComp, SWT.RIGHT);
        towerHeightLbl.setText("Tower Height:");
        towerHeightLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        towerHeightTF = new Text(textComp, SWT.BORDER);
        towerHeightTF.setLayoutData(gd);

        // -----------------------------------------------
        // Create action buttons
        // -----------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        Composite buttonComp = new Composite(paramGroup, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        addBtn = new Button(buttonComp, SWT.PUSH);
        addBtn.setText("Add");
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        updateBtn = new Button(buttonComp, SWT.PUSH);
        updateBtn.setText("Update");
        updateBtn.setLayoutData(gd);
        updateBtn.setEnabled(false);
        updateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 70;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.setEnabled(false);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });
    }

    /**
     * Create the close button at the bottom of the dialog.
     */
    private void createCloseButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Get the text for the data list control label.
     * 
     * @return Label text.
     */
    private String getListLabelText() {
        String format = "%s   %s  %s  %-24s %s    %s    %s    %s   %s  %s   %s";

        String str = String.format(format, "ID", "Num", "Prefix", "Name",
                "State", "Latitude", "Longitude", "Elevation", "Height",
                "Active", "Bias Source");

        return str;
    }

    /**
     * Gets the radar locations from the DB
     */
    private void getDialogData() {
        try {
            radarData = HydroDBDataManager.getInstance().getData(
                    RadarLocData.class);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to get radar location data. ", e);
        }
        updateDisplay();
    }

    /**
     * Populates the dialog's list with the radar locations from the db
     */
    private void updateDisplay() {
        dataList.removeAll();
        clearInformation();

        if (radarData.size() > 0) {
            String format = "%3.3S  %3.3s    %2.2S    %-25.25S %2.2S    %10.10S   %10.10S     %7.7s   %6.6s     %s        %s";

            for (RadarLocData currLoc : radarData) {
                dataList.add(String.format(format, currLoc.getRadarID(),
                        HydroDataUtils.getDisplayString(currLoc
                                .getRadarNumber()), currLoc.getRadarPrefix(),
                        currLoc.getName(), currLoc.getState(), HydroDataUtils
                                .getLatLonDisplayString(currLoc.getLatitude()),
                        HydroDataUtils.getLatLonDisplayString(currLoc
                                .getLongitude()), HydroDataUtils
                                .getDisplayString("%s", "%6.1f",
                                        currLoc.getElevation()), HydroDataUtils
                                .getDisplayString("%s", "%5.1f",
                                        currLoc.getTowerHeight()), currLoc
                                .getUseRadar(), currLoc.getOfficeID()));
            }

            updateDialogState(DialogStates.DATA_AVAILABLE);
        } else {
            updateDialogState(DialogStates.NO_DATA);
        }
    }

    /**
     * Populates the information section with the selected radar location
     */
    private void updateInformation() {
        RadarLocData currData = getSelectedRecord();

        if (currData != null) {
            idTF.setText(currData.getRadarID());
            activeChk.setSelection(currData.getUseRadar().equals("T"));
            numberTF.setText(HydroDataUtils.getDisplayString(currData
                    .getRadarNumber()));
            prefixTF.setText(currData.getRadarPrefix().toUpperCase());
            nameTF.setText(currData.getName());
            stateTF.setText(currData.getState());
            latTF.setText(HydroDataUtils.getLatLonDisplayString(currData
                    .getLatitude()));
            lonTF.setText(HydroDataUtils.getLatLonDisplayString(currData
                    .getLongitude()));
            elevationTF.setText(HydroDataUtils.getDisplayString(currData
                    .getElevation()));
            towerHeightTF.setText(HydroDataUtils.getDisplayString(currData
                    .getTowerHeight()));
            biasTF.setText(currData.getOfficeID());
        }
    }

    /**
     * Clears the information section of the dialog
     */
    private void clearInformation() {
        idTF.setText("");
        activeChk.setSelection(false);
        numberTF.setText("");
        prefixTF.setText("");
        nameTF.setText("");
        stateTF.setText("");
        latTF.setText("");
        lonTF.setText("");
        elevationTF.setText("");
        towerHeightTF.setText("");
        biasTF.setText("");
    }

    /**
     * Save the radar location to the db
     */
    private void saveRecord() {
        if (idTF.getText().equals("")) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Unable to Save");
            mb.setMessage("The Identifier is required.");
            mb.open();
        } else {
            RadarLocData newData = new RadarLocData();

            newData.setRadarID(idTF.getText());
            newData.setName(nameTF.getText());
            newData.setRadarPrefix(prefixTF.getText());

            Integer itmp = HydroDataUtils.getIntegerFromTF(shell, numberTF,
                    "Number");
            if (itmp == null) {
                return;
            }
            newData.setRadarNumber(itmp);

            newData.setState(stateTF.getText());

            // Get Latitude
            Double tmp = HydroDataUtils.getLatitudeFromTF(shell, latTF);
            if (tmp == null) {
                return;
            }
            newData.setLatitude(tmp);

            // Get Latitude
            tmp = HydroDataUtils.getLongitudeFromTF(shell, lonTF);
            if (tmp == null) {
                return;
            }
            newData.setLongitude(tmp);

            // Elevation
            tmp = HydroDataUtils.getDoubleFromTF(shell, elevationTF,
                    "Elevation");
            if (tmp == null) {
                return;
            }
            newData.setElevation(tmp);

            // TowerHT
            tmp = HydroDataUtils.getDoubleFromTF(shell, towerHeightTF,
                    "Tower Height");
            if (tmp == null) {
                return;
            }
            newData.setTowerHeight(tmp);

            // User Radar
            newData.setUseRadar(activeChk.getSelection() ? "T" : "F");

            // Office
            newData.setOfficeID(biasTF.getText());

            try {
                HydroDBDataManager.getInstance().putData(newData);

                getDialogData();
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to save radar location data. ", e);
            }
        }
    }

    /**
     * Prompts the user and after confirmation, deletes the radar location from
     * the db
     */
    private void deleteRecord() {
        RadarLocData currData = getSelectedRecord();

        if (currData != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("WARNING:  You are about to delete radar information \n"
                    + "from multiple tables.  Are you sure you want to do this? \n\n"
                    + "(Note: If you delete this radar now, you will NOT be able to \n"
                    + "recover the non-default values for the affected tables.) \n\n"
                    + "If you do not wish to delete at this time, click \"Cancel\".");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    String deleteFuntion = String.format(
                            "delete_radar ( '%s' )", currData.getRadarID());
                    HydroDBDataManager.getInstance()
                            .execFunction(deleteFuntion);

                    // Refresh the cache
                    getDialogData();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to delete radar location data. ", e);
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a radar location first.");
            mb.open();
        }
    }

    /**
     * @return The selected radar location or NULL if one is not selected
     */
    private RadarLocData getSelectedRecord() {
        RadarLocData currData = null;

        if (dataList.getSelectionCount() > 0) {
            currData = radarData.get(dataList.getSelectionIndex());
        }

        return currData;
    }

    /**
     * Updates the dialog for the current state
     */
    private void updateDialogState(DialogStates currState) {
        switch (currState) {
        case NO_DATA:
            updateBtn.setEnabled(false);
            deleteBtn.setEnabled(false);
            break;
        case DATA_AVAILABLE:
            updateBtn.setEnabled(true);
            deleteBtn.setEnabled(true);
            break;
        default:
            break;
        }

    }
}
