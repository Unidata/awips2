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
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.CityData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Cities dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Sep 2008             lvenable    Initial creation.
 * 08 Jan 2008  1802       askripsk    Connected to DB.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class CitiesDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Cities list control.
     */
    private List citiesList;

    /**
     * City text control.
     */
    private Text cityTF;

    /**
     * State text control.
     */
    private Text stateTF;

    /**
     * Latitude text control.
     */
    private Text latTF;

    /**
     * Longitude text control.
     */
    private Text lonTF;

    /**
     * Display precedence combo box.
     */
    private Combo displayPrecCbo;

    /**
     * Population text control.
     */
    private Text populationTF;

    /**
     * Update button.
     */
    private Button updateBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Cache of Cities
     */
    private ArrayList<CityData> cityData;

    /**
     * States of the dialog
     */
    private enum DialogStates {
        NO_DATA, DATA_AVAILABLE
    }

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public CitiesDlg(Shell parent) {
        super(parent);
        setText("Cities");
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
        setReturnValue(false);

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createCitiesDataList();

        createInformationGroup();

        // Add a separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomButtons();

        getDialogData();
    }

    /**
     * Create the Cities data list and label.
     */
    private void createCitiesDataList() {
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));

        GridData gd = new GridData();
        gd.horizontalIndent = 4;
        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText(getCitiesListText());
        listLbl.setFont(controlFont);
        listLbl.setLayoutData(gd);

        gd = new GridData(575, 200);
        citiesList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        citiesList.setLayoutData(gd);
        citiesList.setFont(controlFont);
        citiesList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateInformation();
            }
        });
    }

    /**
     * Create the Information group and controls.
     */
    private void createInformationGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(4, false);
        gl.horizontalSpacing = 20;
        Group informationGroup = new Group(shell, SWT.NONE);
        informationGroup.setLayout(gl);
        informationGroup.setLayoutData(gd);
        informationGroup.setText(" Information ");

        Label cityLbl = new Label(informationGroup, SWT.NONE);
        cityLbl.setText("City:");

        Label stateLbl = new Label(informationGroup, SWT.NONE);
        stateLbl.setText("State:");

        Label latLbl = new Label(informationGroup, SWT.NONE);
        latLbl.setText("Lat:");

        Label lonLbl = new Label(informationGroup, SWT.NONE);
        lonLbl.setText("Lon:");

        gd = new GridData(150, SWT.DEFAULT);
        cityTF = new Text(informationGroup, SWT.BORDER);
        cityTF.setLayoutData(gd);
        cityTF.setTextLimit(20);

        gd = new GridData(30, SWT.DEFAULT);
        stateTF = new Text(informationGroup, SWT.BORDER);
        stateTF.setLayoutData(gd);
        stateTF.setTextLimit(2);

        gd = new GridData(100, SWT.DEFAULT);
        latTF = new Text(informationGroup, SWT.BORDER);
        latTF.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        lonTF = new Text(informationGroup, SWT.BORDER);
        lonTF.setLayoutData(gd);

        Label displayLbl = new Label(informationGroup, SWT.NONE);
        displayLbl.setText("Display Precedence:");

        gd = new GridData();
        gd.horizontalSpan = 3;
        Label populationLbl = new Label(informationGroup, SWT.NONE);
        populationLbl.setText("Population:");
        populationLbl.setLayoutData(gd);

        displayPrecCbo = new Combo(informationGroup, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        displayPrecCbo.add("Cities (1)");
        displayPrecCbo.add("Towns (2)");
        displayPrecCbo.select(0);

        gd = new GridData(100, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        populationTF = new Text(informationGroup, SWT.BORDER);
        populationTF.setLayoutData(gd);
    }

    /**
     * Create the bottom buttons on the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(5, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 100;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (saveRecord()) {
                    shell.dispose();
                }
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button addBtn = new Button(buttonComp, SWT.PUSH);
        addBtn.setText("Add");
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        updateBtn = new Button(buttonComp, SWT.PUSH);
        updateBtn.setText("Update");
        updateBtn.setEnabled(false);
        updateBtn.setLayoutData(gd);
        updateBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setEnabled(false);
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });
    }

    /**
     * Get the text for the data list label.
     * 
     * @return Label text.
     */
    private String getCitiesListText() {
        String format = "%S                %S    %S        %S         %S    %S";

        String str = String.format(format, "City", "State", "Lat", "Lon",
                "Procedure", "Pop");

        return str;
    }

    private void getDialogData() {
        try {
            cityData = HydroDBDataManager.getInstance().getData(CityData.class);
        } catch (VizException e) {
            e.printStackTrace();
        }

        updateDisplay();
    }

    private void clearInformation() {
        cityTF.setText("");
        stateTF.setText("");
        latTF.setText("");
        lonTF.setText("");
        populationTF.setText("");

        // Display Precedence
        displayPrecCbo.select(0);
    }

    private void updateDisplay() {
        String format = "%-21.21S %2S   %10.10S   %10.10S     %1S    %9s";
        citiesList.removeAll();
        clearInformation();

        if (cityData.size() > 0) {
            for (CityData currCity : cityData) {
                citiesList.add(String.format(format, currCity.getName(),
                        currCity.getState(),
                        HydroDataUtils.getLatLonDisplayString(currCity
                                .getLatitude()),
                        HydroDataUtils.getLatLonDisplayString(currCity
                                .getLongitude()), currCity
                                .getDisplayPrecedence(), HydroDataUtils
                                .getDisplayString(currCity.getPopulation())));
            }

            updateDialogState(DialogStates.DATA_AVAILABLE);
        } else {
            updateDialogState(DialogStates.NO_DATA);
        }
    }

    private void updateInformation() {
        CityData currData = getSelectedRecord();

        if (currData != null) {
            cityTF.setText(currData.getName());
            stateTF.setText(currData.getState());
            latTF.setText(HydroDataUtils.getLatLonDisplayString(currData
                    .getLatitude()));
            lonTF.setText(HydroDataUtils.getLatLonDisplayString(currData
                    .getLongitude()));
            populationTF.setText(HydroDataUtils.getDisplayString(currData
                    .getPopulation()));

            // Display Precedence
            displayPrecCbo.select(currData.getDisplayPrecedence() - 1);
        }
    }

    private boolean saveRecord() {
        boolean successful = false;

        if (cityTF.getText().equals("")) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Unable to Save");
            mb.setMessage("The City name cannot be empty.");
            mb.open();
        } else {
            CityData newData = new CityData();

            newData.setName(cityTF.getText());
            newData.setState(stateTF.getText());

            // Get Latitude
            Double tmp = HydroDataUtils.getLatitudeFromTF(shell, latTF);
            if (tmp == null) {
                return successful;
            }
            newData.setLatitude(tmp);

            // Get Latitude
            tmp = HydroDataUtils.getLongitudeFromTF(shell, lonTF);
            if (tmp == null) {
                return successful;
            }
            newData.setLongitude(tmp);

            // Display Precedence
            newData
                    .setDisplayPrecedence(displayPrecCbo.getSelectionIndex() + 1);

            // Population
            Integer itmp = HydroDataUtils.getIntegerFromTF(shell, populationTF,
                    "Population");
            if (itmp == null) {
                return successful;
            }
            newData.setPopulation(itmp);

            try {
                HydroDBDataManager.getInstance().putData(newData);

                getDialogData();

                successful = true;
            } catch (VizException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb
                        .setMessage("An error occurred while trying to save the City");
                mb.open();

                e.printStackTrace();
            }
        }

        return successful;
    }

    private void deleteRecord() {
        CityData currData = getSelectedRecord();

        if (currData != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete the selected city?");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    HydroDBDataManager.getInstance().deleteRecord(currData);

                    // Refresh the cache
                    getDialogData();
                } catch (VizException e) {
                    mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Unable to Delete");
                    mb
                            .setMessage("An error occurred while trying to delete the City");
                    mb.open();

                    e.printStackTrace();
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select a city first.");
            mb.open();
        }
    }

    private CityData getSelectedRecord() {
        CityData rval = null;

        if (citiesList.getSelectionCount() > 0) {
            rval = cityData.get(citiesList.getSelectionIndex());
        }

        return rval;
    }

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
