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
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.AgencyOfficeData;
import com.raytheon.viz.hydrocommon.data.LocationAgencyOfficeData;
import com.raytheon.viz.hydrocommon.datamanager.AddModifyLocationDataManager;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Cooperation/Agency Office dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Sep 2008             lvenable    Initial creation.
 * 05 Dec 2008  1744       askripsky   Connected data
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class CoopAgencyOfficeDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Available Agency/Office list control.
     */
    private List availList;

    /**
     * Selected Agency/Office list control.
     */
    private List selectedList;

    /**
     * Agency text control.
     */
    private Text agencyTF;

    /**
     * Office text control.
     */
    private Text officeTF;

    /**
     * Add button.
     */
    private Button addBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Selected Location
     */
    private String lid;

    /**
     * Cache of available agencies/offices
     */
    private ArrayList<AgencyOfficeData> availData;

    /**
     * Cache of selected agencies/offices
     */
    private ArrayList<LocationAgencyOfficeData> selectedData;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public CoopAgencyOfficeDlg(Shell parent, String titleInfo, String lid) {
        super(parent);
        setText("Cooperating Agencies/Offices" + titleInfo);

        this.lid = lid;
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

        createLabelAndListControls();

        createTextControls();

        // Add a separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomButtons();

        populateListControl();
    }

    /**
     * Create the labels and list controls.
     */
    private void createLabelAndListControls() {
        Composite mainListComp = new Composite(shell, SWT.NONE);
        mainListComp.setLayout(new GridLayout(3, false));

        GridData gd = new GridData();
        gd.horizontalSpan = 2;
        Label availableLbl = new Label(mainListComp, SWT.NONE);
        availableLbl.setText("Available");
        availableLbl.setLayoutData(gd);

        gd = new GridData();
        Label selectedLbl = new Label(mainListComp, SWT.NONE);
        selectedLbl.setText("Selected");
        selectedLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalIndent = 4;
        gd.horizontalSpan = 2;
        Label agencyOfficeLbl1 = new Label(mainListComp, SWT.NONE);
        agencyOfficeLbl1.setText(getListLabelText());
        agencyOfficeLbl1.setLayoutData(gd);
        agencyOfficeLbl1.setFont(controlFont);

        gd = new GridData();
        gd.horizontalIndent = 4;
        Label agencyOfficeLbl2 = new Label(mainListComp, SWT.NONE);
        agencyOfficeLbl2.setText(getListLabelText());
        agencyOfficeLbl2.setLayoutData(gd);
        agencyOfficeLbl2.setFont(controlFont);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 300;
        gd.heightHint = 200;
        availList = new List(mainListComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        availList.setLayoutData(gd);
        availList.setFont(controlFont);
        availList.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                updateDisplay();
            }
        });

        // -----------------------------------
        // Create Add & Remove buttons
        // -----------------------------------
        Composite buttonComp = new Composite(mainListComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER, false,
                true));

        gd = new GridData(80, SWT.DEFAULT);
        addBtn = new Button(buttonComp, SWT.PUSH);
        addBtn.setText("Add -->");
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveData();
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete >");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteData();
            }
        });

        // Selected List control.
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 300;
        gd.heightHint = 200;
        selectedList = new List(mainListComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        selectedList.setLayoutData(gd);
        selectedList.setFont(controlFont);
    }

    /**
     * Create the text controls.
     */
    private void createTextControls() {
        Composite textComp = new Composite(shell, SWT.NONE);
        textComp.setLayout(new GridLayout(2, false));

        GridData gd = new GridData(200, SWT.DEFAULT);
        Label agencyLbl = new Label(textComp, SWT.NONE);
        agencyLbl.setText("Agency");
        agencyLbl.setLayoutData(gd);

        Label officeLbl = new Label(textComp, SWT.NONE);
        officeLbl.setText("Office");

        gd = new GridData(125, SWT.DEFAULT);
        agencyTF = new Text(textComp, SWT.BORDER);
        agencyTF.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        officeTF = new Text(textComp, SWT.BORDER);
        officeTF.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 90;

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
    }

    /**
     * Get the label text for the list controls.
     * 
     * @return Label text.
     */
    private String getListLabelText() {
        String format = "%S      %S";

        String labelStr = String.format(format, "Agency", "Office");

        return labelStr;
    }

    /**
     * loads the agencies and offices from the DB
     */
    private void populateListControl() {
        try {
            // Retrieve available agencies and offices
            availData = AddModifyLocationDataManager.getInstance()
                    .getAvailableAgenciesAndOfficesData();

            availList.removeAll();

            for (AgencyOfficeData currRow : availData) {
                availList.add(String.format("%-11S %S",
                        currRow.getAgencyCode(), currRow.getOffice()));
            }

            // Retrieve selected agencies and offices
            selectedData = AddModifyLocationDataManager.getInstance()
                    .getSelectedAgenciesAndOfficesData(lid);

            selectedList.removeAll();

            for (LocationAgencyOfficeData currAO : selectedData) {
                selectedList.add(String.format("%-11S %S", currAO
                        .getAgencyCode(), currAO.getOffice()));
            }

            availList.setSelection(0);

        } catch (VizException e) {
            e.printStackTrace();
        }

        updateDisplay();
    }

    /**
     * Save the current agency/office to the DB
     */
    private void saveData() {
        // Check to see if the values are valid
        if (agencyTF.getText().length() > 8) {
            MessageBox messageBox = new MessageBox(new Shell(),
                    SWT.ICON_WARNING | SWT.OK);
            messageBox.setText("Agency too long");
            messageBox.setMessage("Agency cannot be longer than 8 characters");
            messageBox.open();

            return;
        } else if (officeTF.getText().length() > 20) {
            MessageBox messageBox = new MessageBox(new Shell(),
                    SWT.ICON_WARNING | SWT.OK);
            messageBox.setText("Office too long");
            messageBox.setMessage("Office cannot be longer than 20 characters");
            messageBox.open();

            return;
        }

        // Build the object to save
        LocationAgencyOfficeData newData = new LocationAgencyOfficeData();
        newData.setLid(lid);
        newData.setAgencyCode(agencyTF.getText());
        newData.setOffice(officeTF.getText());

        // Send the object to the datamanager to save to the DB, since it
        // automatically handles update vs. insert
        try {
            HydroDBDataManager.getInstance().putData(newData);
        } catch (VizException e) {
            e.printStackTrace();
        }

        // Refresh the data
        populateListControl();
    }

    private void deleteData() {
        // Get the selected agency/office and
        // Send the object to the datamanager to delete from the DB
        if (selectedList.getSelectionCount() > 0) {
            try {
                HydroDBDataManager.getInstance().deleteRecord(
                        selectedData.get(selectedList.getSelectionIndex()));
            } catch (VizException e) {
                e.printStackTrace();
            }

            // Refresh the data
            populateListControl();
        } else {
            MessageBox messageBox = new MessageBox(new Shell(),
                    SWT.ICON_WARNING | SWT.OK);
            messageBox.setText("No Agency/Office Selected");
            messageBox
                    .setMessage("Please select an Agency/Office from the Selected list to delete");
            messageBox.open();
        }
    }

    private void updateDisplay() {
        if (availList.getItemCount() > 0) {
            AgencyOfficeData currOffice = availData.get(availList
                    .getSelectionIndex());

            agencyTF.setText(currOffice.getAgencyCode());
            officeTF.setText(currOffice.getOffice());
        }
    }
}
