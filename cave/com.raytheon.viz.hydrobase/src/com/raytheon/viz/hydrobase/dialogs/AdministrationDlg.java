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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.AdministrationData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Administration dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Sep 2008             lvenable    Initial creation.
 * 14 Nov 2008  1697       askripsky   Connect to DB
 * 16 Apr 2013  1790       rferrel     Made dialog non-blocking.
 * 15 Jan 2015  5054       randerso    Remove unnecessary new Shell
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class AdministrationDlg extends CaveSWTDialog {

    /**
     * Name text control.
     */
    private Text nameTF;

    /**
     * Station text control.
     */
    private Text stationTF;

    /**
     * Station ID text control.
     */
    private Text stationIdTF;

    /**
     * HSA number text control.
     */
    private Text hsaNoTF;

    /**
     * Region text control.
     */
    private Text regionTF;

    /**
     * Number text control.
     */
    private Text noTF;

    /**
     * Phone text control.
     */
    private Text phoneTF;

    /**
     * Ten Year text control.
     */
    private Text tenYearTF;

    /**
     * One Year text control.
     */
    private Text oneYearTF;

    /**
     * CD-404 text control.
     */
    private Text cd404TF;

    /**
     * Password text control.
     */
    private Text passwordTF;

    /**
     * Data representing currently displayed data
     */
    private AdministrationData adminData;

    /**
     * Formats One and Ten year dates
     */
    private SimpleDateFormat adminFormat;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public AdministrationDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Administration");

        adminFormat = new SimpleDateFormat("MM/dd/yyyy");
        adminFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
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

        // Load the data
        getAdministrationData();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        createAdminControls();

        // Add a separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createBottomButtons();
    }

    /**
     * Create the administration controls.
     */
    private void createAdminControls() {
        GridLayout gl = new GridLayout(4, false);
        gl.verticalSpacing = 10;
        Group controlGroup = new Group(shell, SWT.NONE);
        controlGroup.setLayout(gl);
        controlGroup.setText(" Information ");

        // Name
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label nameLbl = new Label(controlGroup, SWT.RIGHT);
        nameLbl.setText("Name:");
        nameLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        nameTF = new Text(controlGroup, SWT.BORDER);
        nameTF.setLayoutData(gd);

        // Phone
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.widthHint = 200;
        Label phoneLbl = new Label(controlGroup, SWT.RIGHT);
        phoneLbl.setText("Phone:");
        phoneLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        phoneTF = new Text(controlGroup, SWT.BORDER);
        phoneTF.setLayoutData(gd);

        // Station
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label stationLbl = new Label(controlGroup, SWT.RIGHT);
        stationLbl.setText("Station:");
        stationLbl.setLayoutData(gd);

        gd = new GridData(180, SWT.DEFAULT);
        stationTF = new Text(controlGroup, SWT.BORDER);
        stationTF.setLayoutData(gd);

        // Ten Year
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label tenYearLbl = new Label(controlGroup, SWT.RIGHT);
        tenYearLbl.setText("Ten Year:");
        tenYearLbl.setLayoutData(gd);

        gd = new GridData(180, SWT.DEFAULT);
        tenYearTF = new Text(controlGroup, SWT.BORDER);
        tenYearTF.setLayoutData(gd);

        // Station ID
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label stationIdLbl = new Label(controlGroup, SWT.RIGHT);
        stationIdLbl.setText("Station Id:");
        stationIdLbl.setLayoutData(gd);

        gd = new GridData(40, SWT.DEFAULT);
        stationIdTF = new Text(controlGroup, SWT.BORDER);
        stationIdTF.setLayoutData(gd);

        // One Year
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label oneYearLbl = new Label(controlGroup, SWT.RIGHT);
        oneYearLbl.setText("One Year:");
        oneYearLbl.setLayoutData(gd);

        gd = new GridData(180, SWT.DEFAULT);
        oneYearTF = new Text(controlGroup, SWT.BORDER);
        oneYearTF.setLayoutData(gd);

        // HSA No.
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label hsaNoLbl = new Label(controlGroup, SWT.RIGHT);
        hsaNoLbl.setText("HSA No.:");
        hsaNoLbl.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        hsaNoTF = new Text(controlGroup, SWT.BORDER);
        hsaNoTF.setLayoutData(gd);

        // CD-404
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label cd404Lbl = new Label(controlGroup, SWT.RIGHT);
        cd404Lbl.setText("CD-404:");
        cd404Lbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        cd404TF = new Text(controlGroup, SWT.BORDER);
        cd404TF.setLayoutData(gd);

        // Region
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label regionLbl = new Label(controlGroup, SWT.RIGHT);
        regionLbl.setText("Region:");
        regionLbl.setLayoutData(gd);

        gd = new GridData(180, SWT.DEFAULT);
        regionTF = new Text(controlGroup, SWT.BORDER);
        regionTF.setLayoutData(gd);

        // CD-404
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label passwordLbl = new Label(controlGroup, SWT.RIGHT);
        passwordLbl.setText("Password:");
        passwordLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        passwordTF = new Text(controlGroup, SWT.BORDER);
        passwordTF.setLayoutData(gd);

        // No.
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label noLbl = new Label(controlGroup, SWT.RIGHT);
        noLbl.setText("No.:");
        noLbl.setLayoutData(gd);

        gd = new GridData(30, SWT.DEFAULT);
        noTF = new Text(controlGroup, SWT.BORDER);
        noTF.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 90;

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
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

    private void getAdministrationData() {
        try {
            java.util.List<AdministrationData> data = HydroDBDataManager
                    .getInstance().getData(AdministrationData.class);

            // if no data is returned, clear the current display data
            adminData = (data.size() > 0) ? data.get(0) : null;
        } catch (VizException e) {
            e.printStackTrace();
        }

        updateAdminDisplay();
    }

    /**
     * Fills the dialog with the current data from the DB.
     */
    private void updateAdminDisplay() {
        if (adminData != null) {
            nameTF.setText(adminData.getName());
            stationTF.setText(adminData.getStationName());
            stationIdTF.setText(adminData.getHsa());
            hsaNoTF.setText((adminData.getHsaNumber() != HydroConstants.MISSING_VALUE) ? Integer
                    .toString(adminData.getHsaNumber()) : "");
            regionTF.setText(adminData.getRegion());
            noTF.setText(adminData.getRegionNumber());
            phoneTF.setText(adminData.getPhoneNumber());
            tenYearTF
                    .setText((adminData.getTenYearDate() != null) ? adminFormat
                            .format(adminData.getTenYearDate()) : "");
            oneYearTF
                    .setText((adminData.getOneYearDate() != null) ? adminFormat
                            .format(adminData.getOneYearDate()) : "");
            cd404TF.setText(adminData.getCd404());
            passwordTF.setText(adminData.getHbPassword());
        } else {
            clearDisplay();
        }
    }

    /**
     * Clears the dialog.
     */
    private void clearDisplay() {
        nameTF.setText("");
        stationTF.setText("");
        stationIdTF.setText("");
        hsaNoTF.setText("");
        regionTF.setText("");
        noTF.setText("");
        phoneTF.setText("");
        tenYearTF.setText("");
        oneYearTF.setText("");
        cd404TF.setText("");
        passwordTF.setText("");
    }

    private AdministrationData getCurrentAdministrationData() {
        int hsaNumber;

        AdministrationData currData = null;

        // Make sure the LID, aka Station ID, is set since it is the primary key
        // for the DB
        if (stationIdTF.getText().compareTo("") == 0) {
            MessageBox messageBox = new MessageBox(getShell(), SWT.ICON_WARNING
                    | SWT.OK);
            messageBox.setText("Station ID Missing");
            messageBox.setMessage("Please enter a Station ID");
            messageBox.open();

            return currData;
        }

        try {
            hsaNumber = (hsaNoTF.getText().compareTo("") != 0) ? Integer
                    .parseInt(hsaNoTF.getText()) : HydroConstants.MISSING_VALUE;

            if (noTF.getText().length() > 1) {
                MessageBox messageBox = new MessageBox(getShell(),
                        SWT.ICON_WARNING | SWT.OK);
                messageBox.setText("Invalid Region Number");
                messageBox
                        .setMessage("Region Number cannot be longer than 1 character");
                messageBox.open();

                return currData;
            }

            currData = new AdministrationData();
            currData.setCd404(cd404TF.getText());
            currData.setHbPassword(passwordTF.getText());
            currData.setHsa(stationIdTF.getText());

            currData.setHsaNumber(hsaNumber);

            currData.setName(nameTF.getText());
            currData.setPhoneNumber(phoneTF.getText());
            currData.setRegion(regionTF.getText());
            currData.setRegionNumber(noTF.getText());
            currData.setStationName(stationTF.getText());
            try {
                currData.setOneYearDate((oneYearTF.getText().compareTo("") == 0) ? (Date) null
                        : adminFormat.parse(oneYearTF.getText()));
            } catch (ParseException e) {
                MessageBox messageBox = new MessageBox(getShell(),
                        SWT.ICON_WARNING | SWT.OK);
                messageBox.setText("Invalid One Year Date");
                messageBox
                        .setMessage("Please enter the One Year Date in the form: MM/DD/YYYY");
                messageBox.open();

                return currData;
            }
            try {
                currData.setTenYearDate((tenYearTF.getText().compareTo("") == 0) ? (Date) null
                        : adminFormat.parse(tenYearTF.getText()));
            } catch (ParseException e) {
                MessageBox messageBox = new MessageBox(getShell(),
                        SWT.ICON_WARNING | SWT.OK);
                messageBox.setText("Invalid Ten Year Date");
                messageBox
                        .setMessage("Please enter the Ten Year Date in the form: MM/DD/YYYY");
                messageBox.open();

                return currData;
            }
        } catch (NumberFormatException e) {
            MessageBox messageBox = new MessageBox(getShell(), SWT.ICON_WARNING
                    | SWT.OK);
            messageBox.setText("Invalid HSA Number");
            messageBox
                    .setMessage("Please enter a numeric value for HSA Number");
            messageBox.open();
        }

        return currData;
    }

    private void saveRecord() {
        try {
            AdministrationData dataToUpdate = getCurrentAdministrationData();

            if (dataToUpdate != null) {
                HydroDBDataManager.getInstance().putData(dataToUpdate);
            }
        } catch (VizException e) {
            e.printStackTrace();
        }

        getAdministrationData();
    }
}
