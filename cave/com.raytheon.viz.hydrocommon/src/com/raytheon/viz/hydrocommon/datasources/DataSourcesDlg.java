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
package com.raytheon.viz.hydrocommon.datasources;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.DcpData;
import com.raytheon.viz.hydrocommon.data.HydroDBData;
import com.raytheon.viz.hydrocommon.data.ObserverData;
import com.raytheon.viz.hydrocommon.data.TelemData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.hydrocommon.util.StnClassSyncUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * This class displays the Data Sources dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008				lvenable	Initial creation.
 * 10/6/2008    1555        grichard    Support data sources.
 * 12/16/2008   1782        grichard    Refreshed Data Sources.
 * 1/11/2008    1802        askripsk    Comlete HydroBase implementation.
 * 07/15/2013   2088        rferrel     Make dialog non-blocking
 * 04/22/2016   5483        dgilling    Correct fixed pixel layouts, refactor 
 *                                      based on CaveJFACEDialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class DataSourcesDlg extends CaveJFACEDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    /**
     * Button ID for Apply button.
     */
    private static final int APPLY_ID = IDialogConstants.CLIENT_ID + 1;

    /**
     * Button ID for Delete button.
     */
    private static final int DELETE_ID = IDialogConstants.CLIENT_ID + 2;

    /**
     * Label for Apply button.
     */
    private static final String APPLY_LABEL = "Apply";

    /**
     * Label for Delete button.
     */
    private static final String DELETE_LABEL = "Delete";

    /**
     * Formatted ISO UTC Date.
     */
    private static final DateFormat ISO_DATE = new SimpleDateFormat(
            "MM/dd/yyyy") {
        {
            setTimeZone(TimeUtil.GMT_TIME_ZONE);
        }
    };

    /**
     * Current Location Identifier.
     */
    private final String lid;

    /**
     * Shell title bar text.
     */
    private final String title;

    /**
     * Flag indicating if the full controls should be displayed.
     */
    private final boolean fullControls;

    /**
     * Type combo box.
     */
    private Combo typeCbo;

    /**
     * Stack composite.
     */
    private Composite stackComposite;

    /**
     * Stack layout.
     */
    private StackLayout stackLayout;

    /**
     * DCP composite.
     */
    private Composite dcpComp;

    /**
     * Observer composite.
     */
    private Composite observerComp;

    /**
     * Telemetry composite.
     */
    private Composite telemetryComp;

    /**
     * Owner list control.
     */
    private List ownerList;

    /**
     * Goes ID text control.
     */
    private Text goesIdTF;

    /**
     * Reporting time text control.
     */
    private Text reportingTimeTF;

    /**
     * Reporting frequency text control.
     */
    private Text reportingFreqTF;

    /**
     * Observation frequency text control.
     */
    private Text observationFreqTF;

    /**
     * DCP criteria text control.
     */
    private Text dcpCriteriaTF;

    /**
     * First name text control.
     */
    private Text firstNameTF;

    /**
     * Last name text control.
     */
    private Text lastNameTF;

    /**
     * Address 1 text control.
     */
    private Text address1TF;

    /**
     * Address 2 text control.
     */
    private Text address2TF;

    /**
     * Address 3 text control.
     */
    private Text address3TF;

    /**
     * City text control.
     */
    private Text cityTF;

    /**
     * DoS text control.
     */
    private Text dosTF;

    /**
     * Home phone text control.
     */
    private Text homePhoneTF;

    /**
     * Work phone text control.
     */
    private Text workPhoneTF;

    /**
     * Social security number text control.
     */
    private Text ssnTF;

    /**
     * ZIP code text control.
     */
    private Text zipCodeTF;

    /**
     * Email text control.
     */
    private Text emailTF;

    /**
     * DoS check box.
     */
    private Button dosChk;

    /**
     * Random Report check box.
     */
    private Button randomReportChk;

    /**
     * Male radio button.
     */
    private Button maleRdo;

    /**
     * Female radio button.
     */
    private Button femaleRdo;

    /**
     * Ignore radio button.
     */
    private Button ignoreRdo;

    /**
     * State combo box.
     */
    private Combo stateCbo;

    /**
     * Comms combo box.
     */
    private Combo commsCbo;

    /**
     * Task number text control.
     */
    private Text taskNoTF;

    /**
     * Rate text control.
     */
    private Text rateTF;

    /**
     * CD-404 text control.
     */
    private Text cd404TF;

    /**
     * Report text control.
     */
    private Text reportTF;

    /**
     * Sponsor list control.
     */
    private List sponsorList;

    /**
     * Recipient list control.
     */
    private List recipList;

    /**
     * Telemetry list control.
     */
    private List telemetryList;

    /**
     * Telemetry owner list control.
     */
    private List telemOwnerList;

    /**
     * Payor list control.
     */
    private List payorList;

    /**
     * Telemetry phone text control.
     */
    private Text telemetryPhoneTF;

    /**
     * Telemetry cost text control.
     */
    private Text telemetryCostTF;

    /**
     * Telemetry sensor ID text control.
     */
    private Text telemetrySensorIdTF;

    /**
     * Telemetry report frequency text control.
     */
    private Text telemetryReportFreqTF;

    /**
     * Telemetry observation frequency text control.
     */
    private Text telemetryObsFreqTF;

    /**
     * Telemetry criteria text control.
     */
    private Text telemetryCriteriaTF;

    /**
     * Cache of DCP data
     */
    private DcpData dcpData;

    /**
     * Cache of Obs data
     */
    private ObserverData obsData;

    /**
     * Cache of Telem data
     */
    private TelemData telemData;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     * @param fullControls
     *            Flag indicating if the full controls should be displayed.
     */
    public DataSourcesDlg(Shell parent, String titleInfo, String lid,
            boolean fullControls) {
        super(parent);
        setShellStyle(SWT.DIALOG_TRIM);
        setBlockOnOpen(false);

        this.lid = lid;
        this.title = titleInfo;
        this.fullControls = fullControls;
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText(String.format("Data Sources %s", title));
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);

        /*
         * Initialize all of the controls and layouts
         * 
         * Populate the data sources for the dialog.
         */
        createTypeComboBox(composite);
        createStackComposite(composite);

        loadStaticData();
        getDialogData();

        return composite;
    }

    /**
     * Create the Type label and combo box.
     * 
     * @param parent
     */
    private void createTypeComboBox(Composite parent) {
        Composite typeComp = new Composite(parent, SWT.NONE);
        typeComp.setLayout(new GridLayout(2, false));

        Label typeLbl = new Label(typeComp, SWT.RIGHT);
        typeLbl.setText("Type");

        typeCbo = new Combo(typeComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        typeCbo.setItems(new String[] { "DCP", "Observer", "Telemetry" });
        typeCbo.select(0);
        typeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateDisplay();
            }
        });

        Label sepLbl = new Label(parent, SWT.SEPARATOR | SWT.HORIZONTAL);
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        gd.horizontalSpan = 2;
        sepLbl.setLayoutData(gd);
    }

    /**
     * Create stack composite that will contain the DCP, Observer, and telemetry
     * composites.
     * 
     * @param parent
     */
    private void createStackComposite(Composite parent) {
        stackComposite = new Composite(parent, SWT.NONE);
        stackLayout = new StackLayout();
        stackComposite.setLayout(stackLayout);

        // create the composites in the stack here
        dcpComp = createDcpComposite(stackComposite);
        observerComp = createObserverComposite(stackComposite);
        telemetryComp = createTelemetryComposite(stackComposite);

        stackLayout.topControl = dcpComp;
        stackComposite.layout();
    }

    /**
     * Create the DCP composite and controls.
     * 
     * @param parent
     * @return
     */
    private Composite createDcpComposite(Composite parent) {
        Composite dcpComp = new Composite(parent, SWT.NONE);
        dcpComp.setLayout(new GridLayout(1, false));
        dcpComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        // -----------------------------------------
        // Create the General Group
        // -----------------------------------------
        Group generalGroup = new Group(dcpComp, SWT.NONE);
        generalGroup.setText("General");
        generalGroup.setLayout(new GridLayout(2, false));
        generalGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        // Create the left composite
        Composite leftComp = new Composite(generalGroup, SWT.NONE);
        leftComp.setLayout(new GridLayout(2, false));

        Label ownerLbl = new Label(leftComp, SWT.RIGHT);
        ownerLbl.setText("Owner:");
        ownerLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));

        ownerList = new List(leftComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        GridData gd = new GridData(SWT.LEFT, SWT.TOP, false, true);
        gd.heightHint = ownerList.getItemHeight() * 5;
        GC gc = new GC(ownerList);
        gd.widthHint = ownerList.computeTrim(0, 0, gc.getFontMetrics()
                .getAverageCharWidth() * 15, gc.getFontMetrics().getHeight()).width;
        gc.dispose();
        ownerList.setLayoutData(gd);

        // Create the right composite
        Composite rightComp = new Composite(generalGroup, SWT.NONE);
        rightComp.setLayout(new GridLayout(2, false));

        Label goesIdLbl = new Label(rightComp, SWT.RIGHT);
        goesIdLbl.setText("GOES ID:");
        goesIdLbl
                .setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        goesIdTF = new Text(rightComp, SWT.BORDER);
        goesIdTF.setTextLimit(8);
        gc = new GC(goesIdTF);
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        int charHeight = gc.getFontMetrics().getHeight();
        gc.dispose();
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = goesIdTF.computeTrim(0, 0, charWidth * 8, charHeight).width;
        goesIdTF.setLayoutData(gd);

        Label reportingTimeLbl = new Label(rightComp, SWT.RIGHT);
        reportingTimeLbl.setText("Reporting Time:");
        reportingTimeLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER,
                false, true));

        reportingTimeTF = new Text(rightComp, SWT.BORDER);
        reportingTimeTF.setTextLimit(8);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = reportingTimeTF.computeTrim(0, 0, charWidth * 8,
                charHeight).width;
        reportingTimeTF.setLayoutData(gd);

        Label reportingFreqLbl = new Label(rightComp, SWT.RIGHT);
        reportingFreqLbl.setText("Reporting Frequency:");
        reportingFreqLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER,
                false, true));

        reportingFreqTF = new Text(rightComp, SWT.BORDER);
        reportingFreqTF.setTextLimit(4);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = reportingTimeTF.computeTrim(0, 0, charWidth * 4,
                charHeight).width;
        reportingFreqTF.setLayoutData(gd);

        Label obsFreqTFLabel = new Label(rightComp, SWT.RIGHT);
        obsFreqTFLabel.setText("Observation Frequency:");
        obsFreqTFLabel.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        observationFreqTF = new Text(rightComp, SWT.BORDER);
        observationFreqTF.setTextLimit(4);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = observationFreqTF.computeTrim(0, 0, charWidth * 4,
                charHeight).width;

        randomReportChk = new Button(rightComp, SWT.CHECK);
        randomReportChk.setText("Random Report");
        randomReportChk.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER,
                false, true));

        // -----------------------------------------
        // Create the Criteria Group
        // -----------------------------------------
        Group criteriaGroup = new Group(dcpComp, SWT.NONE);
        criteriaGroup.setText("Criteria");
        criteriaGroup.setLayout(new GridLayout(1, false));
        criteriaGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        dcpCriteriaTF = new Text(criteriaGroup, SWT.BORDER);
        dcpCriteriaTF.setTextLimit(50);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        gd.widthHint = dcpCriteriaTF.computeTrim(0, 0, charWidth * 50,
                charHeight).width;
        dcpCriteriaTF.setLayoutData(gd);

        return dcpComp;
    }

    /**
     * Create Observer composite.
     */
    private Composite createObserverComposite(Composite parent) {
        Composite observerComp = new Composite(parent, SWT.NONE);
        observerComp.setLayout(new GridLayout(1, false));
        observerComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        // -----------------------------------------
        // Create the Observer Group
        // -----------------------------------------
        Group observerGroup = new Group(observerComp, SWT.NONE);
        observerGroup.setText("Observer");
        observerGroup.setLayout(new GridLayout(4, false));
        observerGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        Label firstNameLbl = new Label(observerGroup, SWT.RIGHT);
        firstNameLbl.setText("First Name:");
        firstNameLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        firstNameTF = new Text(observerGroup, SWT.BORDER);
        firstNameTF.setTextLimit(12);
        GC gc = new GC(firstNameTF);
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        int charHeight = gc.getFontMetrics().getHeight();
        gc.dispose();
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = firstNameTF
                .computeTrim(0, 0, charWidth * 12, charHeight).width;
        firstNameTF.setLayoutData(gd);

        dosChk = new Button(observerGroup, SWT.CHECK);
        dosChk.setText("DoS:");
        dosChk.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));
        dosChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDosDate();
            }
        });

        dosTF = new Text(observerGroup, SWT.BORDER);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = dosTF.computeTrim(0, 0, charWidth * 10, charHeight).width;
        dosTF.setLayoutData(gd);

        Label lastNameLbl = new Label(observerGroup, SWT.RIGHT);
        lastNameLbl.setText("I/Last Name:");
        lastNameLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        lastNameTF = new Text(observerGroup, SWT.BORDER);
        lastNameTF.setTextLimit(28);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = lastNameTF.computeTrim(0, 0, charWidth * 28, charHeight).width;
        lastNameTF.setLayoutData(gd);

        Label homeLbl = new Label(observerGroup, SWT.RIGHT);
        homeLbl.setText("Home:");
        homeLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        homePhoneTF = new Text(observerGroup, SWT.BORDER);
        homePhoneTF.setTextLimit(18);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = homePhoneTF
                .computeTrim(0, 0, charWidth * 18, charHeight).width;
        homePhoneTF.setLayoutData(gd);

        Label address1Lbl = new Label(observerGroup, SWT.RIGHT);
        address1Lbl.setText("Address1:");
        address1Lbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        address1TF = new Text(observerGroup, SWT.BORDER);
        address1TF.setTextLimit(30);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = address1TF.computeTrim(0, 0, charWidth * 30, charHeight).width;
        address1TF.setLayoutData(gd);

        Label workLbl = new Label(observerGroup, SWT.RIGHT);
        workLbl.setText("Work:");
        workLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        workPhoneTF = new Text(observerGroup, SWT.BORDER);
        workPhoneTF.setTextLimit(18);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = workPhoneTF
                .computeTrim(0, 0, charWidth * 18, charHeight).width;
        workPhoneTF.setLayoutData(gd);

        Label address2Lbl = new Label(observerGroup, SWT.RIGHT);
        address2Lbl.setText("Address2:");
        address2Lbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        address2TF = new Text(observerGroup, SWT.BORDER);
        address2TF.setTextLimit(30);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = address2TF.computeTrim(0, 0, charWidth * 30, charHeight).width;
        address2TF.setLayoutData(gd);

        Label ssnLbl = new Label(observerGroup, SWT.RIGHT);
        ssnLbl.setText("SSN:");
        ssnLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        ssnTF = new Text(observerGroup, SWT.BORDER);
        ssnTF.setTextLimit(11);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = ssnTF.computeTrim(0, 0, charWidth * 11, charHeight).width;
        ssnTF.setLayoutData(gd);

        Label address3Lbl = new Label(observerGroup, SWT.RIGHT);
        address3Lbl.setText("Address3:");
        address3Lbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        address3TF = new Text(observerGroup, SWT.BORDER);
        address3TF.setTextLimit(30);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = address3TF.computeTrim(0, 0, charWidth * 30, charHeight).width;
        address3TF.setLayoutData(gd);

        Label genderLbl = new Label(observerGroup, SWT.RIGHT);
        genderLbl.setText("Gender:");
        genderLbl
                .setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        Composite radioComp = new Composite(observerGroup, SWT.NONE);
        radioComp.setLayout(new RowLayout());

        maleRdo = new Button(radioComp, SWT.RADIO);
        maleRdo.setText("M");
        femaleRdo = new Button(radioComp, SWT.RADIO);
        femaleRdo.setText("F");
        ignoreRdo = new Button(radioComp, SWT.RADIO);
        ignoreRdo.setText("I");

        Label cityLbl = new Label(observerGroup, SWT.RIGHT);
        cityLbl.setText("City:");
        cityLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        cityTF = new Text(observerGroup, SWT.BORDER);
        cityTF.setTextLimit(30);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = cityTF.computeTrim(0, 0, charWidth * 30, charHeight).width;
        gd.horizontalSpan = 3;
        cityTF.setLayoutData(gd);

        Label stateLbl = new Label(observerGroup, SWT.RIGHT);
        stateLbl.setText("State:");
        stateLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        stateCbo = new Combo(observerGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        stateCbo.setVisibleItemCount(10);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.horizontalSpan = 3;
        stateCbo.setLayoutData(gd);

        Label zipCodeLbl = new Label(observerGroup, SWT.RIGHT);
        zipCodeLbl.setText("Zip:");
        zipCodeLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        zipCodeTF = new Text(observerGroup, SWT.BORDER);
        zipCodeTF.setTextLimit(10);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = zipCodeTF.computeTrim(0, 0, charWidth * 10, charHeight).width;
        gd.horizontalSpan = 3;
        zipCodeTF.setLayoutData(gd);

        Label emailLbl = new Label(observerGroup, SWT.RIGHT);
        emailLbl.setText("E-Mail:");
        emailLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        emailTF = new Text(observerGroup, SWT.BORDER);
        emailTF.setTextLimit(60);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = emailTF.computeTrim(0, 0, charWidth * 30, charHeight).width;
        gd.horizontalSpan = 3;
        emailTF.setLayoutData(gd);

        // -----------------------------------------
        // Create the Administration Group
        // -----------------------------------------
        Group adminGroup = new Group(observerComp, SWT.NONE);
        adminGroup.setText("Administration");
        adminGroup.setLayout(new GridLayout(6, false));
        adminGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        Label commsLbl = new Label(adminGroup, SWT.RIGHT);
        commsLbl.setText("Comms:");
        commsLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        commsCbo = new Combo(adminGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.horizontalSpan = 3;
        commsCbo.setLayoutData(gd);

        Label taskNoLbl = new Label(adminGroup, SWT.RIGHT);
        taskNoLbl.setText("Task No:");
        taskNoLbl
                .setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        taskNoTF = new Text(adminGroup, SWT.BORDER);
        taskNoTF.setTextLimit(13);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = taskNoTF.computeTrim(0, 0, charWidth * 13, charHeight).width;
        taskNoTF.setLayoutData(gd);

        Label sponsorLbl = new Label(adminGroup, SWT.RIGHT);
        sponsorLbl.setText("Sponsor:");
        sponsorLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, true));

        sponsorList = new List(adminGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        gc = new GC(sponsorList);
        int charWidthList = gc.getFontMetrics().getAverageCharWidth();
        int charHeightList = gc.getFontMetrics().getHeight();
        gc.dispose();
        gd = new GridData(SWT.LEFT, SWT.TOP, false, true);
        gd.heightHint = sponsorList.getItemHeight() * 5;
        gd.widthHint = sponsorList.computeTrim(0, 0, charWidthList * 10,
                charHeightList).width;
        sponsorList.setLayoutData(gd);

        Label rateLbl = new Label(adminGroup, SWT.RIGHT);
        rateLbl.setText("Rate:");
        rateLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        rateTF = new Text(adminGroup, SWT.BORDER);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = rateTF.computeTrim(0, 0, charWidth * 13, charHeight).width;
        rateTF.setLayoutData(gd);

        Label cd404Lbl = new Label(adminGroup, SWT.RIGHT);
        cd404Lbl.setText("CD-404:");
        cd404Lbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        cd404TF = new Text(adminGroup, SWT.BORDER);
        cd404TF.setTextLimit(4);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = rateTF.computeTrim(0, 0, charWidth * 13, charHeight).width;
        cd404TF.setLayoutData(gd);

        Label recipLbl = new Label(adminGroup, SWT.RIGHT);
        recipLbl.setText("Recip:");
        recipLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, true));

        recipList = new List(adminGroup, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        gd = new GridData(SWT.LEFT, SWT.TOP, false, true);
        gd.heightHint = recipList.getItemHeight() * 5;
        gd.widthHint = recipList.computeTrim(0, 0, charWidthList * 20,
                charHeightList).width;
        gd.horizontalSpan = 5;
        recipList.setLayoutData(gd);

        Label reportLbl = new Label(adminGroup, SWT.RIGHT);
        reportLbl.setText("Report:");
        reportLbl
                .setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        reportTF = new Text(adminGroup, SWT.BORDER);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = taskNoTF.computeTrim(0, 0, charWidth * 60, charHeight).width;
        gd.horizontalSpan = 5;
        reportTF.setLayoutData(gd);

        return observerComp;
    }

    /**
     * Create the telemetry composite.
     */
    private Composite createTelemetryComposite(Composite parent) {
        Composite telemetryComp = new Composite(parent, SWT.NONE);
        telemetryComp.setLayout(new GridLayout(1, false));
        telemetryComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        // -----------------------------------------
        // Create the General Group
        // -----------------------------------------
        Group generalGroup = new Group(telemetryComp, SWT.NONE);
        generalGroup.setText("General");
        generalGroup.setLayout(new GridLayout(2, false));
        generalGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        // Create the left composite
        Composite leftComp = new Composite(generalGroup, SWT.NONE);
        leftComp.setLayout(new GridLayout(2, false));

        Label telemetryLbl = new Label(leftComp, SWT.NONE);
        telemetryLbl.setText("Telemetry:");
        telemetryLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false,
                false));

        telemetryList = new List(leftComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        GC gc = new GC(ownerList);
        int charWidth = gc.getFontMetrics().getAverageCharWidth();
        int charHeight = gc.getFontMetrics().getHeight();
        gc.dispose();
        GridData gd = new GridData(SWT.LEFT, SWT.TOP, false, true);
        gd.heightHint = telemetryList.getItemHeight() * 5;
        gd.widthHint = telemetryList.computeTrim(0, 0, charWidth * 10,
                charHeight).width;
        telemetryList.setLayoutData(gd);

        Label ownerLbl = new Label(leftComp, SWT.NONE);
        ownerLbl.setText("Owner:");
        ownerLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));

        telemOwnerList = new List(leftComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        gd = new GridData(SWT.LEFT, SWT.TOP, false, true);
        gd.heightHint = telemOwnerList.getItemHeight() * 5;
        gd.widthHint = telemOwnerList.computeTrim(0, 0, charWidth * 10,
                charHeight).width;
        telemOwnerList.setLayoutData(gd);

        Label payorLbl = new Label(leftComp, SWT.NONE);
        payorLbl.setText("Payor:");
        payorLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.TOP, false, false));

        payorList = new List(leftComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        gd = new GridData(SWT.LEFT, SWT.TOP, false, true);
        gd.heightHint = payorList.getItemHeight() * 5;
        gd.widthHint = payorList.computeTrim(0, 0, charWidth * 10, charHeight).width;
        payorList.setLayoutData(gd);

        // Create the right composite
        Composite rightComp = new Composite(generalGroup, SWT.NONE);
        rightComp.setLayout(new GridLayout(2, false));
        rightComp
                .setLayoutData(new GridData(SWT.DEFAULT, SWT.TOP, false, true));

        Label phoneLbl = new Label(rightComp, SWT.RIGHT);
        phoneLbl.setText("Phone:");
        phoneLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        telemetryPhoneTF = new Text(rightComp, SWT.BORDER);
        gc = new GC(telemetryPhoneTF);
        charWidth = gc.getFontMetrics().getAverageCharWidth();
        charHeight = gc.getFontMetrics().getHeight();
        gc.dispose();
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = telemetryPhoneTF.computeTrim(0, 0, charWidth * 12,
                charHeight).width;
        telemetryPhoneTF.setTextLimit(12);
        telemetryPhoneTF.setLayoutData(gd);

        Label costLbl = new Label(rightComp, SWT.RIGHT);
        costLbl.setText("Cost:");
        costLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, true));

        telemetryCostTF = new Text(rightComp, SWT.BORDER);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = telemetryCostTF.computeTrim(0, 0, charWidth * 12,
                charHeight).width;
        telemetryCostTF.setLayoutData(gd);

        Label sensorIdLbl = new Label(rightComp, SWT.RIGHT);
        sensorIdLbl.setText("Sensor Id:");
        sensorIdLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        telemetrySensorIdTF = new Text(rightComp, SWT.BORDER);
        telemetrySensorIdTF.setTextLimit(10);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = telemetrySensorIdTF.computeTrim(0, 0, charWidth * 10,
                charHeight).width;
        telemetrySensorIdTF.setLayoutData(gd);

        Label reportFreqLbl = new Label(rightComp, SWT.RIGHT);
        reportFreqLbl.setText("Reporting Frequency:");
        reportFreqLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        telemetryReportFreqTF = new Text(rightComp, SWT.BORDER);
        telemetryReportFreqTF.setTextLimit(4);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = telemetryReportFreqTF.computeTrim(0, 0, charWidth * 4,
                charHeight).width;
        telemetryReportFreqTF.setLayoutData(gd);

        Label obsFreqLbl = new Label(rightComp, SWT.RIGHT);
        obsFreqLbl.setText("Observation Frequency:");
        obsFreqLbl.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false,
                true));

        telemetryObsFreqTF = new Text(rightComp, SWT.BORDER);
        telemetryObsFreqTF.setTextLimit(4);
        gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
        gd.widthHint = telemetryReportFreqTF.computeTrim(0, 0, charWidth * 4,
                charHeight).width;
        telemetryObsFreqTF.setLayoutData(gd);

        // -----------------------------------------
        // Create the Criteria Group
        // -----------------------------------------
        Group criteriaGroup = new Group(telemetryComp, SWT.NONE);
        criteriaGroup.setText("Criteria");
        criteriaGroup.setLayout(new GridLayout(1, false));
        criteriaGroup.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        telemetryCriteriaTF = new Text(criteriaGroup, SWT.BORDER);
        telemetryCriteriaTF.setTextLimit(50);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        gd.widthHint = telemetryCriteriaTF.computeTrim(0, 0, charWidth * 50,
                charHeight).width;
        telemetryCriteriaTF.setLayoutData(gd);

        return telemetryComp;
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        if (fullControls) {
            createButton(parent, APPLY_ID, APPLY_LABEL, false);
        }

        createButton(parent, IDialogConstants.CLOSE_ID,
                IDialogConstants.CLOSE_LABEL, false);

        if (fullControls) {
            createButton(parent, DELETE_ID, DELETE_LABEL, false);
        }
    }

    @Override
    protected void buttonPressed(int buttonId) {
        switch (buttonId) {
        case APPLY_ID:
            saveRecord();
            break;
        case IDialogConstants.CLOSE_ID:
            close();
            break;
        case DELETE_ID:
            deleteRecord();
            break;
        default:
            statusHandler.warn(String.format(
                    "Unrecognized button [%d] pressed.", buttonId));
        }
    }

    /**
     * Loads static data from the database.
     */
    private void loadStaticData() {
        try {
            // GetDcpOwner("")
            ownerList.setItems(getData("owner", "dcpowner").toArray(
                    new String[0]));
            ownerList.select(0);

            // GetState(" ORDER BY state ");
            stateCbo.setItems(getData("state", "state").toArray(new String[0]));
            stateCbo.select(0);
            // GetCoopComms(" ORDER BY comm ")
            commsCbo.setItems(getData("comm", "coopcomms").toArray(
                    new String[0]));
            commsCbo.select(0);
            // GetCoopSpons(" ORDER BY spons ");
            sponsorList.setItems(getData("spons", "coopspons").toArray(
                    new String[0]));
            sponsorList.select(0);
            // GetCoopRecip(" ORDER BY recip ");
            recipList.setItems(getData("recip", "cooprecip").toArray(
                    new String[0]));
            recipList.select(0);

            // GetTelmOwner(" ORDER BY owner ")
            telemOwnerList.setItems(getData("owner", "telmowner").toArray(
                    new String[0]));
            telemOwnerList.select(0);
            // GetTelmPayor(" ORDER BY payor ")
            payorList.setItems(getData("payor", "telmpayor").toArray(
                    new String[0]));
            payorList.select(0);
            // GetTelmType(" ORDER BY type ")
            telemetryList.setItems(getData("type", "telmtype").toArray(
                    new String[0]));
            telemetryList.select(0);
        } catch (Exception e) {
            statusHandler.error("Unable to load static data: ", e);
        }
    }

    /**
     * Retrieves all data from the column from the table specified.
     * 
     * @param column
     *            The column from the database to return.
     * @param table
     *            The table to get the data from.
     * @return
     * @throws VizException
     */
    private Collection<String> getData(String column, String table)
            throws VizException {
        // The base select statement for the queries
        String query = String.format("SELECT %s FROM %s ORDER BY %s", column,
                table, column);

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                query);
        if (data != null) {
            Collection<String> dataList = new ArrayList<>(data.getResultCount());

            // Populate the list with the results from the query
            for (QueryResultRow currRow : data.getRows()) {
                String rowVal = currRow.getColumn(
                        data.getColumnNames().get(column)).toString();
                dataList.add(rowVal);
            }

            return dataList;
        }

        return Collections.emptyList();
    }

    // --------------------------------------------
    // Get Data
    // --------------------------------------------
    private void getDialogData() {
        dcpData = getDCPData();
        obsData = getObsData();
        telemData = getTelemData();

        updateDisplay();
    }

    /**
     * Load DCP data from the manager.
     * 
     * @return
     */
    private DcpData getDCPData() {
        DcpData dcpData = null;
        try {
            DcpData seedData = new DcpData();
            seedData.setLid(lid);

            java.util.List<DcpData> data = HydroDBDataManager.getInstance()
                    .getData(seedData);
            if (!data.isEmpty()) {
                // There will only be one record per lid
                dcpData = data.get(0);
            }
        } catch (VizException e) {
            statusHandler
                    .handle(Priority.ERROR, "Unable to load DCP data: ", e);
        }

        return dcpData;
    }

    /**
     * Load observation data from the manager.
     * 
     * @return
     */
    private ObserverData getObsData() {
        ObserverData obsData = null;
        try {
            ObserverData seedData = new ObserverData();
            seedData.setLid(lid);

            java.util.List<ObserverData> data = HydroDBDataManager
                    .getInstance().getData(seedData);
            if (!data.isEmpty()) {
                // There will only be one record per lid
                obsData = data.get(0);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to load Observation data: ", e);
        }

        return obsData;
    }

    /**
     * Load Telemetry data from the manager.
     * 
     * @return
     */
    private TelemData getTelemData() {
        TelemData telemData = null;
        try {
            TelemData seedData = new TelemData();
            seedData.setLid(lid);

            java.util.List<TelemData> data = HydroDBDataManager.getInstance()
                    .getData(seedData);
            if (!data.isEmpty()) {
                // There will only be one record per lid
                telemData = data.get(0);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to load Telemetry data: ", e);
        }

        return telemData;
    }

    // --------------------------------------------
    // Update display
    // --------------------------------------------
    private void updateDisplay() {
        if (typeCbo.getSelectionIndex() == 0) {
            stackLayout.topControl = dcpComp;
            stackComposite.layout();

            updateDCPDisplay();
        } else if (typeCbo.getSelectionIndex() == 1) {
            stackLayout.topControl = observerComp;
            stackComposite.layout();

            updateObsDisplay();
        } else if (typeCbo.getSelectionIndex() == 2) {
            stackLayout.topControl = telemetryComp;
            stackComposite.layout();

            updateTelemDisplay();
        }
    }

    /**
     * This handles updating the DCP display.
     */
    private void updateDCPDisplay() {
        clearDCPInformation();

        if (dcpData != null) {
            goesIdTF.setText(dcpData.getGoesId());
            reportingTimeTF.setText(dcpData.getReportTime());
            reportingFreqTF.setText(dcpData.getReportFrequency());
            dcpCriteriaTF.setText(dcpData.getCriteria());
            observationFreqTF.setText(dcpData.getObservationFrequency());

            // Owner
            selectSetOption(dcpData.getOwner(), ownerList);

            // Random report - Check box if randrept == 'T'
            randomReportChk.setSelection(dcpData.getRandomReport()
                    .equalsIgnoreCase("T"));

            updateDialogState(true);
        } else {
            updateDialogState(false);
        }
    }

    /**
     * Update the Observation display.
     */
    private void updateObsDisplay() {
        clearObsInformation();

        if (obsData != null) {
            // Name
            firstNameTF.setText(obsData.getFirstName());
            lastNameTF.setText(obsData.getLastName());

            // Address
            address1TF.setText(obsData.getAddressLine1());
            address2TF.setText(obsData.getAddressLine2());
            address3TF.setText(obsData.getAddressLine3());

            // City/State
            cityTF.setText(obsData.getCity());
            selectSetOption(obsData.getState(), stateCbo);

            // Zipcode
            zipCodeTF.setText(obsData.getZipCode());

            // email
            emailTF.setText(obsData.getEmail());

            // Date of service
            dosTF.setText(ISO_DATE.format(obsData.getDateOfService()));

            // Phone Numbers
            homePhoneTF.setText(obsData.getHomePhone());
            workPhoneTF.setText(obsData.getWorkPhone());

            // SSN
            ssnTF.setText(obsData.getSsn());

            // Gender
            String gender = obsData.getGender();
            if (gender.equals("M")) {
                maleRdo.setSelection(true);
                femaleRdo.setSelection(false);
                ignoreRdo.setSelection(false);
            } else if (gender.equals("F")) {
                maleRdo.setSelection(false);
                femaleRdo.setSelection(true);
                ignoreRdo.setSelection(false);
            } else {
                maleRdo.setSelection(false);
                femaleRdo.setSelection(false);
                ignoreRdo.setSelection(true);
            }

            // Administration
            taskNoTF.setText(obsData.getTaskNumber());
            cd404TF.setText(obsData.getCd404());
            rateTF.setText(HydroDataUtils.getDisplayString(obsData.getRate()));
            reportTF.setText(obsData.getReport());

            // Comms
            selectSetOption(obsData.getComms(), commsCbo);

            // Sponsor
            selectSetOption(obsData.getSponsor(), sponsorList);

            // Recipient
            selectSetOption(obsData.getRecipient(), recipList);

            updateDialogState(true);
        } else {
            updateDialogState(false);
        }
    }

    /**
     * Update the Telemetry display.
     */
    private void updateTelemDisplay() {
        clearTelemInformation();

        if (telemData != null) {
            selectSetOption(telemData.getType(), telemetryList);
            selectSetOption(telemData.getOwner(), telemOwnerList);
            selectSetOption(telemData.getPayor(), payorList);

            telemetryPhoneTF.setText(telemData.getPhone());
            telemetryCostTF.setText(HydroDataUtils.getDisplayString(telemData
                    .getCost()));
            telemetrySensorIdTF.setText(telemData.getSensorId());
            telemetryReportFreqTF.setText(telemData.getReportFrequency());
            telemetryObsFreqTF.setText(telemData.getObservationFrequency());
            telemetryCriteriaTF.setText(telemData.getCriteria());

            updateDialogState(true);
        } else {
            updateDialogState(false);
        }
    }

    // --------------------------------------------
    // Save
    // --------------------------------------------\
    /**
     * Save DCP, Obs and Telemetry records.
     */
    private void saveRecord() {
        if (typeCbo.getSelectionIndex() == 0) {
            saveDCPRecord();
        } else if (typeCbo.getSelectionIndex() == 1) {
            saveObsRecord();
        } else if (typeCbo.getSelectionIndex() == 2) {
            saveTelemRecord();
        }
    }

    /**
     * Save the DCP Record.
     */
    private void saveDCPRecord() {
        DcpData newData = new DcpData();

        newData.setLid(lid);
        newData.setCriteria(dcpCriteriaTF.getText());
        newData.setOwner(getSelectedValue(ownerList));
        newData.setGoesId(goesIdTF.getText());
        newData.setReportFrequency(reportingFreqTF.getText());
        newData.setReportTime(reportingTimeTF.getText());
        newData.setRandomReport(randomReportChk.getSelection() ? "T" : "F");
        newData.setObservationFrequency(observationFreqTF.getText());

        try {
            HydroDBDataManager.getInstance().putData(newData);

            // Synchronize StnClass table
            StnClassSyncUtil.setStnClass(lid);

            // Refresh Cache
            getDCPData();
        } catch (VizException e) {
            MessageDialog.openError(getShell(), "Unable to Save",
                    "An error occurred while trying to save");
        }
    }

    /**
     * Save the observation record.
     */
    private void saveObsRecord() {
        ObserverData newData = new ObserverData();

        newData.setLid(lid);
        newData.setAddressLine1(address1TF.getText());
        newData.setAddressLine2(address2TF.getText());
        newData.setAddressLine3(address3TF.getText());
        newData.setCity(cityTF.getText());
        newData.setState(getSelectedValue(stateCbo));
        newData.setZipCode(zipCodeTF.getText());
        newData.setComms(getSelectedValue(commsCbo));

        // Date of Service
        if (!dosTF.getText().equals("")) {
            try {
                newData.setDateOfService(ISO_DATE.parse(dosTF.getText()));
            } catch (ParseException e) {
                MessageDialog.openError(getShell(), "Invalid Date",
                        "Please enter a Service Date in the form: YYYY-MM-DD");
                return;
            }
        } else {
            newData.setDateOfService(null);
        }

        // Gender
        if (maleRdo.getSelection()) {
            newData.setGender("M");
        } else if (femaleRdo.getSelection()) {
            newData.setGender("F");
        } else {
            newData.setGender("I");
        }

        newData.setHomePhone(homePhoneTF.getText());
        newData.setFirstName(firstNameTF.getText());
        newData.setLastName(lastNameTF.getText());
        newData.setWorkPhone(workPhoneTF.getText());
        newData.setEmail(emailTF.getText());
        newData.setCd404(cd404TF.getText());

        // Rate
        Double tmp = HydroDataUtils.getDoubleFromTF(getShell(), rateTF, "Rate");
        if (tmp == null) {
            return;
        }
        newData.setRate(tmp);

        newData.setRecipient(getSelectedValue(recipList));
        newData.setReport(reportTF.getText());
        newData.setSponsor(getSelectedValue(sponsorList));
        newData.setSsn(ssnTF.getText());
        newData.setTaskNumber(taskNoTF.getText());

        try {
            HydroDBDataManager.getInstance().putData(newData);

            // Synchronize StnClass table
            StnClassSyncUtil.setStnClass(lid);

            // Refresh Cache
            getObsData();
        } catch (VizException e) {
            MessageDialog.openError(getShell(), "Unable to Save",
                    "An error occurred while trying to save");
        }
    }

    /**
     * Save the Telemetry record.
     */
    private void saveTelemRecord() {
        TelemData newData = new TelemData();

        newData.setLid(lid);
        newData.setType(getSelectedValue(telemetryList));
        newData.setPayor(getSelectedValue(payorList));

        // Cost
        Double cost = HydroDataUtils.getDoubleFromTF(getShell(),
                telemetryCostTF, "Cost");
        if (cost == null) {
            return;
        }
        newData.setCost(cost);

        newData.setCriteria(telemetryCriteriaTF.getText());
        newData.setOwner(getSelectedValue(telemOwnerList));
        newData.setPhone(telemetryPhoneTF.getText());
        newData.setSensorId(telemetrySensorIdTF.getText());
        newData.setReportFrequency(telemetryReportFreqTF.getText());
        newData.setObservationFrequency(telemetryObsFreqTF.getText());

        try {
            HydroDBDataManager.getInstance().putData(newData);

            // Synchronize StnClass table
            StnClassSyncUtil.setStnClass(lid);

            // Refresh Cache
            getTelemData();
        } catch (VizException e) {
            MessageDialog.openError(getShell(), "Unable to Save",
                    "An error occurred while trying to save");
        }
    }

    // --------------------------------------------
    // Delete
    // --------------------------------------------
    /**
     * Delete the DCP, OBS and Telemetry records.
     */
    private void deleteRecord() {
        if (typeCbo.getSelectionIndex() == 0) {
            deleteDataSourceRecord(dcpData);

            // Refresh the cache
            getDCPData();
        } else if (typeCbo.getSelectionIndex() == 1) {
            deleteDataSourceRecord(obsData);

            // Refresh the cache
            getObsData();
        } else if (typeCbo.getSelectionIndex() == 2) {
            deleteDataSourceRecord(telemData);

            // Refresh the cache
            getTelemData();
        }
    }

    /**
     * Confirm and delete the record.
     * 
     * @param currData
     */
    private <T extends HydroDBData> void deleteDataSourceRecord(T currData) {
        if (currData != null) {
            boolean result = MessageDialog.openQuestion(getShell(),
                    "Delete Confirmation", "Do you wish to delete this entry?");

            if (result) {
                try {
                    HydroDBDataManager.getInstance().deleteRecord(currData);

                    // Synchronize StnClass table
                    StnClassSyncUtil.setStnClass(lid);
                } catch (VizException e) {
                    MessageDialog.openError(getShell(), "Unable to Delete",
                            "An error occurred while trying to delete");
                }
            }
        }
    }

    // --------------------------------------------
    // Update display
    // --------------------------------------------
    /**
     * Update the enable state of the delete button.
     * 
     * @param isDataAvailable
     */
    private void updateDialogState(boolean isDataAvailable) {
        if (fullControls) {
            getButton(DELETE_ID).setEnabled(isDataAvailable);
        }
    }

    /**
     * Clears the dialog of information
     */
    private void clearDCPInformation() {
        goesIdTF.setText("");
        reportingTimeTF.setText("");
        reportingFreqTF.setText("");
        dcpCriteriaTF.setText("");
        observationFreqTF.setText("");

        // Owner
        ownerList.select(0);

        // Random Report - Check box if randrept == 'T'
        randomReportChk.setSelection(false);
    }

    /**
     * Clear the fields of the OBS record.
     */
    private void clearObsInformation() {
        // Name
        firstNameTF.setText("");
        lastNameTF.setText("");

        // Address
        address1TF.setText("");
        address2TF.setText("");
        address3TF.setText("");

        // City/State
        cityTF.setText("");
        stateCbo.select(0);

        // Zipcode
        zipCodeTF.setText("");

        // email
        emailTF.setText("");

        // Date of service
        dosTF.setText("");

        // Phone Numbers
        homePhoneTF.setText("");
        workPhoneTF.setText("");

        // SSN
        ssnTF.setText("");

        // Gender
        maleRdo.setSelection(false);
        femaleRdo.setSelection(false);
        ignoreRdo.setSelection(false);

        // Administration
        taskNoTF.setText("");
        cd404TF.setText("");
        rateTF.setText("");
        reportTF.setText("");

        // Comms
        commsCbo.select(0);

        // Sponsor
        sponsorList.select(0);

        // Recipient
        recipList.select(0);
    }

    /**
     * Clear the Telemetry fields.
     */
    private void clearTelemInformation() {
        telemetryList.select(0);
        telemOwnerList.select(0);
        payorList.select(0);

        telemetryPhoneTF.setText("");
        telemetryCostTF.setText("");
        telemetrySensorIdTF.setText("");
        telemetryReportFreqTF.setText("");
        telemetryCriteriaTF.setText("");
        telemetryObsFreqTF.setText("");
    }

    /**
     * Finds and selects the selected option.
     * 
     * @param optionSet
     *            The selected option.
     * @param dataList
     *            The list to set the selection in.
     */
    private void selectSetOption(String optionSet, Combo dataList) {
        dataList.deselectAll();
        int index = getIndexIgnoreCase(optionSet, dataList.getItems());
        dataList.select(index);
    }

    /**
     * Finds and selects the selected option.
     * 
     * @param optionSet
     *            The selected option.
     * @param dataList
     *            The list to set the selection in.
     */
    private void selectSetOption(String optionSet, List dataList) {
        dataList.deselectAll();
        int index = getIndexIgnoreCase(optionSet, dataList.getItems());
        dataList.select(index);
    }

    /**
     * Returns the index within the array of the first occurrence of the
     * specified string. The search performed is case-insensitive.
     * 
     * @param str
     *            the string to search for.
     * @param items
     *            the array of strings to search
     * @return the index of the first occurrence of the specified string, or
     *         {@code -1} if there is no such occurrence.
     */
    private int getIndexIgnoreCase(String str, String[] items) {
        int i = 0;
        for (String item : items) {
            if (str.equalsIgnoreCase(item)) {
                return i;
            }
            i++;
        }
        return -1;
    }

    /**
     * Returns the selected option in the list
     */
    private String getSelectedValue(List dataList) {
        String rval = "";

        if (dataList.getSelectionCount() > 0) {
            rval = dataList.getItem(dataList.getSelectionIndex());
        }

        return rval;
    }

    /**
     * Get the dataList selection or empty string when non selection.
     * 
     * @param dataList
     * @return
     */
    private String getSelectedValue(Combo dataList) {
        String rval = "";

        if (dataList.getSelectionIndex() >= 0) {
            rval = dataList.getItem(dataList.getSelectionIndex());
        }

        return rval;
    }

    /**
     * Determine if DOS time should be updated to current simulated time or the
     * date in the database.
     */
    private void updateDosDate() {
        /*
         * If the Checkbox is checked, set the Date to the current simulated
         * time. Else load the date from the database
         */
        Date toFormat = null;
        if (dosChk.getSelection()) {
            toFormat = SimulatedTime.getSystemTime().getTime();
        } else if (obsData != null) {
            toFormat = obsData.getDateOfService();
        }
        String dateString = (toFormat != null) ? ISO_DATE.format(toFormat) : "";
        dosTF.setText(dateString);
    }
}
