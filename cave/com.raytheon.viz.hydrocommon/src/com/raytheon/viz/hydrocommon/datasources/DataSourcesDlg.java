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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
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

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.DcpData;
import com.raytheon.viz.hydrocommon.data.HydroDBData;
import com.raytheon.viz.hydrocommon.data.ObserverData;
import com.raytheon.viz.hydrocommon.data.TelemData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.hydrocommon.util.StnClassSyncUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

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
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class DataSourcesDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataSourcesDlg.class);

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Current Location Identifier.
     */
    private String lid;

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
     * Flag indicating if the full controls should be displayed.
     */
    private boolean fullControls = false;

    /**
     * Delete button
     */
    private Button deleteBtn;

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
     * Possible states for the dialog
     */
    private enum DialogStates {
        NO_DATA, DATA_AVAILABLE
    }

    /**
     * Formatted ISO UTC Date.
     */
    SimpleDateFormat isoDate = new SimpleDateFormat("MM/dd/yyyy");

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
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Data Sources" + titleInfo);

        this.lid = lid;
        this.fullControls = fullControls;

        isoDate.setTimeZone(TimeZone.getTimeZone("UTC"));
        setReturnValue(lid);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
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
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        // Populate the data sources for the dialog.
        // populateDataSources();
        createTypeComboBox();
        addSeparator();
        createStackComposite();
        addSeparator();
        createBottomButtons();

        loadStaticData();

        getDialogData();
    }

    /**
     * Create the Type label and combo box.
     */
    private void createTypeComboBox() {
        Composite typeComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        typeComp.setLayout(gl);

        Label typeLbl = new Label(typeComp, SWT.NONE);
        typeLbl.setText("Type");

        typeCbo = new Combo(typeComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        typeCbo.add("DCP");
        typeCbo.add("Observer");
        typeCbo.add("Telemetry");
        typeCbo.select(0);
        typeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateDisplay();
            }
        });
    }

    /**
     * Create stack composite that will contain the DCP, Observer, and telemetry
     * composites.
     */
    private void createStackComposite() {
        stackComposite = new Composite(shell, SWT.NONE);
        stackLayout = new StackLayout();
        stackComposite.setLayout(stackLayout);

        // create the composites in the stack here
        createDcpComposite();
        createObserverComposite();
        createTelemetryComposite();

        stackLayout.topControl = dcpComp;
        stackComposite.layout();
    }

    /**
     * Create the DCP composite.
     */
    private void createDcpComposite() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dcpComp = new Composite(stackComposite, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        dcpComp.setLayout(gl);
        dcpComp.setLayoutData(gd);

        createDcpControls();
    }

    /**
     * Create the DCP controls.
     */
    private void createDcpControls() {
        // -----------------------------------------
        // Create the General Group
        // -----------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group generalGroup = new Group(dcpComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        generalGroup.setLayout(gl);
        generalGroup.setLayoutData(gd);
        generalGroup.setText("General");

        // Create the left composite
        Composite leftComp = new Composite(generalGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        leftComp.setLayout(gl);

        gd = new GridData(SWT.RIGHT, SWT.TOP, false, false);
        gd.widthHint = 80;
        Label ownerLbl = new Label(leftComp, SWT.RIGHT);
        ownerLbl.setText("Owner:");
        ownerLbl.setLayoutData(gd);

        gd = new GridData(90, 100);
        ownerList = new List(leftComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        ownerList.setLayoutData(gd);

        // Create the right composite
        Composite rightComp = new Composite(generalGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        rightComp.setLayout(gl);

        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        Label goesIdLbl = new Label(rightComp, SWT.RIGHT);
        goesIdLbl.setText("GOES ID:");
        goesIdLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        goesIdTF = new Text(rightComp, SWT.BORDER);
        goesIdTF.setLayoutData(gd);
        goesIdTF.setTextLimit(8);

        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        Label reportingTimeLbl = new Label(rightComp, SWT.RIGHT);
        reportingTimeLbl.setText("Reporting Time:");
        reportingTimeLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        reportingTimeTF = new Text(rightComp, SWT.BORDER);
        reportingTimeTF.setLayoutData(gd);
        reportingTimeTF.setTextLimit(8);

        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        Label reportingFreqLbl = new Label(rightComp, SWT.RIGHT);
        reportingFreqLbl.setText("Reporting Frequency:");
        reportingFreqLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        reportingFreqTF = new Text(rightComp, SWT.BORDER);
        reportingFreqTF.setLayoutData(gd);
        reportingFreqTF.setTextLimit(4);

        gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        Label obsFreqTFLabel = new Label(rightComp, SWT.RIGHT);
        obsFreqTFLabel.setText("Observation Frequency:");
        obsFreqTFLabel.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        observationFreqTF = new Text(rightComp, SWT.BORDER);
        observationFreqTF.setLayoutData(gd);
        observationFreqTF.setTextLimit(4);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        randomReportChk = new Button(rightComp, SWT.CHECK);
        randomReportChk.setText("Random Report");
        randomReportChk.setLayoutData(gd);

        // -----------------------------------------
        // Create the Criteria Group
        // -----------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group criteriaGroup = new Group(dcpComp, SWT.NONE);
        gl = new GridLayout(2, false);
        criteriaGroup.setLayout(gl);
        criteriaGroup.setLayoutData(gd);
        criteriaGroup.setText("Criteria");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dcpCriteriaTF = new Text(criteriaGroup, SWT.BORDER);
        dcpCriteriaTF.setLayoutData(gd);
        dcpCriteriaTF.setTextLimit(50);
    }

    /**
     * Create Observer composite.
     */
    private void createObserverComposite() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        observerComp = new Composite(stackComposite, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        observerComp.setLayout(gl);
        observerComp.setLayoutData(gd);

        createObserverControls();
    }

    /**
     * Create the Observer controls.
     */
    private void createObserverControls() {
        // -----------------------------------------
        // Create the Observer Group
        // -----------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group observerGroup = new Group(observerComp, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        observerGroup.setLayout(gl);
        observerGroup.setLayoutData(gd);
        observerGroup.setText("Observer");

        int labelWidth = 100;

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label firstNameLbl = new Label(observerGroup, SWT.RIGHT);
        firstNameLbl.setText("First Name:");
        firstNameLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        firstNameTF = new Text(observerGroup, SWT.BORDER);
        firstNameTF.setLayoutData(gd);
        firstNameTF.setTextLimit(12);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        dosChk = new Button(observerGroup, SWT.CHECK);
        dosChk.setText("DoS:");
        dosChk.setLayoutData(gd);
        dosChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateDosDate();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        dosTF = new Text(observerGroup, SWT.BORDER);
        dosTF.setLayoutData(gd);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label lastNameLbl = new Label(observerGroup, SWT.RIGHT);
        lastNameLbl.setText("I/Last Name:");
        lastNameLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        lastNameTF = new Text(observerGroup, SWT.BORDER);
        lastNameTF.setLayoutData(gd);
        lastNameTF.setTextLimit(28);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label homeLbl = new Label(observerGroup, SWT.RIGHT);
        homeLbl.setText("Home:");
        homeLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        homePhoneTF = new Text(observerGroup, SWT.BORDER);
        homePhoneTF.setLayoutData(gd);
        homePhoneTF.setTextLimit(18);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label address1Lbl = new Label(observerGroup, SWT.RIGHT);
        address1Lbl.setText("Address1:");
        address1Lbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        address1TF = new Text(observerGroup, SWT.BORDER);
        address1TF.setLayoutData(gd);
        address1TF.setTextLimit(30);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label workLbl = new Label(observerGroup, SWT.RIGHT);
        workLbl.setText("Work:");
        workLbl.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        workPhoneTF = new Text(observerGroup, SWT.BORDER);
        workPhoneTF.setLayoutData(gd);
        workPhoneTF.setTextLimit(18);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label address2Lbl = new Label(observerGroup, SWT.RIGHT);
        address2Lbl.setText("Address2:");
        address2Lbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        address2TF = new Text(observerGroup, SWT.BORDER);
        address2TF.setLayoutData(gd);
        address2TF.setTextLimit(30);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label ssnLbl = new Label(observerGroup, SWT.RIGHT);
        ssnLbl.setText("SSN:");
        ssnLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        ssnTF = new Text(observerGroup, SWT.BORDER);
        ssnTF.setLayoutData(gd);
        ssnTF.setTextLimit(11);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label address3Lbl = new Label(observerGroup, SWT.RIGHT);
        address3Lbl.setText("Address3:");
        address3Lbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        address3TF = new Text(observerGroup, SWT.BORDER);
        address3TF.setLayoutData(gd);
        address3TF.setTextLimit(30);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label genderLbl = new Label(observerGroup, SWT.RIGHT);
        genderLbl.setText("Gender:");
        genderLbl.setLayoutData(gd);

        Composite radioComp = new Composite(observerGroup, SWT.NONE);
        radioComp.setLayout(new RowLayout());

        maleRdo = new Button(radioComp, SWT.RADIO);
        maleRdo.setText("M");
        femaleRdo = new Button(radioComp, SWT.RADIO);
        femaleRdo.setText("F");
        ignoreRdo = new Button(radioComp, SWT.RADIO);
        ignoreRdo.setText("I");

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label cityLbl = new Label(observerGroup, SWT.RIGHT);
        cityLbl.setText("City:");
        cityLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        cityTF = new Text(observerGroup, SWT.BORDER);
        cityTF.setLayoutData(gd);
        cityTF.setTextLimit(30);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label filler1 = new Label(observerGroup, SWT.NONE);
        filler1.setLayoutData(gd);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label stateLbl = new Label(observerGroup, SWT.RIGHT);
        stateLbl.setText("State:");
        stateLbl.setLayoutData(gd);

        stateCbo = new Combo(observerGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        stateCbo.setVisibleItemCount(10);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label filler2 = new Label(observerGroup, SWT.NONE);
        filler2.setLayoutData(gd);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label zipCodeLbl = new Label(observerGroup, SWT.RIGHT);
        zipCodeLbl.setText("Zip:");
        zipCodeLbl.setLayoutData(gd);

        gd = new GridData(60, SWT.DEFAULT);
        zipCodeTF = new Text(observerGroup, SWT.BORDER);
        zipCodeTF.setLayoutData(gd);
        zipCodeTF.setTextLimit(10);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label filler3 = new Label(observerGroup, SWT.NONE);
        filler3.setLayoutData(gd);

        gd = new GridData(labelWidth, SWT.DEFAULT);
        Label emailLbl = new Label(observerGroup, SWT.RIGHT);
        emailLbl.setText("E-Mail:");
        emailLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        emailTF = new Text(observerGroup, SWT.BORDER);
        emailTF.setLayoutData(gd);
        emailTF.setTextLimit(60);

        // -----------------------------------------
        // Create the Administration Group
        // -----------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group adminGroup = new Group(observerComp, SWT.NONE);
        gl = new GridLayout(6, false);
        adminGroup.setLayout(gl);
        adminGroup.setLayoutData(gd);
        adminGroup.setText("Administration");

        int adminLabelWidth = 80;

        gd = new GridData(adminLabelWidth, SWT.DEFAULT);
        Label commsLbl = new Label(adminGroup, SWT.RIGHT);
        commsLbl.setText("Comms:");
        commsLbl.setLayoutData(gd);

        commsCbo = new Combo(adminGroup, SWT.DROP_DOWN | SWT.READ_ONLY);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label filler4 = new Label(adminGroup, SWT.NONE);
        filler4.setLayoutData(gd);

        gd = new GridData(adminLabelWidth, SWT.DEFAULT);
        Label taskNoLbl = new Label(adminGroup, SWT.RIGHT);
        taskNoLbl.setText("Task No:");
        taskNoLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        taskNoTF = new Text(adminGroup, SWT.BORDER);
        taskNoTF.setLayoutData(gd);
        taskNoTF.setTextLimit(13);

        gd = new GridData(SWT.RIGHT, SWT.TOP, false, false);
        gd.widthHint = adminLabelWidth;
        Label sponsorLbl = new Label(adminGroup, SWT.RIGHT);
        sponsorLbl.setText("Sponsor:");
        sponsorLbl.setLayoutData(gd);

        gd = new GridData(80, 100);
        sponsorList = new List(adminGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        sponsorList.setLayoutData(gd);

        gd = new GridData(adminLabelWidth, SWT.DEFAULT);
        Label rateLbl = new Label(adminGroup, SWT.RIGHT);
        rateLbl.setText("Rate:");
        rateLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        rateTF = new Text(adminGroup, SWT.BORDER);
        rateTF.setLayoutData(gd);

        gd = new GridData(adminLabelWidth, SWT.DEFAULT);
        Label cd404Lbl = new Label(adminGroup, SWT.RIGHT);
        cd404Lbl.setText("CD-404:");
        cd404Lbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        cd404TF = new Text(adminGroup, SWT.BORDER);
        cd404TF.setLayoutData(gd);
        cd404TF.setTextLimit(4);

        gd = new GridData(SWT.RIGHT, SWT.TOP, false, false);
        gd.widthHint = adminLabelWidth;
        Label recipLbl = new Label(adminGroup, SWT.RIGHT);
        recipLbl.setText("Recip:");
        recipLbl.setLayoutData(gd);

        gd = new GridData(80, 100);
        recipList = new List(adminGroup, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        recipList.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 4;
        Label filler5 = new Label(adminGroup, SWT.NONE);
        filler5.setLayoutData(gd);

        gd = new GridData(adminLabelWidth, SWT.DEFAULT);
        Label reportLbl = new Label(adminGroup, SWT.RIGHT);
        reportLbl.setText("Report:");
        reportLbl.setLayoutData(gd);

        gd = new GridData(300, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        reportTF = new Text(adminGroup, SWT.BORDER);
        reportTF.setLayoutData(gd);
        reportTF.setTextLimit(60);
    }

    /**
     * Create the telemetry composite.
     */
    private void createTelemetryComposite() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        telemetryComp = new Composite(stackComposite, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        telemetryComp.setLayout(gl);
        telemetryComp.setLayoutData(gd);

        createTelemetryControls();
    }

    /**
     * Create the telemetry controls.
     */
    private void createTelemetryControls() {
        // -----------------------------------------
        // Create the General Group
        // -----------------------------------------
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group generalGroup = new Group(telemetryComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        generalGroup.setLayout(gl);
        generalGroup.setLayoutData(gd);
        generalGroup.setText("General");

        // Create the left composite
        Composite leftComp = new Composite(generalGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        leftComp.setLayout(gl);

        Label telemetryLbl = new Label(leftComp, SWT.NONE);
        telemetryLbl.setText("Telemetry:");

        gd = new GridData(80, 100);
        telemetryList = new List(leftComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        telemetryList.setLayoutData(gd);

        Label ownerLbl = new Label(leftComp, SWT.NONE);
        ownerLbl.setText("Owner:");

        gd = new GridData(80, 100);
        telemOwnerList = new List(leftComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        telemOwnerList.setLayoutData(gd);

        Label payorLbl = new Label(leftComp, SWT.NONE);
        payorLbl.setText("Payor:");

        gd = new GridData(80, 100);
        payorList = new List(leftComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        payorList.setLayoutData(gd);

        // Create the right composite
        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Composite rightComp = new Composite(generalGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        rightComp.setLayout(gl);
        rightComp.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        Label phoneLbl = new Label(rightComp, SWT.RIGHT);
        phoneLbl.setText("Phone:");
        phoneLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        telemetryPhoneTF = new Text(rightComp, SWT.BORDER);
        telemetryPhoneTF.setLayoutData(gd);
        telemetryPhoneTF.setTextLimit(12);

        gd = new GridData(200, SWT.DEFAULT);
        Label costLbl = new Label(rightComp, SWT.RIGHT);
        costLbl.setText("Cost:");
        costLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        telemetryCostTF = new Text(rightComp, SWT.BORDER);
        telemetryCostTF.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        Label sensorIdLbl = new Label(rightComp, SWT.RIGHT);
        sensorIdLbl.setText("Sensor Id:");
        sensorIdLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        telemetrySensorIdTF = new Text(rightComp, SWT.BORDER);
        telemetrySensorIdTF.setLayoutData(gd);
        telemetrySensorIdTF.setTextLimit(10);

        gd = new GridData(200, SWT.DEFAULT);
        Label reportFreqLbl = new Label(rightComp, SWT.RIGHT);
        reportFreqLbl.setText("Reporting Frequency:");
        reportFreqLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        telemetryReportFreqTF = new Text(rightComp, SWT.BORDER);
        telemetryReportFreqTF.setLayoutData(gd);
        telemetryReportFreqTF.setTextLimit(4);

        gd = new GridData(200, SWT.DEFAULT);
        Label obsFreqLbl = new Label(rightComp, SWT.RIGHT);
        obsFreqLbl.setText("Observation Frequency:");
        obsFreqLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        telemetryObsFreqTF = new Text(rightComp, SWT.BORDER);
        telemetryObsFreqTF.setLayoutData(gd);
        telemetryObsFreqTF.setTextLimit(4);

        // -----------------------------------------
        // Create the Criteria Group
        // -----------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group criteriaGroup = new Group(telemetryComp, SWT.NONE);
        gl = new GridLayout(2, false);
        criteriaGroup.setLayout(gl);
        criteriaGroup.setLayoutData(gd);
        criteriaGroup.setText("Criteria");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        telemetryCriteriaTF = new Text(criteriaGroup, SWT.BORDER);
        telemetryCriteriaTF.setLayoutData(gd);
        telemetryCriteriaTF.setTextLimit(50);
    }

    /**
     * Add a horizontal separator bar to the main display.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 4;
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        int numControls = 1;
        if (fullControls == true) {
            numControls = 3;
        }

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(numControls, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 90;

        if (fullControls == true) {
            gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
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
        }

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
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

        if (fullControls == true) {
            gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
            gd.widthHint = buttonWidth;
            deleteBtn = new Button(buttonComp, SWT.PUSH);
            deleteBtn.setText("Delete");
            deleteBtn.setLayoutData(gd);
            deleteBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    deleteRecord();
                }
            });
        }
    }

    /**
     * Loads static data from the database.
     */
    private void loadStaticData() {

        try {
            // GetState(" ORDER BY state ");
            loadList("state", "state", stateCbo);

            // GetDcpOwner("")
            loadList("owner", "dcpowner", ownerList);

            // GetCoopComms(" ORDER BY comm ")
            loadList("comm", "coopcomms", commsCbo);
            // GetCoopSpons(" ORDER BY spons ");
            loadList("spons", "coopspons", sponsorList);
            // GetCoopRecip(" ORDER BY recip ");
            loadList("recip", "cooprecip", recipList);

            // GetTelmOwner(" ORDER BY owner ")
            loadList("owner", "telmowner", telemOwnerList);
            // GetTelmPayor(" ORDER BY payor ")
            loadList("payor", "telmpayor", payorList);
            // GetTelmType(" ORDER BY type ")
            loadList("type", "telmtype", telemetryList);

        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to load static data: ", e);
        }

    }

    /**
     * Populates the given List or Combo with the column from the table
     * specified.
     * 
     * @param <T>
     * @param column
     *            The column from the database to return.
     * @param table
     *            The table to get the data from.
     * @param dataList
     *            The List or Combo to populate with data.
     * @throws VizException
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     * @throws InvocationTargetException
     * @throws SecurityException
     * @throws NoSuchMethodException
     */
    private <T> void loadList(String column, String table, T dataList)
            throws VizException, IllegalArgumentException,
            IllegalAccessException, InvocationTargetException,
            SecurityException, NoSuchMethodException {
        dataList.getClass().getMethod("removeAll").invoke(dataList);

        // The base select statement for the queries
        String query = String.format("SELECT %s FROM %s ORDER BY %s", column,
                table, column);

        QueryResult data = HydroDBDataManager.getInstance().runMappedQuery(
                query);

        if (data != null) {
            Method addMeth = dataList.getClass().getMethod("add", String.class);
            String rowVal;

            // Populate the list with the results from the query
            for (QueryResultRow currRow : data.getRows()) {
                rowVal = (String) currRow.getColumn(data.getColumnNames().get(
                        column));
                addMeth.invoke(dataList, rowVal);
            }

            // Select the first option
            dataList.getClass().getMethod("select", int.class)
                    .invoke(dataList, 0);
        }
    }

    // --------------------------------------------
    // Get Data
    // --------------------------------------------
    private void getDialogData() {

        getDCPData();
        getObsData();
        getTelemData();

        updateDisplay();
    }

    /**
     * Load DCP data from the manager.
     */
    private void getDCPData() {
        DcpData seedData = new DcpData();
        seedData.setLid(lid);

        try {
            java.util.List<DcpData> data = HydroDBDataManager.getInstance()
                    .getData(seedData);

            if (data.size() > 0) {
                // There will only be one record per lid
                dcpData = data.get(0);
            } else {
                dcpData = null;
            }
        } catch (VizException e) {
            statusHandler
                    .handle(Priority.ERROR, "Unable to load DCP data: ", e);
        }

        updateDCPDisplay();
    }

    /**
     * Load observation data from the manager.
     */
    private void getObsData() {
        ObserverData seedData = new ObserverData();
        seedData.setLid(lid);

        try {
            java.util.List<ObserverData> data = HydroDBDataManager
                    .getInstance().getData(seedData);

            if (data.size() > 0) {
                // There will only be one record per lid
                obsData = data.get(0);
            } else {
                obsData = null;
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to load Observation data: ", e);
        }

        updateObsDisplay();
    }

    /**
     * Load Telemetry data from the manager.
     */
    private void getTelemData() {
        TelemData seedData = new TelemData();
        seedData.setLid(lid);

        try {
            java.util.List<TelemData> data = HydroDBDataManager.getInstance()
                    .getData(seedData);

            if (data.size() > 0) {
                // There will only be one record per lid
                telemData = data.get(0);
            } else {
                telemData = null;
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to load Telemetry data: ", e);
        }

        updateTelemDisplay();
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

            updateDialogState(DialogStates.DATA_AVAILABLE);
        } else {
            updateDialogState(DialogStates.NO_DATA);
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
            dosTF.setText(HydroDataUtils.getDisplayString(
                    obsData.getDateOfService(), isoDate));

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

            updateDialogState(DialogStates.DATA_AVAILABLE);
        } else {
            updateDialogState(DialogStates.NO_DATA);
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

            updateDialogState(DialogStates.DATA_AVAILABLE);
        } else {
            updateDialogState(DialogStates.NO_DATA);
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
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Unable to Save");
            mb.setMessage("An error occurred while trying to save");
            mb.open();
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
                newData.setDateOfService(isoDate.parse(dosTF.getText()));
            } catch (ParseException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Date");
                mb.setMessage("Please enter a Service Date in the form: YYYY-MM-DD");
                mb.open();

                return;
            }
        } else {
            newData.setDateOfService((Date) null);
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
        Double tmp = HydroDataUtils.getDoubleFromTF(shell, rateTF, "Rate");
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
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Unable to Save");
            mb.setMessage("An error occurred while trying to save");
            mb.open();
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
        Double cost = HydroDataUtils.getDoubleFromTF(shell, telemetryCostTF,
                "Cost");
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
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Unable to Save");
            mb.setMessage("An error occurred while trying to save");
            mb.open();
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
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete this entry?");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    HydroDBDataManager.getInstance().deleteRecord(currData);

                    // Synchronize StnClass table
                    StnClassSyncUtil.setStnClass(lid);
                } catch (VizException e) {
                    mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Unable to Delete");
                    mb.setMessage("An error occurred while trying to delete");
                    mb.open();
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
     * @param currState
     */
    private void updateDialogState(DialogStates currState) {
        if (fullControls == true) {
            switch (currState) {
            case NO_DATA:
                deleteBtn.setEnabled(false);

                break;
            case DATA_AVAILABLE:
                deleteBtn.setEnabled(true);

                break;
            default:
                break;
            }
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
        for (int i = 0; i < dataList.getItemCount(); i++) {
            if (dataList.getItem(i).equalsIgnoreCase(optionSet)) {
                dataList.select(i);
                break;
            }
        }
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
        for (int i = 0; i < dataList.getItemCount(); i++) {
            if (dataList.getItem(i).equalsIgnoreCase(optionSet)) {
                dataList.select(i);
                break;
            }
        }
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
        // If the Checkbox is checked, set the Date to the
        // current simulated time.
        // Else load the date from the database
        if (dosChk.getSelection()) {
            Date now = SimulatedTime.getSystemTime().getTime();
            dosTF.setText(isoDate.format(now));
        } else if (obsData != null) {
            dosTF.setText((obsData.getDateOfService() != null) ? isoDate
                    .format(obsData.getDateOfService()) : "");
        } else {
            dosTF.setText("");
        }
    }
}
