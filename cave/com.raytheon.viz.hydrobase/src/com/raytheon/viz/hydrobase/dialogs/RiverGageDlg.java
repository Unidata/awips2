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
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
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
import com.raytheon.viz.hydrobase.FcstPointGroupDlg;
import com.raytheon.viz.hydrobase.listeners.IForecastGroupAssignmentListener;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.RPFFcstGroupData;
import com.raytheon.viz.hydrocommon.data.RPFFcstPointData;
import com.raytheon.viz.hydrocommon.data.RiverStatData;
import com.raytheon.viz.hydrocommon.datamanager.DataTrashCanDataManager;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.hydrocommon.util.StnClassSyncUtil;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the River Gage dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 8, 2008				lvenable	Initial creation
 * Jan 6, 2008  1802        askripsk    Connect to DB.
 * Mar 29,2012  14463       wkwock      Fix max # of char for remark text box to 255
 *                                      Also see https://bugs.eclipse.org/bugs/show_bug.cgi?id=43004
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class RiverGageDlg extends CaveSWTDialog implements
        IForecastGroupAssignmentListener {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Stack layout composite.
     */
    private Composite stackLayoutComp;

    /**
     * Stack layout.
     */
    private StackLayout stackLayout;

    /**
     * Geophysical composite.
     */
    private Composite geophysicalComp;

    /**
     * Information group container.
     */
    private Group infoGroup;

    /**
     * Geophysical additional information combo box.
     */
    private Combo geoAddInfoCbo;

    /**
     * Stream text control.
     */
    private Text streamTF;

    /**
     * Latitude text control.
     */
    private Text latitudeTF;

    /**
     * Longitude text control.
     */
    private Text longitudeTF;

    /**
     * Drainage area text control.
     */
    private Text drainageAreaTF;

    /**
     * River mile text control.
     */
    private Text riverMileTF;

    /**
     * Flood stage text control.
     */
    private Text floodStageTF;

    /**
     * Flood stage flow text control.
     */
    private Text floodStageFlowTF;

    /**
     * Action stage text control.
     */
    private Text actionStageTF;

    /**
     * Action stage flow text control.
     */
    private Text actionStageFlowTF;

    /**
     * Zero Datum text control.
     */
    private Text zeroDatumTF;

    /**
     * Threshold run-off text control.
     */
    private Text thresholdRunoffTF;

    /**
     * Revise check box.
     */
    private Button reviseChk;

    /**
     * Date text control.
     */
    private Text dateTF;

    /**
     * Forecast point text control.
     */
    private Text forecastPointTF;

    /**
     * Stage flow list control.
     */
    private List stageFlowList;

    /**
     * Latest forecast check box.
     */
    private Button latestForecastChk;

    /**
     * Remarks text control.
     */
    private Text remarksTF;

    /**
     * Tidal effect combo box.
     */
    private Combo tidalEffectCbo;

    /**
     * Back water combo box.
     */
    private Combo backWaterCbo;

    /**
     * Period record text control.
     */
    private Text periodRecordTF;

    /**
     * Latitude/Longitude text control.
     */
    private Text latLonSourceTF;

    /**
     * Level text control.
     */
    private Text levelTF;

    /**
     * Vertical Datum text control.
     */
    private Text verticalDatumTF;

    /**
     * Rated text control.
     */
    private Text ratedTF;

    /**
     * Date rating text control.
     */
    private Text dateRatingTF;

    /**
     * USGS rating text control.
     */
    private Text usgsRatingTF;

    /**
     * USGS number text control.
     */
    private Text usgsNoTF;

    /**
     * Bank-full text control.
     */
    private Text bankfullTF;

    /**
     * Check bar text control.
     */
    private Text checkBarTF;

    /**
     * Pool text control.
     */
    private Text poolTF;

    /**
     * Delete button
     */
    private Button deleteBtn;

    /**
     * Forecast Group ID for the lid
     */
    private String fcstGroup;

    /**
     * Location for gage
     */
    private String lid;

    /**
     * Format used for dates
     */
    private SimpleDateFormat dateFormat;

    /**
     * Data for the current river gage
     */
    private RiverStatData riverGageData;

    /**
     * Value for no Forecast Group assignment
     */
    private final String NO_FCST_GROUP_SELECTED = "(Not a Forecast Point)";

    /**
     * States for the dialog.
     */
    private enum DialogState {
        NO_DATA_AVAILABLE, DATA_AVAILABLE
    }

    /**
     * text from the remark text box
     */
    private String currentRemarkText=null;
    
    /**
     * maximum number of character allowed in the remark text box
     */
    private final int MAX_REMARK_CHAR=255;
    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public RiverGageDlg(Shell parent, String titleInfo, String lid) {
        super(parent);
        setText("River Gage" + titleInfo);

        this.lid = lid;
        dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
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

        // Initialize all of the controls and layouts
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createTopControls();

        // Horizontal separator line
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        stackLayoutComp = new Composite(shell, SWT.NONE);
        stackLayout = new StackLayout();
        stackLayoutComp.setLayout(stackLayout);

        createGeophysicalComp(stackLayoutComp);
        createAdditionInfoGroup(stackLayoutComp);

        stackLayout.topControl = geophysicalComp;
        stackLayoutComp.layout();

        createBottomButtons();

        loadStaticData();

        getDialogData();
    }

    /**
     * Create the control at the top pf the dialog.
     */
    private void createTopControls() {
        Composite topComp = new Composite(shell, SWT.NONE);
        topComp.setLayout(new GridLayout(2, false));
        topComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label pageLbl = new Label(topComp, SWT.NONE);
        pageLbl.setText("Page ");

        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, true, false);
        geoAddInfoCbo = new Combo(topComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        geoAddInfoCbo.add("Geophysical");
        geoAddInfoCbo.add("Additional Info");
        geoAddInfoCbo.select(0);
        geoAddInfoCbo.setLayoutData(gd);
        geoAddInfoCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (geoAddInfoCbo.getSelectionIndex() == 0) {
                    stackLayout.topControl = geophysicalComp;
                    stackLayoutComp.layout();
                } else {
                    stackLayout.topControl = infoGroup;
                    stackLayoutComp.layout();
                }
            }
        });
    }

    /**
     * Create the geophysical composite.
     * 
     * @param stackLayoutComp
     *            Stack layout composite.
     */
    private void createGeophysicalComp(Composite stackLayoutComp) {
        geophysicalComp = new Composite(stackLayoutComp, SWT.NONE);
        geophysicalComp.setLayout(new GridLayout(1, false));
        geophysicalComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        createGeographicGroup(geophysicalComp);
        createRemarksGroup(geophysicalComp);
    }

    /**
     * Create the geographic group and controls.
     * 
     * @param parentComp
     */
    private void createGeographicGroup(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group geographicGroup = new Group(parentComp, SWT.NONE);
        geographicGroup.setLayout(new GridLayout(2, false));
        geographicGroup.setLayoutData(gd);
        geographicGroup.setText(" Geographic/Physical ");

        // ------------------------------------------
        // Create the left side composite of the
        // Geographic/Physical group.
        // ------------------------------------------
        Composite leftComp = new Composite(geographicGroup, SWT.NONE);
        leftComp.setLayout(new GridLayout(4, false));

        // Stream
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label streamLbl = new Label(leftComp, SWT.RIGHT);
        streamLbl.setText("Stream:");
        streamLbl.setLayoutData(gd);

        gd = new GridData(250, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        streamTF = new Text(leftComp, SWT.BORDER);
        streamTF.setLayoutData(gd);
        streamTF.setTextLimit(32);

        // Latitude/Longitude
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label latLonLbl = new Label(leftComp, SWT.RIGHT);
        latLonLbl.setText("Lat/Lon:");
        latLonLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        latitudeTF = new Text(leftComp, SWT.BORDER);
        latitudeTF.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        longitudeTF = new Text(leftComp, SWT.BORDER);
        longitudeTF.setLayoutData(gd);

        // Drainage Area
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label drainageLbl = new Label(leftComp, SWT.RIGHT);
        drainageLbl.setText("Drainage Area:");
        drainageLbl.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        drainageAreaTF = new Text(leftComp, SWT.BORDER);
        drainageAreaTF.setLayoutData(gd);

        // River Mile
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label riverMileLbl = new Label(leftComp, SWT.RIGHT);
        riverMileLbl.setText("River Mile:");
        riverMileLbl.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        riverMileTF = new Text(leftComp, SWT.BORDER);
        riverMileTF.setLayoutData(gd);

        // Flood Stage
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label floodStageLbl = new Label(leftComp, SWT.RIGHT);
        floodStageLbl.setText("Flood Stage:");
        floodStageLbl.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        floodStageTF = new Text(leftComp, SWT.BORDER);
        floodStageTF.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        Label floodStageFlowLbl = new Label(leftComp, SWT.RIGHT);
        floodStageFlowLbl.setText("Flow:");
        floodStageFlowLbl.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        floodStageFlowTF = new Text(leftComp, SWT.BORDER);
        floodStageFlowTF.setLayoutData(gd);

        // Action Stage
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label actionStageLbl = new Label(leftComp, SWT.RIGHT);
        actionStageLbl.setText("Action Stage:");
        actionStageLbl.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        actionStageTF = new Text(leftComp, SWT.BORDER);
        actionStageTF.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        Label actionStageFlowLbl = new Label(leftComp, SWT.RIGHT);
        actionStageFlowLbl.setText("Flow:");
        actionStageFlowLbl.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        actionStageFlowTF = new Text(leftComp, SWT.BORDER);
        actionStageFlowTF.setLayoutData(gd);

        // Zero Datum
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label zeroDatumLbl = new Label(leftComp, SWT.RIGHT);
        zeroDatumLbl.setText("Zero Datum:");
        zeroDatumLbl.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        zeroDatumTF = new Text(leftComp, SWT.BORDER);
        zeroDatumTF.setLayoutData(gd);

        // Threshold Runoff
        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        Label threasholdRunoffLbl = new Label(leftComp, SWT.RIGHT);
        threasholdRunoffLbl.setText("Threshold Runoff:");
        threasholdRunoffLbl.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        thresholdRunoffTF = new Text(leftComp, SWT.BORDER);
        thresholdRunoffTF.setLayoutData(gd);

        // ------------------------------------------
        // Create the right side composite of the
        // Geographic/Physical group.
        // ------------------------------------------
        Composite rightComp = new Composite(geographicGroup, SWT.NONE);
        rightComp.setLayout(new GridLayout(3, false));

        reviseChk = new Button(rightComp, SWT.CHECK);
        reviseChk.setText("Revise");
        reviseChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateRevisionDate();
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Label dateLbl = new Label(rightComp, SWT.RIGHT);
        dateLbl.setText("Date:");
        dateLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        dateTF = new Text(rightComp, SWT.BORDER);
        dateTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Button forecastPointBtn = new Button(rightComp, SWT.PUSH);
        forecastPointBtn.setText("Forecast Point\nGroup Assignment");
        forecastPointBtn.setLayoutData(gd);
        forecastPointBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openFcstGroupAssignmentDlg();
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        forecastPointTF = new Text(rightComp, SWT.BORDER);
        forecastPointTF.setLayoutData(gd);
        forecastPointTF.setEditable(false);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 3;
        Label listlbl = new Label(rightComp, SWT.CENTER);
        listlbl.setText("Primary Stage/Flow Physical Element");
        listlbl.setLayoutData(gd);

        gd = new GridData(300, 100);
        gd.horizontalSpan = 3;
        stageFlowList = new List(rightComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        stageFlowList.setLayoutData(gd);
        stageFlowList.setFont(controlFont);

        gd = new GridData();
        gd.horizontalSpan = 3;
        latestForecastChk = new Button(rightComp, SWT.CHECK);
        latestForecastChk
                .setText("Use Latest Forecast When Computing\nMaximum Forecast Value");
        latestForecastChk.setLayoutData(gd);
    }

    /**
     * Create the remarks group and text control.
     * 
     * @param parentComp
     */
    private void createRemarksGroup(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group remarksGroup = new Group(parentComp, SWT.NONE);
        remarksGroup.setLayout(new GridLayout(1, false));
        remarksGroup.setLayoutData(gd);
        remarksGroup.setText(" Remarks ");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 100;
        gd.widthHint = 150;
        remarksTF = new Text(remarksGroup, SWT.BORDER | SWT.MULTI | SWT.WRAP);
        remarksTF.setLayoutData(gd);
        remarksTF.setFont(controlFont);
        remarksTF.setTextLimit(MAX_REMARK_CHAR);
        
        /*Note: use this method to control number of character in remarkTF
         * because a bug in the Text class. 
         * See https://bugs.eclipse.org/bugs/show_bug.cgi?id=43004*/
        currentRemarkText=remarksTF.getText();
        ModifyListener listener = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (remarksTF.getText().length()>MAX_REMARK_CHAR){
        			remarksTF.setText(currentRemarkText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentRemarkText=remarksTF.getText();
        	}
        };

        remarksTF.addModifyListener(listener);
    }

    /**
     * Create the additional information group and controls.
     * 
     * @param stackLayoutComp
     *            Stack layout composite.
     */
    private void createAdditionInfoGroup(Composite stackLayoutComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        infoGroup = new Group(stackLayoutComp, SWT.NONE);
        infoGroup.setLayout(new GridLayout(4, false));
        infoGroup.setLayoutData(gd);
        infoGroup.setText(" Information ");

        // Period of Record
        gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
        Label periodRecordLbl = new Label(infoGroup, SWT.RIGHT);
        periodRecordLbl.setText("Period of Record:");
        periodRecordLbl.setLayoutData(gd);

        gd = new GridData(250, SWT.DEFAULT);
        periodRecordTF = new Text(infoGroup, SWT.BORDER);
        periodRecordTF.setLayoutData(gd);
        periodRecordTF.setTextLimit(30);

        // USGS No.
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label usgsNoLbl = new Label(infoGroup, SWT.RIGHT);
        usgsNoLbl.setText("USGS No:");
        usgsNoLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        usgsNoTF = new Text(infoGroup, SWT.BORDER);
        usgsNoTF.setLayoutData(gd);
        usgsNoTF.setTextLimit(10);

        // Lat/Lon Source
        gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
        Label latLonLbl = new Label(infoGroup, SWT.RIGHT);
        latLonLbl.setText("Lat/Lon Source:");
        latLonLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        latLonSourceTF = new Text(infoGroup, SWT.BORDER);
        latLonSourceTF.setLayoutData(gd);
        latLonSourceTF.setTextLimit(20);

        // Bankfull
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label bankfullLbl = new Label(infoGroup, SWT.RIGHT);
        bankfullLbl.setText("Bankfull:");
        bankfullLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        bankfullTF = new Text(infoGroup, SWT.BORDER);
        bankfullTF.setLayoutData(gd);

        // Level
        gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
        Label levelLbl = new Label(infoGroup, SWT.RIGHT);
        levelLbl.setText("Level:");
        levelLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        levelTF = new Text(infoGroup, SWT.BORDER);
        levelTF.setLayoutData(gd);
        levelTF.setTextLimit(20);

        // Check Bar
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label checkBarLbl = new Label(infoGroup, SWT.RIGHT);
        checkBarLbl.setText("Check Bar:");
        checkBarLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        checkBarTF = new Text(infoGroup, SWT.BORDER);
        checkBarTF.setLayoutData(gd);

        // Vertical Datum
        gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
        Label verticalDatumLbl = new Label(infoGroup, SWT.RIGHT);
        verticalDatumLbl.setText("Vertical Datum:");
        verticalDatumLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        verticalDatumTF = new Text(infoGroup, SWT.BORDER);
        verticalDatumTF.setLayoutData(gd);
        verticalDatumTF.setTextLimit(20);

        // Pool
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        Label poolLbl = new Label(infoGroup, SWT.RIGHT);
        poolLbl.setText("Pool:");
        poolLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        poolTF = new Text(infoGroup, SWT.BORDER);
        poolTF.setLayoutData(gd);

        // Rated
        gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
        Label ratedLbl = new Label(infoGroup, SWT.RIGHT);
        ratedLbl.setText("Rated:");
        ratedLbl.setLayoutData(gd);

        gd = new GridData(250, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        ratedTF = new Text(infoGroup, SWT.BORDER);
        ratedTF.setLayoutData(gd);
        ratedTF.setTextLimit(20);

        // Date of Rating
        gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
        Label dateOfRatingLbl = new Label(infoGroup, SWT.RIGHT);
        dateOfRatingLbl.setText("Date of Rating:");
        dateOfRatingLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        dateRatingTF = new Text(infoGroup, SWT.BORDER);
        dateRatingTF.setLayoutData(gd);

        // USGS Rating
        gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
        Label usgsRatingLbl = new Label(infoGroup, SWT.RIGHT);
        usgsRatingLbl.setText("USGS Rating No.:");
        usgsRatingLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        gd.horizontalSpan = 3;
        usgsRatingTF = new Text(infoGroup, SWT.BORDER);
        usgsRatingTF.setLayoutData(gd);
        usgsRatingTF.setTextLimit(5);

        // Tidal Effect
        gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
        Label tidalEffectLbl = new Label(infoGroup, SWT.RIGHT);
        tidalEffectLbl.setText("Tidal Effect:");
        tidalEffectLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 3;
        tidalEffectCbo = new Combo(infoGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        tidalEffectCbo.add("None");
        tidalEffectCbo.add("Slight");
        tidalEffectCbo.add("Moderate");
        tidalEffectCbo.add("Major");
        tidalEffectCbo.select(0);
        tidalEffectCbo.setLayoutData(gd);

        // Backwater
        gd = new GridData(SWT.FILL, SWT.CENTER, false, false);
        Label backwaterLbl = new Label(infoGroup, SWT.RIGHT);
        backwaterLbl.setText("Backwater:");
        backwaterLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 3;
        backWaterCbo = new Combo(infoGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        backWaterCbo.add("None");
        backWaterCbo.add("Minor");
        backWaterCbo.add("Moderate");
        backWaterCbo.add("Major");
        backWaterCbo.select(0);
        backWaterCbo.setLayoutData(gd);
    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(4, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 90;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (saveRecord()) {
                    shell.dispose();
                }
            }
        });

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

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setEnabled(false);
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
                shell.dispose();
            }
        });
    }

    private void loadStaticData() {
        // Load Physical Element Lists
        stageFlowList.removeAll();
        try {
            for (String currPE : DataTrashCanDataManager.getInstance()
                    .getPEList()) {
                stageFlowList.add(currPE);
            }
            stageFlowList.select(0);
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    private void getDialogData() {
        RiverStatData seedData = new RiverStatData();
        seedData.setLid(lid);

        ArrayList<RiverStatData> data = null;
        try {
            data = HydroDBDataManager.getInstance().getData(seedData);

            RPFFcstPointData seedDataGroup = new RPFFcstPointData();
            seedDataGroup.setLid(lid);

            ArrayList<RPFFcstPointData> dataGroup = HydroDBDataManager
                    .getInstance().getData(seedDataGroup);

            if (dataGroup.size() > 0) {
                // They should be only one record per lid
                fcstGroup = dataGroup.get(0).getGroupID();
            } else {
                fcstGroup = NO_FCST_GROUP_SELECTED;
            }
        } catch (VizException e) {
            e.printStackTrace();
        }

        if ((data != null) && (data.size() > 0)) {
            // There will only be one record per lid
            riverGageData = data.get(0);

            updateDialogState(DialogState.DATA_AVAILABLE);
        } else {
            riverGageData = null;
            updateDialogState(DialogState.NO_DATA_AVAILABLE);
        }

        updateDisplay();
    }

    private void updateDisplay() {
        if (riverGageData != null) {
            // Stream
            streamTF.setText(riverGageData.getStream());

            // Lat/Lon
            latitudeTF
                    .setText((riverGageData.getLatitude() != HydroConstants.MISSING_VALUE) ? GeoUtil
                            .getInstance().cvt_latlon_from_double(
                                    riverGageData.getLatitude())
                            : "");
            longitudeTF
                    .setText((riverGageData.getLongitude() != HydroConstants.MISSING_VALUE) ? GeoUtil
                            .getInstance().cvt_latlon_from_double(
                                    riverGageData.getLongitude())
                            : "");

            // Drainage Area
            drainageAreaTF.setText(HydroDataUtils
                    .getDisplayString(riverGageData.getDrainageArea()));

            // River Mile
            riverMileTF.setText(HydroDataUtils.getDisplayString(riverGageData
                    .getRiverMile()));

            // Flood Stage/Flow
            floodStageTF.setText(HydroDataUtils.getDisplayString(riverGageData
                    .getFloodStage()));
            floodStageFlowTF.setText(HydroDataUtils
                    .getDisplayString(riverGageData.getFloodFlow()));

            // Action Stage/Flow
            actionStageTF.setText(HydroDataUtils.getDisplayString(riverGageData
                    .getActionStage()));
            actionStageFlowTF.setText(HydroDataUtils
                    .getDisplayString(riverGageData.getActionFlow()));

            // Zero Datum
            zeroDatumTF.setText(HydroDataUtils.getDisplayString(riverGageData
                    .getZeroDatum()));

            // Threshold Runoff
            thresholdRunoffTF.setText(HydroDataUtils
                    .getDisplayString(riverGageData.getThresholdRunoff()));

            // PE
            String currPE = riverGageData.getPrimaryPE();
            for (int i = 0; i < stageFlowList.getItemCount(); i++) {
                if (stageFlowList.getItem(i).startsWith(currPE)) {
                    stageFlowList.setSelection(i);
                    break;
                }
            }

            // Date
            Date reviseDate = riverGageData.getReviseDate();
            dateTF.setText((reviseDate != null) ? dateFormat.format(reviseDate)
                    : "");

            // Forecast Point Group

            // Remarks
            remarksTF.setText(riverGageData.getRemark());

            // Use Latest Forecast
            latestForecastChk.setSelection(riverGageData.getUseLatestForecast()
                    .equals("T"));

            // Period of Record
            periodRecordTF.setText(riverGageData.getPeriodOfRecord());

            // lat/lon source
            latLonSourceTF.setText(riverGageData.getLatLonSource());

            // level
            levelTF.setText(riverGageData.getLevel());

            // vert. datum
            verticalDatumTF.setText(riverGageData.getVerticalDatum());

            // Rated
            ratedTF.setText(riverGageData.getRated());

            // Date of Rating
            Date rating = riverGageData.getDateOfRating();
            dateRatingTF.setText((rating != null) ? dateFormat.format(rating)
                    : "");

            // USGS rating number
            usgsRatingTF.setText(riverGageData.getUsgsRateNumber());

            // Tidal Effect
            String tidal = riverGageData.getTidalEffect();
            for (int i = 0; i < tidalEffectCbo.getItemCount(); i++) {
                if (tidalEffectCbo.getItem(i).equalsIgnoreCase(tidal)) {
                    tidalEffectCbo.select(i);
                    break;
                }
            }

            // BackWater
            String backWater = riverGageData.getBackWater();
            for (int i = 0; i < backWaterCbo.getItemCount(); i++) {
                if (backWaterCbo.getItem(i).equalsIgnoreCase(backWater)) {
                    backWaterCbo.select(i);
                    break;
                }
            }

            // USGS number
            usgsNoTF.setText(riverGageData.getGageNumber());

            // Bankfull
            bankfullTF.setText(HydroDataUtils.getDisplayString(riverGageData
                    .getBankFull()));

            // Check bar
            checkBarTF.setText(HydroDataUtils.getDisplayString(riverGageData
                    .getCheckBar()));

            // Pool
            poolTF.setText(HydroDataUtils.getDisplayString(riverGageData
                    .getPool()));

            // Fcst Group
            forecastPointTF.setText(fcstGroup);
        }
    }

    /**
     * Saves the current River Gage to the database.
     * 
     * @return True if the save was successful, otherwise False
     */
    private boolean saveRecord() {
        boolean successful = false;

        RiverStatData newData = new RiverStatData();
        Double dTemp = null;

        // Lid
        newData.setLid(lid);

        // PE
        if (stageFlowList.getSelectionIndex() >= 0) {
            String currPE = stageFlowList.getItem(
                    stageFlowList.getSelectionIndex()).split(" ")[0];
            newData.setPrimaryPE(currPE);
        } else {
            newData.setPrimaryPE("");
        }

        // BankFull
        dTemp = HydroDataUtils.getDoubleFromTF(shell, bankfullTF, "Bankfull");
        if (dTemp == null) {
            return successful;
        }
        newData.setBankFull(dTemp);

        // Check Bar
        dTemp = HydroDataUtils.getDoubleFromTF(shell, checkBarTF, "Check Bar");
        if (dTemp == null) {
            return successful;
        }
        newData.setCheckBar(dTemp);

        // Drainage Area
        dTemp = HydroDataUtils.getDoubleFromTF(shell, drainageAreaTF,
                "Drainage Area");
        if (dTemp == null) {
            return successful;
        }
        newData.setDrainageArea(dTemp);

        // Threshold Runoff
        dTemp = HydroDataUtils.getDoubleFromTF(shell, thresholdRunoffTF,
                "Threshold Runoff");
        if (dTemp == null) {
            return successful;
        }
        newData.setThresholdRunoff(dTemp);

        // Flood Flow/Stage
        dTemp = HydroDataUtils.getDoubleFromTF(shell, floodStageFlowTF,
                "Flood Flow");
        if (dTemp == null) {
            return successful;
        }
        newData.setFloodFlow(dTemp);

        dTemp = HydroDataUtils.getDoubleFromTF(shell, floodStageTF,
                "Flood Stage");
        if (dTemp == null) {
            return successful;
        }
        newData.setFloodStage(dTemp);

        // Gage Number
        newData.setGageNumber(usgsNoTF.getText());

        // Level
        newData.setLevel(levelTF.getText());

        // River Mile
        dTemp = HydroDataUtils
                .getDoubleFromTF(shell, riverMileTF, "River Mile");
        if (dTemp == null) {
            return successful;
        }
        newData.setRiverMile(dTemp);

        // Pool
        dTemp = HydroDataUtils.getDoubleFromTF(shell, poolTF, "Pool");
        if (dTemp == null) {
            return successful;
        }
        newData.setPool(dTemp);

        // Period of Record
        newData.setPeriodOfRecord(periodRecordTF.getText());

        // Rated
        newData.setRated(ratedTF.getText());

        // Latitude
        String latTxt = latitudeTF.getText();
        double lat = HydroConstants.MISSING_VALUE;
        if (!latTxt.equals("")) {
            boolean invalidLat = false;

            try {
                lat = GeoUtil.getInstance().cvt_spaced_format(latTxt, 0);
            } catch (Exception e) {
                invalidLat = true;
            }

            if ((lat < -90) || (lat > 90) || invalidLat) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Value");
                mb
                        .setMessage("Please enter a VALID (-90 to 90) Latitude\nin the form: DD MM SS");
                mb.open();

                return successful;
            }
        }
        newData.setLatitude(lat);

        // Longitude
        String lonTxt = longitudeTF.getText();
        double lon = HydroConstants.MISSING_VALUE;
        if (!lonTxt.equals("")) {
            boolean invalidLon = false;

            try {
                lon = GeoUtil.getInstance().cvt_spaced_format(lonTxt, 0);
            } catch (Exception e) {
                invalidLon = true;
                e.printStackTrace();
            }

            if ((lon > 180) || (lon < -180) || invalidLon) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Value");
                mb
                        .setMessage("Please enter a VALID (-180 to 180) Longitude\nin the form: DD MM SS");
                mb.open();

                return successful;
            }
        }
        newData.setLongitude(lon);

        // Remarks
        newData.setRemark(remarksTF.getText());

        // Revised Data
        if (!dateTF.getText().equals("")) {
            try {
                newData.setReviseDate(dateFormat.parse(dateTF.getText()));
            } catch (ParseException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Value");
                mb.setMessage("Please enter a Date\nin the form: YYYY-MM-DD");
                mb.open();

                e.printStackTrace();

                return successful;
            }
        } else {
            newData.setReviseDate((Date) null);
        }

        // Lat/Lon Source
        newData.setLatLonSource(latLonSourceTF.getText());

        // Stream
        newData.setStream(streamTF.getText());

        // Tidal Effect
        newData.setTidalEffect(tidalEffectCbo.getItem(tidalEffectCbo
                .getSelectionIndex()));

        // BackWater
        newData.setBackWater(backWaterCbo.getItem(backWaterCbo
                .getSelectionIndex()));

        // V. Datum
        newData.setVerticalDatum(verticalDatumTF.getText());

        // Action Flow/Stage
        dTemp = HydroDataUtils.getDoubleFromTF(shell, actionStageFlowTF,
                "Action Flow");
        if (dTemp == null) {
            return successful;
        }
        newData.setActionFlow(dTemp);

        dTemp = HydroDataUtils.getDoubleFromTF(shell, actionStageTF,
                "Action Stage");
        if (dTemp == null) {
            return successful;
        }
        newData.setActionStage(dTemp);

        // Zero Datum
        dTemp = HydroDataUtils
                .getDoubleFromTF(shell, zeroDatumTF, "Zero Datum");
        if (dTemp == null) {
            return successful;
        }
        newData.setZeroDatum(dTemp);

        // Rating Date
        if (!dateRatingTF.getText().equals("")) {
            try {
                newData.setDateOfRating(dateFormat
                        .parse(dateRatingTF.getText()));
            } catch (ParseException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Value");
                mb
                        .setMessage("Please enter a Date of Rating\nin the form: YYYY-MM-DD");
                mb.open();

                e.printStackTrace();

                return successful;
            }
        } else {
            newData.setDateOfRating((Date) null);
        }

        // USGS Rating Number
        newData.setUsgsRateNumber(usgsRatingTF.getText());

        // Use Latest Forecast
        newData.setUseLatestForecast(latestForecastChk.getSelection() ? "T"
                : "F");

        // Response Time/UHD - not used in dialog
        if (riverGageData != null) {
            newData.setResponseTime(riverGageData.getResponseTime());
            newData.setUnitHydrographDuration(riverGageData
                    .getUnitHydrographDuration());
        } else {
            newData.setResponseTime(Double
                    .valueOf(HydroConstants.MISSING_VALUE));
            newData.setUnitHydrographDuration(HydroConstants.MISSING_VALUE);
        }

        // Save the River Gage
        try {
            HydroDBDataManager.getInstance().putData(newData);

            // Save the Fcst Group
            saveFcstGroup();

            successful = true;
        } catch (VizException e) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Unable to Save");
            mb
                    .setMessage("An error occurred while trying to save the River Gage");
            mb.open();

            e.printStackTrace();
        }

        // Refresh the data
        getDialogData();

        return successful;
    }

    /**
     * Saves the Fcst Group for the station.
     * 
     * @throws VizException
     */
    private void saveFcstGroup() throws VizException {
        RPFFcstPointData currPt;
        // if Fcst Group set, update DB record, else delete it
        if (!fcstGroup.equals(NO_FCST_GROUP_SELECTED)) {
            // Get the existing fcst pt, if it exists
            currPt = getFcstPoint();

            if (currPt == null) {
                // Create a new pt if one doesn't exist
                currPt = new RPFFcstPointData();
                currPt.setLid(lid);
                currPt.setRecordType("PE");
                currPt.setPrimaryBackup("XXX");
                currPt.setSecondaryBackup("XXX");
                currPt.setChangeThreshold(0.5);
                currPt.setOrdinal(1);
                currPt.setBackHours(24);
                currPt.setForwardHours(240);
                currPt.setAdjustEndHours(6);
            }

            currPt.setGroupID(fcstGroup);

            // Update the DB
            HydroDBDataManager.getInstance().putData(currPt);
        } else {
            // delete the record, if it exists
            currPt = getFcstPoint();
            if (currPt != null) {
                HydroDBDataManager.getInstance().deleteRecord(currPt);
            }
        }

        // Synchronize StnClass table
        StnClassSyncUtil.setStnClass(lid);
    }

    /**
     * Retrieves the Fcst Point for the location from the database.
     * 
     * @return NULL if the station is not a forecast point.
     * @throws VizException
     */
    private RPFFcstPointData getFcstPoint() throws VizException {
        RPFFcstPointData rval = null;

        RPFFcstPointData seedData = new RPFFcstPointData();
        seedData.setLid(lid);

        ArrayList<RPFFcstPointData> data = HydroDBDataManager.getInstance()
                .getData(seedData);

        if (data.size() > 0) {
            // Should be only one record per lid
            rval = data.get(0);
        }

        return rval;
    }

    /**
     * Deletes the River Gage from the database.
     */
    private void deleteRecord() {
        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Delete Confirmation");
        mb.setMessage("Do you wish to delete this entry?");

        int result = mb.open();

        if (result == SWT.OK) {
            try {
                String deleteFunction = String.format("delete_riverstat('%s')",
                        lid);

                HydroDBDataManager.getInstance().execFunction(deleteFunction);

                // Synchronize StnClass table
                StnClassSyncUtil.setStnClass(lid);
            } catch (VizException e) {
                mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Delete");
                mb
                        .setMessage("An error occurred while trying to delete the River Gage");
                mb.open();

                e.printStackTrace();
            }
        }
    }

    /**
     * Updates the state of the dialog.
     * 
     * @param currState
     */
    private void updateDialogState(DialogState currState) {
        switch (currState) {
        case DATA_AVAILABLE:
            deleteBtn.setEnabled(true);
            break;
        case NO_DATA_AVAILABLE:
            deleteBtn.setEnabled(false);
            break;
        default:
            break;
        }
    }

    @Override
    public void notifyUpdate(RPFFcstGroupData selectedForecastGroup) {
        if (selectedForecastGroup != null) {
            forecastPointTF.setText(selectedForecastGroup.getGroupID());
            fcstGroup = selectedForecastGroup.getGroupID();
        } else {
            forecastPointTF.setText(NO_FCST_GROUP_SELECTED);
            fcstGroup = NO_FCST_GROUP_SELECTED;
        }
    }

    /**
     * Opens the Forecast Point Group Assignment Dialog
     */
    private void openFcstGroupAssignmentDlg() {
        FcstPointGroupDlg fcstPointDlg = new FcstPointGroupDlg(shell,
                forecastPointTF.getText());
        fcstPointDlg.addListener(this);

        // Open the Fcst Assignment dlg
        fcstPointDlg.open();

        fcstPointDlg.removeListener(this);
    }

    /**
     * Handles the changes in the Revision Date
     */
    private void updateRevisionDate() {
        // If the Revision Checkbox is checked, set the Revision Date to the
        // current date
        // Else load the date from the database
        Date now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        if (reviseChk.getSelection()) {
            dateTF.setText(dateFormat.format(now));
        } else if (riverGageData != null) {
            dateTF.setText((riverGageData.getReviseDate() != null) ? dateFormat
                    .format(riverGageData.getReviseDate()) : "");
        } else {
            dateTF.setText("");
        }
    }
}
