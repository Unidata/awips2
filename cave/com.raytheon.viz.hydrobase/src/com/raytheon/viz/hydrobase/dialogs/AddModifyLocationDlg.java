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
import java.util.Locale;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrobase.listeners.ICountyStateListener;
import com.raytheon.viz.hydrobase.listeners.IStationListener;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.AdministrationData;
import com.raytheon.viz.hydrocommon.data.CountiesData;
import com.raytheon.viz.hydrocommon.data.LocationData;
import com.raytheon.viz.hydrocommon.data.StationClassData;
import com.raytheon.viz.hydrocommon.datamanager.AddModifyLocationDataManager;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.StnClassSyncUtil;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

/**
 * This class displays the Add/Modify location dialog. A flag is passed into the
 * constructor to determine if the dialog will be an Add or Modify dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#       Engineer   Description
 * ------------- ------------- ---------- --------------------------------------
 * Sep 02, 2008                lvenable   Initial creation.
 * Sep 09, 2009  2769          mpduff     Added event handling for add/delete
 *                                        events.
 * Feb 22, 2010  1985          mpduff     Made combo boxes scroll selection into
 *                                        view.
 * Apr 09, 2010  1870          mpduff     Upon adding a new site the setStnClass
 *                                        util is called and the stnclass table
 *                                        is updated.
 * Oct 27, 2010  5519          Judy Wang  Converted lower case to upper case in
 *                                        Location box.
 * May 10, 2011  9309          djingtao   the elevation fields should be
 *                                        defaults as 0.0 when user wipe out the
 *                                        field (e.g. blank)
 * Nov 26, 2012  15440         lbousaidi  display lat/lon in the GUI in decimal
 *                                        degrees
 * Apr 16, 2013  1790          rferrel    Make dialog non-blocking. Changes for
 *                                        non-blocking CoopAgencyOfficeDlg.
 *                                        Changes for non-blocking
 *                                        CopyNewLocationDlg. Changes for
 *                                        non-blocking CountyStateDlg.
 * Jan 08, 2015  15695, 15488  djingtao   fix the save/update text field with
 *                                        apostrophe, repleace the single
 *                                        apostrophe to two single apostrophe
 *                                        before save/update to database.
 * Feb 02, 2015  13372         djingtao   change the GMT time to local time for
 *                                        "revise" field
 * Apr 08, 2015  17338         djingtao   "Apostrophe" entered into HB text
 *                                        fields are not written to IHFS
 *                                        database remove the changes in
 *                                        15695/15488,  move the apostrophe fix
 *                                        into a more central position
 * Jul 01, 2015  15642         xwei       Added read-only lat/lon in DMS format
 * Nov 30, 2015  14228         wkwock     Update remark limit to 510.
 * Feb 26, 2018  7097          mduff      Increase the size of the Revised Date
 *                                        text field.
 * Jan 15, 2020  8013          bhurley    Display blank fields when the dialog
 *                                        is opened as "Add Location".
 * 
 * </pre>
 *
 * @author lvenable
 *
 */
public class AddModifyLocationDlg extends CaveSWTDialog
        implements ICountyStateListener {

    private static final String EMPTY_STRING = "";

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AddModifyLocationDlg.class);

    private static final String INACTIVE = "I";

    private static final int MAX_REMARK_CHAR = 510;

    /**
     * Allow one Coop Agency Office dialog.
     */
    private CoopAgencyOfficeDlg coopAgencyOfficeDlg;

    /**
     * Allow one Copy New dialog.
     */
    private CopyNewLocationDlg copyDlg;

    /**
     * Allow one County/State dialog.
     */
    private CountyStateDlg countyStateDlg;

    /**
     * Font used for controls.
     */
    private Font controlFont;

    /**
     * Flag indicating if the dialog is a Modify or Add dialog. True is Modify,
     * false is Add.
     */
    private boolean modifyFlag = false;

    /**
     * Stacklayout composite.
     */
    private Composite stackLayoutComp;

    /**
     * Stacklayout (is a layout, not a composite).
     */
    private StackLayout stackLayout;

    /**
     * Composite for geophysical controls.
     */
    private Composite geophysicalComp;

    /**
     * Information group.
     */
    private Group infoGroup;

    /**
     * Geophysical additional information group.
     */
    private Combo geoAddInfoCbo;

    /**
     * Location text control.
     */
    private Text locationTF;

    /**
     * Inactive check box.
     */
    private Button inactiveChk;

    /**
     * Revise check box.
     */
    private Button reviseChk;

    /**
     * Revise text control.
     */
    private Text reviseTF;

    /**
     * Name text control.
     */
    private Text nameTF;

    /**
     * Basin text control.
     */
    private Text basinTF;

    /**
     * Latitude text control.
     */
    private Text latTF;

    /**
     * Longitude text control.
     */
    private Text lonTF;

    /**
     * Latitude in DMS text control.
     */
    private Text latDMSTF;

    /**
     * Longitude in DMS text control.
     */
    private Text lonDMSTF;

    /**
     * Elevation text control.
     */
    private Text elevationTF;

    /**
     * Station number text control.
     */
    private Text stationNumTF;

    /**
     * County/State text control.
     */
    private Text countyStateTF;

    /**
     * Time zone combo control.
     */
    private Combo timeZoneCbo;

    /**
     * Detail text control.
     */
    private Text detailTF;

    /**
     * Detail combo control.
     */
    private Combo detailCbo;

    /**
     * Network combo control.
     */
    private Combo networkCbo;

    /**
     * RFC combo control.
     */
    private Combo rfcCbo;

    /**
     * HSA list control.
     */
    private List hsaList;

    /**
     * WFO list control.
     */
    private List wfoList;

    /**
     * Remarks text control.
     */
    private Text remarksTF;

    /**
     * text from the remark text box
     */
    private String currentRemarkText = null;

    /**
     * Forecast Point check box.
     */
    private Button forecastPointChk;

    /**
     * Reservoir check box.
     */
    private Button reservoirChk;

    /**
     * Snow check box.
     */
    private Button snowChk;

    /**
     * Other check box.
     */
    private Button otherChk;

    /**
     * River Data check box.
     */
    private Button riverDataChk;

    /**
     * Precipitation check box.
     */
    private Button precipitationChk;

    /**
     * Temperature check box.
     */
    private Button tempChk;

    /**
     * Undefined check box.
     */
    private Button undefinedChk;

    /**
     * DCP check box.
     */
    private Button dcpChk;

    /**
     * Observer check box.
     */
    private Button observerChk;

    /**
     * Telemetry check box.
     */
    private Button telemetryChk;

    /**
     * Description text control.
     */
    private Text descTF;

    /**
     * Information text control.
     */
    private Text infoTF;

    /**
     * Horizontal datum text control.
     */
    private Text horizDatumTF;

    /**
     * Hydro unit text control.
     */
    private Text hydroUnitTF;

    /**
     * Beginning date text control.
     */
    private Text beginDateTF;

    /**
     * Station type text control.
     */
    private Text stationTypeTF;

    /**
     * Post observation values check box.
     */
    private Button postObsValuesChk;

    /**
     * Coop agency/office list control.
     */
    private List coopAgencyOfficeList;

    /**
     * Number of cooperators.
     */
    private Label numCooperatorsLbl;

    /**
     * Holds the data for the current location
     */
    private LocationData locData;

    /**
     * The current location/Station ID that the dialog's data refers to
     */
    private String lid;

    /**
     * The location id and/or name for the dialog
     */
    private String titleString;

    /**
     * Used to format dates in dialog
     */
    private SimpleDateFormat locDateFormat;

    private Button copyToNewLocationBtn;

    private Button deleteBtn;

    private Button coopAgencyOfficeBtn;

    /**
     * Listeners to notify main HB Dialog of station list changes
     */
    private java.util.List<IStationListener> stationListeners;

    /**
     * States for the dialog
     */
    private enum DialogStates {
        LOCATION_ADD, LOCATION_MODIFY
    }

    /**
     * The current state of the dialog
     */
    private DialogStates dialogState;

    private int charWidth;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param modifyFlag
     *            Flag indicating if the dialog should be in modification mode.
     */
    public AddModifyLocationDlg(Shell parent, boolean modifyFlag, String lid,
            String titleString) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);

        this.lid = lid;
        this.titleString = titleString;
        this.modifyFlag = modifyFlag;

        locDateFormat = new SimpleDateFormat("MM/dd/yyyy");
        locDateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
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
        setReturnValue(lid);

        // Initialize all of the controls and layouts
        init();

        // Get data if modify
        if (modifyFlag) {
            // Load the data for the selected lid
            loadDialogData();
            updateDialogDisplay();
            modifyRecord();
        } else {
            // Else setup for a new record
            newRecord();
        }
    }

    /**
     * Initialize the components on the display.
     */
    private void init() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);
        GC gc = new GC(shell);
        this.charWidth = gc.getFontMetrics().getAverageCharWidth();
        gc.dispose();

        createTopControls();

        stackLayoutComp = new Composite(shell, SWT.NONE);
        stackLayout = new StackLayout();
        stackLayoutComp.setLayout(stackLayout);

        createGeophysicalComp(stackLayoutComp);
        createAdditionInfoComp(stackLayoutComp);

        stackLayout.topControl = geophysicalComp;
        stackLayoutComp.layout();

        createBottomButtons();
    }

    /**
     * Create the controls at the top of the display.
     */
    private void createTopControls() {
        Composite topComp = new Composite(shell, SWT.NONE);
        topComp.setLayout(new GridLayout(3, false));
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

        copyToNewLocationBtn = new Button(topComp, SWT.PUSH);
        copyToNewLocationBtn.setText("Copy to New Location");
        copyToNewLocationBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (copyDlg == null) {
                    copyDlg = new CopyNewLocationDlg(shell, locData);
                    copyDlg.addCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            copyDlg = null;
                            fireUpdateEvent();
                        }
                    });
                    copyDlg.open();
                } else {
                    copyDlg.bringToTop();
                }
            }
        });
    }

    /**
     * Create the geophysical composite.
     * 
     * @param stackLayoutComp
     *            Stacklayout composite.
     */
    private void createGeophysicalComp(Composite stackLayoutComp) {
        geophysicalComp = new Composite(stackLayoutComp, SWT.NONE);
        geophysicalComp.setLayout(new GridLayout(1, false));
        geophysicalComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        createGeographicGroup(geophysicalComp);
        initData();
        createRemarksGroup(geophysicalComp);
        createStationCharGroup(geophysicalComp);
    }

    /**
     * Create the geographic group and controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createGeographicGroup(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group geographicGroup = new Group(parentComp, SWT.NONE);
        geographicGroup.setLayout(new GridLayout(1, false));
        geographicGroup.setLayoutData(gd);
        geographicGroup.setText(" Geographic/Physical ");

        String[] labels = new String[] { "Location: ", "Name: ", "Basin: ",
                "Lat/Lon: ", "Lat/Lon (DMS): ", "Elevation: ", "Station Num: ",
                "County/State: ", };

        GC gc = new GC(parentComp);
        int leftCellWidth = 100;
        for (String label : labels) {
            int length = gc.textExtent(label).x;
            leftCellWidth = Math.max(leftCellWidth, length);
        }

        // --------------------------------------
        // Location & Name controls composite
        // --------------------------------------
        Composite locationNameComp = new Composite(geographicGroup, SWT.NONE);
        locationNameComp.setLayout(new GridLayout(5, false));
        locationNameComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label locationLbl = new Label(locationNameComp, SWT.RIGHT);
        locationLbl.setText(labels[0]);
        locationLbl.setLayoutData(gd);

        int locationTextWidth = gc.textExtent("XXXXXXXX").x;
        gd = new GridData(locationTextWidth, SWT.DEFAULT);
        locationTF = new Text(locationNameComp, SWT.BORDER);
        locationTF.setLayoutData(gd);
        locationTF.setTextLimit(8);
        locationTF.addListener(SWT.Verify, new Listener() {
            @Override
            public void handleEvent(Event e) {
                String newStr = e.text;
                char[] newChars = new char[newStr.length()];
                newStr.getChars(0, newChars.length, newChars, 0);
                for (int i = 0; i < newChars.length; i++) {
                    if (!(('0' <= newChars[i]) && (newChars[i] <= '9'))
                            && !(('a' <= newChars[i]) && (newChars[i] <= 'z'))
                            && !(('A' <= newChars[i])
                                    && (newChars[i] <= 'Z'))) {
                        e.doit = false;
                    }
                }
                e.text = e.text.toUpperCase();
            }
        });
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalIndent = 30;
        inactiveChk = new Button(locationNameComp, SWT.CHECK);
        inactiveChk.setText("Inactive");
        inactiveChk.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        reviseChk = new Button(locationNameComp, SWT.CHECK);
        reviseChk.setText("Revise: ");
        reviseChk.setLayoutData(gd);
        reviseChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateRevisionDate();
            }
        });

        int reviseTextWidth = gc.textExtent("00/00/0000").x;
        gd = new GridData(reviseTextWidth, SWT.DEFAULT);
        reviseTF = new Text(locationNameComp, SWT.BORDER);
        reviseTF.setLayoutData(gd);

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label nameLbl = new Label(locationNameComp, SWT.RIGHT);
        nameLbl.setText(labels[1]);
        nameLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 4;
        nameTF = new Text(locationNameComp, SWT.BORDER);
        nameTF.setLayoutData(gd);
        nameTF.setTextLimit(50);

        // ------------------------------------------------------------
        // ------------------------------------------------------------
        // Create a composite that will contain the the remaining
        // geographic/physical controls (left and right) composites
        // ------------------------------------------------------------
        // ------------------------------------------------------------
        Composite bottomComp = new Composite(geographicGroup, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        gl.horizontalSpacing = 15;
        bottomComp.setLayout(gl);
        bottomComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        // ------------------------------------
        // Left composite and controls
        // ------------------------------------
        Composite leftComp = new Composite(bottomComp, SWT.NONE);
        leftComp.setLayout(new GridLayout(3, false));
        leftComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label basinLbl = new Label(leftComp, SWT.RIGHT);
        basinLbl.setText(labels[2]);
        basinLbl.setLayoutData(gd);

        // 30 Char limit on data so set it 3 chars wider for buffer
        gd = new GridData(33 * this.charWidth, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        basinTF = new Text(leftComp, SWT.BORDER);
        basinTF.setLayoutData(gd);
        basinTF.setTextLimit(30);

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label latLonLbl = new Label(leftComp, SWT.RIGHT);
        latLonLbl.setText(labels[3]);
        latLonLbl.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        latTF = new Text(leftComp, SWT.BORDER);
        latTF.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        lonTF = new Text(leftComp, SWT.BORDER);
        lonTF.setLayoutData(gd);

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label latLonDMSLbl = new Label(leftComp, SWT.RIGHT);
        latLonDMSLbl.setText(labels[4]);
        latLonDMSLbl.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        latDMSTF = new Text(leftComp, SWT.BORDER);
        latDMSTF.setLayoutData(gd);
        latDMSTF.setEnabled(false);

        gd = new GridData(110, SWT.DEFAULT);
        lonDMSTF = new Text(leftComp, SWT.BORDER);
        lonDMSTF.setLayoutData(gd);
        lonDMSTF.setEnabled(false);

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label elevationLbl = new Label(leftComp, SWT.RIGHT);
        elevationLbl.setText(labels[5]);
        elevationLbl.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        elevationTF = new Text(leftComp, SWT.BORDER);
        elevationTF.setLayoutData(gd);

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label stationLbl = new Label(leftComp, SWT.RIGHT);
        stationLbl.setText(labels[6]);
        stationLbl.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        stationNumTF = new Text(leftComp, SWT.BORDER);
        stationNumTF.setLayoutData(gd);
        stationNumTF.setTextLimit(10);

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Button countyStateBtn = new Button(leftComp, SWT.PUSH);
        countyStateBtn.setText(labels[7]);
        countyStateBtn.setLayoutData(gd);
        countyStateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openCountyStateDlg();
            }
        });

        gd = new GridData(150, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        countyStateTF = new Text(leftComp, SWT.BORDER);
        countyStateTF.setLayoutData(gd);
        countyStateTF.setEditable(false);

        gd = new GridData();
        gd.horizontalSpan = 3;
        Label fillerLbl = new Label(leftComp, SWT.NONE);
        fillerLbl.setLayoutData(gd);

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label timeZoneLbl = new Label(leftComp, SWT.RIGHT);
        timeZoneLbl.setText("Time Zone: ");
        timeZoneLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        timeZoneCbo = new Combo(leftComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        timeZoneCbo.setLayoutData(gd);

        // ---------------------------------------
        // Right composite and controls
        // ---------------------------------------
        Composite rightComp = new Composite(bottomComp, SWT.NONE);
        gl = new GridLayout(5, false);
        gl.verticalSpacing = 10;
        rightComp.setLayout(gl);
        rightComp.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

        Label detailLbl = new Label(rightComp, SWT.RIGHT);
        detailLbl.setText("Detail: ");

        gd = new GridData(30, SWT.DEFAULT);
        detailTF = new Text(rightComp, SWT.BORDER);
        detailTF.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 3;
        detailCbo = new Combo(rightComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        detailCbo.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalSpan = 2;
        Label networkLbl = new Label(rightComp, SWT.RIGHT);
        networkLbl.setText("Network: ");
        networkLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 3;
        networkCbo = new Combo(rightComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        networkCbo.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalSpan = 2;
        Label rfcLbl = new Label(rightComp, SWT.RIGHT);
        rfcLbl.setText("RFC: ");
        rfcLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 3;
        rfcCbo = new Combo(rightComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        rfcCbo.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label hsaLbl = new Label(rightComp, SWT.RIGHT);
        hsaLbl.setText("HSA: ");
        hsaLbl.setLayoutData(gd);

        int fontHeight = gc.getFontMetrics().getHeight();
        gd = new GridData(charWidth * 5, fontHeight * 6);
        hsaList = new List(rightComp, SWT.BORDER | SWT.V_SCROLL);
        hsaList.setFont(controlFont);
        hsaList.setLayoutData(gd);

        gd = new GridData(20, SWT.DEFAULT);
        Label filler2Lbl = new Label(rightComp, SWT.NONE);
        filler2Lbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.TOP, true, true);
        Label wfoLbl = new Label(rightComp, SWT.RIGHT);
        wfoLbl.setText("WFO: ");
        wfoLbl.setLayoutData(gd);

        gd = new GridData(charWidth * 5, fontHeight * 6);
        wfoList = new List(rightComp, SWT.BORDER | SWT.V_SCROLL);
        wfoList.setFont(controlFont);
        wfoList.setLayoutData(gd);

        gc.dispose();
    }

    /**
     * Create the Remarks group and text control.
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
        gd.widthHint = 750;

        remarksTF = new Text(remarksGroup, SWT.BORDER | SWT.MULTI | SWT.WRAP);
        remarksTF.setLayoutData(gd);
        remarksTF.setFont(controlFont);
        remarksTF.setTextLimit(MAX_REMARK_CHAR);
        currentRemarkText = remarksTF.getText();
        ModifyListener listener = new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                if (remarksTF.getText().length() > MAX_REMARK_CHAR) {
                    remarksTF.setText(currentRemarkText);
                    shell.getDisplay().beep();
                } else {
                    currentRemarkText = remarksTF.getText();
                }
            }
        };

        remarksTF.addModifyListener(listener);
    }

    /**
     * Create the Station Characteristics group and controls.
     *
     * @param parentComp
     *            Parent composite.
     */
    private void createStationCharGroup(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group stationGroup = new Group(parentComp, SWT.NONE);
        stationGroup.setLayout(new GridLayout(3, false));
        stationGroup.setLayoutData(gd);
        stationGroup.setText(" Station Characteristics (View-Only) ");

        // -----------------------------------------------
        // Create Station Type controls
        // -----------------------------------------------
        Composite stationTypeComp = new Composite(stationGroup, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        stationTypeComp.setLayout(new GridLayout(4, true));
        stationTypeComp.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 4;
        Label stationLbl = new Label(stationTypeComp, SWT.NONE);
        stationLbl.setText("Station Type:");
        stationLbl.setLayoutData(gd);

        forecastPointChk = new Button(stationTypeComp, SWT.CHECK);
        forecastPointChk.setText("Forecast Point");
        forecastPointChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                forecastPointChk.setSelection(!forecastPointChk.getSelection());
            }
        });

        reservoirChk = new Button(stationTypeComp, SWT.CHECK);
        reservoirChk.setText("Reservoir");
        reservoirChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                reservoirChk.setSelection(!reservoirChk.getSelection());
            }
        });

        snowChk = new Button(stationTypeComp, SWT.CHECK);
        snowChk.setText("Snow");
        snowChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                snowChk.setSelection(!snowChk.getSelection());
            }
        });

        otherChk = new Button(stationTypeComp, SWT.CHECK);
        otherChk.setText("Other");
        otherChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                otherChk.setSelection(!otherChk.getSelection());
            }
        });

        riverDataChk = new Button(stationTypeComp, SWT.CHECK);
        riverDataChk.setText("River Data");
        riverDataChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                riverDataChk.setSelection(!riverDataChk.getSelection());
            }
        });

        precipitationChk = new Button(stationTypeComp, SWT.CHECK);
        precipitationChk.setText("Precipitation");
        precipitationChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                precipitationChk.setSelection(!precipitationChk.getSelection());
            }
        });

        tempChk = new Button(stationTypeComp, SWT.CHECK);
        tempChk.setText("Temp");
        tempChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                tempChk.setSelection(!tempChk.getSelection());
            }
        });

        undefinedChk = new Button(stationTypeComp, SWT.CHECK);
        undefinedChk.setText("Undefinded");
        undefinedChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                undefinedChk.setSelection(!undefinedChk.getSelection());
            }
        });

        // ----------------------------------------------
        // Create vertical separator bar.
        // ----------------------------------------------

        gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        Label sepLbl = new Label(stationGroup, SWT.SEPARATOR | SWT.VERTICAL);
        sepLbl.setLayoutData(gd);

        // -----------------------------------------------
        // Create Data Sources controls
        // -----------------------------------------------
        Composite dataSourcesComp = new Composite(stationGroup, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dataSourcesComp.setLayout(new GridLayout(2, true));
        dataSourcesComp.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        Label dataSourcesLbl = new Label(dataSourcesComp, SWT.NONE);
        dataSourcesLbl.setText("Data Sources:");
        dataSourcesLbl.setLayoutData(gd);

        dcpChk = new Button(dataSourcesComp, SWT.CHECK);
        dcpChk.setText("Dcp");
        dcpChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                dcpChk.setSelection(!dcpChk.getSelection());
            }
        });

        observerChk = new Button(dataSourcesComp, SWT.CHECK);
        observerChk.setText("Observer");
        observerChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                observerChk.setSelection(!observerChk.getSelection());
            }
        });

        telemetryChk = new Button(dataSourcesComp, SWT.CHECK);
        telemetryChk.setText("Telemetry");
        telemetryChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                telemetryChk.setSelection(!telemetryChk.getSelection());
            }
        });
    }

    /**
     * Create the Additional Information GRoup and controls.
     * 
     * @param stackLayoutComp
     *            Stacklayout composite.
     */
    private void createAdditionInfoComp(Composite stackLayoutComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        infoGroup = new Group(stackLayoutComp, SWT.NONE);
        infoGroup.setLayout(new GridLayout(3, false));
        infoGroup.setLayoutData(gd);
        infoGroup.setText(" Information ");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label descLbl = new Label(infoGroup, SWT.RIGHT);
        descLbl.setText("Description: ");
        descLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        descTF = new Text(infoGroup, SWT.BORDER);
        descTF.setLayoutData(gd);
        descTF.setTextLimit(30);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label infoLbl = new Label(infoGroup, SWT.RIGHT);
        infoLbl.setText("Information: ");
        infoLbl.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        infoTF = new Text(infoGroup, SWT.BORDER);
        infoTF.setLayoutData(gd);
        infoTF.setTextLimit(30);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label horizLbl = new Label(infoGroup, SWT.RIGHT);
        horizLbl.setText("Horizontal Datum: ");
        horizLbl.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        horizDatumTF = new Text(infoGroup, SWT.BORDER);
        horizDatumTF.setLayoutData(gd);
        horizDatumTF.setTextLimit(9);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label hydroUnitLbl = new Label(infoGroup, SWT.RIGHT);
        hydroUnitLbl.setText("Hydrologic Unit: ");
        hydroUnitLbl.setLayoutData(gd);

        gd = new GridData(70, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        hydroUnitTF = new Text(infoGroup, SWT.BORDER);
        hydroUnitTF.setLayoutData(gd);
        hydroUnitTF.setTextLimit(8);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label beginDateLbl = new Label(infoGroup, SWT.RIGHT);
        beginDateLbl.setText("Begin Date: ");
        beginDateLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        beginDateTF = new Text(infoGroup, SWT.BORDER);
        beginDateTF.setLayoutData(gd);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, false, false);
        Label stationTypeLbl = new Label(infoGroup, SWT.RIGHT);
        stationTypeLbl.setText("Station Type: ");
        stationTypeLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        stationTypeTF = new Text(infoGroup, SWT.BORDER);
        stationTypeTF.setLayoutData(gd);
        stationTypeTF.setTextLimit(4);

        // Filler label
        new Label(infoGroup, SWT.NONE);

        gd = new GridData();
        gd.horizontalSpan = 2;
        postObsValuesChk = new Button(infoGroup, SWT.CHECK);
        postObsValuesChk.setText("Post Observed Values");
        postObsValuesChk.setLayoutData(gd);

        // Vertical filler label
        gd = new GridData(SWT.DEFAULT, 80);
        gd.horizontalSpan = 3;
        Label vFillerLbl = new Label(infoGroup, SWT.NONE);
        vFillerLbl.setLayoutData(gd);

        // Filler label
        new Label(infoGroup, SWT.NONE);

        gd = new GridData();
        gd.horizontalIndent = 4;
        gd.horizontalSpan = 2;
        Label agencyOfficeLbl = new Label(infoGroup, SWT.NONE);
        agencyOfficeLbl.setText(getListLabelText());
        agencyOfficeLbl.setFont(controlFont);
        agencyOfficeLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, false);
        coopAgencyOfficeBtn = new Button(infoGroup, SWT.PUSH);
        coopAgencyOfficeBtn
                .setText("Setup + Apply\nCooperating\nAgencies/Offices");
        coopAgencyOfficeBtn.setLayoutData(gd);
        coopAgencyOfficeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (coopAgencyOfficeDlg == null) {
                    coopAgencyOfficeDlg = new CoopAgencyOfficeDlg(shell,
                            titleString, lid);
                    coopAgencyOfficeDlg.addCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            coopAgencyOfficeDlg = null;
                            updateCoops();
                        }
                    });
                    coopAgencyOfficeDlg.open();
                } else {
                    coopAgencyOfficeDlg.bringToTop();
                }
            }
        });

        gd = new GridData(400, 150);
        gd.horizontalSpan = 2;
        coopAgencyOfficeList = new List(infoGroup,
                SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        coopAgencyOfficeList.setLayoutData(gd);
        coopAgencyOfficeList.setFont(controlFont);

        // Filler label
        new Label(infoGroup, SWT.NONE);

        gd = new GridData();
        gd.horizontalSpan = 2;
        numCooperatorsLbl = new Label(infoGroup, SWT.NONE);
        numCooperatorsLbl.setText("( 0 Cooperators )");
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(4, true));
        buttonComp.setLayoutData(gd);

        GC gc = new GC(buttonComp);
        int buttonWidth = gc.stringExtent(" XXXXXXXX ").x;
        gc.dispose();

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
                fireUpdateEvent();
                close();
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
                fireUpdateEvent();
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
                close();
            }
        });

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

    /**
     * Update the Cooperators label with the number of cooperators in the list.
     */
    private void updateCooperatorsLabel() {
        int numCooperators = coopAgencyOfficeList.getItemCount();
        numCooperatorsLbl.setText("( " + numCooperators + " Cooperators )");
    }

    /**
     * Get the text for the data list.
     * 
     * @return The label text.
     */
    private String getListLabelText() {
        String format = "%S                %S";

        String labelStr = String.format(format, "Agency", "Office");

        return labelStr;
    }

    /**
     * Loads the data into the Detail, Network, RFC, HSA, WFO and Timezone menus
     */
    private void initData() {
        // Clear the lists first
        detailCbo.removeAll();
        networkCbo.removeAll();
        rfcCbo.removeAll();
        hsaList.removeAll();
        wfoList.removeAll();
        timeZoneCbo.removeAll();

        AddModifyLocationDataManager dataManager = AddModifyLocationDataManager
                .getInstance();

        // Details
        for (String currDir : dataManager.getDirections()) {
            detailCbo.add(currDir);
        }
        detailCbo.select(0);

        try {
            // Network
            for (String currNet : dataManager.getNetworks()) {
                networkCbo.add(currNet);
            }
            networkCbo.select(0);

            // RFC
            for (String currRFC : dataManager.getRFCs()) {
                rfcCbo.add(currRFC);
            }
            rfcCbo.select(0);

            // HSA
            for (String currHSA : dataManager.getHSAs()) {
                hsaList.add(currHSA);
            }
            hsaList.select(0);

            // WFO
            for (String currWFO : dataManager.getWFOs()) {
                wfoList.add(currWFO);
            }
            wfoList.select(0);

            // Timezone
            for (String currTZ : dataManager.getTimeZones()) {
                timeZoneCbo.add(currTZ);
            }
            timeZoneCbo.select(0);

        } catch (VizException e) {
            statusHandler.error("Error setting up menus.", e);
        }
    }

    /**
     * Sets the checks box value for the Station Type and Data Source sections
     */
    private void getStationTypeAndDataSources() {
        StationClassData seedData = new StationClassData();
        seedData.setLid(lid);

        java.util.List<StationClassData> classData = new ArrayList<>();
        try {
            classData = HydroDBDataManager.getInstance().getData(seedData);

            if (!classData.isEmpty()) {
                StationClassData stnClass = classData.get(0);

                String stType = stnClass.getStationType();

                // Forecast Point
                forecastPointChk.setSelection(stType.contains("F"));

                // River Data
                riverDataChk.setSelection(stType.contains("R"));

                // Reservoir
                reservoirChk.setSelection(stType.contains("D"));

                // Precipitation
                precipitationChk.setSelection(stType.contains("P"));

                // Snow
                snowChk.setSelection(stType.contains("S"));

                // Temp
                tempChk.setSelection(stType.contains("T"));

                // Other
                otherChk.setSelection(stType.contains("O"));

                // Undefined
                undefinedChk.setSelection(stType.contains("U"));

                // DCP
                dcpChk.setSelection(stnClass.getDcp().charAt(0) == 'T');

                // Telemetry
                telemetryChk.setSelection(stnClass.getTelemetry().length() > 0);

                // Observer
                observerChk
                        .setSelection(stnClass.getObserver().charAt(0) == 'T');
            }
        } catch (VizException e) {
            statusHandler
                    .error("Error querying for station type and data sources for LID "
                            + lid + ".", e);
        }
    }

    /**
     * Loads the location data from the DB
     */
    private void loadDialogData() {
        try {
            locData = AddModifyLocationDataManager.getInstance()
                    .getLocationData(lid);
        } catch (VizException e) {
            statusHandler.error("Error querying location data for LID " + lid,
                    e);
        }
    }

    /**
     * Fills in the data on the Geophysical and Additional Info pages of the
     * dialog with data from the database
     */
    private void updateDialogDisplay() {
        // Set the Text fields' text to the data in the locdata
        locationTF.setText(locData.getLid());

        // if the location type is I, then it is inactive
        inactiveChk.setSelection(locData.getType().equalsIgnoreCase(INACTIVE));

        // Load the Revision Date
        updateRevisionDate();

        nameTF.setText(locData.getName());
        basinTF.setText(locData.getRiverBasin());

        // Only Display Lat/Lon if not missing

        latTF.setText((locData.getLatitude() != HydroConstants.MISSING_VALUE)
                ? String.valueOf(locData.getLatitude()) : EMPTY_STRING);
        lonTF.setText((locData.getLongitude() != HydroConstants.MISSING_VALUE)
                ? String.valueOf(locData.getLongitude()) : EMPTY_STRING);

        latDMSTF.setText(GeoUtil.getInstance().cvt_latlon_from_double(
                (locData.getLatitude() != HydroConstants.MISSING_VALUE)
                        ? locData.getLatitude() : 0));

        lonDMSTF.setText(GeoUtil.getInstance().cvt_latlon_from_double(
                (locData.getLongitude() != HydroConstants.MISSING_VALUE)
                        ? locData.getLongitude() : 0));

        // Only display elevation if it isn't missing, i.e. null in DB
        elevationTF.setText(
                (locData.getElevation() != HydroConstants.MISSING_VALUE)
                        ? String.format("%5.1f", locData.getElevation())
                        : "  0.0");

        // Station Number
        stationNumTF.setText(locData.getStationNumber());

        // County/State
        if (!locData.getCounty().isEmpty()) {
            String countyState = locData.getCounty() + ", "
                    + locData.getState();
            countyStateTF.setText(countyState);
        }

        // Remarks
        remarksTF.setText(locData.getRemark());

        // Additional Information
        descTF.setText(locData.getDescription());
        infoTF.setText(locData.getInformation());
        horizDatumTF.setText(locData.getHorizontalDatum());
        hydroUnitTF.setText(locData.getHu());
        beginDateTF.setText((locData.getBeginDate() != null)
                ? locDateFormat.format(locData.getBeginDate()) : EMPTY_STRING);

        // Set Station Type
        stationTypeTF.setText(locData.getStationType());

        // if Post is equal to 1, then check
        postObsValuesChk.setSelection(locData.getPost() == 1);

        // Set the states of the check boxes
        getStationTypeAndDataSources();

        // Populates the Cooperating Agencies and Offices
        updateCoops();

        // Network
        String net = locData.getNetwork();
        String[] netData = networkCbo.getItems();
        for (int i = 0; i < netData.length; i++) {
            if (net.equalsIgnoreCase(netData[i])) {
                networkCbo.select(i);
                break;
            }
        }

        // RFC
        String rfc = locData.getRfc();
        String[] rfcData = rfcCbo.getItems();
        for (int i = 0; i < rfcData.length; i++) {
            if (rfc.equalsIgnoreCase(rfcData[i])) {
                rfcCbo.select(i);
                break;
            }
        }

        // load the HSA
        String currHSA = locData.getHsa();
        for (int i = 0; i < hsaList.getItemCount(); i++) {
            if (currHSA.equalsIgnoreCase(hsaList.getItem(i))) {
                hsaList.select(i);
                break;
            }
        }
        hsaList.showSelection();

        // Set the WFO, too
        String currWFO = locData.getWfo();
        for (int i = 0; i < wfoList.getItemCount(); i++) {
            if (currWFO.equalsIgnoreCase(wfoList.getItem(i))) {
                wfoList.select(i);
                break;
            }
        }
        wfoList.showSelection();

        // Get the TimeZone
        String currTZ = locData.getTimeZone();
        for (int i = 0; i < timeZoneCbo.getItemCount(); i++) {
            if (timeZoneCbo.getItem(i).contains(currTZ)) {
                timeZoneCbo.select(i);
                break;
            }
        }

        // Parse out the Details
        String[] details = AddModifyLocationDataManager.getInstance()
                .parseDetails(locData.getDetail());

        // Set the mileage part of Details
        detailTF.setText(details[0]);

        // Set the direction part of Details
        for (int i = 0; i < detailCbo.getItemCount(); i++) {
            if (detailCbo.getItem(i).equalsIgnoreCase(details[1])) {
                detailCbo.select(i);
                break;
            }
        }
    }

    private void updateCoops() {
        // Populate the Agencies and offices
        coopAgencyOfficeList.removeAll();

        try {
            java.util.List<String> offices = AddModifyLocationDataManager
                    .getInstance().getSelectedAgenciesAndOffices(lid);

            for (String currOffice : offices) {
                coopAgencyOfficeList.add(currOffice);
            }

            updateCooperatorsLabel();
        } catch (VizException e) {
            statusHandler.error("Error accessing Agencies and Offices.", e);
        }
    }

    /**
     * Confirms the deletion of the location and closes the dialog if the
     * location is deleted.
     */
    private void deleteRecord() {
        MessageBox mb = new MessageBox(shell,
                SWT.ICON_QUESTION | SWT.OK | SWT.CANCEL);
        mb.setText("Delete Confirmation");
        mb.setMessage("Do you wish to delete all entries for this location?");

        int result = mb.open();

        if (result == SWT.OK) {
            try {
                AddModifyLocationDataManager.getInstance()
                        .deleteRecord(locData);

                close();
                fireUpdateEvent();
            } catch (VizException e) {
                statusHandler.error("Error deleting record.", e);
            }
        }
    }

    /**
     * Setup the dialog to add a new location
     */
    private void newRecord() {
        // Have HSA and WFO default to setting from Admin table
        setDefaultHsaWfo();

        /* default to always post when adding a station */
        postObsValuesChk.setSelection(true);

        // Set dialog state
        dialogState = DialogStates.LOCATION_ADD;
        setDialogState();
    }

    /**
     * Setup the dialog to modify an existing location
     */
    private void modifyRecord() {
        // Set dialog state
        dialogState = DialogStates.LOCATION_MODIFY;
        setDialogState();
    }

    /**
     * Retrieve and set the default values for WFO and HSA to the HSA set in the
     * Admin table
     */
    private void setDefaultHsaWfo() {
        // If adding location, HSA and WFO default to Admin table values
        try {
            java.util.List<AdministrationData> data = HydroDBDataManager
                    .getInstance().getData(AdministrationData.class);

            if (!data.isEmpty()) {
                // There will only at most one record
                AdministrationData currAdminData = data.get(0);

                // Set the HSA
                String currHSA = currAdminData.getHsa();
                for (int i = 0; i < hsaList.getItemCount(); i++) {
                    if (currHSA.equalsIgnoreCase(hsaList.getItem(i))) {
                        hsaList.select(i);
                        hsaList.showSelection();
                        break;
                    }
                }

                // Set the WFO, too
                for (int i = 0; i < wfoList.getItemCount(); i++) {
                    if (currHSA.equalsIgnoreCase(wfoList.getItem(i))) {
                        wfoList.select(i);
                        wfoList.showSelection();
                        break;
                    }
                }
            }
        } catch (VizException e) {
            statusHandler.warn("Error getting administration data.", e);
        }
    }

    /**
     * valid and stuff values from text fields into a new data object and save
     * via data manager. Automatically handles insert vs. update.
     */
    private void saveRecord() {
        if (locationTF.getText().trim().isEmpty()) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Value");
            mb.setMessage("Please enter a new Location value");
            mb.open();
            return;
        } else if (countyStateTF.getText().trim().isEmpty()) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Value");
            mb.setMessage("Please select a county and state");
            mb.open();
            return;
        } else {
            // Create a new location data object
            LocationData dataToSave = new LocationData();

            // LID
            dataToSave.setLid(locationTF.getText());

            // Save County/State
            String[] countState = AddModifyLocationDataManager.getInstance()
                    .parseCountyState(countyStateTF.getText());
            dataToSave.setCounty(countState[0]);
            dataToSave.setState(countState[1]);

            // Detail - Mileage and Direction
            String detail = detailTF.getText();
            double miles = 0.0;
            if (!detail.trim().isEmpty()) {
                try {
                    miles = Double.parseDouble(detail);
                } catch (NumberFormatException e) {
                    MessageBox mb = new MessageBox(shell,
                            SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Invalid Value");
                    mb.setMessage(
                            "Please enter a valid numeric value for Detail mileage");
                    mb.open();

                    return;
                }
            }

            detail = detail + " "
                    + detailCbo.getItem(detailCbo.getSelectionIndex());
            dataToSave.setDetail(detail);

            // Elevation - set to 0.0 if nothing is entered
            String elevStr = elevationTF.getText();
            double elevation = 0.0;
            if (!elevStr.trim().isEmpty()) {
                try {
                    elevation = Double.parseDouble(elevStr);
                } catch (NumberFormatException e) {
                    MessageBox mb = new MessageBox(shell,
                            SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Invalid Value");
                    mb.setMessage(
                            "Please enter a valid numeric value for Elevation");
                    mb.open();

                    return;
                }
            }

            dataToSave.setElevation(elevation);

            // Latitude
            String latTxt = latTF.getText();
            double lat = HydroConstants.MISSING_VALUE;
            if (!latTxt.isEmpty()) {
                boolean invalidLat = false;

                try {
                    lat = GeoUtil.getInstance().cvt_spaced_format(latTxt, 0);
                } catch (NumberFormatException e) {
                    invalidLat = true;
                }

                if ((lat < -90) || (lat > 90) || invalidLat) {
                    MessageBox mb = new MessageBox(shell,
                            SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Invalid Value");
                    mb.setMessage(
                            "Please enter a VALID (-90 to 90) Latitude\nin the form: DD MM SS");
                    mb.open();

                    return;
                }
            }
            dataToSave.setLatitude(lat);

            // Longitude
            String lonTxt = lonTF.getText();
            double lon = HydroConstants.MISSING_VALUE;
            if (!lonTxt.isEmpty()) {
                boolean invalidLon = false;

                try {
                    lon = GeoUtil.getInstance().cvt_spaced_format(lonTxt, 0);
                } catch (NumberFormatException e) {
                    invalidLon = true;
                }

                if ((lon > 180) || (lon < -180) || invalidLon) {
                    MessageBox mb = new MessageBox(shell,
                            SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Invalid Value");
                    mb.setMessage(
                            "Please enter a VALID (-180 to 180) Longitude\nin the form: DD MM SS");
                    mb.open();

                    return;
                }
            }
            dataToSave.setLongitude(lon);

            // Remark
            dataToSave.setRemark(remarksTF.getText());

            // Revise Date
            if (!reviseTF.getText().isEmpty()) {
                try {
                    dataToSave.setReviseDate(
                            locDateFormat.parse(reviseTF.getText()));
                } catch (ParseException e) {
                    MessageBox mb = new MessageBox(shell,
                            SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Invalid Date");
                    mb.setMessage(
                            "Please enter a Revision Date in the form: MM/DD/YYYY");
                    mb.open();

                    return;
                }
            }

            // Name
            dataToSave.setName(nameTF.getText());

            // River Basin
            dataToSave.setRiverBasin(basinTF.getText());

            // Station Number
            dataToSave.setStationNumber(stationNumTF.getText());

            // Description
            dataToSave.setDescription(descTF.getText());

            // Information a.k.a. det
            dataToSave.setInformation(infoTF.getText());

            // Horiz. Datum
            dataToSave.setHorizontalDatum(horizDatumTF.getText());

            // Hydro Unit
            dataToSave.setHu(hydroUnitTF.getText());

            // Station Type
            dataToSave.setStationType(stationTypeTF.getText());

            // Station Begin Date
            if (!beginDateTF.getText().isEmpty()) {
                try {
                    dataToSave.setBeginDate(
                            locDateFormat.parse(beginDateTF.getText()));
                } catch (ParseException e) {
                    MessageBox mb = new MessageBox(shell,
                            SWT.ICON_ERROR | SWT.OK);
                    mb.setText("Invalid Date");
                    mb.setMessage(
                            "Please enter a valid Begin Date in the form: MM/DD/YYYY");
                    mb.open();

                    return;
                }
            }

            // Post
            dataToSave.setPost((postObsValuesChk.getSelection()) ? 1 : 0);

            // Network
            dataToSave.setNetwork(
                    networkCbo.getItem(networkCbo.getSelectionIndex()));

            // RFC
            dataToSave.setRfc(rfcCbo.getItem(rfcCbo.getSelectionIndex()));

            // TimeZone - Grab the first 8 characters from the selected value
            // and
            // trim
            dataToSave.setTimeZone(
                    timeZoneCbo.getItem(timeZoneCbo.getSelectionIndex())
                            .substring(0, 8).trim());

            // WFO
            dataToSave.setWfo(wfoList.getItem(wfoList.getSelectionIndex()));

            // HSA
            dataToSave.setHsa(hsaList.getItem(hsaList.getSelectionIndex()));

            // Type
            /*
             * If station is "Inactive", store an 'I' in the loc.type field.
             */
            dataToSave.setType(
                    (inactiveChk.getSelection()) ? INACTIVE : EMPTY_STRING);

            // Save to DB via DataManager
            try {
                HydroDBDataManager.getInstance().putData(dataToSave);
            } catch (VizException e) {
                statusHandler.error("Error saving data to DB.", e);
                return;
            }

            // Set dialog state
            dialogState = DialogStates.LOCATION_MODIFY;
            setDialogState();

            // Set the current lid to the one saved
            lid = dataToSave.getLid();
            locData = dataToSave;

            // Reload Data
            loadDialogData();

            try {
                StnClassSyncUtil.setStnClass(lid);
            } catch (VizException e) {
                statusHandler.error(
                        "The StnClass table in IHFS was not updated properly.",
                        e);
            }
        }
    }

    /**
     * Handles the states (enable/disable)
     */
    private void setDialogState() {
        switch (dialogState) {
        case LOCATION_MODIFY:
            shell.setText("Modify Location" + titleString);

            // LID can't be modified
            locationTF.setEditable(false);

            copyToNewLocationBtn.setEnabled(true);
            coopAgencyOfficeBtn.setEnabled(true);

            deleteBtn.setEnabled(true);
            break;
        case LOCATION_ADD:
            // clear titleString so selected station HydroBase dlg doesn't show
            // up
            titleString = EMPTY_STRING;

            shell.setText("Add Location");

            locationTF.setEditable(true);

            copyToNewLocationBtn.setEnabled(false);
            coopAgencyOfficeBtn.setEnabled(false);

            deleteBtn.setEnabled(false);
            break;
        default:
            break;
        }
    }

    private void openCountyStateDlg() {
        if (countyStateDlg == null || countyStateDlg.isDisposed()) {
            countyStateDlg = new CountyStateDlg(shell);
            countyStateDlg.addListener(this);
            countyStateDlg.open();
        } else {
            countyStateDlg.bringToTop();
        }

        // Set the selection county/state dlg
        if (!countyStateTF.getText().isEmpty()) {
            // Set selection in dialog to current location's settings
            CountiesData currCountyState = new CountiesData();

            String[] countyState = countyStateTF.getText().split(",");
            currCountyState.setCounty(countyState[0].trim());
            currCountyState.setState(countyState[1].trim());

            // Tell county/state dialog what to select
            countyStateDlg.setSelection(currCountyState);
        }
    }

    @Override
    public void notifyUpdate(CountiesData selectedCountyState) {
        countyStateTF.setText(selectedCountyState.getCounty() + ", "
                + selectedCountyState.getState());
    }

    private void updateRevisionDate() {
        /*
         * If the Revision Checkbox is checked, set the Revision Date to the
         * current date Else load the date from the database
         */
        Calendar now = Calendar.getInstance(Locale.getDefault());
        String revise_str = new SimpleDateFormat("MM/dd/yyyy")
                .format(now.getTime());

        if (reviseChk.getSelection()) {
            reviseTF.setText(revise_str);
        } else {
            if (locData == null) {
                reviseTF.setText(EMPTY_STRING);
            } else {
                Date reviseDate = locData.getReviseDate();
                reviseTF.setText((reviseDate != null)
                        ? locDateFormat.format(reviseDate) : EMPTY_STRING);
            }
        }
    }

    /**
     * Adds the listener
     *
     * @param prefListener
     *            The listener to add
     */
    public void addListener(IStationListener stationListener) {
        if (stationListeners == null) {
            stationListeners = new ArrayList<>();
        }

        stationListeners.add(stationListener);
    }

    /**
     * Notifies the listeners of the station list changes
     */
    private void fireUpdateEvent() {
        for (IStationListener currListener : stationListeners) {
            currListener.notifyStationUpdate(lid);
        }
    }

    /**
     * Removes the listener
     */
    public void removeListener(IStationListener stationListener) {
        stationListeners.remove(stationListener);
    }
}
