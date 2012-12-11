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

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
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
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

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

/**
 * This class displays the Add/Modify location dialog. A flag is passed into the
 * constructor to determine if the dialog will be an Add or Modify dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Sep 2008             lvenable    Initial creation.
 * 09 Sep 2009  2769       mpduff      Added event handling for add/delete events.
 * 22 Feb 2010  1985       mpduff      Made combo boxes scroll selection into view.
 * 09 Apr 2010  1870       mpduff      Upon adding a new site the setStnClass util is 
 *                                     called and the stnclass table is updated.
 * 27 Oct 2010  5519       Judy Wang   Converted lower case to upper
 *                                     case in Location box.
 * 10 May 2011  9309       djingtao    the elevation fields should be defaults as 0.0 when user
 *                                     wipe out the field (e.g. blank)
 * 26 Nov 2012 15440       lbousaidi   display lat/lon in the GUI in decimal degrees                                   
 * 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class AddModifyLocationDlg extends CaveSWTDialog implements
        ICountyStateListener {

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
    private String currentRemarkText=null;

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
    private SimpleDateFormat locDate;

    /**
     * Holds the Station Characteristics
     */
    private StationClassData stnClass;

    private Button copyToNewLocationBtn;

    private Button okBtn;

    private Button applyBtn;

    private Button cancelBtn;

    private Button deleteBtn;

    private Button coopAgencyOfficeBtn;

    private String defaultNewLid = "XXXXX";

    private String defaultNewCountyState = "XXXXXXXXXXXXXXXXXXXX, XX";

    /**
     * Listeners to notify main HB Dialog of station list changes
     */
    private ArrayList<IStationListener> stationListeners;

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
        super(parent);

        this.lid = lid;
        this.titleString = titleString;
        this.modifyFlag = modifyFlag;

        locDate = new SimpleDateFormat("MM/dd/yyyy");
        locDate.setTimeZone(TimeZone.getTimeZone("UTC"));
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

        // Initialize all of the controls and layouts
        initializeComponents();

        // Fill in the static pull down menus
        initData();

        // Get data if modify
        if (modifyFlag) {
            modifyRecord();
        } else {
            // Else setup for a new record
            newRecord();
        }
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

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
                CopyNewLocationDlg copyDlg = new CopyNewLocationDlg(shell,
                        locData);
                copyDlg.open();
                fireUpdateEvent();
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
        geophysicalComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        createGeographicGroup(geophysicalComp);
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

        int leftCellWidth = 110;

        // --------------------------------------
        // Location & Name controls composite
        // --------------------------------------
        Composite locationNameComp = new Composite(geographicGroup, SWT.NONE);
        locationNameComp.setLayout(new GridLayout(5, false));
        locationNameComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT,
                true, false));

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label locationLbl = new Label(locationNameComp, SWT.RIGHT);
        locationLbl.setText("Location: ");
        locationLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        locationTF = new Text(locationNameComp, SWT.BORDER);
        locationTF.setLayoutData(gd);
        locationTF.setTextLimit(8);
        locationTF.addListener(SWT.Verify, new Listener() {
            public void handleEvent(Event e) {
                String newStr = e.text;
                char[] newChars = new char[newStr.length()];
                newStr.getChars(0, newChars.length, newChars, 0);
                for (int i = 0; i < newChars.length; i++) {
                    if (!(('0' <= newChars[i]) && (newChars[i] <= '9'))
                            && !(('a' <= newChars[i]) && (newChars[i] <= 'z'))
                            && !(('A' <= newChars[i]) && (newChars[i] <= 'Z'))) {
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

        gd = new GridData(80, SWT.DEFAULT);
        reviseTF = new Text(locationNameComp, SWT.BORDER);
        reviseTF.setLayoutData(gd);

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label nameLbl = new Label(locationNameComp, SWT.RIGHT);
        nameLbl.setText("Name: ");
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
        bottomComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        // ------------------------------------
        // Left composite and controls
        // ------------------------------------
        Composite leftComp = new Composite(bottomComp, SWT.NONE);
        leftComp.setLayout(new GridLayout(3, false));
        leftComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label basinLbl = new Label(leftComp, SWT.RIGHT);
        basinLbl.setText("Basin: ");
        basinLbl.setLayoutData(gd);

        gd = new GridData(180, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        basinTF = new Text(leftComp, SWT.BORDER);
        basinTF.setLayoutData(gd);
        basinTF.setTextLimit(30);

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label latLonLbl = new Label(leftComp, SWT.RIGHT);
        latLonLbl.setText("Lat/Lon: ");
        latLonLbl.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        latTF = new Text(leftComp, SWT.BORDER);
        latTF.setLayoutData(gd);

        gd = new GridData(110, SWT.DEFAULT);
        lonTF = new Text(leftComp, SWT.BORDER);
        lonTF.setLayoutData(gd);

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label elevationLbl = new Label(leftComp, SWT.RIGHT);
        elevationLbl.setText("Elevation: ");
        elevationLbl.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        elevationTF = new Text(leftComp, SWT.BORDER);
        elevationTF.setLayoutData(gd);

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Label stationLbl = new Label(leftComp, SWT.RIGHT);
        stationLbl.setText("Station Num: ");
        stationLbl.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        stationNumTF = new Text(leftComp, SWT.BORDER);
        stationNumTF.setLayoutData(gd);
        stationNumTF.setTextLimit(10);

        gd = new GridData(leftCellWidth, SWT.DEFAULT);
        Button countyStateBtn = new Button(leftComp, SWT.PUSH);
        countyStateBtn.setText("County/State");
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
        timeZoneCbo.add("Sample Time Zone drop down data...");
        timeZoneCbo.select(0);

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
        detailCbo.add("AT");
        detailCbo.select(0);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalSpan = 2;
        Label networkLbl = new Label(rightComp, SWT.RIGHT);
        networkLbl.setText("Network: ");
        networkLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 3;
        networkCbo = new Combo(rightComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        networkCbo.setLayoutData(gd);
        networkCbo.add("A");
        networkCbo.select(0);

        gd = new GridData(SWT.FILL, SWT.CENTER, false, true);
        gd.horizontalSpan = 2;
        Label rfcLbl = new Label(rightComp, SWT.RIGHT);
        rfcLbl.setText("RFC: ");
        rfcLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 3;
        rfcCbo = new Combo(rightComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        rfcCbo.setLayoutData(gd);
        rfcCbo.add("ABRFC");
        rfcCbo.select(0);

        gd = new GridData(SWT.FILL, SWT.TOP, false, true);
        Label hsaLbl = new Label(rightComp, SWT.RIGHT);
        hsaLbl.setText("HSA: ");
        hsaLbl.setLayoutData(gd);

        gd = new GridData(40, 70);
        hsaList = new List(rightComp, SWT.BORDER | SWT.V_SCROLL);
        hsaList.setFont(controlFont);
        hsaList.setLayoutData(gd);

        gd = new GridData(20, SWT.DEFAULT);
        Label filler2Lbl = new Label(rightComp, SWT.NONE);
        filler2Lbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.TOP, true, true);
        gd.widthHint = 40;
        Label wfoLbl = new Label(rightComp, SWT.RIGHT);
        wfoLbl.setText("WFO: ");
        wfoLbl.setLayoutData(gd);

        gd = new GridData(40, 70);
        wfoList = new List(rightComp, SWT.BORDER | SWT.V_SCROLL);
        wfoList.setFont(controlFont);
        wfoList.setLayoutData(gd);
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
        remarksTF.setTextLimit(255);
        currentRemarkText=remarksTF.getText();
        ModifyListener listener = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (remarksTF.getText().length()>255){
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
                CoopAgencyOfficeDlg coopAgencyOfficeDlg = new CoopAgencyOfficeDlg(
                        shell, titleString, lid);
                coopAgencyOfficeDlg.open();
                updateCoops();
            }
        });

        gd = new GridData(400, 150);
        gd.horizontalSpan = 2;
        coopAgencyOfficeList = new List(infoGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
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

        int buttonWidth = 90;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
                fireUpdateEvent();
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        applyBtn = new Button(buttonComp, SWT.PUSH);
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
        cancelBtn = new Button(buttonComp, SWT.PUSH);
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

        // Details
        for (String currDir : AddModifyLocationDataManager.getInstance()
                .getDirections()) {
            detailCbo.add(currDir);
        }
        detailCbo.select(0);

        try {
            // Network
            for (String currNet : AddModifyLocationDataManager.getInstance()
                    .getNetworks()) {
                networkCbo.add(currNet);
            }
            networkCbo.select(0);

            // RFC
            for (String currRFC : AddModifyLocationDataManager.getInstance()
                    .getRFCs()) {
                rfcCbo.add(currRFC);
            }
            rfcCbo.select(0);

            // HSA
            for (String currHSA : AddModifyLocationDataManager.getInstance()
                    .getHSAs()) {
                hsaList.add(currHSA);
            }
            hsaList.select(0);

            // WFO
            for (String currWFO : AddModifyLocationDataManager.getInstance()
                    .getWFOs()) {
                wfoList.add(currWFO);
            }
            wfoList.select(0);

            // Timezone
            for (String currTZ : AddModifyLocationDataManager.getInstance()
                    .getTimeZones()) {
                timeZoneCbo.add(currTZ);
            }
            timeZoneCbo.select(0);

        } catch (VizException e) {
            e.printStackTrace();
        }

    }

    /**
     * Sets the checks box value for the Station Type and Data Source sections
     */
    private void getStationTypeAndDataSources() {
        StationClassData seedData = new StationClassData();
        seedData.setLid(lid);

        ArrayList<StationClassData> classData = new ArrayList<StationClassData>();
        try {
            classData = HydroDBDataManager.getInstance().getData(seedData);

            if (classData.size() > 0) {
                stnClass = classData.get(0);

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
            e.printStackTrace();
        }
    }

    /**
     * Loads the location data from the DB
     */
    private void getDialogData() {
        try {
            locData = AddModifyLocationDataManager.getInstance()
                    .getLocationData(lid);
        } catch (VizException e) {
            e.printStackTrace();
        }

        updateDialogDisplay();
    }

    /**
     * Fills in the data on the Geophysical and Additional Info pages of the
     * dialog with data from the database
     */
    private void updateDialogDisplay() {
        // Set the Text fields' text to the data in the locdata
        locationTF.setText(locData.getLid());

        // if the location type is I, then it is inactive
        inactiveChk.setSelection(locData.getType().equalsIgnoreCase("I"));

        // Load the Revision Date
        updateRevisionDate();

        nameTF.setText(locData.getName());
        basinTF.setText(locData.getRiverBasin());

        // Only Display Lat/Lon if not missing
      
       latTF.setText((locData.getLatitude() != HydroConstants.MISSING_VALUE) ?
                String.valueOf(locData.getLatitude()): "");
       lonTF.setText((locData.getLongitude() != HydroConstants.MISSING_VALUE) ?
              String.valueOf(locData.getLongitude()): "");
       
        // Only display elevation if it isn't missing, i.e. null in DB
        elevationTF
                .setText((locData.getElevation() != HydroConstants.MISSING_VALUE) ? String
                        .format("%5.1f", locData.getElevation()) : "  0.0");

        // Station Number
        stationNumTF.setText(locData.getStationNumber());

        // County/State
        if (!locData.getCounty().equalsIgnoreCase("")) {
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
        beginDateTF.setText((locData.getBeginDate() != null) ? locDate
                .format(locData.getBeginDate()) : "");

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
            ArrayList<String> offices = AddModifyLocationDataManager
                    .getInstance().getSelectedAgenciesAndOffices(lid);

            for (String currOffice : offices) {
                coopAgencyOfficeList.add(currOffice);
            }

            updateCooperatorsLabel();
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * Confirms the deletion of the location and closes the dialog if the
     * location is deleted.
     */
    private void deleteRecord() {
        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Delete Confirmation");
        mb.setMessage("Do you wish to delete all entries for this location?");

        int result = mb.open();

        if (result == SWT.OK) {
            try {
                AddModifyLocationDataManager.getInstance()
                        .deleteRecord(locData);

                shell.dispose();
                fireUpdateEvent();
            } catch (VizException e) {
                MessageBox mbFail = new MessageBox(shell, SWT.ICON_ERROR
                        | SWT.OK);
                mbFail.setText("Unable to Delete");
                mbFail.setMessage("Unable to Delete Record");
                mbFail.open();

                e.printStackTrace();
            }
        }
    }

    /**
     * Setup the dialog to add a new location
     */
    private void newRecord() {
        // Have HSA and WFO default to setting from Admin table
        setDefaultHsaWfo();

        // Default lid value
        locationTF.setText(defaultNewLid);

        // Default County/State
        countyStateTF.setText(defaultNewCountyState);

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
        // Load the data for the selected lid
        getDialogData();

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
            ArrayList<AdministrationData> data = HydroDBDataManager
                    .getInstance().getData(AdministrationData.class);

            if (data.size() > 0) {
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
            // ignore since the first option is selected by default
        }
    }

    /**
     * valid and stuff values from text fields into a new data object and save
     * via data manager. Automatically handles insert vs. update.
     */
    private void saveRecord() {
        if (locationTF.getText().equalsIgnoreCase(defaultNewLid)) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Value");
            mb.setMessage("Please enter a new Location value");
            mb.open();
            return;
        } else if (countyStateTF.getText().equalsIgnoreCase(
                defaultNewCountyState)) {
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
            detail = detail + " "
                    + detailCbo.getItem(detailCbo.getSelectionIndex());
            dataToSave.setDetail(detail);

            // Elevation - set to 0.0 if nothing is entered
            String elevStr = elevationTF.getText();
            double elevation = 0.0;
            if (!elevStr.trim().equals("")) {
                try {
                    elevation = Double.parseDouble(elevStr);
                } catch (Exception e) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Invalid Value");
                    mb.setMessage("Please enter a valid numeric value for Elevation");
                    mb.open();

                    return;
                }
            }

            dataToSave.setElevation(elevation);

            // Latitude
            String latTxt = latTF.getText();
            double lat = HydroConstants.MISSING_VALUE;
            if (!latTxt.equals("")) {
                boolean invalidLat = false;

                try {
                    lat = GeoUtil.getInstance().cvt_spaced_format(latTxt, 0);
                } catch (Exception e) {
                    invalidLat = true;
                }

                if ((lat < -90) || (lat > 90) || invalidLat) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Invalid Value");
                    mb.setMessage("Please enter a VALID (-90 to 90) Latitude\nin the form: DD MM SS");
                    mb.open();

                    return;
                }
            }
            dataToSave.setLatitude(lat);

            // Longitude
            String lonTxt = lonTF.getText();
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
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Invalid Value");
                    mb.setMessage("Please enter a VALID (-180 to 180) Longitude\nin the form: DD MM SS");
                    mb.open();

                    return;
                }
            }
            dataToSave.setLongitude(lon);

            // Remark
            dataToSave.setRemark(remarksTF.getText());

            // Revise Date
            if (!reviseTF.getText().equals("")) {
                try {
                    dataToSave.setReviseDate(locDate.parse(reviseTF.getText()));
                } catch (Exception e) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Invalid Date");
                    mb.setMessage("Please enter a Revision Date in the form: MM/DD/YYYY");
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
            if (!beginDateTF.getText().equals("")) {
                try {
                    dataToSave
                            .setBeginDate(locDate.parse(beginDateTF.getText()));
                } catch (Exception e) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Invalid Date");
                    mb.setMessage("Please enter a valid Begin Date in the form: MM/DD/YYYY");
                    mb.open();

                    return;
                }
            }

            // Post
            dataToSave.setPost((postObsValuesChk.getSelection()) ? 1 : 0);

            // Network
            dataToSave.setNetwork(networkCbo.getItem(networkCbo
                    .getSelectionIndex()));

            // RFC
            dataToSave.setRfc(rfcCbo.getItem(rfcCbo.getSelectionIndex()));

            // TimeZone - Grab the first 8 characters from the selected value
            // and
            // trim
            dataToSave.setTimeZone(timeZoneCbo
                    .getItem(timeZoneCbo.getSelectionIndex()).substring(0, 8)
                    .trim());

            // WFO
            dataToSave.setWfo(wfoList.getItem(wfoList.getSelectionIndex()));

            // HSA
            dataToSave.setHsa(hsaList.getItem(hsaList.getSelectionIndex()));

            // Type
            /*
             * If station is "Inactive", store an 'I' in the loc.type field.
             */
            dataToSave.setType((inactiveChk.getSelection()) ? "I" : "");

            // Save to DB via DataManager
            try {
                HydroDBDataManager.getInstance().putData(dataToSave);
            } catch (VizException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("Unable to Save Record");
                mb.open();

                e.printStackTrace();
                return;
            }

            // Set dialog state
            dialogState = DialogStates.LOCATION_MODIFY;
            setDialogState();

            // Set the current lid to the one saved
            lid = dataToSave.getLid();
            locData = dataToSave;

            // Reload Data
            getDialogData();

            try {
                StnClassSyncUtil.setStnClass(lid);
            } catch (VizException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("StnClass Error");
                mb.setMessage("The StnClass table in IHFS was not updated properly.\n"
                        + e.getMessage());
                mb.open();
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
            titleString = "";

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
        CountyStateDlg countyStateDlg = new CountyStateDlg(shell);
        countyStateDlg.addListener(this);

        // Open the county/state dlg
        if (!countyStateTF.getText().equals("")) {
            // Set selection in dialog to current location's settings
            CountiesData currCountyState = new CountiesData();

            String[] countyState = countyStateTF.getText().split(",");
            currCountyState.setCounty(countyState[0].trim());
            currCountyState.setState(countyState[1].trim());

            // Tell county/state dialog what to select
            countyStateDlg.open(currCountyState);
        } else {
            countyStateDlg.open();
        }

        countyStateDlg.removeListener(this);
    }

    @Override
    public void notifyUpdate(CountiesData selectedCountyState) {
        countyStateTF.setText(selectedCountyState.getCounty() + ", "
                + selectedCountyState.getState());
    }

    private void updateRevisionDate() {
        // If the Revision Checkbox is checked, set the Revision Date to the
        // current date
        // Else load the date from the database
        if (reviseChk.getSelection()) {
            reviseTF.setText(locDate.format(Calendar.getInstance(
                    TimeZone.getTimeZone("GMT")).getTime()));
        } else if (locData != null) {
            reviseTF.setText((locData.getReviseDate() != null) ? locDate
                    .format(locData.getReviseDate()) : "");
        } else {
            reviseTF.setText("");
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
            stationListeners = new ArrayList<IStationListener>();
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
