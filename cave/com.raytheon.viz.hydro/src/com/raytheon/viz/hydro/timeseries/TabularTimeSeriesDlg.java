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
package com.raytheon.viz.hydro.timeseries;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Random;
import java.util.TimeZone;

import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.shef.tables.Fcstheight;
import com.raytheon.uf.common.dataplugin.shef.tables.FcstheightId;
import com.raytheon.uf.common.dissemination.OUPRequest;
import com.raytheon.uf.common.dissemination.OUPResponse;
import com.raytheon.uf.common.dissemination.OfficialUserProduct;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.Pair;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.auth.UserController;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.hydro.timeseries.table.DataRecord;
import com.raytheon.viz.hydro.timeseries.table.ForecastDataAttribute;
import com.raytheon.viz.hydro.timeseries.table.SiteInfo;
import com.raytheon.viz.hydro.timeseries.table.TabInfo;
import com.raytheon.viz.hydro.timeseries.table.TabularData;
import com.raytheon.viz.hydro.timeseries.util.GroupInfo;
import com.raytheon.viz.hydro.timeseries.util.StageDischargeUtils;
import com.raytheon.viz.hydro.timeseries.util.TimeSeriesUtil;
import com.raytheon.viz.hydro.util.LoadMaxFcst;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.texteditor.TextEditorDlg;
import com.raytheon.viz.hydrocommon.util.DbUtils;
import com.raytheon.viz.hydrocommon.util.HydroQC;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;

/**
 * This class displays the Tabular Time Series dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORYlid
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation.
 * 20 OCT 2008  1520       mpduff      Implement the dialog.
 * 06 NOV 2009  2641/2     mpduff      Implement shef encode, review product
 *                                     and clear product buttons.
 * 19 July 2010 5964       lbousaidi   being able to enter data for empty 
 * 									   timeseries
 * Sep 14 2010  5282	   lbousaidi   added disposeTabularTS 
 * Oct 19 2010  6785       lbousaidi   implement setMissing for forecast data
 * Oct 28 2010  2640	   lbousaidi   fixed ProductTime, basistime, and Obstime 
 * 									   times updates and other bugs.
 * Jan 31 2010  5274       bkowal      long-running queries are now done
 *                                     asynchronously so that a status indicator
 *                                     can be displayed and so that the
 *                                     interface is not locked up. Extracted
 *                                     the data retrieval logic from the function
 *                                     that updates the interface with the
 *                                     data retrieved.
 * Apr 04 2011 5966 	   lbousaidi   fixed Save Table to File and to Printer
 * May 27 2011 9584        jpiatt      Modified to not save updated forecast data 
 *                                     in rejecteddata table.
 * Sep 09 2011 9962        lbousaidi   reload time series when there is update/insert
 * 									   and highlight the row that was updated.
 * Feb 05,2013 1578        rferrel     Changes for non-blocking singleton TimeSeriesDlg.
 *                                     Code clean up for non-blocking dialog.
 * Feb 27,2013 1790        rferrel     Bug fix for non-blocking dialogs.
 * Jun 07, 2013 1981       mpduff      Set user's id on the OUPRequest as it is now protected.
 * Jul 16, 2013 2088       rferrel     Changes for non-blocking TextEditorDlg.
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * Oct 13, 2015 4933       rferrel     Refactored to use selected variables.
 * Oct 27, 2015 4900       mduff       Don't transmit SHEF files if in DRT.
 * Nov 06, 2015 17846      lbousaidi   change the query so that after QC, the quality_code  
 * Feb 17, 2016 14471      amoore      Update/insert/modify in Latest obs table
 *                                     is reset from Bad to Good.
 * Jan 29, 2016 5289       tgurney     Add missing minimize button in trim
 * Mar 17, 2016 5483       randerso    Major GUI cleanup
 * May 02, 2016 5616       randerso    Fix parsing of duration value
 * May 11, 2016 5483       bkowal      Fix GUI sizing issues.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class TabularTimeSeriesDlg extends CaveSWTDialog implements
        ForecastDataAttributeListener, IJobChangeListener {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TabularTimeSeriesDlg.class);

    /** Line terminator to use when reading a line from a shef file. */
    private final String CARRIAGECONTROL = "\r";

    /** Maximum number time series to place in a list. */
    private final int MAX_TS_ON_LIST = 120;

    /** Quality control value for manual "Good" */
    private final int QC_MANUAL_PASSED = 121;

    /** Quality control value for manual "Quest". */
    private final int QC_MANUAL_QUEST = 122;

    /** Quality control value for manual "Bad". */
    private final int QC_MANUAL_FAILED = 123;

    /** Value used for undefined type source. */
    private final String UNDEFINED_TYPESOURCE = "??";

    /** Default value or product ID. */
    private final String INSERT_PROD_ID = "CCCCWRKXXX";

    /** The base for all shef file names. */
    private final String SHEF_FILE_NAME = "shef_product";

    /** Message title to use when invalid product ID error message. */
    private final String INVALID_PRODUCT_ID = "Invalid Product ID";

    /** the Send configuration dialog. */
    private SendConfigDlg sendConfigDlg;

    /**
     * Simple date formatter.
     */
    private final SimpleDateFormat sdf = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm");

    /**
     * Simple date formatter for the forecast basis time.
     */
    private final SimpleDateFormat prodBasisFmt = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss");

    /** Date format for shef information. */
    private final SimpleDateFormat shefDateFormat = new SimpleDateFormat(
            "yyyyMMdd");

    /** Time format for shef information. */
    private final SimpleDateFormat shefTimeFormat = new SimpleDateFormat("HHmm");

    /**
     * Date & Time label.
     */
    private Label dateTimeLbl;

    /**
     * Beginning time.
     */
    private final Date beginningTime;

    /**
     * Ending time.
     */
    private final Date endingTime;

    /**
     * List all forecasts check box.
     */
    private Button listAllFcstChk;

    /**
     * Data table at the top of the dialog.
     */
    private final List<Pair<String, Integer>> topTableLabels = Arrays.asList(
            new Pair<>("Loc", SWT.LEFT), new Pair<>("PE", SWT.LEFT),
            new Pair<>("Dur", SWT.RIGHT), new Pair<>("TS", SWT.LEFT),
            new Pair<>("E", SWT.LEFT), new Pair<>("BasisTime(Z)", SWT.LEFT));

    private Table topDataTable;

    /**
     * Use product time check box.
     */
    private Button useProductTimeChk;

    /**
     * Forecast basis time check box.
     */
    private Button fcstBasisTimeChk;

    /**
     * Forecast type source check box.
     */
    private Button fcstTypSrcChk;

    /**
     * Product time label.
     */
    private Label productTimeLbl;

    /**
     * Forecast basis time label.
     */
    private Label fcstBasisTimeLbl;

    /**
     * Dummy time to use; based on the simulated time.
     */
    private Calendar dummyTime;

    /**
     * Product label ID.
     */
    private Label productIdLbl;

    /**
     * Insert Data Edit button.
     */
    private Button insertDataEditBtn;

    /**
     * Copy full forecast button.
     */
    private Button copyFullFcstBtn;

    /**
     * Selected location name button.
     */
    private Label selectedLocNameLbl;

    /**
     * Selected location information label.
     */
    private Label selectedLocInfoLbl;

    /**
     * Selected flood label.
     */
    private Label selectedFloodLbl;

    /**
     * Bottom Table control.
     */

    private final List<Pair<String, Integer>> bottomTableColumnLabels = Arrays
            .asList(new Pair<>("Value", SWT.RIGHT), new Pair<>("Flow",
                    SWT.RIGHT), new Pair<>("Stage", SWT.RIGHT), new Pair<>(
                    "Time(Z)", SWT.LEFT), new Pair<>("RV", SWT.RIGHT),
                    new Pair<>("SQ", SWT.RIGHT), new Pair<>("QC", SWT.RIGHT),
                    new Pair<>("Product", SWT.LEFT), new Pair<>("Time",
                            SWT.LEFT), new Pair<>("Posted", SWT.LEFT));

    private Table bottomTable;

    /**
     * Bottom list control.
     */
    private Text bottomListTextControl;

    /**
     * Value text control.
     */
    private Text valueTF;

    /**
     * Time text control.
     */
    private Text timeTF;

    /**
     * Quality control combo box.
     */
    private Combo qcCbo;

    /**
     * Update/Insert button.
     */
    private Button updateInsertBtn;

    /**
     * Set Missing button.
     */
    private Button setMissingBtn;

    /**
     * Set Quality Control button.
     */
    private Button setQcBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * SHEF Encode Selected button.
     */
    private Button shefEncodeBtn;

    /**
     * Clear Product button.
     */
    private Button clearProductBtn;

    /**
     * Save Table button.
     */
    private Button saveTableBtn;

    /**
     * Review product button.
     */
    private Button reviewProductBtn;

    /**
     * Review Send Script button.
     */
    private Button reviewSendScriptBtn;

    /**
     * Send Product button.
     */
    private Button sendProductBtn;

    /**
     * Send Table to Printer button.
     */
    private Button sendTableToPrinterBtn;

    /**
     * Product text control.
     */
    private Text productTF;

    /** The location id */
    private String lid = null;

    /** Selected site name */
    private String siteName = null;

    /** The group info */
    private GroupInfo groupInfo;

    /** The TabInfo */
    private TabInfo tabInfo = null;

    /** The current TabInfo */
    private TabInfo currentTabInfo = null;

    /** The Site Label Text */
    private String siteLabel = null;

    /** The list of data for the bottom data display list */
    private final ArrayList<String> modifiedTSList = new ArrayList<String>();

    /** The parent dialog */
    private TimeSeriesDlg parentDialog = null;

    /** The selected duration. */
    private String dur = null;

    /** The selected physical element */
    private String pe = null;

    /** The selected type source */
    private String ts = null;

    /** The selected extremum. */
    private String extremum = null;

    /** The selected basis time. */
    private String basisTime = null;

    /** Date format for the tabular display */
    private static SimpleDateFormat tabularFormat;

    /** Date format for the database */
    private static SimpleDateFormat dbFormat;

    /* Setup the format static variables. */
    static {
        tabularFormat = new SimpleDateFormat("MM/dd HH:mm");
        tabularFormat.setTimeZone(TimeZone.getTimeZone("GMT"));

        dbFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        dbFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /** List of TabularData objects */
    private ArrayList<TabularData> tabularDataList = null;

    /** The number of records retrieved using the Record Count query **/
    private long ratingRecordCount = 0;

    /** List of TabInfo objects */
    private ArrayList<TabInfo> tabInfoList = new ArrayList<TabInfo>();

    /** The original value */
    private double oldValue;

    /** SHEF broadcast filename */
    String attachedFilename = null;

    /** Printer to use. */
    private Printer printer;

    /** Printer's line height. */
    private int lineHeight = 0;

    /** Printer's tab width. */
    private int tabWidth = 0;

    /** Printer's left margin. */
    private int leftMargin;

    /** Printer's right margin. */
    private int rightMargin;

    /** Printer's top margin. */
    private int topMargin;

    /** Printer's bottom margin. */
    private int bottomMargin;

    /** Printer's horizontal location for printing. */
    private int x;

    /** Printer's vertical location for printing. */
    private int y;

    /** Location in the text that is being printed. */
    private int index;

    /** Number of characters in the text being printed. */
    private int end;

    private StringBuffer wordBuffer;

    private GC gc;

    private ForecastAttributeDlg fcstAttDlg = null;

    private ForecastDataAttribute fcstAtt = null;

    private String whfsProductDir = null;

    private String shefFileName = null;

    private TextEditorDlg editor = null;

    private int pid = HydroConstants.MISSING_VALUE;

    private boolean updateFlag = false;

    private int indexSelected = 0;

    private TimeSeriesDataJobManager tsDataJobManager = null;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param beginningTime
     *            Beginning time.
     * @param endingTime
     *            Ending time.
     * @param parentDialog
     *            Parent dialog
     */
    public TabularTimeSeriesDlg(Shell parent, Date beginningTime,
            Date endingTime, TimeSeriesDlg parentDialog) {
        super(parent, SWT.DIALOG_TRIM | SWT.MIN,
                CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        setText("Tabular Time Series");

        this.beginningTime = beginningTime;
        this.endingTime = endingTime;
        this.parentDialog = parentDialog;
        this.tsDataJobManager = new TimeSeriesDataJobManager();
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        return mainLayout;
    }

    @Override
    protected void disposed() {
        // Delete the SHEF encode file if it exists
        if (shefFileName != null) {
            File shefFile = new File(shefFileName);
            if (shefFile.exists()) {
                shefFile.delete();
            }
        }
    }

    /**
     * Set up to retrieve selection data.
     */
    private void scheduleDataRetrieval() {
        shell.setCursor(shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT));
        this.topDataTable.setEnabled(false);
        this.extractFormInformation();
        tsDataJobManager.scheduleGetTableData(this, this);
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        // Set to the first on in the list
        tabInfo = tabInfoList.get(0);
        lid = tabInfo.getLid();

        // load the info about the available time series
        currentTabInfo = new TabInfo();
        currentTabInfo.setBeginTime(tabInfo.getBeginTime());
        currentTabInfo.setEndTime(tabInfo.getEndTime());
        currentTabInfo.setInfoList(tabInfo.getInfoList());

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        // Get the dummy time
        dummyTime = TimeUtil.newGmtCalendar();
        Date d = SimulatedTime.getSystemTime().getTime();
        dummyTime.setTime(d);

        sdf.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        prodBasisFmt.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        shefDateFormat.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        shefTimeFormat.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        createTopTimeLabels();
        createTopListAndGroup();
        createSelectedInfoLabels();
        createBottomTableControls();
        createEditSelectedGroup();
        createBottomButtonControls();
        addSeparator();
        createCloseButton();

        tabularLoadTimeseries();
    }

    /**
     * Create the time labels at the top of the display.
     */
    private void createTopTimeLabels() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite timeComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        timeComp.setLayout(gl);
        timeComp.setLayoutData(gd);

        Label timeZLbl = new Label(timeComp, SWT.NONE);
        timeZLbl.setText("TimeZ:");
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        timeZLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        dateTimeLbl = new Label(timeComp, SWT.NONE);
        StringBuffer strBuf = new StringBuffer();
        strBuf.append(sdf.format(beginningTime)).append(" - ")
                .append(sdf.format(endingTime));
        dateTimeLbl.setText(strBuf.toString());
        dateTimeLbl.setLayoutData(gd);

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        listAllFcstChk = new Button(timeComp, SWT.CHECK);
        listAllFcstChk.setText("List ALL Forecasts");
        listAllFcstChk.setLayoutData(gd);
        listAllFcstChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                tabularLoadTimeseries();
            }
        });
    }

    /**
     * Create the list control and the inserted data attributes group container
     * at the top of the display.
     */
    private void createTopListAndGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite listGroupComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        listGroupComp.setLayout(gl);
        listGroupComp.setLayoutData(gd);

        addLabelsAndTopDataList(listGroupComp);
        addInsertedDataGroup(listGroupComp);
    }

    /**
     * Add the top list control to the display.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void addLabelsAndTopDataList(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite topListComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginWidth = 0;
        topListComp.setLayout(gl);
        topListComp.setLayoutData(gd);

        topDataTable = new Table(topListComp, SWT.SINGLE | SWT.FULL_SELECTION
                | SWT.BORDER);
        topDataTable.setHeaderVisible(true);

        GC gc = new GC(topDataTable);
        // Estimate width of table text.
        int textWidth = gc.textExtent("WWWWW WW WWWW WW W 9999-99-99 99:99:99").x;
        gc.dispose();

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = textWidth;
        gd.heightHint = topDataTable.getItemHeight() * 8;
        topDataTable.setLayoutData(gd);
        topDataTable.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                topDataSelectionAction();
            }
        });

        for (Pair<String, Integer> pair : topTableLabels) {
            TableColumn column = new TableColumn(topDataTable, pair.getSecond());
            column.setText(pair.getFirst());
        }
    }

    private void topDataSelectionAction() {
        scheduleDataRetrieval();
        updateSelectedLocInfoLabel();
        updateStationLabel();
        updateFloodStageLabel();
    }

    /**
     * Add the Inserted Data Group and controls to the display.
     * 
     * @param parentComp
     */
    private void addInsertedDataGroup(Composite parentComp) {
        Group insertedGroup = new Group(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        insertedGroup.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        insertedGroup.setLayoutData(gd);
        insertedGroup.setText("Inserted Data Attributes");

        Composite leftComp = new Composite(insertedGroup, SWT.NONE);
        gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        leftComp.setLayout(gl);
        gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        leftComp.setLayoutData(gd);

        useProductTimeChk = new Button(leftComp, SWT.CHECK);
        useProductTimeChk.setText("Use Product Time/ID:");

        productTimeLbl = new Label(leftComp, SWT.NONE);
        productTimeLbl.setText(prodBasisFmt.format(dummyTime.getTime()));

        productIdLbl = new Label(leftComp, SWT.NONE);
        productIdLbl.setText("CCCWRKXXX   ");

        fcstBasisTimeChk = new Button(leftComp, SWT.CHECK);
        fcstBasisTimeChk.setText("Fcst BasisTime:");

        fcstBasisTimeLbl = new Label(leftComp, SWT.NONE);
        fcstBasisTimeLbl.setText(prodBasisFmt.format(dummyTime.getTime()));

        Composite rightComp = new Composite(insertedGroup, SWT.NONE);
        gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        rightComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        rightComp.setLayoutData(gd);

        fcstTypSrcChk = new Button(leftComp, SWT.CHECK);
        fcstTypSrcChk.setText("Fcst TypSrc:    ??");

        gd = new GridData(SWT.FILL, SWT.BOTTOM, true, true);
        copyFullFcstBtn = new Button(rightComp, SWT.PUSH);
        copyFullFcstBtn.setText("Copy Full\nForecast");
        copyFullFcstBtn.setLayoutData(gd);
        copyFullFcstBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                tabularCopyTS();
            }
        });

        gd = new GridData(SWT.FILL, SWT.BOTTOM, true, true);
        insertDataEditBtn = new Button(leftComp, SWT.PUSH);
        insertDataEditBtn.setText("Edit");
        insertDataEditBtn.setLayoutData(gd);
        insertDataEditBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (fcstAttDlg == null) {
                    fcstAtt = new ForecastDataAttribute(productIdLbl.getText(),
                            productTimeLbl.getText(), fcstBasisTimeLbl
                                    .getText(), new String[] { "FF", "FZ" });
                    fcstAttDlg = new ForecastAttributeDlg(shell, fcstAtt,
                            TimeUtil.newCalendar(dummyTime), TimeUtil
                                    .newCalendar(dummyTime));
                    fcstAttDlg.addListener(TabularTimeSeriesDlg.this);
                    fcstAttDlg.open();
                } else {
                    fcstAttDlg.showDialog();
                }
            }
        });
    }

    /**
     * Create the information labels located between the 2 list box controls.
     */
    private void createSelectedInfoLabels() {
        Composite selectedInfoComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        selectedInfoComp.setLayout(gl);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        selectedLocNameLbl = new Label(selectedInfoComp, SWT.NONE);
        selectedLocNameLbl.setLayoutData(gd);

        gd = new GridData(350, SWT.DEFAULT);
        selectedLocInfoLbl = new Label(selectedInfoComp, SWT.NONE);
        selectedLocInfoLbl.setLayoutData(gd);

        selectedFloodLbl = new Label(selectedInfoComp, SWT.NONE);
    }

    /**
     * Create the bottom table and text control below the table.
     */
    private void createBottomTableControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite bottomListComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        bottomListComp.setLayout(gl);
        bottomListComp.setLayoutData(gd);

        bottomTable = new Table(bottomListComp, SWT.BORDER | SWT.MULTI
                | SWT.FULL_SELECTION);

        GC gc = new GC(bottomTable);
        int textWidth = gc
                .textExtent("00000.00 00000.00 99/99 99:99 WW WW WW WWWWWWWWWW 99/99 99:99 99/99 99:99").x;
        gc.dispose();

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textWidth;
        gd.heightHint = bottomTable.getItemHeight() * 12;
        bottomTable.setLayoutData(gd);
        bottomTable.setHeaderVisible(true);
        for (Pair<String, Integer> pair : bottomTableColumnLabels) {
            TableColumn column = new TableColumn(bottomTable, pair.getSecond());
            column.setText(pair.getFirst());
        }
        bottomTable.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleBottomTableSelection();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        bottomListTextControl = new Text(shell, SWT.BORDER);
        bottomListTextControl.setLayoutData(gd);
    }

    /**
     * Create the Edit Selected group container and the associated controls.
     */
    private void createEditSelectedGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group editSelectedGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, true);
        gl.horizontalSpacing = 10;
        editSelectedGroup.setLayout(gl);
        editSelectedGroup.setLayoutData(gd);
        editSelectedGroup.setText("Edit Selected");

        // ---------------------------------------
        // Create the top row of controls
        // ---------------------------------------
        Composite comp = new Composite(editSelectedGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        comp.setLayout(gl);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        comp.setLayoutData(gd);

        Label valueLbl = new Label(comp, SWT.NONE);
        valueLbl.setText("Value:");

        valueTF = new Text(comp, SWT.BORDER);
        GC gc = new GC(valueTF);
        int textWidth = gc.textExtent("0000000.000000").x;
        gc.dispose();

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = textWidth;
        valueTF.setLayoutData(gd);

        comp = new Composite(editSelectedGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        comp.setLayout(gl);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        comp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        Label timeLbl = new Label(comp, SWT.RIGHT);
        timeLbl.setText("Time:");
        timeLbl.setLayoutData(gd);

        timeTF = new Text(comp, SWT.BORDER);
        gc = new GC(timeTF);
        textWidth = gc.textExtent("0000-00-00 00:00:00").x;
        gc.dispose();

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = textWidth;
        timeTF.setLayoutData(gd);

        comp = new Composite(editSelectedGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        comp.setLayout(gl);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        comp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        Label qcLbl = new Label(comp, SWT.RIGHT);
        qcLbl.setText("QC:");
        qcLbl.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        qcCbo = new Combo(comp, SWT.DROP_DOWN | SWT.READ_ONLY);
        qcCbo.add("Good");
        qcCbo.add("Quest.");
        qcCbo.add("Bad");
        qcCbo.select(0);
        qcCbo.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        updateInsertBtn = new Button(editSelectedGroup, SWT.NONE);
        updateInsertBtn.setText("Update/Insert Value");
        updateInsertBtn.setLayoutData(gd);
        updateInsertBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateFlag = true;
                updateInsertValue();
                updateFlag = false;
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        setMissingBtn = new Button(editSelectedGroup, SWT.NONE);
        setMissingBtn.setText("Set Missing");
        setMissingBtn.setLayoutData(gd);
        setMissingBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setMissing();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        setQcBtn = new Button(editSelectedGroup, SWT.NONE);
        setQcBtn.setText("Set QC");
        setQcBtn.setLayoutData(gd);
        setQcBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setQC();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        deleteBtn = new Button(editSelectedGroup, SWT.NONE);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteData();
            }
        });
    }

    /**
     * Create the bottom control buttons.
     */
    private void createBottomButtonControls() {
        Composite mainBtnComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        mainBtnComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        mainBtnComp.setLayoutData(gd);

        // --------------------------------------------
        // Buttons on the left side on the composite
        // --------------------------------------------
        Composite leftComp = new Composite(mainBtnComp, SWT.NONE);
        gl = new GridLayout(2, true);
        gl.marginWidth = 0;
        gl.horizontalSpacing = 10;
        leftComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        leftComp.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        shefEncodeBtn = new Button(leftComp, SWT.PUSH);
        shefEncodeBtn.setText("SHEF Encode Selected");
        shefEncodeBtn.setLayoutData(gd);
        shefEncodeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                tabularShefEncode();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        clearProductBtn = new Button(leftComp, SWT.PUSH);
        clearProductBtn.setText("Clear Product");
        clearProductBtn.setLayoutData(gd);
        clearProductBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                clearProduct();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        reviewProductBtn = new Button(leftComp, SWT.PUSH);
        reviewProductBtn.setText("Review Product");
        reviewProductBtn.setLayoutData(gd);
        reviewProductBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                reviewShefEncodedProduct();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        reviewSendScriptBtn = new Button(leftComp, SWT.PUSH);
        reviewSendScriptBtn.setText("Review Send Script");
        reviewSendScriptBtn.setLayoutData(gd);
        reviewSendScriptBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (sendConfigDlg == null || sendConfigDlg.isDisposed()) {
                    sendConfigDlg = new SendConfigDlg(shell);
                    sendConfigDlg.open();
                } else {
                    sendConfigDlg.bringToTop();
                }

            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        sendProductBtn = new Button(leftComp, SWT.PUSH);
        sendProductBtn.setText("Send Product");
        sendProductBtn.setLayoutData(gd);
        sendProductBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                sendProduct();
            }
        });

        Composite comp = new Composite(leftComp, SWT.NONE);
        gl = new GridLayout(2, false);
        comp.setLayout(gl);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        comp.setLayoutData(gd);

        Label productLbl = new Label(comp, SWT.NONE);
        productLbl.setText("Product:");
        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
        productLbl.setLayoutData(gd);

        productTF = new Text(comp, SWT.BORDER);
        GC gc = new GC(productTF);
        int textWidth = gc.textExtent("WWWWWWWWWW").x;
        gc.dispose();

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        gd.widthHint = textWidth;
        productTF.setLayoutData(gd);
        AppsDefaults defaults = AppsDefaults.getInstance();
        String product = defaults.getToken("shefencode_prodid");
        productTF.setText(product);

        // --------------------------------------------
        // Buttons on the right side on the composite
        // --------------------------------------------
        Composite rightComp = new Composite(mainBtnComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        gl.horizontalSpacing = 10;
        rightComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        rightComp.setLayoutData(gd);

        gd = new GridData(GridData.FILL_VERTICAL);
        gd.verticalSpan = 2;
        Label sepLbl = new Label(rightComp, SWT.SEPARATOR | SWT.VERTICAL);
        sepLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        saveTableBtn = new Button(rightComp, SWT.PUSH);
        saveTableBtn.setText("Save Table\nto File");
        saveTableBtn.setLayoutData(gd);
        saveTableBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveTable();
            }
        });

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        sendTableToPrinterBtn = new Button(rightComp, SWT.PUSH);
        sendTableToPrinterBtn.setText("Send Table\nto Printer");
        sendTableToPrinterBtn.setLayoutData(gd);
        sendTableToPrinterBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                sendTableToPrinter();
            }
        });
    }

    /**
     * Add a horizontal separator line to the display.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Create the bottom Close button.
     */
    private void createCloseButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        centeredComp.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        Button closeBtn = new Button(centeredComp, SWT.NONE);
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
     * Load the data types list
     */
    private void tabularLoadTimeseries() {
        int entryNumber = 0;
        int count;
        topDataTable.removeAll();
        modifiedTSList.clear();
        ArrayList<SiteInfo> siteInfoList = new ArrayList<SiteInfo>();
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();

        try {
            /* Get the unique time series defined from the parent info */
            for (TabInfo ti : tabInfoList) {
                ArrayList<SiteInfo> peList = ti.getInfoList();

                updateStationLabel();

                ArrayList<Object[]> results;

                /* loop on the unique time series defined from the parent info */
                for (int i = 0; i < peList.size(); i++) {
                    SiteInfo row = peList.get(i);

                    /* if a forecast timeseries then find all basis times */
                    if (row.getTs().toUpperCase().startsWith("F")
                            || row.getTs().toUpperCase().startsWith("C")) {
                        String tableName = DbUtils.getTableName(row.getPe(),
                                row.getTs());
                        if (!tableName.equals("INVALID")) {
                            results = (ArrayList<Object[]>) dataManager
                                    .getUniqueList(tableName, row.getLid(), row
                                            .getPe().toUpperCase(), row
                                            .getDur(), row.getTs()
                                            .toUpperCase(), row.getExt()
                                            .toUpperCase(), beginningTime,
                                            endingTime);
                            results.trimToSize();

                            if (results != null && results.size() > 0) {
                                /* loop through number of unique basis times */
                                /*
                                 * if list ALL basis TB is not pressed then loop
                                 * only ONCE
                                 */
                                if (listAllFcstChk.getSelection()) {
                                    count = results.size();
                                } else {
                                    count = 1;
                                }

                                for (int j = 0; j < count; j++) {
                                    if (entryNumber < MAX_TS_ON_LIST) {
                                        String str = String.format(
                                                "%-5s %2s %4s %2s %s %-19s",
                                                row.getLid(), row.getPe()
                                                        .toUpperCase(), row
                                                        .getDur(), row.getTs()
                                                        .toUpperCase(),
                                                row.getExt().toUpperCase(),
                                                HydroConstants.DATE_FORMAT
                                                        .format((Date) results
                                                                .get(j)[0]));
                                        modifiedTSList.add(str);

                                        TableItem item = new TableItem(
                                                topDataTable, SWT.NONE);
                                        item.setText(0, row.getLid());
                                        item.setText(1, row.getPe()
                                                .toUpperCase());
                                        item.setText(
                                                2,
                                                String.format("%4s",
                                                        row.getDur()));
                                        item.setText(3, row.getTs()
                                                .toUpperCase());
                                        item.setText(4, row.getExt()
                                                .toUpperCase());
                                        item.setText(5,
                                                HydroConstants.DATE_FORMAT
                                                        .format((Date) results
                                                                .get(j)[0]));

                                        row.setBasisTime(HydroConstants.DATE_FORMAT
                                                .format((Date) results.get(j)[0]));
                                        siteInfoList.add(row);
                                        entryNumber++;
                                    }

                                }
                            } else {
                                /* if NO basis times found */
                                if (entryNumber < MAX_TS_ON_LIST) {
                                    String str = String.format(
                                            "%-5s %2s %4s %2s %s ", row
                                                    .getLid(), row.getPe()
                                                    .toUpperCase(), row
                                                    .getDur(), row.getTs()
                                                    .toUpperCase(), row
                                                    .getExt().toUpperCase());
                                    modifiedTSList.add(str + "No Data");

                                    TableItem item = new TableItem(
                                            topDataTable, SWT.NONE);
                                    item.setText(0, row.getLid());
                                    item.setText(1, row.getPe().toUpperCase());
                                    item.setText(2,
                                            String.format("%4s", row.getDur()));
                                    item.setText(3, row.getTs().toUpperCase());
                                    item.setText(4, row.getExt().toUpperCase());
                                    item.setText(5, "No Data");

                                    entryNumber++;
                                    siteInfoList.add(row);
                                }
                            }
                        }
                    } else {
                        /*
                         * if an observed timeseries then just store in modified
                         * list
                         */
                        if (entryNumber < MAX_TS_ON_LIST) {
                            String str = String.format("%-5s %2s %4s %2s %s ",
                                    row.getLid(), row.getPe().toUpperCase(),
                                    row.getDur(), row.getTs().toUpperCase(),
                                    row.getExt().toUpperCase());
                            modifiedTSList.add(str);

                            TableItem item = new TableItem(topDataTable,
                                    SWT.NONE);
                            item.setText(0, row.getLid());
                            item.setText(1, row.getPe().toUpperCase());
                            item.setText(2, String.format("%4s", row.getDur()));
                            item.setText(3, row.getTs().toUpperCase());
                            item.setText(4, row.getExt().toUpperCase());

                            entryNumber++;
                            siteInfoList.add(row);
                        }
                    }
                }
            }
            for (TableColumn column : topDataTable.getColumns()) {
                column.pack();
            }

            topDataTable.setSelection(0);

            /* Find the selected types */
            for (int i = 0; i < siteInfoList.size(); i++) {
                SiteInfo siteInfo = siteInfoList.get(i);
                boolean selected = siteInfo.isSelected();
                if (selected) {
                    for (int j = 0; j < topDataTable.getItemCount(); j++) {
                        TableItem item = topDataTable.getItem(j);
                        ts = item.getText(3);
                        pe = item.getText(1);
                        if (pe.equals(siteInfo.getPe())
                                && Integer.parseInt(item.getText(2).trim()) == siteInfo
                                        .getDur()
                                && ts.equals(siteInfo.getTs())
                                && item.getText(4).equals(siteInfo.getExt())) {
                            topDataTable.setSelection(j);
                            break;
                        }
                    }
                    break;
                }
            }

            if (updateFlag) {
                topDataTable.setSelection(indexSelected);
            }
            topDataSelectionAction();
        } catch (Exception ve) {
            statusHandler.handle(Priority.PROBLEM, "Time Series Load", ve);
        }
    }

    /**
     * Get table data for the selection.
     */
    public void getDataForTable() {
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();

        String tableName = DbUtils.getTableName(pe, ts);
        boolean forecast = false;
        if (ts.toUpperCase().startsWith("C")
                || ts.toUpperCase().startsWith("F")) {
            forecast = true;
        }

        String myBasisTime = basisTime;
        if (myBasisTime != null && myBasisTime.equalsIgnoreCase("No Data")) {
            myBasisTime = prodBasisFmt.format(dummyTime.getTime());
        }

        try {
            String where = " where lid = '" + lid + "'";

            this.ratingRecordCount = dataManager.recordCount("Rating", where);

            tabularDataList = dataManager.getTabularData(tableName, lid, pe,
                    ts, dur, extremum, beginningTime, endingTime, myBasisTime,
                    forecast);
        } catch (ClassNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, "Getting Table Data: ", e);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Getting Table Data: ", e);
        }
    }

    /**
     * Use this method to extract the information that we will need to run the
     * data retrieval query. This has to be done outside of the function that
     * actually retrieves the data or we will end up with a
     * "Invalid Thread Access" error.
     */
    private void extractFormInformation() {
        int selectedIndex = topDataTable.getSelectionIndex();
        TableItem item = topDataTable.getItem(selectedIndex);

        bottomTable.removeAll();

        /* Get all selected parts for latter use. */

        lid = item.getText(0);
        pe = item.getText(1);
        dur = item.getText(2).trim();
        ts = item.getText(3);
        extremum = item.getText(4);
        basisTime = item.getText(5);
    }

    /**
     * Load the data list.
     */
    private synchronized void loadDataList() {
        updateStationLabel();
        updateSelectedLocInfoLabel();

        useProductTimeChk.setEnabled(true);
        useProductTimeChk.setSelection(true);

        if (tabularDataList == null || tabularDataList.size() == 0) {
            // if there is no forecast data, don't allow certain actions

            if (ts.toUpperCase().startsWith("C")
                    || ts.toUpperCase().startsWith("F")) {
                enableDataAttributes(true);
            } else {
                enableDataAttributes(false);
            }

            enableEditButtons(false);
            TableItem item = new TableItem(bottomTable, SWT.LEFT);
            item.setText(0, "No Data");
            bottomTable.setSelection(0);
            timeTF.setText("");
            valueTF.setText("");
            qcCbo.select(0);

        } else {
            bottomTable.removeAll();
            enableEditButtons(true);
            if (ts.toUpperCase().startsWith("C")
                    || ts.toUpperCase().startsWith("F")) {
                fcstTypSrcChk.setEnabled(true);
                fcstBasisTimeChk.setEnabled(true);
                fcstTypSrcChk.setSelection(false);
                fcstBasisTimeChk.setSelection(false);
            } else {
                enableDataAttributes(false);
            }

            double derivedValue;
            String revision;

            for (int i = 0; i < tabularDataList.size(); i++) {
                TabularData td = tabularDataList.get(i);
                if (td.getRevision() == 0) {
                    revision = "F";
                } else {
                    revision = "T";
                }

                TableItem item = new TableItem(bottomTable, SWT.LEFT);

                if (pe.equals("HG") || pe.equals("HT")
                        && this.ratingRecordCount > 1) {
                    if (td.getValue() == HydroConstants.MISSING_VALUE) {
                        derivedValue = HydroConstants.MISSING_VALUE;
                    } else {
                        derivedValue = StageDischargeUtils.stage2discharge(
                                td.getLid(), td.getValue());
                    }
                    item.setText(0, String.format("%6.2f", td.getValue()));
                    item.setText(1, String.format("%7.0f", derivedValue));
                    item.setText(3, tabularFormat.format(td.getObsTime()));
                    item.setText(4, revision);
                    item.setText(5, td.getShefQualCode());
                    item.setText(6,
                            TimeSeriesUtil.buildQcSymbol(td.getQualityCode()));
                    item.setText(7, td.getProductId());
                    item.setText(8, tabularFormat.format(td.getProductTime()));
                    item.setText(9, tabularFormat.format(td.getPostingTime()));

                } else if (pe.equals("QR") && this.ratingRecordCount > 1) {
                    if (td.getValue() == HydroConstants.MISSING_VALUE) {
                        derivedValue = HydroConstants.MISSING_VALUE;
                    } else {
                        derivedValue = StageDischargeUtils.discharge2stage(
                                td.getLid(), td.getValue());
                    }
                    item.setText(0, String.format("%8.2f", td.getValue()));
                    item.setText(2, String.format("%8.0f", derivedValue));
                    item.setText(3, tabularFormat.format(td.getObsTime()));
                    item.setText(4, revision);
                    item.setText(5, td.getShefQualCode());
                    item.setText(6,
                            TimeSeriesUtil.buildQcSymbol(td.getQualityCode()));
                    item.setText(7, td.getProductId());
                    item.setText(8, tabularFormat.format(td.getProductTime()));
                    item.setText(9, tabularFormat.format(td.getPostingTime()));
                } else {
                    item.setText(0, String.format("%8.2f", td.getValue()));
                    item.setText(3, tabularFormat.format(td.getObsTime()));
                    item.setText(4, revision);
                    item.setText(5, td.getShefQualCode());
                    item.setText(6,
                            TimeSeriesUtil.buildQcSymbol(td.getQualityCode()));
                    item.setText(7, td.getProductId());
                    item.setText(8, tabularFormat.format(td.getProductTime()));
                    item.setText(9, tabularFormat.format(td.getPostingTime()));
                }

                /*
                 * load the list of modified times series and select the first
                 * one in the list.
                 */
                TabularData tdSelection = tabularDataList.get(0);
                bottomTable.setSelection(0);
                valueTF.setText(String.valueOf(tdSelection.getValue()));
                timeTF.setText(dbFormat.format(tdSelection.getObsTime()));
                qcCbo.setData(TimeSeriesUtil.buildQcSymbol(tdSelection
                        .getQualityCode()));
            }
        }

        for (TableColumn column : bottomTable.getColumns()) {
            column.pack();
        }

        /* determine which columns to hide. */
        if (pe.equals("HG") || pe.equals("HT") && this.ratingRecordCount > 1) {
            bottomTable.getColumn(2).setWidth(0);
        } else if (pe.equals("QR") && this.ratingRecordCount > 1) {
            bottomTable.getColumn(1).setWidth(0);
        } else {
            bottomTable.getColumn(1).setWidth(0);
            bottomTable.getColumn(2).setWidth(0);
        }

        shell.setCursor(null);
        this.topDataTable.setEnabled(true);
        this.parentDialog.enableTableButton();
        this.parentDialog.enableBothButton();
    }

    /**
     * Update/Insert the edited value into the database
     */
    private void updateInsertValue() {

        TabularData td = null;
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        Date now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        String selectCheck = bottomTable.getItem(
                bottomTable.getSelectionIndex()).getText(0);

        if (bottomTable.getSelectionIndex() == -1) {
            td = tabularDataList.get(0);
        } else {
            if (!selectCheck.equalsIgnoreCase("No Data")) {
                td = tabularDataList.get(bottomTable.getSelectionIndex());
            }
        }

        if (tabularDataList.size() != 0) {
            oldValue = td.getValue();
        }

        indexSelected = topDataTable.getSelectionIndex();
        String tablename = DbUtils.getTableName(pe, ts);
        DataRecord dr = new DataRecord();

        Date newDateTime;
        String newDataTime = timeTF.getText();
        try {
            newDateTime = dbFormat.parse(newDataTime);
            dr.setObsTime(newDateTime);
        } catch (ParseException e) {
            MessageDialog
                    .openError(shell, "Invalid date/time",
                            "Invalid date/time entered.\nRequired format:  01-01-2002 12:00:00");
            return;
        }

        /* code to update an observation */
        if (ts.toUpperCase().startsWith("R")
                || ts.toUpperCase().startsWith("P")) {
            /* set the update/add structure with data which doesn't change */

            dr.setLid(lid);
            dr.setPe(pe);
            dr.setDur(Integer.parseInt(dur));
            dr.setTs(ts);
            dr.setExt(extremum);
            /* set posting time to current time */
            dr.setPostingTime(now);

            /* set the update structure with data from the original entry */

            if (tabularDataList.size() != 0) {

                dr.setProductId(td.getProductId());
                dr.setProductTime(td.getProductTime());
                dr.setValue(Double.parseDouble(valueTF.getText()));

                long qualityCode;
                if (qcCbo.getItem(qcCbo.getSelectionIndex()).equals("Good")) {
                    qualityCode = TimeSeriesUtil.setQcCode(QC_MANUAL_PASSED,
                            td.getQualityCode());
                } else if (qcCbo.getItem(qcCbo.getSelectionIndex()).equals(
                        "Bad")) {
                    qualityCode = TimeSeriesUtil.setQcCode(QC_MANUAL_FAILED,
                            td.getQualityCode());
                } else {
                    qualityCode = TimeSeriesUtil.setQcCode(QC_MANUAL_QUEST,
                            td.getQualityCode());
                }

                dr.setQualityCode(qualityCode);

            } else { /* if no data in list, set defaults values. */

                dr.setProductId(INSERT_PROD_ID);
                dr.setValue(Double.parseDouble(valueTF.getText()));
                dr.setQualityCode(TimeSeriesUtil.setQcCode(QC_MANUAL_PASSED, 0));
                dr.setRevision(0);
                newDateTime = now;

                try {
                    Date defaultDate = dbFormat.parse(productTimeLbl.getText());
                    dr.setProductTime(defaultDate);
                } catch (ParseException e) {
                    statusHandler.error("Parse Error", e);
                }

            }
            /* always set the shefQualCode with a "M" for Manual edit */
            dr.setShefQualCode("M");

            /* do the update */
            String where;
            String sql;
            where = createUpdDelWhereObs(dr);
            /* if toggle button ProductTime/ID is checked */

            if (useProductTimeChk.getSelection()) {
                try {
                    Date useProductDate = dbFormat.parse(productTimeLbl
                            .getText());
                    dr.setProductTime(useProductDate);
                    dr.setProductId(INSERT_PROD_ID);

                } catch (ParseException e) {
                    statusHandler.handle(Priority.PROBLEM, "Parse Error: " + e);
                }
            }

            try {
                long recordCount = dataManager.recordCount(tablename, where);
                /* already a record with same key do an update */
                if (recordCount == 1) {
                    dr.setRevision(1);
                    sql = "update "
                            + tablename
                            + " set value = "
                            + dr.getValue()
                            + ", quality_code = "
                            + dr.getQualityCode()
                            + ", obstime = '"
                            + HydroConstants.DATE_FORMAT.format(newDateTime)
                            + "', postingtime = '"
                            + HydroConstants.DATE_FORMAT.format(dr
                                    .getPostingTime())
                            + "', product_id = '"
                            + dr.getProductId()
                            + "', "
                            + "producttime = '"
                            + HydroConstants.DATE_FORMAT.format(dr
                                    .getProductTime()) + "', revision = "
                            + dr.getRevision() + ", shef_qual_code = '"
                            + dr.getShefQualCode() + "' ";

                    dataManager.update(sql + where);

                    /*
                     * Insert original data into rejected obs table ans set
                     * revision to 1
                     */
                    dataManager.insertRejectedData(dr);
                    dr.setRevision(1);

                } else { /* if no record, insert a new one and set revision to 0 */

                    dr.setRevision(0);
                    dataManager.addDataRecord(tablename, dr);

                }
                scheduleDataRetrieval();

            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, "", e);
            }
        }

        /* code to insert or update a forecast */
        if (ts.startsWith("F") || ts.startsWith("C")) {
            /*
             * set the update/add structure with data which doesn't change.
             * although the type-source may be changed...
             */

            dr.setLid(lid);
            dr.setPe(pe);
            dr.setDur(Integer.parseInt(dur));
            dr.setTs(ts);
            dr.setExt(extremum);
            dr.setPostingTime(now);

            /*
             * set the update structure with data from the original entry note
             * that the basistime may be changed below...
             */

            if (tabularDataList.size() != 0) {
                /*
                 * read data from the value and time widgets and replace in
                 * structure
                 */
                dr.setProductId(td.getProductId());
                dr.setProductTime(td.getProductTime());
                dr.setQualityCode(td.getQualityCode());
                dr.setValue(Double.parseDouble(valueTF.getText()));
                dr.setPostingTime(now);

                long qualityCode;
                if (qcCbo.getItem(qcCbo.getSelectionIndex()).equals("Good")) {
                    qualityCode = TimeSeriesUtil.setQcCode(QC_MANUAL_PASSED,
                            td.getQualityCode());
                } else if (qcCbo.getItem(qcCbo.getSelectionIndex()).equals(
                        "Bad")) {
                    qualityCode = TimeSeriesUtil.setQcCode(QC_MANUAL_FAILED,
                            td.getQualityCode());
                } else {
                    qualityCode = TimeSeriesUtil.setQcCode(QC_MANUAL_QUEST,
                            td.getQualityCode());
                }

                dr.setQualityCode(qualityCode);
                if (fcstBasisTimeChk.getSelection()) {
                    dr.setBasisTime(fcstBasisTimeLbl.getText());
                } else {
                    dr.setBasisTime(basisTime);
                }

                if (fcstTypSrcChk.getSelection() && fcstAtt != null) {
                    dr.setTs(fcstAtt.getSelectedTS());
                }

            } else {
                /*
                 * if no data in list, set defaults for product info and for the
                 * type source
                 */

                dr.setProductId(INSERT_PROD_ID);
                dr.setValue(Double.parseDouble(valueTF.getText()));
                dr.setQualityCode(TimeSeriesUtil.setQcCode(QC_MANUAL_PASSED, 0));

                String newDefaultTime = timeTF.getText();
                try {
                    newDateTime = dbFormat.parse(newDefaultTime);
                    dr.setProductTime(newDateTime);
                    dr.setBasisTime(newDefaultTime);
                } catch (ParseException e) {
                    statusHandler.handle(Priority.PROBLEM, "Parse error: ", e);
                }

                newDateTime = now;

                /* set posting time to current time */
                dr.setPostingTime(now);

            }

            /* always set the shefQualCode with a "M" for Manual edit */
            dr.setShefQualCode("M");

            /* Check if the record exists already */
            String where = createUpdDelWhereFcst(td, dr);
            /* use producTime/ID if checked */
            if (useProductTimeChk.getSelection()) {
                try {
                    Date useProductDate = dbFormat.parse(productTimeLbl
                            .getText());
                    dr.setProductTime(useProductDate);
                    dr.setProductId(INSERT_PROD_ID);

                } catch (ParseException e) {
                    statusHandler.handle(Priority.PROBLEM, "Parse error: ", e);
                }
            }

            /*
             * use the info for BasisTime and Type Source if button pressed.
             * these two fields are part of the data key. use a type source if a
             * valid one has been selected.
             */

            try {
                if (dataManager.recordCount(tablename, where) == 0) {
                    /* add a new record and set revision to 0 */
                    dr.setRevision(0);
                    int status = dataManager.addDataRecord(tablename, dr);
                    if (status != 1) {
                        statusHandler.handle(Priority.PROBLEM, "Data Query:"
                                + " Error inserting forecast record.");
                    } else { /* successful add of a new forecast record */

                        scheduleDataRetrieval();
                    }

                } else {
                    /* already a record with same key */
                    /* place a copy of this record in the Rejected Data table */
                    dr.setValue(oldValue);
                    int status = 1;
                    if (!ts.startsWith("F")) {
                        status = dataManager.insertRejectedData(dr);
                    }
                    if (status != 1) {
                        // error
                        statusHandler.handle(Priority.PROBLEM, "Data Query:"
                                + " Error inserting rejected record.");
                    }

                    dr.setRevision(1);
                    dr.setValue(Double.parseDouble(valueTF.getText()));

                    String sql = null;
                    if (useProductTimeChk.getSelection()) {
                        sql = "update "
                                + tablename
                                + " set value = "
                                + dr.getValue()
                                + ", quality_code = "
                                + dr.getQualityCode()
                                + ", postingtime = '"
                                + HydroConstants.DATE_FORMAT.format(dr
                                        .getPostingTime())
                                + "', producttime = '"
                                + HydroConstants.DATE_FORMAT.format(dr
                                        .getProductTime()) + "', revision = "
                                + dr.getRevision() + ", " + "product_id = '"
                                + dr.getProductId() + "', " + " basistime = '"
                                + dr.getBasisTime() + "', shef_qual_code = '"
                                + dr.getShefQualCode() + "' ";
                    } else {

                        sql = "update "
                                + tablename
                                + " set value = "
                                + dr.getValue()
                                + ", quality_code = "
                                + dr.getQualityCode()
                                + ", postingtime = '"
                                + HydroConstants.DATE_FORMAT.format(dr
                                        .getPostingTime())
                                + "', producttime = '"
                                + HydroConstants.DATE_FORMAT
                                        .format(newDateTime) + "', revision = "
                                + dr.getRevision() + ", "
                                + "shef_qual_code = '" + dr.getShefQualCode()
                                + "' ";

                    }

                    status = dataManager.update(sql + where);

                    if (status != 1) {
                        statusHandler.handle(Priority.PROBLEM, "Data Query:"
                                + " Error inserting forecast record.");
                    }

                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, "Data Query:"
                        + " Error inserting forecast data.", e);
            }

            /* call Load Max Forecast if update or insert of H or Q PE's */
            if (pe.toUpperCase().startsWith("H")
                    || pe.toUpperCase().startsWith("Q")) {
                try {
                    LoadMaxFcst.loadMaxFcstItem(lid, pe, ts);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM, "Data Query:"
                            + " Error inserting max forecast record.", e);
                }
            }
        } // end if fcst
        else {
            /* code for inserting/updating latestobsvalue table, if not forecast */
            updateInsertLatestObsValue(td, dataManager, now, dr, newDateTime);
        }
        /* reload list of timeseries */
        scheduleDataRetrieval();
        tabularLoadTimeseries();
    }

    /**
     * Update/Insert the edited value into the latestobsvalue table, if newer
     * than the existing record.
     * 
     * @param tabularData
     *            tabular data.
     * @param dataManager
     *            data manager.
     * @param postingTime
     *            posting time to use.
     * @param dataRecord
     *            data record to use/update with information.
     * @param obsTime
     *            observation time to use.
     */
    private void updateInsertLatestObsValue(TabularData tabularData,
            TimeSeriesDataManager dataManager, Date postingTime,
            DataRecord dataRecord, Date obsTime) {
        String tablename = "latestobsvalue";

        /* set the update/add structure with data which doesn't change */
        dataRecord.setLid(lid);
        dataRecord.setPe(pe);
        dataRecord.setDur(Integer.parseInt(dur));
        dataRecord.setTs(ts);
        dataRecord.setExt(extremum);
        /* set posting time to current time */
        dataRecord.setPostingTime(postingTime);

        /* set the update structure with data from the original entry */

        if (tabularDataList.size() != 0) {

            dataRecord.setProductId(tabularData.getProductId());
            dataRecord.setProductTime(tabularData.getProductTime());
            dataRecord.setValue(Double.parseDouble(valueTF.getText()));

            long qualityCode;
            if (qcCbo.getItem(qcCbo.getSelectionIndex()).equals("Good")) {
                qualityCode = TimeSeriesUtil.setQcCode(QC_MANUAL_PASSED,
                        tabularData.getQualityCode());
            } else if (qcCbo.getItem(qcCbo.getSelectionIndex()).equals("Bad")) {
                qualityCode = TimeSeriesUtil.setQcCode(QC_MANUAL_FAILED,
                        tabularData.getQualityCode());
            } else {
                qualityCode = TimeSeriesUtil.setQcCode(QC_MANUAL_QUEST,
                        tabularData.getQualityCode());
            }

            dataRecord.setQualityCode(qualityCode);

        } else { /* if no data in list, set defaults values. */

            dataRecord.setProductId(INSERT_PROD_ID);
            dataRecord.setValue(Double.parseDouble(valueTF.getText()));
            dataRecord.setQualityCode(TimeSeriesUtil.setQcCode(
                    QC_MANUAL_PASSED, 0));
            dataRecord.setRevision(0);
            obsTime = postingTime;

            try {
                Date defaultDate = dbFormat.parse(productTimeLbl.getText());
                dataRecord.setProductTime(defaultDate);
            } catch (ParseException e) {
                statusHandler.error("Parse Error: Could not parse ["
                        + productTimeLbl.getText() + "]", e);
            }

        }
        /* always set the shefQualCode with a "M" for Manual edit */
        dataRecord.setShefQualCode("M");

        /* do the update */
        String whereExists = createWhereExistsLatestObsValue(dataRecord);
        String whereOlderExists = createOlderThanWhereLatestObsValue(dataRecord);
        /* if toggle button ProductTime/ID is checked */

        if (useProductTimeChk.getSelection()) {
            try {
                Date useProductDate = dbFormat.parse(productTimeLbl.getText());
                dataRecord.setProductTime(useProductDate);
                dataRecord.setProductId(INSERT_PROD_ID);

            } catch (ParseException e) {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Parse Error: Could not parse ["
                                + productTimeLbl.getText() + "]", e);
            }
        }

        try {
            long existingRecordCount = dataManager.recordCount(tablename,
                    whereExists);
            long olderRecordCount = dataManager.recordCount(tablename,
                    whereOlderExists);
            /* already a record with same key that is older */
            if (olderRecordCount == 1) {
                dataRecord.setRevision(1);
                String updateSQL = "update "
                        + tablename
                        + " set value = "
                        + dataRecord.getValue()
                        + ", quality_code = "
                        + dataRecord.getQualityCode()
                        + ", obstime = '"
                        + HydroConstants.DATE_FORMAT.format(obsTime)
                        + "', postingtime = '"
                        + HydroConstants.DATE_FORMAT.format(dataRecord
                                .getPostingTime())
                        + "', product_id = '"
                        + dataRecord.getProductId()
                        + "', "
                        + "producttime = '"
                        + HydroConstants.DATE_FORMAT.format(dataRecord
                                .getProductTime()) + "', revision = "
                        + dataRecord.getRevision() + ", shef_qual_code = '"
                        + dataRecord.getShefQualCode() + "' ";

                String query = updateSQL + whereExists;
                try {
                    dataManager.update(query);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Failed to execute query [" + query + "]", e);
                }
            } else if (existingRecordCount == 0) {
                /*
                 * if no record, insert a new one and set revision to 0
                 */
                try {
                    dataRecord.setRevision(0);
                    dataManager.addDataRecord(tablename, dataRecord);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Failed to add data record to table [" + tablename
                                    + "]", e);
                }
            }
            scheduleDataRetrieval();

        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to get record count for table [" + tablename + "]",
                    e);
        }
    }

    /**
     * Sets just the value of the selected row to missing.
     */
    private void setMissing() {

        String checkSelect = bottomTable.getItem(
                bottomTable.getSelectionIndex()).getText(0);

        if (bottomTable.getSelectionCount() == 0
                || checkSelect.equalsIgnoreCase("No Data")) {

            return;
        }
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        Date postTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                .getTime();

        String tablename = DbUtils.getTableName(pe, ts);

        DataRecord dr = new DataRecord();
        int[] selectionIndices = bottomTable.getSelectionIndices();

        StringBuilder sb = new StringBuilder();
        ArrayList<DataRecord> dataRecordList = new ArrayList<DataRecord>();

        for (int selectionIndice : selectionIndices) {
            TabularData td = tabularDataList.get(selectionIndice);

            /* set the update structure with data which doesn't change */
            dr = new DataRecord();
            dr.setLid(lid);
            dr.setPe(pe);
            dr.setDur(Integer.parseInt(dur));
            dr.setTs(ts);
            dr.setExt(extremum);
            dr.setPostingTime(postTime);

            /* set the update structure with data from the original entry */
            dr.setProductId(td.getProductId());
            dr.setProductTime(td.getProductTime());
            dr.setObsTime(td.getObsTime());

            /* Set value to MISSING */
            dr.setValue(HydroConstants.MISSING_VALUE);

            /* set the shefQualCode with a "M" for Manual edit */
            dr.setShefQualCode("M");

            dr.setQualityCode(TimeSeriesUtil.setQcCode(QC_MANUAL_PASSED,
                    td.getQualityCode()));

            dr.setRevision((short) 1);

            String sql = "update " + tablename + " set value = "
                    + HydroConstants.MISSING_VALUE
                    + ", revision= 1, shef_qual_code = 'M' , quality_code= '"
                    + dr.getQualityCode() + "' " + ", postingtime= '"
                    + HydroConstants.DATE_FORMAT.format(postTime) + "'  ";

            /* code to update an observation to MISSING */
            if (ts.toUpperCase().startsWith("R")
                    || ts.toUpperCase().startsWith("P")) {
                sb.append(sql);
                String where = createUpdDelWhereObs(dr);
                sb.append(where);
            }

            /* code to update a forecast to MISSING */
            if (ts.toUpperCase().startsWith("F")
                    || ts.toUpperCase().startsWith("C")) {

                dr.setBasisTime(basisTime);

                sb.append(sql);
                String where = createUpdDelWhereFcst(td, dr);
                sb.append(where);
            }
            dataRecordList.add(dr);
        }

        try {
            int status = dataManager.update(sb.toString());
            if (status > 0) {
                /*
                 * Data updated successfully Add data record to rejected data
                 */
                status = dataManager.insertRejectedData(dataRecordList);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Data Query:"
                    + " Error updating records.", e);
        }

        /* call Load Max Forecast if update of H or Q PE's */
        if (ts.toUpperCase().startsWith("F")
                || ts.toUpperCase().startsWith("C")) {
            try {
                LoadMaxFcst.loadMaxFcstItem(lid, pe, ts);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, "Data Query:"
                        + " Error loading Max Forecast Table.", e);
            }

        }

        scheduleDataRetrieval();
    }

    /* From tabular_show.c */
    /**
     * Delete the selected record(s) from the database
     */
    private void deleteData() {

        String checkSelect = bottomTable.getItem(
                bottomTable.getSelectionIndex()).getText(0);

        if (bottomTable.getSelectionCount() == 0
                || checkSelect.equalsIgnoreCase("No Data")) {
            return;
        }

        boolean choice = MessageDialog.openConfirm(shell,
                "Delete Confirmation", "Do you wish to delete this record?");

        /* If true then delete the record */
        if (choice) {
            TimeSeriesDataManager dataManager = TimeSeriesDataManager
                    .getInstance();
            ArrayList<String> queryList = new ArrayList<String>();
            ArrayList<DataRecord> dataRecordList = new ArrayList<DataRecord>();

            String tablename = DbUtils.getTableName(pe, ts);

            /* Loop through the data values selected and delete each one */
            int[] selectionIndices = bottomTable.getSelectionIndices();

            for (int selectionIndice : selectionIndices) {
                TabularData td = tabularDataList.get(selectionIndice);
                DataRecord dr = new DataRecord();

                dr.setLid(lid);
                dr.setPe(pe.toUpperCase());
                dr.setDur(Integer.parseInt(dur));
                dr.setTs(ts);
                dr.setExt(extremum);
                dr.setPostingTime(td.getPostingTime());

                /* set the update structure with data from the original entry */
                dr.setProductId(td.getProductId());
                dr.setProductTime(td.getProductTime());
                dr.setObsTime(td.getObsTime());
                dr.setShefQualCode(td.getShefQualCode());

                /********** This part is for OBSERVED or PROCCESSED data **********/
                if (ts.toUpperCase().startsWith("R")
                        || ts.toUpperCase().startsWith("P")) {
                    String where = createUpdDelWhereObs(dr);
                    queryList.add("delete from " + tablename + " " + where);

                    /* if precip then delete from curprecip table as well */
                    if (dr.getPe().startsWith("P") && !dr.getPe().endsWith("A")
                            && !dr.getPe().endsWith("D")
                            && !dr.getPe().endsWith("E")
                            && !dr.getPe().endsWith("L")) {
                        if (dr.getPe().endsWith("P")) {
                            queryList.add("delete from curpp " + where);
                            // dataManager.deleteRecord("curpp", where);
                        }
                    }

                    dataRecordList.add(dr);
                    /* copy the deleted record to RejectedData */

                }

                /********** This part is for FORECAST data **********/
                if (ts.toUpperCase().startsWith("F")
                        || ts.toUpperCase().startsWith("C")) {

                    dr.setBasisTime(basisTime);
                    /* Delete all rows that have been selected */
                    String where = createUpdDelWhereFcst(td, dr);
                    queryList.add("delete from " + tablename + " " + where);

                    /* copy the deleted record to RejectedData */
                    dataRecordList.add(dr);
                }

                /********** This part is for latestobsvalue data ****/
                String where = createUpdDelWhereObs(dr);
                queryList.add("delete from latestobsvalue" + " " + where);
            }

            // execute the queries
            try {
                dataManager.deleteRecords(queryList);
                dataManager.insertRejectedData(dataRecordList);
            } catch (VizException e1) {
                statusHandler.handle(Priority.PROBLEM, "Data Query:"
                        + " Error Deleting records.", e1);
            }

            /*
             * if height or discharge then calculate new RiverStatus as well
             */
            if (pe.toUpperCase().startsWith("H")
                    || pe.toUpperCase().startsWith("Q")) {
                String command = String.format(
                        "load_obs_river('%s', '%s', '%s')", lid, pe, ts);

                try {
                    dataManager.execFunction(command);
                } catch (VizException e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Data Query:"
                                            + " An error occurred executing load_obs_river function",
                                    e);
                }
            }
            if (ts.toUpperCase().startsWith("F")
                    || ts.toUpperCase().startsWith("C")) {
                if (pe.toUpperCase().startsWith("H")
                        || pe.toUpperCase().startsWith("Q")) {

                    try {
                        LoadMaxFcst.loadMaxFcstItem(lid, pe, ts);
                    } catch (VizException e) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Data Query:"
                                                + " An error occurred executing loadMaxFcst function",
                                        e);
                    }
                }
            }
        }
        scheduleDataRetrieval();
    }

    /**
     * Set the QC value for the selected record
     */
    private void setQC() {
        String checkSelect = bottomTable.getItem(
                bottomTable.getSelectionIndex()).getText(0);

        if (bottomTable.getSelectionCount() == 0
                || checkSelect.equalsIgnoreCase("No Data")) {
            return;
        }

        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        Date postTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                .getTime();

        String tablename = DbUtils.getTableName(pe, ts);

        int[] selectionIndices = bottomTable.getSelectionIndices();

        DataRecord dr = new DataRecord();

        /* code to update an observation qc info */
        for (int selectionIndice : selectionIndices) {
            TabularData td = tabularDataList.get(selectionIndice);

            /* set the update structure with data which doesn't change */
            dr.setLid(lid);
            dr.setPe(pe.toUpperCase());
            dr.setDur(Integer.parseInt(dur));
            dr.setTs(ts.toUpperCase());
            dr.setExt(extremum.toUpperCase());
            dr.setPostingTime(postTime);

            /* set the update structure with data from the original entry */
            dr.setProductId(td.getProductId());
            dr.setProductTime(td.getProductTime());
            dr.setObsTime(td.getObsTime());

            /* set the shefQualCode with a "M" for Manual edit */
            dr.setShefQualCode("M");
            dr.setRevision((short) 1);

            long qualityCode;
            if (qcCbo.getItem(qcCbo.getSelectionIndex()).equals("Good")) {
                qualityCode = TimeSeriesUtil.setQcCode(QC_MANUAL_PASSED,
                        td.getQualityCode());
            } else if (qcCbo.getItem(qcCbo.getSelectionIndex()).equals("Bad")) {
                qualityCode = TimeSeriesUtil.setQcCode(QC_MANUAL_FAILED,
                        td.getQualityCode());
            } else {
                qualityCode = TimeSeriesUtil.setQcCode(QC_MANUAL_QUEST,
                        td.getQualityCode());
            }
            dr.setQualityCode(qualityCode);

            if (ts.toUpperCase().startsWith("R")
                    || ts.toUpperCase().startsWith("P")) {

                /* do the update */
                String where = createUpdDelWhereObs(dr);
                String sql = "update "
                        + tablename
                        + " set quality_code= "
                        + dr.getQualityCode()
                        + ",revision= "
                        + dr.getRevision()
                        + ", postingtime= '"
                        + HydroConstants.DATE_FORMAT
                                .format(dr.getPostingTime()) + "'  ";
                int status;
                try {
                    status = dataManager.update(sql + where);
                    if (status != 1) {
                        throw new VizException("Error Updating QC value");
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Updating QC value: ", e);
                }
            }

            if (ts.toUpperCase().startsWith("F")
                    || ts.toUpperCase().startsWith("C")) {

                dr.setBasisTime(basisTime);

                /* do an update */

                String where = createUpdDelWhereFcst(td, dr);

                String sql = "update "
                        + tablename
                        + " set quality_code = "
                        + dr.getQualityCode()
                        + ", revision= "
                        + dr.getRevision()
                        + ", postingtime= '"
                        + HydroConstants.DATE_FORMAT
                                .format(dr.getPostingTime()) + "'  ";

                int status;
                try {
                    status = dataManager.update(sql + where);
                    if (status > 0) {
                        /*
                         * Data updated successfully Add data record to rejected
                         * data
                         */
                        status = dataManager.insertRejectedData(dr);
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Inserted Rejected Data: ", e);
                }

            }
        }
        scheduleDataRetrieval();
    }

    /**
     * Update the edit widgets upon selection of a data record
     */
    private void handleBottomTableSelection() {
        int index = bottomTable.getSelectionIndex();
        if (tabularDataList.size() > 0) {
            TabularData td = tabularDataList.get(index);
            valueTF.setText(String.valueOf(td.getValue()));
            timeTF.setText(dbFormat.format(td.getObsTime()));

            if (TimeSeriesUtil.buildQcSymbol(td.getQualityCode()) == "G") {
                qcCbo.select(0);
            } else if (TimeSeriesUtil.buildQcSymbol(td.getQualityCode()) == "B") {
                qcCbo.select(2);
            } else {
                qcCbo.select(1);
            }
            /* set default value when there is no Data */
        } else {
            double noDatavalue = HydroConstants.MISSING_VALUE;
            enableEditButtons(true);
            timeTF.setText(prodBasisFmt.format(dummyTime.getTime()));
            valueTF.setText(String.valueOf(noDatavalue));
        }

    }

    /**
     * Save the table data in a text file
     */
    private void saveTable() {
        String text = createTableText();
        FileDialog dialog = new FileDialog(shell, SWT.SAVE);
        String filename = dialog.open();
        if (filename == null) {
            return;
        }

        try (BufferedWriter out = new BufferedWriter(new FileWriter(filename))) {
            out.write(text);
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Saving Table: ", e);
        }
    }

    /**
     * Handle the print table selection
     */
    private void sendTableToPrinter() {
        final String text = createTableText();
        if (text != null) {
            PrintDialog dialog = new PrintDialog(shell, SWT.NONE);
            PrinterData data = dialog.open();

            if (data == null) {
                return;
            }

            printer = new Printer(data);

            /*
             * Do the printing in a background thread so that spooling does not
             * freeze the UI.
             */
            Thread printingThread = new Thread("PrintTable") {
                @Override
                public void run() {
                    print(printer, text);
                    printer.dispose();
                }
            };
            printingThread.start();
        }
    }

    /**
     * Send the text to the printer
     * 
     * @param printer
     *            The printer
     * @param text
     *            The text to print
     */
    private void print(Printer printer, String text) {
        if (printer.startJob("Text")) {
            Rectangle clientArea = printer.getClientArea();
            Rectangle trim = printer.computeTrim(0, 0, 0, 0);
            Point dpi = printer.getDPI();
            // one inch from left side of paper
            leftMargin = dpi.x + trim.x;
            // one inch from right side of paper
            rightMargin = clientArea.width - dpi.x + trim.x + trim.width;
            // one inch from top edge of paper
            topMargin = dpi.y + trim.y;
            // one inch from bottom edge of paper
            bottomMargin = clientArea.height - dpi.y + trim.y + trim.height;

            // Create a buffer for computing tab width.
            int tabSize = 4; // is tab width a user setting in your UI?
            StringBuffer tabBuffer = new StringBuffer(tabSize);
            for (int i = 0; i < tabSize; i++) {
                tabBuffer.append(' ');
            }
            String tabs = tabBuffer.toString();

            /*
             * Create printer GC, and create and set the printer font &
             * foreground color.
             */
            gc = new GC(printer);

            Font printerFont = new Font(printer, "Monospace", 8, SWT.NORMAL);

            Color printerForegroundColor = new Color(printer, new RGB(0, 0, 0));
            Color printerBackgroundColor = new Color(printer, new RGB(255, 255,
                    255));

            gc.setFont(printerFont);
            gc.setForeground(printerForegroundColor);
            gc.setBackground(printerBackgroundColor);
            tabWidth = gc.stringExtent(tabs).x;
            lineHeight = gc.getFontMetrics().getHeight();

            /* Print text to current gc using word wrap */
            printText(text);

            printer.endJob();

            /* Cleanup graphics resources used in printing */
            printerFont.dispose();
            printerForegroundColor.dispose();
            printerBackgroundColor.dispose();
            gc.dispose();
        }
    }

    /**
     * Create the text to be saved/printed
     * 
     * @return the formated text
     */
    private String createTableText() {
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        boolean ratingCurveExists = false;
        double derivedValue = HydroConstants.MISSING_VALUE;
        String timeValue = null;
        String prodTime = null;
        String postTime = null;

        StringBuilder sb = new StringBuilder();
        sb.append("Beginning Time(Z):   " + dbFormat.format(beginningTime)
                + "\n");
        sb.append("Ending Time(Z):      " + dbFormat.format(endingTime) + "\n");

        String myBasisTime = basisTime;
        if (myBasisTime != null && myBasisTime.equalsIgnoreCase("No Data")) {
            myBasisTime = "";
        }

        sb.append("Station Identifier:  " + lid + "\n");
        sb.append("Physical Element:    " + pe + "\n");
        sb.append("Duration:            " + dur + "\n");
        sb.append("SHEF Type Source:    " + ts + "\n");
        sb.append("SHEF Extremum:       " + extremum + "\n");

        if (ts.startsWith("F") || ts.startsWith("C")) {
            sb.append("Basis Time(Z):       " + myBasisTime + "\n");
        }

        String where = " where lid = '" + lid.toUpperCase() + "'";

        try {
            if ((pe.equals("HG") || pe.equals("HT"))
                    && dataManager.recordCount("Rating", where) > 1) {
                sb.append("\n");
                sb.append("          Derived             R S Q\n");
                sb.append("  Value    Flow     Time(Z)   V Q C  Product      Time       Posted\n");
                sb.append("-------- -------- ----------- - - - ---------- ----------- -----------\n");
                ratingCurveExists = true;
            } else if (pe.equals("QR")
                    && dataManager.recordCount("Rating", where) > 1) {
                sb.append("\n");
                sb.append("          Derived             R S Q\n");
                sb.append("  Value    Stage    Time(Z)   V Q C  Product      Time       Posted\n");
                sb.append("-------- -------- ----------- - - - ---------- ----------- -----------\n");
                ratingCurveExists = true;
            } else {
                sb.append("\n");
                sb.append("                     R S Q\n");
                sb.append("  Value    Time(Z)   V Q C  Product      Time       Posted\n");
                sb.append("-------- ----------- - - - ---------- ----------- -----------\n");
                ratingCurveExists = false;
            }

            /*
             * For both observed and Forecast data
             */
            if (tabularDataList.size() > 0) {
                for (TabularData data : tabularDataList) {
                    if (ts.startsWith("F") || ts.startsWith("C")) {
                        timeValue = tabularFormat.format(data.getObsTime());
                    } else {
                        timeValue = tabularFormat.format(data.getObsTime());
                    }
                    prodTime = tabularFormat.format(data.getProductTime());
                    postTime = tabularFormat.format(data.getPostingTime());
                    String qcSymbol = TimeSeriesUtil.buildQcSymbol(data
                            .getQualityCode());

                    String revision;
                    if (data.getRevision() == 1) {
                        revision = "T";
                    } else {
                        revision = "F";
                    }

                    if ((pe.equals("HG") || pe.equals("HT"))
                            && ratingCurveExists) {
                        if (data.getValue() == HydroConstants.MISSING_VALUE) {
                            derivedValue = HydroConstants.MISSING_VALUE;
                        } else {
                            derivedValue = StageDischargeUtils.stage2discharge(
                                    lid, data.getValue());
                        }
                        sb.append(String.format(
                                "%8.2f %8.0f %11s %s %1s %1s %10s %11s %11s\n",
                                data.getValue(), derivedValue, timeValue,
                                revision, data.getShefQualCode(), qcSymbol,
                                data.getProductId(), prodTime, postTime));
                    } else if (pe.equals("QR") && ratingCurveExists) {
                        if (data.getValue() == HydroConstants.MISSING_VALUE) {
                            derivedValue = HydroConstants.MISSING_VALUE;
                        } else {
                            derivedValue = StageDischargeUtils.discharge2stage(
                                    lid, data.getValue());
                        }
                        sb.append(String.format(
                                "%8.2f %8.2f %11s %s %1s %1s %10s %11s %11s\n",
                                data.getValue(), derivedValue, timeValue,
                                revision, data.getShefQualCode(), qcSymbol,
                                data.getProductId(), prodTime, postTime));
                    } else {
                        sb.append(String.format(
                                "%8.2f %11s %s %1s %1s %10s %11s %11s\n",
                                data.getValue(), timeValue, revision,
                                data.getShefQualCode(), qcSymbol,
                                data.getProductId(), prodTime, postTime));
                    }
                }
            } else {
                sb.append("\nNO data for the parameters defined above.\n");
            }

            return sb.toString();
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "", e);
        }
        return null;
    }

    /**
     * Print the text
     * 
     * @param text
     *            The text to be printed
     */
    private void printText(String text) {
        printer.startPage();
        wordBuffer = new StringBuffer();
        x = leftMargin;
        y = topMargin;
        index = 0;
        end = text.length();
        while (index < end) {
            char c = text.charAt(index);
            index++;
            if (c != 0) {
                if (c == 0x0a || c == 0x0d) {
                    if (c == 0x0d && index < end && text.charAt(index) == 0x0a) {
                        index++; // if this is cr-lf, skip the lf
                    }
                    printWordBuffer();
                    newline();
                } else {
                    if (c != '\t') {
                        wordBuffer.append(c);
                    }
                    if (Character.isWhitespace(c)) {
                        printWordBuffer();
                        if (c == '\t') {
                            x += tabWidth;
                        }
                    }
                }
            }
        }
        if (y + lineHeight <= bottomMargin) {
            printer.endPage();
        }
        wordBuffer = null;
    }

    /**
     * Word buffer for formating lines on the printed page
     */
    private void printWordBuffer() {
        if (wordBuffer.length() > 0) {
            String word = wordBuffer.toString();
            int wordWidth = gc.stringExtent(word).x;
            if (x + wordWidth > rightMargin) {
                // word doesn't fit on current line, so wrap
                newline();
            }
            gc.drawString(word, x, y, false);
            x += wordWidth;
            wordBuffer.setLength(0);
        }
    }

    /**
     * New line on the printed page
     */
    private void newline() {
        x = leftMargin;
        y += lineHeight;
        if (y + lineHeight > bottomMargin) {
            printer.endPage();
            if (index + 1 < end) {
                y = topMargin;
                printer.startPage();
            }
        }
    }

    /**
     * Create the where clause for latestobsvalue table, seeing if matching keys
     * and the given record's obstime is newer.
     * 
     * @param dataRecord
     *            The DataRecord to update
     * @return The Where clause used in the update
     */
    private String createOlderThanWhereLatestObsValue(DataRecord dr) {
        StringBuilder sb = new StringBuilder(" where ");
        sb.append("lid = '");
        sb.append(dr.getLid());
        sb.append("' and ");
        sb.append("pe = '");
        sb.append(dr.getPe().toUpperCase());
        sb.append("' and ");
        sb.append("dur = ");
        sb.append(dr.getDur());
        sb.append(" and ");
        sb.append("ts = '");
        sb.append(dr.getTs().toUpperCase());
        sb.append("' and ");
        sb.append("extremum = '");
        sb.append(dr.getExt().toUpperCase());
        sb.append("' and ");
        sb.append("obstime < '");
        sb.append(dbFormat.format(dr.getObsTime()));
        sb.append("';");
        return sb.toString();
    }

    /**
     * Create the update where clause for latestobsvalue table.
     * 
     * @param dataRecord
     *            The DataRecord to update or delete
     * @return The Where clause used in the update or delete
     */
    private String createWhereExistsLatestObsValue(DataRecord dr) {
        StringBuilder sb = new StringBuilder(" where ");
        sb.append("lid = '");
        sb.append(dr.getLid());
        sb.append("' and ");
        sb.append("pe = '");
        sb.append(dr.getPe().toUpperCase());
        sb.append("' and ");
        sb.append("dur = ");
        sb.append(dr.getDur());
        sb.append(" and ");
        sb.append("ts = '");
        sb.append(dr.getTs().toUpperCase());
        sb.append("' and ");
        sb.append("extremum = '");
        sb.append(dr.getExt().toUpperCase());
        sb.append("';");
        return sb.toString();
    }

    /**
     * Create the update where clause
     * 
     * @param dataRecord
     *            The DataRecord to update
     * @return The Where clause used in the update or delete
     */
    private String createUpdDelWhereObs(DataRecord dr) {
        StringBuilder sb = new StringBuilder(" where ");
        sb.append("lid = '");
        sb.append(dr.getLid());
        sb.append("' and ");
        sb.append("pe = '");
        sb.append(dr.getPe().toUpperCase());
        sb.append("' and ");
        sb.append("obstime = '");
        sb.append(dbFormat.format(dr.getObsTime()));
        sb.append("' and ");
        sb.append("dur = ");
        sb.append(dr.getDur());
        sb.append(" and ");
        sb.append("ts = '");
        sb.append(dr.getTs().toUpperCase());
        sb.append("' and ");
        sb.append("extremum = '");
        sb.append(dr.getExt().toUpperCase());
        sb.append("';");
        return sb.toString();
    }

    /**
     * Enable/Disable the edit buttons
     * 
     * @param enabled
     *            Enable if true, disable if false
     */
    private void enableEditButtons(boolean enabled) {
        updateInsertBtn.setEnabled(enabled);
        setMissingBtn.setEnabled(enabled);
        setQcBtn.setEnabled(enabled);
        deleteBtn.setEnabled(enabled);
    }

    /**
     * Enable/Disable the Fcst Inserted Data Attributes
     * 
     * @param enabled
     *            Enable if true, disable if false
     */
    private void enableDataAttributes(boolean enabled) {
        fcstTypSrcChk.setEnabled(enabled);
        fcstBasisTimeChk.setEnabled(enabled);
        fcstTypSrcChk.setSelection(enabled);
        fcstBasisTimeChk.setSelection(enabled);

    }

    /**
     * Update the station label
     */
    private void updateStationLabel() {
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        try {
            /* append the river name info */
            String[] sa = dataManager.getStnRiverName(lid);

            if (sa != null && sa[0] != null && sa[1] != null) {
                if (sa[0].equalsIgnoreCase(HydroConstants.UNDEFINED)
                        && sa[1].equalsIgnoreCase(HydroConstants.UNDEFINED)) {
                    siteLabel = lid;
                } else if (!sa[0].equals(HydroConstants.UNDEFINED)
                        && !sa[1].equals(HydroConstants.UNDEFINED)) {
                    siteLabel = lid + " (" + sa[0] + " - " + sa[1] + ")";
                } else if (!sa[0].equals(HydroConstants.UNDEFINED)
                        && sa[1].equals(HydroConstants.UNDEFINED)) {
                    siteLabel = lid + " (" + sa[0] + ")";
                } else {
                    siteLabel = lid;
                }
            } else {
                siteLabel = lid;
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.INFO, "Uable to get site: ", e);
            siteLabel = lid;
        }

        selectedLocNameLbl.setText(siteLabel);
    }

    /**
     * Update the selected location information label with lid, PE, T and
     * basistime
     */
    private void updateSelectedLocInfoLabel() {
        if (basisTime != null) {
            selectedLocInfoLbl.setText(pe + " " + ts + " " + extremum + "  "
                    + basisTime);
        } else {
            selectedLocInfoLbl.setText(pe + " " + ts + " " + extremum);
        }
    }

    /**
     * Update the flood stage label based on selection.
     */
    private void updateFloodStageLabel() {
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();

        /* Find the flood stg/flow if a river station */
        java.util.List<Object[]> floodList = null;
        try {
            floodList = dataManager.getFloodStage(lid);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to get Flood Stage List: ", e);
        }

        /* Should only be one here, lid is primary key */
        if (floodList != null && floodList.size() > 0) {
            Object[] oa = floodList.get(0);
            String floodStage = "0.0";
            String floodFlow = "0";
            if (oa != null) {
                if (oa[1] != null) {
                    floodStage = String.format("%.1f", oa[1]);
                }

                if (oa[2] != null) {
                    floodFlow = String.format("%.0f", oa[2]);
                }
            }
            selectedFloodLbl.setText("Flood Stg/Flow:  " + floodStage + "/"
                    + floodFlow);
        } else {
            StringBuilder sb = new StringBuilder("Flood Stg/Flow:  ");
            sb.append("0.0/");
            sb.append("0");
            selectedFloodLbl.setText(sb.toString());
        }
    }

    /**
     * copy the current forecast time series in its entirety to a new type
     * source and/or basis time
     */
    private void tabularCopyTS() {
        TimeSeriesDataManager dataManager = TimeSeriesDataManager.getInstance();
        boolean keyChanged = false;
        int cnt = 0;
        Date postingTime;
        int duplicateCnt = 0;

        if (fcstBasisTimeChk.getSelection() || fcstTypSrcChk.getSelection()) {
            keyChanged = true;
        } else {
            keyChanged = false;
        }

        /* get the count of the records in the list */
        if (tabularDataList == null) {
            cnt = 0;
        } else {
            cnt = tabularDataList.size();
        }

        try {
            /*
             * only do something if we have forecast or contingency data and a
             * valid type source has been selected.
             */
            if ((ts.toUpperCase().startsWith("F") || ts.toUpperCase()
                    .startsWith("C"))
                    && cnt > 0
                    && keyChanged
                    && !fcstAtt.getSelectedTS().equals(UNDEFINED_TYPESOURCE)) {
                /* set postingtime to current time */
                postingTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"))
                        .getTime();

                SiteInfo si = tabInfo.getSiteInfo(topDataTable
                        .getSelectionIndex());

                /* load the structure with the general data */
                Fcstheight fcstRow = new Fcstheight();
                FcstheightId fhid = new FcstheightId();
                fhid.setLid(si.getLid());
                fhid.setPe(si.getPe());
                fhid.setDur((short) si.getDur());
                fhid.setExtremum(si.getExt());
                fcstRow.setPostingtime(postingTime);

                /* load the type source key field specific to the copy request. */
                if (fcstTypSrcChk.getSelection()) {
                    fhid.setTs(fcstAtt.getSelectedTS());
                } else {
                    fhid.setTs(si.getTs());
                }

                /* load the basistime key field specific to the copy request */
                if (fcstBasisTimeChk.getSelection()) {
                    fhid.setBasistime(HydroConstants.DATE_FORMAT.parse(fcstAtt
                            .getBasisTime()));
                } else {
                    si.setBasisTime(basisTime);
                    fhid.setBasistime(HydroConstants.DATE_FORMAT.parse(si
                            .getBasisTime()));
                }

                /* load the product id and time data fields accordingly */
                if (useProductTimeChk.getSelection()) {
                    fcstRow.setProducttime(HydroConstants.DATE_FORMAT
                            .parse(fcstAtt.getTime()));
                    fcstRow.setProductId(fcstAtt.getProductId());
                } else {
                    fcstRow.setProductId(tabularDataList.get(0).getProductId());
                    fcstRow.setProducttime(tabularDataList.get(0)
                            .getProductTime());
                }

                fcstRow.setId(fhid);

                /* define the table name to insert data into */
                String tableName = DbUtils.getTableName(
                        fcstRow.getId().getPe(), fcstRow.getId().getTs());

                long qualityCode = HydroConstants.MISSING_VALUE;
                for (TabularData td : tabularDataList) {
                    fhid.setProbability(td.getProbability());
                    fcstRow.setValue(td.getValue());
                    fhid.setValidtime(td.getObsTime());
                    fcstRow.setShefQualCode("M");
                    fcstRow.setQualityCode((int) HydroQC.setQcCode(
                            HydroQC.QC_MANUAL_NEW, qualityCode));
                    fcstRow.setRevision((short) 0);

                    /* build the where clause */
                    final String format = " where lid = '%s' and pe = '%s' "
                            + "and validtime = '%s' and basistime = '%s' "
                            + "and dur = %d and ts = '%s'  and extremum = '%s' ";

                    String where = String.format(format, fcstRow.getId()
                            .getLid(), fcstRow.getId().getPe(),
                            HydroConstants.DATE_FORMAT.format(fcstRow.getId()
                                    .getValidtime()),
                            HydroConstants.DATE_FORMAT.format(fcstRow.getId()
                                    .getBasistime()), fcstRow.getId().getDur(),
                            fcstRow.getId().getTs(), fcstRow.getId()
                                    .getExtremum());

                    if (dataManager.recordCount(tableName, where) > 0) {
                        /* already a record with same key */
                        duplicateCnt++;
                    }

                }

                // report on any duplicates which are ignored
                if (duplicateCnt > 0) {
                    statusHandler.handle(Priority.INFO, duplicateCnt
                            + " records detected in copy operation.");
                }

                int selection = topDataTable.getSelectionIndex();

                // reload list of timeseries
                tabularLoadTimeseries();
                topDataTable.select(selection);

            }
        } catch (ParseException pe) {
            statusHandler.handle(Priority.PROBLEM, "Parse eror: ", pe);
        } catch (VizException ve) {
            statusHandler.handle(Priority.PROBLEM, "", ve);
        }

    }

    /**
     * Create an update/delete where clause for forecast data.
     * 
     * @param td
     *            The TabularData object
     * @param dr
     *            The DataRecord
     * @return The where clause
     */
    private String createUpdDelWhereFcst(TabularData td, DataRecord dr) {
        final String format = " where lid = '%s' and pe = '%s' "
                + "and validtime = '%s' and basistime = '%s' "
                + "and dur = %d and ts = '%s'  and extremum = '%s' ; ";

        String where = String.format(format, dr.getLid(), dr.getPe(),
                HydroConstants.DATE_FORMAT.format(dr.getObsTime()),
                dr.getBasisTime(), dr.getDur(), dr.getTs(), dr.getExt());

        return where;
    }

    /**
     * Handle the SHEF Encode Selected button selection. Writes the .A or .AR
     * rec to the shef encode file.
     */
    private void tabularShefEncode() {
        AppsDefaults appsDefaults = AppsDefaults.getInstance();
        String fmtType = ".A ";
        String txtValue = null;

        if (shefFileName == null) {
            if (whfsProductDir == null) {
                whfsProductDir = appsDefaults.getToken("whfs_product_dir");
            }

            if (whfsProductDir == null || whfsProductDir.length() == 0) {
                shefFileName = "." + SHEF_FILE_NAME + "." + getPid();
            } else {
                shefFileName = whfsProductDir + "/" + SHEF_FILE_NAME + "."
                        + getPid();
            }
        }

        try (BufferedWriter out = new BufferedWriter(new FileWriter(
                shefFileName, true))) {
            int[] indices = bottomTable.getSelectionIndices();
            String tablename = DbUtils.getTableName(pe, ts);
            boolean forecast = ts.toUpperCase().startsWith("C")
                    || ts.toUpperCase().startsWith("F");

            TimeSeriesDataManager dataManager = TimeSeriesDataManager
                    .getInstance();
            // update the tabular data list with the latest data
            tabularDataList = dataManager.getTabularData(tablename, lid, pe,
                    ts, dur, extremum, beginningTime, endingTime, basisTime,
                    forecast);

            for (int indice : indices) {

                /* if manually edited data then format as .AR message */
                TabularData td = tabularDataList.get(indice);
                if (td.getShefQualCode().startsWith("M")) {
                    fmtType = ".AR";
                }

                /* convert time */
                Date d = td.getObsTime();
                String dateStr = shefDateFormat.format(d);
                String timeStr = shefTimeFormat.format(d);
                String timeBuf = dateStr + " Z DH" + timeStr;

                /* convert '-9999' to missing symbol 'M' */
                if (td.getValue() == HydroConstants.MISSING_VALUE) {
                    txtValue = "M";
                } else if (pe.startsWith("Q") && !pe.equals("QB")
                        && !pe.equals("QE") && !pe.equals("QF")) {
                    txtValue = String.format("%.3f", td.getValue() / 1000);
                } else {
                    txtValue = String.format("%.2f", td.getValue());
                }

                /* get the internal QC code and set the SHEF data Qualifier */
                String qcSymbol = TimeSeriesUtil.buildQcSymbol(td
                        .getQualityCode());
                String dataQualifier = null;

                if (qcSymbol.startsWith("B")) {
                    dataQualifier = "B";
                } else if (qcSymbol.startsWith("Q")) {
                    if (td.getShefQualCode().startsWith("F")) {
                        dataQualifier = "F";
                    } else {
                        dataQualifier = "Q";
                    }
                } else {
                    dataQualifier = " ";
                }

                /* get durcode and format the outgoing SHEF message */
                String durSymbol = TimeSeriesUtil.convertDur2Code(Integer
                        .parseInt(dur));

                String aRec = String.format("%s %s %s/%s%s%s%s %s%s", fmtType,
                        lid, timeBuf, pe, durSymbol, ts, extremum, txtValue,
                        dataQualifier);

                out.write(aRec + "\n");
            }
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to create shef file: ", e);
            showMessage(shell, SWT.ERROR | SWT.OK, "Unable to Save File",
                    "File:  " + SHEF_FILE_NAME + "." + getPid()
                            + "\nUser does NOT have write permission.");
        } catch (ClassNotFoundException | VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to reload location list.", e);
        }
    }

    /**
     * Review the shef encoded product.
     */
    private void reviewShefEncodedProduct() {
        if (shefFileName != null && shefFileName.length() > 0) {
            File shefFile = new File(shefFileName);
            if (shefFile.exists()) {
                if (editor == null || editor.isDisposed()) {
                    editor = new TextEditorDlg(shell, false, shefFile);
                    editor.open();
                } else {
                    editor.bringToTop();
                }
            } else {
                showMessage(shell, SWT.OK, "Unable to Open File",
                        "Unable to open file:\n" + SHEF_FILE_NAME + "."
                                + getPid());
            }
        }
    }

    /**
     * Broadcast the SHEF product.
     */
    private void sendProduct() {
        // Check for DTR and don't transmit if in DRT
        if (!SimulatedTimeOperations.isTransmitAllowed()) {
            SimulatedTimeOperations.displayFeatureLevelWarning(this.shell,
                    "Transmission of SHEF products");
            return;
        }

        if (sendConfirmation()) {
            // check shef issue configuration
            ShefIssueMgr sim = ShefIssueMgr.getInstance();

            ShefIssueXML xml = sim.getShefIssueXml();

            try {
                if (xml.isDirCopy()) {
                    ArrayList<String> directories = xml.getInternalDirectory()
                            .getDirectories();
                    if (directories != null) {
                        for (String dir : directories) {
                            FileUtil.copyFile(new File(shefFileName),
                                    new File(dir + "/" + SHEF_FILE_NAME + "."
                                            + getPid()));
                        }
                    }
                }

                if (xml.isDistributeProduct()) {
                    String text = getFileText();

                    OUPRequest req = new OUPRequest();
                    OfficialUserProduct oup = new OfficialUserProduct();
                    String awipsWanPil = productTF.getText();
                    oup.setAwipsWanPil(awipsWanPil);
                    oup.setSource("Time Series");
                    oup.setAddress("DEFAULTNCF");
                    oup.setNeedsWmoHeader(true);
                    oup.setFilename(SHEF_FILE_NAME + "." + getPid());
                    oup.setProductText(text);
                    req.setUser(UserController.getUserObject());

                    req.setCheckBBB(true);
                    req.setProduct(oup);

                    OUPResponse response = (OUPResponse) ThriftClient
                            .sendRequest(req);
                    boolean success = response.isSendLocalSuccess();
                    if (response.hasFailure()) {
                        Priority p = Priority.EVENTA;
                        if (!response.isAttempted()) {
                            // if was never attempted to send or store even
                            // locally
                            p = Priority.CRITICAL;
                        } else if (!response.isSendLocalSuccess()) {
                            // if send/store locally failed
                            p = Priority.CRITICAL;
                        } else if (!response.isSendWANSuccess()) {
                            // if send to WAN failed
                            if (response.getNeedAcknowledgment()) {
                                // if ack was needed, if it never sent then no
                                // ack was recieved
                                p = Priority.CRITICAL;
                            } else {
                                // if no ack was needed
                                p = Priority.EVENTA;
                            }
                        } else if (response.getNeedAcknowledgment()
                                && !response.isAcknowledged()) {
                            // if sent but not acknowledged when acknowledgement
                            // is needed
                            p = Priority.CRITICAL;
                        }

                        statusHandler.handle(p, response.getMessage());
                    }

                    if (success) {
                        showMessage(shell, SWT.OK, "Distribution Successful",
                                "Product successfully distributed via HandleOUP");
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error transmitting text product", e);

            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error transmitting text product", e);
            }
        }
    }

    /**
     * Get confirmation from the user and verify the product id is valid.
     * 
     * @return True if ok to send, false otherwise
     */
    private boolean sendConfirmation() {
        boolean retVal = false;

        /* Verify the product id is not invalid */
        String productId = productTF.getText();
        if (productId == null || productId.length() == 0) {
            showMessage(shell, SWT.ERROR, INVALID_PRODUCT_ID,
                    "Product Id cannot be blank.");
            // Apparently CCCCNNNXXX is valid so we'll accept it
            // } else if (productId.equals("CCCCNNNXXX")) {
            // showMessage(shell, SWT.ERROR, INVALID_PRODUCT_ID,
            // INVALID_PRODUCT_ID + ": CCCCNNNXXX");
        } else {
            retVal = true;
        }

        /* Get send confirmation from the user */
        if (retVal == true) {
            int choice = showMessage(shell, SWT.OK | SWT.CANCEL,
                    "SHEF Send Confirmation", "Do you wish to send product "
                            + productId + "?");

            if (choice == SWT.CANCEL) {
                retVal = false;
            }
        }

        return retVal;
    }

    /**
     * Get the text from the shef file.
     * 
     * @return text
     */
    private String getFileText() {
        if (shefFileName != null) {
            StringBuilder sb = new StringBuilder();
            try (BufferedReader in = new BufferedReader(new FileReader(
                    shefFileName))) {
                String str;
                while ((str = in.readLine()) != null) {
                    sb.append(CARRIAGECONTROL);
                    sb.append(str);
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, "Error reading file: "
                        + shefFileName, e);
            }
            return sb.toString();
        }

        return "";
    }

    /**
     * Clear the shef encode file.
     */
    private void clearProduct() {
        int response = showMessage(shell, SWT.OK | SWT.CANCEL,
                "SHEF Clear Confirmation",
                "Do you wish to remove the SHEF encoded product file?");

        if (response == SWT.OK && shefFileName != null) {
            File shefFile = new File(shefFileName);
            if (shefFile.exists()) {
                boolean success = shefFile.delete();
                if (success == false) {
                    showMessage(shell, SWT.ERROR | SWT.OK | SWT.CANCEL,
                            "Error", "Error removing the SHEF encode file.");
                }
            }
        }
    }

    /**
     * Show a dialog message.
     * 
     * @param shell
     *            The parent shell
     * @param style
     *            The dialog style
     * @param title
     *            The dialog title
     * @param msg
     *            The dialog message
     * @return The value representing the button clicked on the dialog
     */
    private int showMessage(Shell shell, int style, String title, String msg) {
        MessageBox messageBox = new MessageBox(shell, style);
        messageBox.setText(title);
        messageBox.setMessage(msg);
        return messageBox.open();
    }

    /**
     * Get a "process id" unique to this session.
     * 
     * @return The pid
     */
    private int getPid() {
        if (pid == HydroConstants.MISSING_VALUE) {
            Random r = new Random();

            // Let's create a hand-made pid for default
            pid = r.nextInt() & 0x7fffffff;
        }

        return pid;
    }

    /**
     * @return the lid
     */
    public String getLid() {
        return lid;
    }

    /**
     * @param lid
     *            the lid to set
     */
    public void setLid(String lid) {
        this.lid = lid;
    }

    /**
     * @return the siteName
     */
    public String getSiteName() {
        return siteName;
    }

    /**
     * @param siteName
     *            the siteName to set
     */
    public void setSiteName(String siteName) {
        this.siteName = siteName;
    }

    /**
     * @return the groupInfo
     */
    public GroupInfo getGroupInfo() {
        return groupInfo;
    }

    /**
     * @param groupInfo
     *            the groupInfo to set
     */
    public void setGroupInfo(GroupInfo groupInfo) {
        this.groupInfo = groupInfo;
    }

    /**
     * @return the tabInfo
     */
    public ArrayList<TabInfo> getTabInfoList() {
        return tabInfoList;
    }

    /**
     * @param tabInfoList
     * @param tabInfo
     *            the tabInfo to set
     */
    public void setTabInfoList(ArrayList<TabInfo> tabInfoList) {
        this.tabInfoList = tabInfoList;
    }

    /**
     * Get a TabInfo object.
     * 
     * @param index
     *            the index of the TabInfo object
     * @return the TabInfo
     */
    public TabInfo getTabInfo(int index) {
        return tabInfoList.get(index);
    }

    /**
     * @return the currentTabInfo
     */
    public TabInfo getCurrentTabInfo() {
        return currentTabInfo;
    }

    /**
     * @param currentTabInfo
     *            the currentTabInfo to set
     */
    public void setCurrentTabInfo(TabInfo currentTabInfo) {
        this.currentTabInfo = currentTabInfo;
    }

    @Override
    public void notifyUpdate(FcstAttUpdateEvent faue) {
        fcstAtt = faue.getFcstAttributes();

        productTimeLbl.setText(fcstAtt.getTime());
        productIdLbl.setText(fcstAtt.getProductId());
        fcstBasisTimeLbl.setText(fcstAtt.getBasisTime());
        fcstTypSrcChk.setText("Fcst TypSrc:   " + fcstAtt.getSelectedTS());
        getParent().redraw();
    }

    @Override
    public void aboutToRun(IJobChangeEvent event) {
    }

    @Override
    public void awake(IJobChangeEvent event) {
    }

    @Override
    public void done(IJobChangeEvent event) {
        if (!isDisposed()) {
            /* Verify The Job Type */
            TimeSeriesDataJobManager.REQUEST_TYPE requestType = (TimeSeriesDataJobManager.REQUEST_TYPE) event
                    .getJob().getProperty(
                            new QualifiedName(null, "REQUEST_TYPE"));
            if (requestType != TimeSeriesDataJobManager.REQUEST_TYPE.REQUEST_TYPE_TABULAR) {
                return;
            }

            tsDataJobManager.removeJobChangeListener(this);
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    loadDataList();
                }
            });
        }
    }

    @Override
    public void running(IJobChangeEvent event) {
    }

    @Override
    public void scheduled(IJobChangeEvent event) {
    }

    @Override
    public void sleeping(IJobChangeEvent event) {
    }
}
