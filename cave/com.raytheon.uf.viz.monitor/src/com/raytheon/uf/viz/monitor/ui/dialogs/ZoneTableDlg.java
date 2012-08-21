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
package com.raytheon.uf.viz.monitor.ui.dialogs;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.MonitorAreaUtils;
import com.raytheon.uf.common.monitor.config.MonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.CommonTableConfig;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.GraphType;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.ObsHistType;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.DisplayVarName;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.data.ObHourReports;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.data.ObStnHourReports;
import com.raytheon.uf.viz.monitor.data.ObTrendDataSet;
import com.raytheon.uf.viz.monitor.data.TableData;
import com.raytheon.uf.viz.monitor.data.TableUtil;
import com.raytheon.uf.viz.monitor.hodograph.HodographDlg;
import com.raytheon.uf.viz.monitor.listeners.IMonitorControlListener;
import com.raytheon.uf.viz.monitor.listeners.IMonitorListener;
import com.raytheon.uf.viz.monitor.trendplot.TrendPlotDlg;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.uf.viz.monitor.util.ObUtil;

/**
 * Abstrct Zone table dialog that is the foundation for all Zone dialogs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2009            lvenable     Initial creation
 * Dec 16, 2009 3424      zhao         use ObMultiHrsReports for obs data archive
 * Jan 04, 2010 3424      Slav/Wen     Adds hodograph and trend plot.
 * Jan 18, 2010 3424      Slav         Change in trend plot
 * Oct 07, 2010 5105      zhao         Disabled the 'X' button of the shell
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public abstract class ZoneTableDlg extends Dialog implements IMonitorListener,
		IMonitorControlListener, IStationTableAction, IZoneTableAction {

    /** Array listening monitors **/
    protected ArrayList<IMonitor> controlListeners = new ArrayList<IMonitor>();

    /**
     * Dialog shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    // private Display display;

    /**
     * top controls & labels
     */
    private Composite controlComp;

    /**
     * Return value when the shell is disposed.
     */
    private final Boolean returnValue = false;

    /**
     * Zone table.
     */
    protected ZoneTableComp zoneTable;

    /**
     * Station table.
     */
    private StationTableComp stationTable;

    /**
     * This object contains all observation data necessary for table dialogs and
     * trending plots
     */
    private ObMultiHrsReports obData;

    /**
     * The nominal time corresponding to which data are displayed in the dialog
     * (get this using obData's getNominalTime or getLatestNominalTime method)
     */
    protected Date nominalTime;

    /**
     * This is the zone table data to be displayed in the zone table dialog for
     * a specific nominal-time (get zoneTblData using obData's getZoneTableData
     * method)
     */
    private TableData zoneTblData;

    /**
     * This is the station table data to be displayed in the station table
     * dialog for a specific nominal-time and a specific zone (get stnTblData
     * using obData's getStationTableData method)
     */
    private TableData stnTblData;

    /**
     * The zone selected to display its station data in the station table
     */
    private String selectedZone = "";

    /**
     * the hover text of the selected zone [this text is to be displayed as part
     * of the Zone/County label on the station table GUI]
     */
    private String selectedZoneHoverText = "";

    /**
     * Variable name for station column
     */
    private ObConst.VarName varName;

    /**
     * Application name.
     */
    private final CommonConfig.AppName appName;

    /**
     * Table configuration file
     */
    // private CommonTableConfig config;

    /**
     * Table attribute dialog.
     */
    private TableAttribDlg attrDlg;

    /**
     * Link to frame check box.
     */
    protected Button linkToFrameChk;

    public boolean linkedToFrame = false;

    /**
     * Vertical check box.
     */
    private Button vertChk;

    /**
     * Nominal time label.
     */
    private Label nominalTimeLbl;

    /**
     * Nominal time label.
     */
    // private Date date;
    /**
     * Table stack composite.
     */
    private Composite tableStackComp;

    /**
     * Table stack layout.
     */
    // private StackLayout tableStack;

    /**
     * Zone table composite that contains the zone table.
     */
    private Composite zoneTblComp;

    /**
     * Station table composite that contains the station table.
     */
    private Composite stationTblComp;

    /** simple date in GMT **/
    private final DateFormat dFormat = new SimpleDateFormat(
            "E MMM dd HH:mm:ss yyyy");

    private ArrayList<String> prodArray;

    /**
     * zone sort column index, initialized to its default value
     */
    private int zoneSortColumn = 0;

    /**
     * zone sort direction, initialized to its default value
     */
    private int zoneSortDirection = SWT.NONE;

    /**
     * station sort column, initialized to its default value
     */
    private int stnSortColumn = 0;

    /**
     * station sort direction, initialized to its default value
     */
    private int stnSortDirection = SWT.NONE;

    private double lat;

    private double lon;

    public static float ZONE_ZOOM_LEVEL = 100;

    public static float STATION_ZOOM_LEVEL = 50;

    protected abstract void shellDisposeAction();

    /**
     * Constructor
     * 
     * @param parent
     * @param appName
     */
    public ZoneTableDlg(Shell parent, ObMultiHrsReports obData,
            CommonConfig.AppName appName) {
        super(parent, 0);
        this.appName = appName;
        dFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        this.obData = obData;
        zoneTblData = obData.getZoneTableData(); // the zone table data of the
        // latest nominal time
        zoneTblData.sortData();
        nominalTime = obData.getLatestNominalTime();
    }

    /**
     * Constructor
     * 
     * @param parent
     * @param appName
     */
    public ZoneTableDlg(Shell parent, CommonConfig.AppName appName) {
        super(parent, 0);
        this.appName = appName;
        dFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    protected void setObData(ObMultiHrsReports obData) {
        this.obData = obData;
        zoneTblData = obData.getZoneTableData(); // the zone table data of the
        // latest nominal time
    }

    public void setDate(Date date) {
        this.nominalTime = TableUtil.getNominalTime(date);
        if (date != null) {
            zoneTblData.sortData();
        }
    }

    /**
     * Open method to display the dialog.
     * 
     * @return Boolean return value.
     */
    public Object open() {

        Shell parent = getParent();
        shell = new Shell(parent.getDisplay(), SWT.DIALOG_TRIM | SWT.RESIZE);
        parent.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                shell.dispose();
            }
        });
        shell.setText(appName.name() + " Threat Level ZONE/COUNTY Table");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();

        /**
         * Disable the close [x] button, prompt the user to use Cave Clear
         * button to close application
         */
        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                MessageBox msgBox = new MessageBox(shell, SWT.OK);
                msgBox.setMessage("Use the Clear button on D2D to dispose this dialog");
                msgBox.open();
                event.doit = false;
            }
        });

        shellDisposeAction();
        shell.pack();
        shell.setVisible(true);

        return returnValue;
    }

    /**
     * Initialize the components on the dialog.
     */
    private void initializeComponents() {
        createTopControls();

        // Add a horizontal separator bar to the display.
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        // tableStackComp = new Composite(shell, SWT.NONE);
        // tableStack = new StackLayout();
        // tableStackComp.setLayout(tableStack);

        tableStackComp = new Composite(shell, SWT.NONE);
        tableStackComp.setLayout(new GridLayout(1, false));
        tableStackComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));

        createZoneTable(tableStackComp);
        createStationTable(tableStackComp);

        // tableStack.topControl = zoneTblComp;
        // tableStackComp.layout();

        ((GridData) zoneTblComp.getLayoutData()).exclude = false;
        zoneTblComp.setVisible(true);

        ((GridData) stationTblComp.getLayoutData()).exclude = true;
        stationTblComp.setVisible(false);

        tableStackComp.layout();
    }

    /**
     * Create the controls at the top of the dialog.
     */
    private void createTopControls() {
        controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(6, false));

        GridData gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        linkToFrameChk = new Button(controlComp, SWT.CHECK);
        linkToFrameChk.setText("Link to Frame");
        linkToFrameChk.setLayoutData(gd);
        linkToFrameChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleLinkToFrame();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.horizontalIndent = 10;
        vertChk = new Button(controlComp, SWT.CHECK);
        vertChk.setText("Vert");
        vertChk.setEnabled(false);
        vertChk.setLayoutData(gd);
        vertChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            }
        });

        gd = new GridData();
        gd.horizontalIndent = 10;
        Button configThreshBtn = new Button(controlComp, SWT.PUSH);
        configThreshBtn.setText(" Configure Thresholds... ");
        configThreshBtn.setLayoutData(gd);
        configThreshBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                configThreshAction();
            }
        });

        Button attrBtn = new Button(controlComp, SWT.PUSH);
        attrBtn.setText(" Attributes... ");
        attrBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayAttributesDialog();
            }
        });

        gd = new GridData();
        gd.horizontalIndent = 10;
        Label nomTimeLbl = new Label(controlComp, SWT.NONE);
        nomTimeLbl.setText("Nominal Time: ");
        nomTimeLbl.setLayoutData(gd);

        nominalTimeLbl = new Label(controlComp, SWT.NONE);

        if (nominalTime != null) {
            nominalTimeLbl.setText(dFormat.format(nominalTime));
        } else {
            nominalTimeLbl.setText(" ********* N/A ********* ");
        }
    }

    /**
     * Create the zone table.
     * 
     * @param stackComp
     *            Stack composite.
     */
    private void createZoneTable(Composite stackComp) {
        // TODO : Need to use a stack layout to show more than on table

        zoneTblComp = new Composite(stackComp, SWT.NONE);
        zoneTblComp.setLayout(new GridLayout(1, false));
        zoneTblComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        zoneTable = new ZoneTableComp(zoneTblComp, zoneTblData, appName, this);
    }

    public void updateZoneTable(Date date) {
        /**
         * before update zoneTblData, set zone sort column and direction to
         * their current values
         */
        setZoneSortColumnAndDirection();
        zoneTblData = obData.getZoneTableData(date);
        zoneTblData
                .setSortColumnAndDirection(zoneSortColumn, zoneSortDirection);
        zoneTblData.sortData();
        zoneTable.setTableData(zoneTblData);
        zoneTable.table.redraw();
    }

    public void updateNominalTimeLabel(Date date) {
        nominalTimeLbl.setText(dFormat.format(date));
        nominalTimeLbl.redraw();
    }

    /**
     * Updates obData and the data displayed in the dialog.
     * 
     * @param obData
     *            (data received from the monitor)
     */
    public void updateTableDlg(ObHourReports obHrData) {
        nominalTime = obHrData.getNominalTime();
        updateZoneTable(nominalTime);
        if (!selectedZone.equals("")) {
            updateStationTable(nominalTime);
        }
        updateNominalTimeLabel(nominalTime);
    }

    /**
     * set zone table sort column index and sort direction to the current zone
     * table sort column index and sort direction
     */
    private void setZoneSortColumnAndDirection() {
        if (zoneTblData != null) {
            zoneSortColumn = zoneTblData.getSortColumn();
            zoneSortDirection = zoneTblData.getSortDirection();
            /**
             * for safeseas swell period sort direction flip
             */
            if ( appName != AppName.SAFESEAS ) {
            	return;
            }
            if ( zoneSortColumn == zoneTable.getColumnIndex(appName,"SSZT_SwellPeriod") 
            		|| zoneSortColumn == zoneTable.getColumnIndex(appName,"SSZT_Swell2Period") ) {
            	if ( MonitorConfigConstants.isRankSwellPeriodHigh() ) {
            		zoneSortDirection = SWT.DOWN; 
            	} else {
            		zoneSortDirection = SWT.UP;
            	}
            }
        }
    }

    /**
     * set station table sort column index and sort direction to the current
     * station table sort column index and sort direction
     */
    private void setStnSortColumnAndDirection() {
        if (stnTblData != null) {
            stnSortColumn = stnTblData.getSortColumn();
            stnSortDirection = stnTblData.getSortDirection();
            /**
             * for safeseas swell period sort direction flip
             */
            if ( appName != AppName.SAFESEAS ) {
            	return;
            }
            if ( stnSortColumn == stationTable.getColumnIndex(appName,"SSZT_SwellPeriod") 
            		|| stnSortColumn == stationTable.getColumnIndex(appName,"SSZT_Swell2Period") ) {
            	if ( MonitorConfigConstants.isRankSwellPeriodHigh() ) {
            		stnSortDirection = SWT.DOWN; 
            	} else {
            		stnSortDirection = SWT.UP;
            	}
            }
        }
    }

    /**
     * Create the station table.
     * 
     * @param stackComp
     *            Stack composite.
     */
    private void createStationTable(Composite stackComp) {
        stationTblComp = new Composite(stackComp, SWT.NONE);
        stationTblComp.setLayout(new GridLayout(1, false));
        stationTblComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        // This is the initial creation of the station table composite.
        // No data is needed at this point as no zone has ever been selected.
        // Use updateStationTable() to display data when a zone is selected.
        // (Dec 16, 2009, zhao)
        stationTable = new StationTableComp(stationTblComp, null, appName, this);
    }

    /**
     * Update the station table (Dec 16, 2009, zhao)
     * 
     * @param nominalTime
     * 
     * @param zone
     */
    public void updateStationTable(Date nominalTime) {
        /**
         * before update stnTblData, set station sort column and direction to
         * their current values
         */
        setStnSortColumnAndDirection();
        stnTblData = obData.getStationTableData(nominalTime, selectedZone);
        stnTblData.setSortColumnAndDirection(stnSortColumn, stnSortDirection);
        stnTblData.sortData();
        stationTable.setTableData(stnTblData);
        stationTable.setZoneCountyId(selectedZone);
        stationTable.setIdLabel(selectedZoneHoverText);
        stationTable.table.redraw();
    }

    /**
     * Display the table attributes dialog.
     */
    void displayAttributesDialog() {
        if (attrDlg == null) {
            attrDlg = new TableAttribDlg(shell, appName,
                    zoneTable.getVisibleColumns());
            boolean[] visCols = attrDlg.open();
            attrDlg = null;

            if (visCols.length > 0) {
                zoneTable.showHideTableColumns(visCols);
            }
        }
    }

    /**
     * Station table action to display the station table.
     */
    @Override
    public void stationTableAction() {

        updateZoneTable(nominalTime);

        ((GridData) zoneTblComp.getLayoutData()).exclude = false;
        zoneTblComp.setVisible(true);

        ((GridData) stationTblComp.getLayoutData()).exclude = true;
        stationTblComp.setVisible(false);

        tableStackComp.layout();

        // tableStack.topControl = zoneTblComp;
        // tableStackComp.layout();

        shell.setText(appName.name() + " Threat Level Zone/County Table");

        // re-set selectedZone to empty string
        selectedZone = "";

        updateNominalTimeLabel(nominalTime);
    }

    /**
     * Zone table action to display the zone table.
     * 
     */
    @Override
    public void zoneTableAction(int rowNum) {

        // set selectedZone to the selected zone
        selectedZone = zoneTblData.getTableRows().get(rowNum)
                .getTableCellData(0).getCellText();
        selectedZoneHoverText = zoneTblData.getTableRows().get(rowNum)
                .getTableCellData(0).getHoverText();

        // Zoom and centered bundle to Zone Scale(selectedZone)
        try {
            zoomToZone(selectedZone);
        } catch (Exception e) {
            e.printStackTrace();
        }

        updateStationTable(nominalTime);

        ((GridData) zoneTblComp.getLayoutData()).exclude = true;
        zoneTblComp.setVisible(false);

        ((GridData) stationTblComp.getLayoutData()).exclude = false;
        stationTblComp.setVisible(true);

        tableStackComp.layout();

        shell.setText(appName.name() + " Threat Level STATION Table");

        updateNominalTimeLabel(nominalTime);
    }

    public void launchTrendPlot(int rowIndex, int colIndex) {
        // get variable name (linked to the column index of the station table)
        CommonTableConfig config = CommonTableConfig.getInstance();
        String colKey = config.getTableColumnKeys(appName)[colIndex];
        GraphType graphType = config.getTableColumnAttr(colKey).getGraphType();
        if (graphType == GraphType.None) {
            return;
        }
        // get station ID (linked to rowIndex of the station table)
        String station = stnTblData.getTableRows().get(rowIndex)
                .getTableCellData(0).getCellText();
        if (graphType == GraphType.Trend) {
            prodArray = new ArrayList<String>();
            // choose correct column names
            if (appName.equals(AppName.SNOW)) {
                varName = config.getSnowZoneStnTableColVarNames()[colIndex];
                // Fill product array
                for (DisplayVarName name : DisplayVarName.values()) {
                    if (colIndex == 1 && name.toString().startsWith("BLIZ_")) {
                        prodArray.add(name.toString());
                    } else if (colIndex == 2
                            && name.toString().startsWith("FRZ_")) {
                        prodArray.add(name.toString());
                    } else if (colIndex == 3
                            && name.toString().startsWith("HSW_")) {
                        prodArray.add(name.toString());
                    } else if (name.toString().startsWith("VAR_")
                            && name.toString().equals("VAR_" + varName.name())) {
                        prodArray.add(name.toString());
                    }
                }
            }
            if (appName.equals(AppName.SAFESEAS)) {
                varName = config.getSafeseasZoneStnTableColVarNames()[colIndex];
                // Fill product arrays
                for (DisplayVarName name : DisplayVarName.values()) {
                    if (colIndex == 1 && name.toString().startsWith("SCA_")) {
                        prodArray.add(name.toString());
                    } else if (colIndex == 2
                            && name.toString().startsWith("GALE_")) {
                        prodArray.add(name.toString());
                    } else if (colIndex == 3
                            && name.toString().startsWith("STORM_")) {
                        prodArray.add(name.toString());
                    } else if (colIndex == 4
                            && name.toString().startsWith("HURRICANE_")) {
                        prodArray.add(name.toString());
                    } else if (name.toString().startsWith("VAR_")
                            && name.toString().equals("VAR_" + varName.name())) {
                        prodArray.add(name.toString());
                    }
                }
            }
            if (appName.equals(AppName.FOG)) {
                varName = config.getFogZoneStnTableColVarNames()[colIndex];
                for (DisplayVarName name : DisplayVarName.values()) {
                    if (name.toString().startsWith("VAR_")
                            && name.toString().equals("VAR_" + varName.name())) {
                        prodArray.add(name.toString());
                    }
                }
            }
            if (varName == ObConst.VarName.UNDEFINED_VARIABLE) {
                TrendPlotDlg tpd = new TrendPlotDlg(shell, selectedZone,
                        station, prodArray, "METAR");
                tpd.setData(obData);
                tpd.open();
            } else {
                // Handle trend plot for variables of "float" type
                TrendPlotDlg tpdNP = new TrendPlotDlg(shell, selectedZone,
                        station, prodArray, "METAR");
                tpdNP.setData(obData);
                tpdNP.open();
            }
        }
        if (graphType == GraphType.HodoWindDir) {

            if (appName.equals(AppName.SAFESEAS)) {
                varName = config.getSafeseasZoneStnTableColVarNames()[colIndex];
            } else {
                varName = ObConst.VarName.WIND_DIR;
            }
            // get data for a hodograph trend plot
            ObTrendDataSet dataset = obData.getTrendDataSet(selectedZone,
                    station, varName, ObConst.ProductName.UNDEFINED_PRODUCT);
            if (dataset != null && !dataset.isEmpty()) {

                float[] thresholds = dataset.getDualValuedThresholds();

                HodographDlg hodographDlg = new HodographDlg(shell,
                        varName.name(), station, "METAR");

                hodographDlg.setCurrentTime(ObUtil.getTimeNow()); //Calendar.getInstance());
                if (thresholds[0] == Float.NaN || thresholds[1] == Float.NaN
                        || thresholds[2] == Float.NaN
                        || thresholds[3] == Float.NaN) {
                    hodographDlg.setGreenThreshold(0, 0);
                } else {
                    hodographDlg.setGreenThreshold(0, 360);
                }
                hodographDlg.setYellowThreshold(thresholds[2], thresholds[3]);
                hodographDlg.setRedThreshold(thresholds[0], thresholds[1]);

                hodographDlg.setHodoGraphData(dataset.getDataSet());
                hodographDlg.open();
            } else {
                prodArray = new ArrayList<String>();
                prodArray.add("VAR_" + varName.name());
                TrendPlotDlg tpdHodo = new TrendPlotDlg(shell, selectedZone,
                        station, prodArray, "METAR");
                tpdHodo.setData(obData);
                tpdHodo.open();
            }
        }
    }

    public void launchObHistoryTable(int rowIndex) {
        String station = stnTblData.getTableRows().get(rowIndex)
                .getTableCellData(0).getCellText();

        ObStnHourReports report = obData.getObHourReports()
                .getObZoneHourReports(selectedZone)
                .getObStnHourReports(station);

        if (report.getStationCenter() != null) {
            lat = report.getStationCenter()[1];
            lon = report.getStationCenter()[0];
        } else {
            lat = 0.0;
            lon = 0.0;
        }

        MonitorConfigurationManager configMgr = getConfigMgr();

        ObsHistType histType = configMgr.getStationType(selectedZone, station);

        /**
         * For Snow monitor, no history table is displayed for a Maritime
         * station
         */
        if (appName == AppName.SNOW && histType == ObsHistType.Maritime) {
            return;
        }

        ObsHistTableDlg obsHstTblDlg = new ObsHistTableDlg(shell,
                obData.getHistTableData(selectedZone, station, histType),
                station, lat, lon, obData.getAppName(), histType);
        obsHstTblDlg.open();
    }

    protected abstract MonitorConfigurationManager getConfigMgr();

    /**
     * Configuration button action method.
     */
    protected abstract void configThreshAction();

    @Override
    public ArrayList<IMonitor> getMonitorControlListeners() {
        return controlListeners;
    }

    public void shellDisposeDialog() {
        shell.dispose();
    }

    public boolean isDisposed() {
        return zoneTable.isDisposed();
    }

    private void zoomToZone(String zone) throws Exception {
        double[] zoneCenter = MonitorAreaUtils.getZoneCenter(zone);
        if (zoneCenter != null) {
            zoomAndRecenter(zoneCenter, ZONE_ZOOM_LEVEL);
        }
    }

    public void zoomToStation(int rowIndex) {
        String selectedStation = stnTblData.getTableRows().get(rowIndex)
                .getTableCellData(0).getCellText();
        ObStnHourReports reports = obData.getObHourReports()
                .getObZoneHourReports(selectedZone)
                .getObStnHourReports(selectedStation);
        double[] stnCenter = reports.getStationCenter();
        if (stnCenter != null) {
            zoomAndRecenter(stnCenter, STATION_ZOOM_LEVEL);
        }
    }

    private void zoomAndRecenter(double[] center, float zoom) {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            IDescriptor descriptor = container.getActiveDisplayPane()
                    .getDescriptor();
            int mapWidth;
            if (descriptor instanceof IMapDescriptor) {
                mapWidth = ((IMapDescriptor) descriptor).getMapWidth() / 1000;
            } else {
                mapWidth = 10000;
            }
            float zoomLevel = (float) zoom / mapWidth;
            for (IDisplayPane pane : container.getDisplayPanes()) {
                pane.getRenderableDisplay().getExtent().reset();
                pane.getRenderableDisplay().recenter(center);
                pane.getRenderableDisplay().zoom(zoomLevel);
                pane.refresh();
            }
            container.refresh();
        }
    }

	public boolean isLinkedToFrame() {
		return linkedToFrame;
	}

	public void setLinkedToFrame(boolean linkedToFrame) {
		this.linkedToFrame = linkedToFrame;
	}

    protected void handleLinkToFrame() {
        linkedToFrame = linkToFrameChk.getSelection();
    }

}
