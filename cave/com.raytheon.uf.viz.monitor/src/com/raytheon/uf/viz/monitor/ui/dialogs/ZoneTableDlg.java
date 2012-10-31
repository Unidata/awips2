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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
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
import com.raytheon.uf.viz.monitor.util.ObUtil;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

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
 * Oct 26, 2012 1280      skorolev     Changes for non-blocking dialog. 
 * Nov.11, 2012 1297      skorolev     new abstract initiateProdArray()
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public abstract class ZoneTableDlg extends CaveSWTDialog implements
        IMonitorListener, IMonitorControlListener, IStationTableAction,
        IZoneTableAction {

    /** Array listening monitors **/
    protected List<IMonitor> controlListeners = new ArrayList<IMonitor>();

    /** top controls & labels **/
    private Composite controlComp;

    /** Zone table. **/
    protected ZoneTableComp zoneTable;

    /** Station table. **/
    protected StationTableComp stationTable;

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
    protected TableData zoneTblData;

    /**
     * This is the station table data to be displayed in the station table
     * dialog for a specific nominal-time and a specific zone (get stnTblData
     * using obData's getStationTableData method)
     */
    protected TableData stnTblData;

    /**
     * The zone selected to display its station data in the station table
     */
    private String selectedZone = "";

    /**
     * the hover text of the selected zone [this text is to be displayed as part
     * of the Zone/County label on the station table GUI]
     */
    private String selectedZoneHoverText = "";

    /** Variable name for station column **/
    public ObConst.VarName varName;

    /** Application name. **/
    protected final CommonConfig.AppName appName;

    /** Table attribute dialog. **/
    private TableAttribDlg attrDlg;

    /** Link to frame check box. **/
    protected Button linkToFrameChk;

    /** Link-to-frame flag. **/
    public boolean linkedToFrame = false;

    /** Vertical check box. **/
    private Button vertChk;

    /** Nominal time label. **/
    private Label nominalTimeLbl;

    /** Table stack composite. **/
    private Composite tableStackComp;

    /** Zone table composite that contains the zone table. **/
    private Composite zoneTblComp;

    /** Station table composite that contains the station table. **/
    private Composite stationTblComp;

    /** simple date in GMT. **/
    private final DateFormat dFormat = new SimpleDateFormat(
            "E MMM dd HH:mm:ss yyyy");

    /** list of variables to plot. **/
    public List<String> prodArray;

    /** zone sort column index, initialized to its default value. **/
    protected int zoneSortColumn = 0;

    /** zone sort direction, initialized to its default value. **/
    protected int zoneSortDirection = SWT.NONE;

    /** station sort column, initialized to its default value. **/
    protected int stnSortColumn = 0;

    /** station sort direction, initialized to its default value. **/
    protected int stnSortDirection = SWT.NONE;

    /** Latitude **/
    private double lat;

    /** Longitude **/
    private double lon;

    /** Zoom to zone level. **/
    public final float ZONE_ZOOM_LEVEL = 100;

    /** Zoom to station level. **/
    public final float STATION_ZOOM_LEVEL = 50;

    /** Data source. **/
    public String dataSrc = "METAR";

    /** trend plots. **/
    private TrendPlotDlg tpd, tpdHodo;

    /** Hodograph. **/
    private HodographDlg hodographDlg;

    /** History table. **/
    private ObsHistTableDlg obsHstTblDlg;

    /** dispose action. **/
    protected abstract void shellDisposeAction();

    /** List of opened plots. **/
    private Map<String, CaveSWTDialog> openedDlgs = new HashMap<String, CaveSWTDialog>();

    /** row index in the station table. **/
    public int rowIndex;

    /** column index in the station table. **/
    public int colIndex;

    /** Common configuration for zone table. **/
    public CommonTableConfig config = CommonTableConfig.getInstance();

    /** title of plot. **/
    private String dlgTitle;

    /**
     * Constructor
     * 
     * @param parent
     * @param appName
     */
    public ZoneTableDlg(Shell parent, ObMultiHrsReports obData,
            CommonConfig.AppName appName) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        this.appName = appName;
        dFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        this.obData = obData;
        // the zone table data of the latest nominal time:
        zoneTblData = obData.getZoneTableData();
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
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        this.appName = appName;
        dFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Sets zone table data
     * 
     * @param obData
     */
    protected void setObData(ObMultiHrsReports obData) {
        this.obData = obData;
        // the zone table data of the latest nominal time:
        zoneTblData = obData.getZoneTableData();
    }

    /**
     * Sets nominal time and sorts data
     * 
     * @param date
     */
    public void setDate(Date date) {
        this.nominalTime = TableUtil.getNominalTime(date);
        if (date != null) {
            zoneTblData.sortData();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        setText(appName.name() + " Threat Level ZONE/COUNTY Table");
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
        initializeComponents();
        getShell().addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent event) {
                MessageBox msgBox = new MessageBox(getShell(), SWT.OK);
                msgBox.setMessage("Use the Clear button on D2D to dispose this dialog");
                msgBox.open();
                event.doit = false;
            }
        });
    }

    /**
     * Initializes the components on the dialog.
     */
    private void initializeComponents() {
        createTopControls();

        // Add a horizontal separator bar to the display.
        Label sepLbl = new Label(getShell(), SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        tableStackComp = new Composite(getShell(), SWT.NONE);
        tableStackComp.setLayout(new GridLayout(1, false));
        tableStackComp.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true,
                true));

        createZoneTable(tableStackComp);
        createStationTable(tableStackComp);

        ((GridData) zoneTblComp.getLayoutData()).exclude = false;
        zoneTblComp.setVisible(true);

        ((GridData) stationTblComp.getLayoutData()).exclude = true;
        stationTblComp.setVisible(false);

        tableStackComp.layout();
    }

    /**
     * Creates the controls at the top of the dialog.
     */
    private void createTopControls() {
        controlComp = new Composite(getShell(), SWT.NONE);
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
     * Creates the zone table.
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

    /**
     * Updates zone table
     * 
     * @param date
     */
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

    /**
     * Updates nominal time label
     * 
     * @param date
     */
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
     * Sets zone table sort column index and sort direction to the current zone
     * table sort column index and sort direction
     */
    protected abstract void setZoneSortColumnAndDirection();

    /**
     * Sets station table sort column index and sort direction to the current
     * station table sort column index and sort direction
     */
    protected abstract void setStnSortColumnAndDirection();

    /**
     * Creates the station table.
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
     * Updates and sorts the station table
     * 
     * @param nominalTime
     */
    public void updateStationTable(Date nominalTime) {
        // before update stnTblData, set station sort column and direction to
        // their current values
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
     * Displays the table attributes dialog.
     */
    void displayAttributesDialog() {
        if (attrDlg == null) {
            attrDlg = new TableAttribDlg(getShell(), appName,
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
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.IStationTableAction#stationTableAction
     * ()
     */
    @Override
    public void stationTableAction() {
        updateZoneTable(nominalTime);
        ((GridData) zoneTblComp.getLayoutData()).exclude = false;
        zoneTblComp.setVisible(true);
        ((GridData) stationTblComp.getLayoutData()).exclude = true;
        stationTblComp.setVisible(false);
        tableStackComp.layout();
        getShell().setText(appName.name() + " Threat Level Zone/County Table");
        // re-set selectedZone to empty string
        selectedZone = "";
        updateNominalTimeLabel(nominalTime);
    }

    /**
     * Zone table action to display the zone table.
     */
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.IZoneTableAction#zoneTableAction
     * (int)
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

        getShell().setText(appName.name() + " Threat Level STATION Table");

        updateNominalTimeLabel(nominalTime);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.IStationTableAction#launchTrendPlot
     * (int, int)
     */
    public void launchTrendPlot(int rowIndex, int colIndex) {

        setRowIndex(rowIndex);
        setColIndex(colIndex);
        String colKey = config.getTableColumnKeys(appName)[colIndex];
        GraphType graphType = config.getTableColumnAttr(colKey).getGraphType();
        if (graphType == GraphType.None) {
            return;
        }
        // get station ID (linked to rowIndex of the station table)
        String station = stnTblData.getTableRows().get(rowIndex)
                .getTableCellData(0).getCellText();
        // get variable names (linked to the column index of the station table)
        initiateProdArray();
        dlgTitle = getTrendPlotName(prodArray) + " Trend Plot for " + station
                + "#" + dataSrc;
        if (graphType == GraphType.Trend) {
            if (mustCreate(tpd) || !openedDlgs.containsKey(dlgTitle)) {
                tpd = new TrendPlotDlg(getShell(), selectedZone, station,
                        prodArray, dataSrc, dlgTitle);
                tpd.setCloseCallback(new ICloseCallback() {
                    @Override
                    public void dialogClosed(Object returnValue) {
                        openedDlgs.remove(returnValue);
                    }
                });
                openedDlgs.put(dlgTitle, tpd);
                tpd.setData(obData);
                tpd.open();
            } else {
                openedDlgs.get(dlgTitle).bringToTop();
            }
        }
        if (graphType == GraphType.HodoWindDir) {
            // get data for a hodograph trend plot
            ObTrendDataSet dataset = obData.getTrendDataSet(selectedZone,
                    station, varName, ObConst.ProductName.UNDEFINED_PRODUCT);
            if (dataset != null && !dataset.isEmpty()) {
                float[] thresholds = dataset.getDualValuedThresholds();

                if (mustCreate(hodographDlg)
                        || !openedDlgs.containsKey(dlgTitle)) {
                    hodographDlg = new HodographDlg(getShell(), station,
                            dataSrc, dlgTitle);
                    hodographDlg.setCloseCallback(new ICloseCallback() {
                        @Override
                        public void dialogClosed(Object returnValue) {
                            openedDlgs.remove(returnValue);
                        }
                    });
                    openedDlgs.put(dlgTitle, hodographDlg);
                    hodographDlg.setCurrentTime(ObUtil.getTimeNow());
                    if (thresholds[0] == Float.NaN
                            || thresholds[1] == Float.NaN
                            || thresholds[2] == Float.NaN
                            || thresholds[3] == Float.NaN) {
                        hodographDlg.setGreenThreshold(0, 0);
                    } else {
                        hodographDlg.setGreenThreshold(0, 360);
                    }
                    hodographDlg.setYellowThreshold(thresholds[2],
                            thresholds[3]);
                    hodographDlg.setRedThreshold(thresholds[0], thresholds[1]);
                    hodographDlg.setHodoGraphData(dataset.getDataSet());
                    hodographDlg.open();
                } else {
                    openedDlgs.get(dlgTitle).bringToTop();
                }
            } else {
                prodArray = new ArrayList<String>();
                prodArray.add("VAR_" + varName.name());
                if (mustCreate(tpdHodo) || !openedDlgs.containsKey(dlgTitle)) {
                    tpdHodo = new TrendPlotDlg(getShell(), selectedZone,
                            station, prodArray, dataSrc, dlgTitle);
                    tpdHodo.setCloseCallback(new ICloseCallback() {
                        @Override
                        public void dialogClosed(Object returnValue) {
                            openedDlgs.remove(returnValue);
                        }
                    });
                    openedDlgs.put(dlgTitle, tpdHodo);
                    tpdHodo.setData(obData);
                    tpdHodo.open();
                } else {
                    openedDlgs.get(dlgTitle).bringToTop();
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.IStationTableAction#
     * launchObHistoryTable(int)
     */
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
        if (mustCreate(obsHstTblDlg)) {
            obsHstTblDlg = new ObsHistTableDlg(getShell(),
                    obData.getHistTableData(selectedZone, station, histType),
                    station, lat, lon, obData.getAppName(), histType);
            obsHstTblDlg.open();
        } else {
            obsHstTblDlg.bringToTop();
        }
    }

    protected abstract MonitorConfigurationManager getConfigMgr();

    /**
     * Configuration button action method.
     */
    protected abstract void configThreshAction();

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.listeners.IMonitorControlListener#
     * getMonitorControlListeners()
     */
    @Override
    public List<IMonitor> getMonitorControlListeners() {
        return controlListeners;
    }

    /**
     * Zooms map to a zone level
     * 
     * @param zone
     * @throws Exception
     */
    private void zoomToZone(String zone) throws Exception {
        double[] zoneCenter = MonitorAreaUtils.getZoneCenter(zone);
        if (zoneCenter != null) {
            zoomAndRecenter(zoneCenter, ZONE_ZOOM_LEVEL);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.IStationTableAction#zoomToStation
     * (int)
     */
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

    /**
     * Re-centers and zooms map
     * 
     * @param center
     * @param zoom
     */
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

    /**
     * @return linkedToFrame
     */
    public boolean isLinkedToFrame() {
        return linkedToFrame;
    }

    /**
     * Sets linkedToFrame
     * 
     * @param linkedToFrame
     */
    public void setLinkedToFrame(boolean linkedToFrame) {
        this.linkedToFrame = linkedToFrame;
    }

    /**
     * Handls checkbox "Link to frame"
     */
    protected void handleLinkToFrame() {
        linkedToFrame = linkToFrameChk.getSelection();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        setReturnValue(true);
    }

    /**
     * Initiates array of products to plot
     * 
     * @return
     */
    protected abstract void initiateProdArray();

    public int getRowIndex() {
        return rowIndex;
    }

    public void setRowIndex(int rowIndex) {
        this.rowIndex = rowIndex;
    }

    public int getColIndex() {
        return colIndex;
    }

    public void setColIndex(int colIndex) {
        this.colIndex = colIndex;
    }

    /**
     * Gets title of plot.
     * 
     * @param prod
     * @return
     */
    private String getTrendPlotName(List<String> prod) {
        String varName = null;
        String name = (String) prod.get(0);
        int stInd = name.indexOf("_");
        if (prod.size() > 1) {
            varName = name.substring(0, stInd);
            if (varName.equals("SCA")) {
                varName = "Small Craft Advisory";
            } else if (varName.equals("GALE")) {
                varName = "Gale Warning";
            } else if (varName.equals("STORM")) {
                varName = "Storm Warning";
            } else if (varName.equals("HURRICANE")) {
                varName = "Hurricane Force Wind Warning";
            } else if (varName.equals("BLIZ")) {
                varName = "Blizzard Warning";
            } else if (varName.equals("FRZ")) {
                varName = "Freezing Precipitation";
            } else if (varName.equals("HSW")) {
                varName = "Heavy Snow Warning";
            }
        } else {
            varName = name.substring(stInd + 1);
        }
        return varName;
    }
}
