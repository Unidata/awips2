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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.monitor.MonitorAreaUtils;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.xml.AreaIdXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.GraphType;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.ObsHistType;
import com.raytheon.uf.viz.monitor.data.ObHourReports;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.data.ObTrendDataSet;
import com.raytheon.uf.viz.monitor.data.TableData;
import com.raytheon.uf.viz.monitor.data.TableUtil;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.hodograph.HodographDlg;
import com.raytheon.uf.viz.monitor.listeners.IMonitorControlListener;
import com.raytheon.uf.viz.monitor.listeners.IMonitorListener;
import com.raytheon.uf.viz.monitor.trendplot.TrendPlotDlg;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.io.ParseException;

/**
 * Abstract Zone table dialog that is the foundation for all Zone dialogs.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 07, 2009           lvenable  Initial creation
 * Dec 16, 2009  3424     zhao      use ObMultiHrsReports for obs data archive
 * Jan 04, 2010  3424     Slav/Wen  Adds hodograph and trend plot.
 * Jan 18, 2010  3424     Slav      Change in trend plot
 * Oct 07, 2010  5105     zhao      Disabled the 'X' button of the shell
 * Oct 26, 2012  1280     skorolev  Changes for non-blocking dialog.
 * Nov.11, 2012  1297     skorolev  new abstract initiateProdArray()
 * May 13, 2014  3133     njensen   Updated getting ObsHistType from configMgr
 * May 15, 2014  3086     skorolev  Replaced MonitorConfigurationManager with
 *                                  FSSObsMonitorConfigurationManager.
 * Sep 15, 2014  3220     skorolev  Added refreshZoneTableData method.
 * Nov 03, 2014  3741     skorolev  Updated zoom procedures.
 * Sep 25, 2015  3873     skorolev  Added center definition for moving
 *                                  platforms.
 * Nov 09, 2015  3841     dhladky   Update all tables when zones/stations are
 *                                  updated.
 * Dec 02, 2015  3873     dhladky   Pulled 3841 to 16.1.1.
 * Dec 17, 2015  3873     dhladky   Set link to Frame to true by default.
 * Sep 27, 2016  5901     randerso  Fix dialog centering issue introduced in
 *                                  Eclipse 4. Handle different return type from
 *                                  CaveSWTDialog
 * Jun 26, 2018  6572     tgurney   Set minimum size of shell
 * Jul 10, 2018  6766     randerso  Fixed dialog closing for recent changes to
 *                                  CaveSWTDialog. Implemented do nothing
 *                                  interface methods in base class.
 * Aug 16, 2018  7410     randerso  Cleaned up HodographDlg construction.
 *
 * </pre>
 *
 * @author lvenable
 */
public abstract class ZoneTableDlg extends CaveSWTDialog
        implements IMonitorListener, IMonitorControlListener,
        IStationTableAction, IZoneTableAction {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ZoneTableDlg.class);

    /** Zoom to zone level. **/
    public static final float ZONE_ZOOM_LEVEL = 100;

    /** Zoom to station level. **/
    public static final float STATION_ZOOM_LEVEL = 50;

    /** simple date in GMT. **/
    private static final String DFORMAT = "E MMM dd HH:mm:ss yyyy";

    /** Array listening monitors **/
    protected List<IMonitor> controlListeners = new ArrayList<>();

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
    protected final AppName appName;

    /** Table attribute dialog. **/
    private TableAttribDlg attrDlg;

    /** Link to frame check box. **/
    protected Button linkToFrameChk;

    /** Link-to-frame flag. **/
    public boolean linkedToFrame = true;

    /** Nominal time label. **/
    private Label nominalTimeLbl;

    /** Table stack composite. **/
    private Composite tableStackComp;

    /** Zone table composite that contains the zone table. **/
    private Composite zoneTblComp;

    /** Station table composite that contains the station table. **/
    private Composite stationTblComp;

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

    /** Data source. **/
    public String dataSrc = "METAR";

    /** List of opened plots. **/
    private final Map<String, CaveSWTDialog> openedDlgs = new HashMap<>();

    /** row index in the station table. **/
    public int rowIndex;

    /** column index in the station table. **/
    public int colIndex;

    /** Common configuration for zone table. **/
    public CommonTableConfig config = CommonTableConfig.getInstance();

    /** current site **/
    protected String site;

    protected FSSObsMonitorConfigurationManager configMgr;

    /**
     * Constructor
     *
     * @param parent
     * @param obData
     * @param appName
     */
    public ZoneTableDlg(Shell parent, ObMultiHrsReports obData,
            CommonConfig.AppName appName) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.DO_NOT_BLOCK | CAVE.INDEPENDENT_SHELL);
        this.appName = appName;
        this.site = LocalizationManager.getInstance().getCurrentSite();
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
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.DO_NOT_BLOCK | CAVE.INDEPENDENT_SHELL);
        this.appName = appName;
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

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        initializeComponents();
    }

    /**
     * Initializes the components on the dialog.
     */
    private void initializeComponents() {
        createTopControls();
        shell.pack();
        shell.setMinimumSize(shell.getSize());

        // Add a horizontal separator bar to the display.
        Label sepLbl = new Label(getShell(), SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        tableStackComp = new Composite(getShell(), SWT.NONE);
        tableStackComp.setLayout(new GridLayout(1, false));
        tableStackComp
                .setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

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
        Composite controlComp = new Composite(getShell(), SWT.NONE);
        controlComp.setLayout(new GridLayout(6, false));

        GridData gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        linkToFrameChk = new Button(controlComp, SWT.CHECK);
        linkToFrameChk.setText("Link to Frame");
        linkToFrameChk.setSelection(linkedToFrame);
        linkToFrameChk.setLayoutData(gd);
        linkToFrameChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleLinkToFrame();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        gd.horizontalIndent = 10;
        Button vertChk = new Button(controlComp, SWT.CHECK);
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
            SimpleDateFormat sdf = new SimpleDateFormat(DFORMAT);
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            nominalTimeLbl.setText(sdf.format(nominalTime));
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
        zoneTblData.setSortColumnAndDirection(zoneSortColumn,
                zoneSortDirection);
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
        SimpleDateFormat sdf = new SimpleDateFormat(DFORMAT);
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        nominalTimeLbl.setText(sdf.format(date));
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
        updateStationTable(nominalTime);
        updateNominalTimeLabel(nominalTime);
    }

    /**
     * Sets Column and Sort Direction for Zone table.
     */
    protected void setZoneSortColumnAndDirection() {
        if (zoneTblData != null) {
            zoneSortColumn = zoneTblData.getSortColumn();
            zoneSortDirection = zoneTblData.getSortDirection();
        }
    }

    /**
     * Sets station table sort column index and sort direction to the current
     * station table sort column index and sort direction
     */
    protected void setStnSortColumnAndDirection() {
        if (stnTblData != null) {
            stnSortColumn = stnTblData.getSortColumn();
            stnSortDirection = stnTblData.getSortDirection();
        }
    }

    /**
     * Creates the station table.
     *
     * @param stackComp
     *            Stack composite.
     */
    private void createStationTable(Composite stackComp) {
        stationTblComp = new Composite(stackComp, SWT.NONE);
        stationTblComp.setLayout(new GridLayout(1, false));
        stationTblComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        // This is the initial creation of the station table composite.
        // No data is needed at this point as no zone has ever been selected.
        // Use updateStationTable() to display data when a zone is selected.
        // (Dec 16, 2009, zhao)
        stationTable = new StationTableComp(stationTblComp, null, appName,
                this);
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
        if (stnTblData != null) {
            stnTblData.setSortColumnAndDirection(stnSortColumn,
                    stnSortDirection);
            stnTblData.sortData();
            stationTable.setTableData(stnTblData);
        }
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
            boolean[] visCols = (boolean[]) attrDlg.open();
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
        getShell().setText(appName.name() + " Threat Level Zone/County Table");
        // re-set selectedZone to empty string
        selectedZone = "";
        updateNominalTimeLabel(nominalTime);
    }

    /**
     * Zone table action to display the zone table.
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
            statusHandler.debug("zoomToZone(\"" + selectedZone + "\") failed",
                    e);
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

    @Override
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
        String dlgTitle = getTrendPlotName(prodArray) + " Trend Plot for "
                + station + "#" + dataSrc;

        if (graphType == GraphType.Trend) {
            TrendPlotDlg tpd = (TrendPlotDlg) openedDlgs.get(dlgTitle);
            if (tpd == null) {
                tpd = new TrendPlotDlg(getShell(), selectedZone, station,
                        prodArray, dataSrc, dlgTitle);
                tpd.addCloseCallback(new ICloseCallback() {
                    @Override
                    public void dialogClosed(Object returnValue) {
                        openedDlgs.remove(returnValue);
                    }
                });
                openedDlgs.put(dlgTitle, tpd);
                tpd.setData(obData);
            }
            tpd.open();
        }
        if (graphType == GraphType.HodoWindDir) {
            // get data for a hodograph trend plot
            ObTrendDataSet dataset = obData.getTrendDataSet(selectedZone,
                    station, varName, ObConst.ProductName.UNDEFINED_PRODUCT);
            if (dataset != null && !dataset.isEmpty()) {
                HodographDlg hodographDlg = (HodographDlg) openedDlgs
                        .get(dlgTitle);
                if (hodographDlg == null) {
                    hodographDlg = new HodographDlg(getShell(), station,
                            dataSrc, dlgTitle, dataset);
                    hodographDlg.addCloseCallback(new ICloseCallback() {
                        @Override
                        public void dialogClosed(Object returnValue) {
                            openedDlgs.remove(returnValue);
                        }
                    });
                    openedDlgs.put(dlgTitle, hodographDlg);
                }
                hodographDlg.open();
            } else {
                prodArray = new ArrayList<>();
                prodArray.add("VAR_" + varName.name());
                TrendPlotDlg tpdHodo = (TrendPlotDlg) openedDlgs.get(dlgTitle);
                if (tpdHodo == null) {
                    tpdHodo = new TrendPlotDlg(getShell(), selectedZone,
                            station, prodArray, dataSrc, dlgTitle);
                    tpdHodo.addCloseCallback(new ICloseCallback() {
                        @Override
                        public void dialogClosed(Object returnValue) {
                            openedDlgs.remove(returnValue);
                        }
                    });
                    openedDlgs.put(dlgTitle, tpdHodo);
                    tpdHodo.setData(obData);
                }
                tpdHodo.open();
            }
        }
    }

    @Override
    public void launchObHistoryTable(int rowIndex) {
        String station = stnTblData.getTableRows().get(rowIndex)
                .getTableCellData(0).getCellText();
        // Set dialog index
        String dialogID = appName.name() + station;
        String strHistType = getMonitorAreaConfigInstance()
                .getStationType(selectedZone, station);
        ObsHistType histType = ObsHistType.valueOf(strHistType);

        /**
         * For Snow monitor, no history table is displayed for a Maritime
         * station
         */
        if (appName == AppName.SNOW && histType == ObsHistType.MARITIME) {
            return;
        }
        ObsHistTableDlg obsHstTblDlg = (ObsHistTableDlg) openedDlgs
                .get(dialogID);
        if (obsHstTblDlg == null) {
            obsHstTblDlg = new ObsHistTableDlg(getShell(),
                    obData.getHistTableData(selectedZone, station, histType),
                    appName, station, lat, lon, histType, dialogID);
            obsHstTblDlg.addCloseCallback(new ICloseCallback() {
                @Override
                public void dialogClosed(Object returnValue) {
                    openedDlgs.remove(returnValue);
                }
            });
            openedDlgs.put(dialogID, obsHstTblDlg);
        }
        obsHstTblDlg.open();
    }

    /**
     * Configuration button action method.
     */
    protected abstract void configThreshAction();

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
        Coordinate zoneCenter = MonitorAreaUtils.getZoneCenter(zone);
        if (zoneCenter == null) {
            // Test a newly added zone.
            AreaIdXML zoneXML = getMonitorAreaConfigInstance().getAreaXml(zone);
            if (zoneXML != null && (zoneXML.getCLon() != null
                    || zoneXML.getCLat() != null)) {
                zoneCenter = new Coordinate(zoneXML.getCLon(),
                        zoneXML.getCLat());
            }
        }
        zoomAndRecenter(zoneCenter, ZONE_ZOOM_LEVEL);
    }

    @Override
    public void zoomToStation(int rowIndex) {
        String selectedStation = stnTblData.getTableRows().get(rowIndex)
                .getTableCellData(0).getCellText();
        try {
            Coordinate stnCenter = MonitorAreaUtils
                    .getStationCenter(selectedStation);
            if (stnCenter == null) {
                // Center for moving platforms.
                SimpleDateFormat datef = new SimpleDateFormat(
                        "yyyy-MM-dd HH:mm:ss");
                datef.setTimeZone(TimeZone.getTimeZone("GMT"));
                String sql = "select longitude, latitude from fssobs where stationid = '"
                        + selectedStation + "'";
                List<Object[]> results = DirectDbQuery.executeQuery(sql,
                        "metadata", QueryLanguage.SQL);
                double x;
                double y;
                if (!results.isEmpty()) {
                    x = Double.parseDouble(results.get(0)[0].toString());
                    y = Double.parseDouble(results.get(0)[1].toString());
                    stnCenter = new Coordinate();
                    stnCenter.x = x;
                    stnCenter.y = y;
                }
            }
            zoomAndRecenter(stnCenter, STATION_ZOOM_LEVEL);
        } catch (SpatialException | ParseException | VizException e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to find the station center for station: "
                            + selectedStation,
                    e);
        }
    }

    /**
     * Re-centers and zooms map
     *
     * @param center
     * @param zoom
     */
    private void zoomAndRecenter(Coordinate center, float zoom) {
        if (center == null) {
            // No zoom.
            return;
        }
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            IDescriptor descriptor = container.getActiveDisplayPane()
                    .getDescriptor();
            int mapWidth;
            if (descriptor instanceof IMapDescriptor) {
                mapWidth = ((IMapDescriptor) descriptor).getMapWidth() / 1000;
            } else {
                mapWidth = 10_000;
            }
            float zoomLevel = zoom / mapWidth;
            for (IDisplayPane pane : container.getDisplayPanes()) {
                pane.getRenderableDisplay().getExtent().reset();
                pane.getRenderableDisplay()
                        .recenter(new double[] { center.x, center.y });
                pane.getRenderableDisplay().zoom(zoomLevel);
                pane.refresh();
            }
            container.refresh();
        }
    }

    /**
     * Linked To Frame
     *
     * @return True/False
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
     * Handles checkbox "Link to frame"
     */
    protected void handleLinkToFrame() {
        linkedToFrame = linkToFrameChk.getSelection();
    }

    @Override
    protected void disposed() {
        setReturnValue(true);
        configMgr = null;
    }

    /**
     * Initiates array of products to plot
     *
     * @return variable names
     */
    protected abstract void initiateProdArray();

    /**
     * Gets row index.
     *
     * @return rowIndex
     */
    public int getRowIndex() {
        return rowIndex;
    }

    /**
     * Sets row index.
     *
     * @param rowIndex
     */
    public void setRowIndex(int rowIndex) {
        this.rowIndex = rowIndex;
    }

    /**
     * Gets column index.
     *
     * @return colIndex
     */
    public int getColIndex() {
        return colIndex;
    }

    /**
     * Sets column index.
     *
     * @param colIndex
     */
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
        String name = prod.get(0);
        int stInd = name.indexOf('_');
        if (prod.size() > 1) {
            varName = name.substring(0, stInd);
            switch (varName) {
            case "SCA":
                varName = "Small Craft Advisory";
                break;
            case "GALE":
                varName = "Gale Warning";
                break;
            case "STORM":
                varName = "Storm Warning";
                break;
            case "HURRICANE":
                varName = "Hurricane Force Wind Warning";
                break;
            case "BLIZ":
                varName = "Blizzard Warning";
                break;
            case "FRZ":
                varName = "Freezing Precipitation";
                break;
            case "HSW":
                varName = "Heavy Snow Warning";
                break;
            default:
                statusHandler.error("Invalid name for variable " + varName);
            }
        } else {
            varName = name.substring(stInd + 1);
        }
        return varName;
    }

    /**
     * Gets Configuration manager.
     *
     * @return manager
     */
    protected abstract FSSObsMonitorConfigurationManager getMonitorAreaConfigInstance();

    /**
     * Refreshes Zone Table.
     *
     * @param obData
     */
    public void refreshZoneTableData(ObMultiHrsReports obData) {
        obData.getObHourReports().updateZones();
        obData.updateTableCache();
        this.updateTableDlg(obData.getObHourReports());
    }

    @Override
    public void fireConfigUpdate(IMonitorConfigurationEvent imce) {
        // do nothing
    }

    @Override
    public void fireThresholdUpdate(IMonitorThresholdEvent imte) {
        // do nothing
    }

    @Override
    public void fireKillMonitor() {
        // do nothing
    }

    @Override
    public void fireDialogShutdown(IMonitorListener iml) {
        // do nothing
    }

    @Override
    public boolean shouldClose() {
        MessageBox msgBox = new MessageBox(getShell(), SWT.OK);
        msgBox.setMessage("Use the Clear button on D2D to dispose this dialog");
        msgBox.open();
        return false;
    }

    /**
     * This method closes the dialog forcefully by disposing the shell.
     *
     * This is needed since the shouldClose() method always refuses to close the
     * dialog
     */
    public void closeDialog() {
        getShell().dispose();
    }
}