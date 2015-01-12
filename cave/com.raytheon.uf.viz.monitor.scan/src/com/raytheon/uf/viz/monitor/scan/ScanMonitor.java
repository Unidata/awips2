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
 * Contractor Address:     6825 Pine Street, Suite 144
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.monitor.scan;

import java.io.File;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListMap;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.scan.ScanRecord;
import com.raytheon.uf.common.dataplugin.scan.data.CellTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.PracticeWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.DMDTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.WARN_TYPE;
import com.raytheon.uf.common.monitor.scan.config.SCANMonitorConfig;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.ResourceMonitor;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.scan.ScanTime.ScanTimeType;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANSplash;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;
import com.raytheon.uf.viz.monitor.scan.data.DMDScanData;
import com.raytheon.uf.viz.monitor.scan.data.UnwarnedCell;
import com.raytheon.uf.viz.monitor.scan.listeners.IScanDialogListener;
import com.raytheon.uf.viz.monitor.scan.listeners.IScanRadarListener;
import com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg;
import com.raytheon.uf.viz.monitor.scan.tables.SCANCellTableDlg;
import com.raytheon.uf.viz.monitor.scan.tables.SCANDmdTableDlg;
import com.raytheon.uf.viz.monitor.scan.tables.SCANMesoTableDlg;
import com.raytheon.uf.viz.monitor.scan.tables.SCANTableData;
import com.raytheon.uf.viz.monitor.scan.tables.SCANTvsTableDlg;
import com.raytheon.viz.core.mode.CAVEMode;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.io.ParseException;
import com.vividsolutions.jts.io.WKBReader;

/**
 * 
 * ScanMonitor, monitor Data that triggers changes to the Scan display.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2009            dhladky     Initial creation
 * Apr 18, 2013     1926   njensen    Changed inner data maps to have Long key
 *                                                       to avoid !TimeStamp.equals(Date) issue
 * Apr 26, 2013     1926   njensen     Optimized getAvailableUris()
 * Jul 24, 2013     2218   mpduff      Improved error handling, optimizations
 * Jul 30, 2013     2143   skorolev    Changes for non-blocking dialogs.
 * Aug 15, 2013     2143   mpduff      Fixed bug in nullifyMonitor()
 * Sep 11, 2013     2277   mschenke    Got rid of ScriptCreator references
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class ScanMonitor extends ResourceMonitor implements IScanDialogListener {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScanMonitor.class);

    /** List of icaos */
    public List<String> icaos = new ArrayList<String>();

    /** Map of icao -> encompassing CWA */
    private final Map<String, String> cwas = new HashMap<String, String>();

    /** Map of icao -> dialog time */
    private final Map<String, Date> cellDialogTime = new HashMap<String, Date>();

    /** Map of icao -> dialog time */
    private final Map<String, Date> dmdDialogTime = new HashMap<String, Date>();

    /** Map of icao -> scan time */
    private final Map<String, Date> cellScanTime = new HashMap<String, Date>();

    /** Map of icao -> scan time */
    private final Map<String, Date> dmdScanTime = new HashMap<String, Date>();

    /** Map of icao -> map of ScanTable -> ScanTime */
    private final Map<String, Map<ScanTables, ScanTime>> times = new HashMap<String, Map<ScanTables, ScanTime>>();

    /** Singleton instance of this class */
    private static ScanMonitor monitor = null;

    /**
     * DMD table data, indexed by time and volume scan (with a record for each
     * azimith and elevation)
     **/
    public Map<String, ConcurrentMap<Long, DMDScanData>> dmdData = null;

    /** tvs table data, indexed by time **/
    @SuppressWarnings("rawtypes")
    public Map<String, ConcurrentMap<Long, ScanTableData>> tvsData = null;

    /** MESO table data, indexed by time **/
    @SuppressWarnings("rawtypes")
    public Map<String, ConcurrentMap<Long, ScanTableData>> mdData = null;

    /** cell table data, indexed by time **/
    @SuppressWarnings("rawtypes")
    public Map<String, ConcurrentMap<Long, ScanTableData>> cellData = null;

    /** Array of scan listeners **/
    private final List<IScanRadarListener> scanListeners = new ArrayList<IScanRadarListener>();

    /** Scan splash **/
    public SCANSplash scanSplashDlg = null;

    /** Map of icao -> cell table dialog */
    private final ConcurrentHashMap<String, SCANCellTableDlg> cellDialogs = new ConcurrentHashMap<String, SCANCellTableDlg>();

    /** Map of icao -> meso table dialog */
    private final ConcurrentHashMap<String, SCANMesoTableDlg> mesoDialogs = new ConcurrentHashMap<String, SCANMesoTableDlg>();

    /** Map of icao -> tvs table dialog */
    private final ConcurrentHashMap<String, SCANTvsTableDlg> tvsDialogs = new ConcurrentHashMap<String, SCANTvsTableDlg>();

    /** Map of icao -> dmd table dialog */
    private final ConcurrentHashMap<String, SCANDmdTableDlg> dmdDialogs = new ConcurrentHashMap<String, SCANDmdTableDlg>();

    /** SCAN Monitor Configuration object **/
    private SCANMonitorConfig config = null;

    /** SCAN table Configuration object **/
    private SCANConfig scanConfig = null;

    /** Map of icao -> cell tilt angle */
    private final Map<String, Double> cellTilts = new HashMap<String, Double>();

    /** Map of icao -> dmd tilt angle */
    private final Map<String, Double> dmdTilts = new HashMap<String, Double>();

    /** The geometry factory */
    private final GeometryFactory factory = new GeometryFactory();

    /** Station -> Coordinate map */
    private final Map<String, Coordinate> stationCoors = new HashMap<String, Coordinate>();

    /** Pattern for dates in radar */
    public static String datePattern = "yyyy-MM-dd HH:mm:ss";

    /** Map of Date -> ScanRecord */
    private final ConcurrentSkipListMap<Date, ScanRecord> dmdRecordList = new ConcurrentSkipListMap<Date, ScanRecord>();

    /**
     * Number of frames for display in cave.
     */
    private int frames = 12;

    /**
     * Set to true when new records come in.
     */
    private boolean dataUpdated = false;

    /** Instantiate flag */
    private boolean instantiated = false;

    /** Latest elevation */
    private Double latestElevation = null;

    /** used for Unwarned TVS, STR **/
    private static int NUMBER_OF_IMAGES = 2048;

    /**
     * Private constructor, singleton
     */
    private ScanMonitor() {
        pluginName = getConfig().getPlugins();
    }

    /**
     * Get instance, actual initialization if necessary
     * 
     * @return The singleton instance
     */
    public static final synchronized ScanMonitor getInstance() {
        if (monitor == null) {
            monitor = new ScanMonitor();
            monitor.createDataStructures();
        }

        return monitor;
    }

    /**
     * Creates the linked maps
     */
    @SuppressWarnings("rawtypes")
    private void createDataStructures() {
        dmdData = new HashMap<String, ConcurrentMap<Long, DMDScanData>>();
        tvsData = new HashMap<String, ConcurrentMap<Long, ScanTableData>>();
        mdData = new HashMap<String, ConcurrentMap<Long, ScanTableData>>();
        cellData = new HashMap<String, ConcurrentMap<Long, ScanTableData>>();
    }

    /**
     * Sets the config
     * 
     * @param config
     *            The config object
     */
    public void setConfig(SCANMonitorConfig config) {
        this.config = config;
    }

    /**
     * gets the config
     * 
     * @return The config object
     */
    public SCANMonitorConfig getConfig() {
        if (config == null) {
            config = new SCANMonitorConfig();
        }
        return config;
    }

    /**
     * gets the scan table config
     * 
     * @return the SCANConfig object
     */
    public SCANConfig getScanConfig() {
        if (scanConfig == null) {
            scanConfig = SCANConfig.getInstance();
        }
        return scanConfig;
    }

    /**
     * reset the data structs
     */
    public void resetData() {
        dmdData.clear();
        cellData.clear();
        tvsData.clear();
        mdData.clear();
    }

    /**
     * Initial setup of monitor
     * 
     * @param icao
     *            The icao to initialize
     */
    @SuppressWarnings("rawtypes")
    public void setup(String icao) {

        addIcao(icao);
        setStationCoordinate(icao);
        setCwa(icao);

        dmdData.put(icao, new ConcurrentHashMap<Long, DMDScanData>());
        cellData.put(icao, new ConcurrentHashMap<Long, ScanTableData>());
        tvsData.put(icao, new ConcurrentHashMap<Long, ScanTableData>());
        mdData.put(icao, new ConcurrentHashMap<Long, ScanTableData>());

        // kill splash
        if (scanSplashDlg != null) {
            scanSplashDlg.disposeDialog();
            scanSplashDlg = null;
        }
    }

    @Override
    public void thresholdUpdate(IMonitorThresholdEvent me) {
        // No-op
    }

    @Override
    public void configUpdate(IMonitorConfigurationEvent me) {
        this.scanConfig = SCANConfig.getInstance();
        fireMonitorEvent(me.getSource().getClass().getName());
        updateDrawingConfig();
    }

    /**
     * Kill this monitor by nullifying the monitor's private instance variable.
     */
    @Override
    public void nullifyMonitor(String icao) {

        if (cellDialogs.get(icao) == null) {
            monitor.removeMonitorListener(cellDialogs.get(icao));
            monitor.cellData.remove(icao);
            monitor.mdData.remove(icao);
            monitor.tvsData.remove(icao);
            cellDialogs.remove(icao);
        }
        if (dmdDialogs.get(icao) == null) {
            monitor.removeMonitorListener(dmdDialogs.get(icao));
            monitor.dmdData.remove(icao);
            dmdDialogs.remove(icao);
        }

        if ((dmdDialogs.isEmpty()) && (cellDialogs.isEmpty())) {
            // kill this monitor
            nullifyMonitor();
        }
    }

    @Override
    protected void nullifyMonitor() {
        monitor = null;
    }

    /**
     * Gets the radar station ICAOs
     * 
     * @return list of icaos
     */
    public List<String> getIcaos() {
        return icaos;
    }

    /**
     * Add an icao.
     * 
     * @param icao
     *            the icao to add
     */
    public void addIcao(String icao) {
        if (icao != null) {
            icaos.add(icao);
        }
    }

    /**
     * Populate the icao -> cwa map.
     * 
     * @param icao
     *            The icao
     */
    public void setCwa(String icao) {
        try {
            cwas.put(icao, getCWABySpatialQuery(getStationCoordinate(icao)));
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Gets the cell tilt angle
     * 
     * @param icao
     *            The icao
     * 
     * @return The cell tilt angle
     */
    public double getCellTilt(String icao) {
        if (cellTilts.get(icao) == null) {
            return 0.0;
        }
        return cellTilts.get(icao);
    }

    /**
     * Set the cell tilt for the provided icao
     * 
     * @param cellTilt
     *            The tilt
     * @param icao
     *            The icao
     */
    public void setCellTilt(double cellTilt, String icao) {
        if (icao != null) {
            cellTilts.put(icao, cellTilt);
        }
    }

    /**
     * Gets the DMD tilt angle
     * 
     * @param icao
     *            The icao
     * 
     * @return The tilt angle for the icao
     */
    public double getDmdTilt(String icao) {
        return dmdTilts.get(icao);
    }

    /**
     * Set the DMD tilt for the provided icao
     * 
     * @param dmdTilt
     *            The tilt
     * @param icao
     *            The icao
     */
    public void setDmdTilt(double dmdTilt, String icao) {
        dmdTilts.put(icao, dmdTilt);
    }

    /**
     * Gets the latest records
     * 
     * @param type
     *            The type of record
     * @param icao
     *            The icao
     * @param date
     *            The date
     * @param tilt
     *            The tilt (aka azimuth)
     * @return The corresponding ScanRecord
     * @throws VizException
     */
    public ScanRecord getScanRecord(ScanTables type, String icao, Date date,
            double tilt) throws VizException {
        String uri = getAvailableUri(tilt, date, type, icao);
        ScanRecord scanRec = null;
        if (uri != null) {
            scanRec = (ScanRecord) Loader.loadData(uri);
            File loc = HDF5Util.findHDF5Location(scanRec);
            IDataStore dataStore = DataStoreFactory.getDataStore(loc);
            if (scanRec != null) {
                scanRec.retrieveMapFromDataStore(dataStore);
            }
        }

        return scanRec;
    }

    /**
     * Return the tilt angle at which the table is currently at
     * 
     * @param type
     *            table type
     * @param icao
     *            icao
     * @return current tilt angle
     */
    public Double getTiltAngle(ScanTables type, String icao) {
        double angle = 0.0;
        if (type == ScanTables.DMD) {
            angle = getDmdTilt(icao);
        } else {
            angle = getCellTilt(icao);
        }

        return angle;
    }

    /**
     * Get most recent time
     * 
     * @param type
     *            Table Type
     * @param icao
     *            The ICAO
     * @return The DataTime
     */
    public DataTime getMostRecent(String type, String icao) {
        ScanTables scanType = ScanTables.valueOf(type);
        if ((scanType == ScanTables.MESO) || (scanType == ScanTables.TVS)) {
            scanType = ScanTables.CELL;
        }
        Set<Long> ds = getData(scanType, icao).keySet();
        if (ds == null || ds.isEmpty()) {
            return null;
        }
        List<Long> timeList = new ArrayList<Long>();
        for (Long d : ds) {
            timeList.add(d);
        }

        Collections.sort(timeList);

        return new DataTime(new Date(timeList.get(timeList.size() - 1)));
    }

    /**
     * Get the Keys for my table
     * 
     * @param type
     *            table type
     * @param icao
     *            the icao
     * @param time
     *            the time
     * @return The keys
     */

    public Set<String> getTableKeys(ScanTables type, String icao, Date time) {
        Set<String> keySet = null;
        if (getTableData(type, icao, time) != null) {
            keySet = getTableData(type, icao, time).getTableData().keySet();
        } else {
            return null;
        }
        return keySet;
    }

    /**
     * Sends a String from the TABLE enum for which table data to grab
     * 
     * @param table
     *            The table
     * @param icao
     *            The icao
     * @return The table data for the icao
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public ConcurrentMap<Long, ?> getData(ScanTables table, String icao) {
        ConcurrentMap<Long, ?> data = null;

        switch (table) {
        case CELL:
            data = cellData.get(icao);
            if (data == null) {
                data = new ConcurrentHashMap<Long, ScanTableData>();
                cellData.put(icao, (ConcurrentMap<Long, ScanTableData>) data);
            }
            break;
        case DMD:
            data = dmdData.get(icao);
            if (data == null) {
                data = new ConcurrentHashMap<Long, DMDScanData>();
                dmdData.put(icao, (ConcurrentHashMap<Long, DMDScanData>) data);
            }
            break;
        case MESO:
            data = mdData.get(icao);
            if (data == null) {
                data = new ConcurrentHashMap<Long, ScanTableData>();
                mdData.put(icao, (ConcurrentMap<Long, ScanTableData>) data);
            }
            break;
        case TVS:
            data = tvsData.get(icao);
            if (data == null) {
                data = new ConcurrentHashMap<Long, ScanTableData>();
                tvsData.put(icao, (ConcurrentMap<Long, ScanTableData>) data);
            }
            break;
        }

        return data;
    }

    /**
     * Gets the tableData by time, type
     * 
     * @param table
     *            The table
     * @param icao
     *            The icao
     * @param time
     *            The time
     * @return The table data for the time and icao
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public ScanTableData<?> getTableData(ScanTables table, String icao,
            Date time) {
        ScanTableData<?> tableData = null;
        ConcurrentMap<Long, ?> data = getData(table, icao);
        long longtime = time.getTime();
        switch (table) {
        case CELL:
        case TVS:
        case MESO:
            tableData = (ScanTableData<?>) data.get(longtime);
            if (tableData == null) {
                tableData = getNewTableData(table, icao, time,
                        getTiltAngle(table, icao));
                if (tableData != null) {
                    ((ConcurrentHashMap<Long, ScanTableData>) data).put(
                            longtime, tableData);
                }
            }
            break;
        case DMD:
            DMDScanData dmdsd = (DMDScanData) data.get(longtime);
            if (dmdsd != null) {
                tableData = dmdsd.getTableData(longtime);
            }
            break;
        }

        return tableData;
    }

    /**
     * Get the DMD Table Data by elevation.
     * 
     * @param icao
     *            The icao
     * @param time
     *            The time
     * @param tiltAngle
     *            The tilt angle
     * @return The DMD table data
     */
    public ScanTableData<?> getDmdTableData(String icao, Date time,
            double tiltAngle) {
        ScanTableData<?> tableData = null;
        ConcurrentMap<Long, ?> data = getData(ScanTables.DMD, icao);
        DMDScanData dmdsd = (DMDScanData) data.get(time.getTime());
        if (dmdsd != null) {
            tableData = dmdsd.getTableData(time.getTime());
        }

        return tableData;
    }

    /**
     * gets the newest tableData by time, type
     * 
     * @param type
     *            The type
     * @param icao
     *            the icao
     * @param date
     *            the date
     * @param tilt
     *            the tilt
     * @return table data
     */
    public ScanTableData<?> getNewTableData(ScanTables type, String icao,
            Date date, double tilt) {
        ScanTableData<?> data = null;
        try {
            ScanRecord rec = getScanRecord(type, icao, date, tilt);
            if (rec != null) {
                data = rec.getTableData();
            }
        } catch (VizException ve) {
            statusHandler.warn("Couldn't load new ScanRecord: " + type.name());
        }

        return data;
    }

    /**
     * Set new row data
     * 
     * @param icao
     * @param data
     * @param date
     * @param angle
     * @param scanTime
     * @param type
     */
    @SuppressWarnings({ "unchecked" })
    public void setTableData(String icao, ScanTableData<?> data, Date date,
            double angle, Date scanTime, String type) {
        long longScanTime = scanTime.getTime();
        if (type.equals(ScanTables.DMD.name())) {
            ConcurrentMap<Long, DMDScanData> dataMap = (ConcurrentMap<Long, DMDScanData>) getData(
                    ScanTables.DMD, icao);
            if (dataMap.containsKey(longScanTime)) {
                DMDScanData dmdsd = dataMap.get(longScanTime);
                if (!dmdsd.containsKey(angle)) {
                    dmdsd.addData(angle, date.getTime(), data);
                }
            } else { // new volume scan
                DMDScanData sdata = new DMDScanData(longScanTime);
                sdata.addData(angle, date.getTime(), data);
                dataMap.put(longScanTime, sdata);
            }
        } else if (type.equals(ScanTables.CELL.name())) {
            ConcurrentMap<Long, ScanTableData<?>> dataMap = (ConcurrentMap<Long, ScanTableData<?>>) getData(
                    ScanTables.CELL, icao);
            if (!dataMap.containsKey(longScanTime)) {
                dataMap.put(longScanTime, data);
            }
        }
    }

    /**
     * launch one of the scan table dialogs
     * 
     * @param shell
     *            The parent shell
     * @param icao
     *            The icao
     * @param table
     *            the table type
     */
    @Override
    public void launchDialog(Shell shell, String icao, ScanTables table) {
        if (table.equals(ScanTables.CELL)) {
            if (cellDialogs.isEmpty()) {
                getScanConfig().reload(ScanTables.CELL);
            }
            if (cellDialogs.get(icao) == null
                    || cellDialogs.get(icao).isDisposed()) {
                SCANCellTableDlg cellDialog = new SCANCellTableDlg(shell, icao,
                        new SCANTableData(ScanTables.CELL));
                cellDialogs.put(icao, cellDialog);
                monitor.addMonitorListener(cellDialog);
                cellDialog.addMonitorControlListener(monitor);
            } else {
                cellDialogs.get(icao).bringToTop();
            }
        } else if (table.equals(ScanTables.DMD)) {
            if (dmdDialogs.isEmpty()) {
                getScanConfig().reload(ScanTables.DMD);
            }
            if (dmdDialogs.get(icao) == null
                    || dmdDialogs.get(icao).isDisposed()) {
                SCANDmdTableDlg dmdDialog = new SCANDmdTableDlg(shell, icao,
                        new SCANTableData(ScanTables.DMD));
                dmdDialogs.put(icao, dmdDialog);
                monitor.addMonitorListener(dmdDialog);
                dmdDialog.addMonitorControlListener(monitor);
            } else {
                dmdDialogs.get(icao).bringToTop();
            }
        } else if (table.equals(ScanTables.MESO)) {
            if (mesoDialogs.isEmpty()) {
                getScanConfig().reload(ScanTables.MESO);
            }
            if (mesoDialogs.get(icao) == null
                    || mesoDialogs.get(icao).isDisposed()) {
                SCANMesoTableDlg mesoDialog = new SCANMesoTableDlg(shell, icao,
                        new SCANTableData(ScanTables.MESO));
                mesoDialogs.put(icao, mesoDialog);
                monitor.addMonitorListener(mesoDialog);
                mesoDialog.addMonitorControlListener(monitor);
            } else {
                mesoDialogs.get(icao).bringToTop();
                return;
            }
        } else if (table.equals(ScanTables.TVS)) {
            if (tvsDialogs.isEmpty()) {
                getScanConfig().reload(ScanTables.TVS);
            }
            if (tvsDialogs.get(icao) == null
                    || tvsDialogs.get(icao).isDisposed()) {
                SCANTvsTableDlg tvsDialog = new SCANTvsTableDlg(shell, icao,
                        new SCANTableData(ScanTables.TVS));
                tvsDialogs.put(icao, tvsDialog);
                monitor.addMonitorListener(tvsDialog);
                tvsDialog.addMonitorControlListener(monitor);
            } else {
                tvsDialogs.get(icao).bringToTop();
                return;
            }
        }
        // fill the table for the first time
        fireMonitorEvent(getDialog(table, icao).getClass().getName());
    }

    /**
     * add a listener
     * 
     * @param isrl
     *            listener to add
     */
    public void addScanRadarListener(IScanRadarListener isrl) {
        scanListeners.add(isrl);
    }

    /**
     * remove a listener
     * 
     * @param isrl
     *            listener to remove
     */
    public void removeScanRadarListener(IScanRadarListener isrl) {
        scanListeners.remove(isrl);
    }

    @Override
    public void recenter(String ident, ScanTables type, String icao) {
        if (getTableData(type, icao, getScanTime(type, icao)) != null) {
            if (getTableData(type, icao, getScanTime(type, icao)).getRow(ident) != null) {
                double lat = getTableData(type, icao, getScanTime(type, icao))
                        .getRow(ident).getLat();
                double lon = getTableData(type, icao, getScanTime(type, icao))
                        .getRow(ident).getLon();
                final Coordinate coor = new Coordinate(lon, lat, 0.0);
                Display.getDefault().asyncExec(new Runnable() {
                    @Override
                    public void run() {
                        Iterator<IScanRadarListener> iter = scanListeners
                                .iterator();
                        while (iter.hasNext()) {
                            iter.next().recenter(coor);
                        }
                    }
                });
            }
        }
    }

    @Override
    public void paintScan() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                Iterator<IScanRadarListener> iter = scanListeners.iterator();
                while (iter.hasNext()) {
                    iter.next().paintScan();
                }
            }
        });
    }

    @Override
    public void updateDrawingConfig() {
        Display.getDefault().asyncExec(new Runnable() {
            @Override
            public void run() {
                // actually using IScanRadarListener interface as gateway to
                // call other methods on ScanResource.
                Iterator<IScanRadarListener> iter = scanListeners.iterator();
                while (iter.hasNext()) {
                    iter.next().updateDrawingConfig();
                }
            }
        });
    }

    /**
     * close down the dialog
     * 
     * @param type
     *            dialog type
     * @param icao
     *            the icao
     */
    public void closeDialog(ScanTables type, String icao) {
        if (type.equals(ScanTables.CELL)) {
            if ((cellDialogs.get(icao) != null)
                    && (cellDialogs.get(icao).getCurrentShell() != null)) {
                monitor.removeMonitorListener(cellDialogs.get(icao));
                cellDialogs.get(icao).shellDisposeDialog();
                cellDialogs.remove(icao);
            }
            if ((mesoDialogs.get(icao) != null)
                    && (mesoDialogs.get(icao).getCurrentShell() != null)) {
                monitor.removeMonitorListener(mesoDialogs.get(icao));
                mesoDialogs.get(icao).shellDisposeDialog();
                mesoDialogs.remove(icao);
            }
            if ((tvsDialogs.get(icao) != null)
                    && (tvsDialogs.get(icao).getCurrentShell() != null)) {
                monitor.removeMonitorListener(tvsDialogs.get(icao));
                tvsDialogs.get(icao).shellDisposeDialog();
                tvsDialogs.remove(icao);
            }
        } else if (type.equals(ScanTables.DMD)) {
            monitor.removeMonitorListener(dmdDialogs.get(icao));
            if ((dmdDialogs.get(icao) != null)
                    && (dmdDialogs.get(icao).getCurrentShell() != null)) {
                dmdDialogs.get(icao).shellDisposeDialog();
                dmdDialogs.remove(icao);
            }
        } else if (type.equals(ScanTables.MESO)) {
            monitor.removeMonitorListener(mesoDialogs.get(icao));
            if (mesoDialogs.get(icao) != null) {
                mesoDialogs.get(icao).shellDisposeDialog();
                mesoDialogs.remove(icao);
            }
        } else if (type.equals(ScanTables.TVS)) {
            monitor.removeMonitorListener(tvsDialogs.get(icao));
            if (tvsDialogs.get(icao) != null) {
                tvsDialogs.get(icao).shellDisposeDialog();
                tvsDialogs.remove(icao);
            }
        }
    }

    /**
     * close down all open scan dialogs
     * 
     * @param icao
     *            the icao
     */
    public void closeDialog(String icao) {
        final String ficao = icao;
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                for (ScanTables type : ScanTables.values()) {
                    closeDialog(type, ficao);
                }

            }
        });
    }

    /**
     * Gets the dialogs
     * 
     * @param scanTables
     *            table type
     * @param icao
     *            the icao
     * @return the dialog
     */
    public AbstractTableDlg getDialog(
            com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables scanTables,
            String icao) {
        switch (scanTables) {
        case CELL:
            return cellDialogs.get(icao);
        case DMD:
            return dmdDialogs.get(icao);
        case MESO:
            return mesoDialogs.get(icao);
        case TVS:
            return tvsDialogs.get(icao);
        }

        return null;
    }

    @Override
    public TrendGraphData getGraphData(ScanTables type, String icao,
            String field, String ident) {
        TrendGraphData tgData = new TrendGraphData();
        LinkedHashMap<Date, Double> graphData = new LinkedHashMap<Date, Double>();
        LinkedHashMap<Date, Color> dataColors = new LinkedHashMap<Date, Color>();
        SCANConfig scanCfg = SCANConfig.getInstance();

        if (type == ScanTables.CELL) {
            for (SCANConfigEnums.CELLTable fenum : SCANConfigEnums.CELLTable
                    .values()) {

                if (fenum.getColName().compareTo(field) == 0) {
                    for (Date dt : getTimeOrderedKeys(this, type.name(), icao)) {
                        if (getTableData(type, icao, dt).getTableData()
                                .containsKey(ident)) {

                            ScanTableDataRow stdr = getTableData(type, icao, dt)
                                    .getRow(ident);

                            // Set the value
                            Double val = stdr.getValue(fenum.getColName());
                            graphData.put(dt, val);

                            // Set the color for the value
                            dataColors
                                    .put(dt, scanCfg.getThresholdColor(type,
                                            field, val));
                        }
                    }
                }
            }
        } else if (type == ScanTables.DMD) {
            for (SCANConfigEnums.DMDTable fenum : SCANConfigEnums.DMDTable
                    .values()) {

                if (fenum.getColName().compareTo(field) == 0) {
                    // Carry these values forward so the graph doesn't show 0
                    double prevMsi = 0;
                    double prevLlconv = 0;
                    double prevLlshr = 0;
                    for (Date dt : getTimeOrderedKeys(this, type.name(), icao)) {
                        ScanTableData<?> tableData = getTableData(type, icao,
                                dt);
                        if (tableData != null) {
                            DMDTableDataRow dtdr = (DMDTableDataRow) tableData
                                    .getRow(ident);
                            double value;
                            if (dtdr != null) {
                                value = dtdr.getValue(fenum.getColName());
                                if (fenum.getColName().equalsIgnoreCase(
                                        DMDTable.MSI.getColName())) {
                                    if (value == 0) {
                                        value = prevMsi;
                                    }
                                    prevMsi = value;
                                } else if (fenum.getColName().equalsIgnoreCase(
                                        DMDTable.LLCONV.getColName())) {
                                    if (value == 0) {
                                        value = prevLlconv;
                                    }
                                    prevLlconv = value;
                                } else if (fenum.getColName().equalsIgnoreCase(
                                        DMDTable.LLSHR.getColName())) {
                                    if (value == 0) {
                                        value = prevLlshr;
                                    }
                                    prevLlshr = value;
                                }

                                if (fenum.getColName().equalsIgnoreCase(
                                        DMDTable.BASE.getColName())) {
                                    Color color = scanCfg.getDMDBaseBGColor(
                                            dtdr.getElev0(), value);
                                    dataColors.put(dt, color);
                                } else {
                                    // Set the color for the value
                                    dataColors.put(dt, scanCfg
                                            .getThresholdColor(type, field,
                                                    value));
                                }
                                graphData.put(dt, value);
                            }
                        }
                    }
                }
            }
        }

        tgData.setGraphData(graphData);
        tgData.setGraphDataColors(dataColors);

        return tgData;
    }

    /**
     * Updates the Monitor and Dialog with info from resource
     * 
     * @param type
     *            the type
     * @param icao
     *            the icao
     * @param dialogTime
     *            the dialog time
     * @param scanTime
     *            the scan time
     * @param tilt
     *            the tilt angle
     */
    public void updateDialog(ScanTables type, String icao, Date dialogTime,
            Date scanTime, double tilt) {
        if (type.equals(ScanTables.CELL)) {
            cellDialogTime.put(icao, dialogTime);
            cellScanTime.put(icao, scanTime);
            setCellTilt(tilt, icao);
        } else if (type.equals(ScanTables.DMD)) {
            dmdDialogTime.put(icao, dialogTime);
            dmdScanTime.put(icao, scanTime);
            setDmdTilt(tilt, icao);
        }
        if (getDialog(type, icao) != null) {
            fireMonitorEvent(getDialog(type, icao).getClass().getName());
        }
    }

    /**
     * Dialogs retrieve the drawTime
     * 
     * @param type
     *            The dialog type
     * @param icao
     *            the icao
     * 
     * @return the dialog time
     */
    public Date getDialogTime(ScanTables type, String icao) {
        if (type.equals(ScanTables.DMD)) {
            return dmdDialogTime.get(icao);
        } else {
            return cellDialogTime.get(icao);
        }
    }

    /**
     * Dialogs retrieve the drawTime
     * 
     * @param type
     *            The dialog type
     * @param icao
     *            the icao
     * 
     * @return the scan time
     */
    public Date getScanTime(ScanTables type, String icao) {
        if (type.equals(ScanTables.DMD)) {
            return dmdScanTime.get(icao);
        } else {
            return cellScanTime.get(icao);
        }
    }

    /**
     * launches the splash screen
     * 
     * @param shell
     *            The parent shell
     */
    public void launchSplash(Shell shell) {
        scanSplashDlg = new SCANSplash(shell);
    }

    /**
     * triggers the receiving table that it needs to request a trend set
     */
    @Override
    public void launchTrendGraphs(ScanTables table, String icao, String id) {
        getDialog(table, icao).fireTrendSet(id, table);
    }

    @Override
    public void configurationLoaded(ScanTables table, String icao) {
        if (getDialog(table, icao) != null) {
            fireMonitorEvent(getDialog(table, icao).getClass().getName());
        }
        paintScan();
    }

    @Override
    public void thresholdUpdated(ScanTables table, String icao, String attr) {
        switch (table) {
        case MESO:
            if ((mesoDialogs.get(icao) != null)
                    && (!mesoDialogs.get(icao).getCurrentShell().isDisposed())) {
                mesoDialogs.get(icao).updateThresh(attr);
            }
            break;
        case CELL:
            if ((cellDialogs.get(icao) != null)
                    && (!cellDialogs.get(icao).getCurrentShell().isDisposed())) {
                cellDialogs.get(icao).updateThresh(attr);
            }
            break;

        case DMD:
            if ((dmdDialogs.get(icao) != null)
                    && (!dmdDialogs.get(icao).getCurrentShell().isDisposed())) {
                dmdDialogs.get(icao).updateThresh(attr);
            }
            break;
        case TVS:
            if ((tvsDialogs.get(icao) != null)
                    && (!tvsDialogs.get(icao).getCurrentShell().isDisposed())) {
                tvsDialogs.get(icao).updateThresh(attr);
            }
            break;
        }
    }

    /**
     * Get CWA for the provided coordinate
     * 
     * @param c
     *            the Coordinate of the CWA
     * @return the CWA for he provided coordinate
     * @throws Exception
     *             on error
     */
    private String getCWABySpatialQuery(Coordinate c) throws Exception {
        ISpatialQuery sq = null;
        String cwa = null;
        try {
            sq = SpatialQueryFactory.create();
            // convert coordinate to point geometry
            Point geometry = factory.createPoint(c);
            SpatialQueryResult[] results = null;
            results = sq.query("cwa", new String[] { "cwa" }, geometry, null,
                    false, SearchMode.WITHIN);
            cwa = (String) results[0].attributes.get("cwa");
        } catch (SpatialException e) {
            throw new SpatialException(
                    "Error getting CWA by provided coordinates");
        }

        return cwa;
    }

    /**
     * Gets the station Coordinate
     * 
     * @param icao
     *            The icao
     */
    public void setStationCoordinate(String icao) {

        String sql = "select AsBinary(the_geom) from radar_spatial where rda_id = \'"
                + icao.toUpperCase() + "\'";
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(sql,
                    "metadata", QueryLanguage.SQL);
            Object[] objs = results.get(0);
            WKBReader reader = new WKBReader();
            Geometry stationGeo = reader.read((byte[]) objs[0]);
            stationCoors.put(icao, stationGeo.getCoordinate());

        } catch (VizException e) {
            statusHandler.error("Error setting station coordinates", e);
        } catch (ParseException pe) {
            statusHandler.error("Error setting station coordinates", pe);
        }
    }

    /**
     * Gets the StationCoordinate
     * 
     * @param icao
     *            The icao
     * 
     * @return the coordinates for the icao
     */
    public Coordinate getStationCoordinate(String icao) {
        return stationCoors.get(icao);
    }

    /**
     * Gets the CWA
     * 
     * @param icao
     *            The icao
     * 
     * @return the CWA for the icao
     */
    public String getCwa(String icao) {
        return cwas.get(icao);
    }

    /**
     * Get the DMD frames for the max angle for the Volume Scan Time.
     * 
     * @param icao
     *            The icao
     * 
     * @return Array of times for the specified data
     */
    public long[] getDMDMaxAngleTimes(String icao) {
        String sql = "(select reftime from scan where type = 'DMD'"
                + " and icao = '"
                + icao
                + "' and lastelevationangle = 't' "
                + "order by reftime desc) union "
                + "(select reftime from scan where type = 'DMD'"
                + " and icao = '"
                + icao
                + "' order by reftime desc, tilt desc limit 1) order by reftime desc;";
        long[] recordList = new long[0];
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(sql,
                    "metadata", QueryLanguage.SQL);

            if (!results.isEmpty()) {
                recordList = new long[results.size()];
                for (int i = 0; i < recordList.length; i++) {
                    Object[] oa = results.get(i);
                    Timestamp reftime = (Timestamp) oa[0];
                    recordList[i] = reftime.getTime();
                }
            }
        } catch (VizException e) {
            statusHandler.error("Error getting DMD max angle times", e);
        }
        return recordList;
    }

    /**
     * Getting the available uri
     * 
     * @param angle
     *            (aka azimuth)
     * @param date
     *            the date
     * @param type
     *            the type
     * @param icao
     *            the icao
     * @return data uri
     */
    public String getAvailableUri(Double angle, Date date, ScanTables type,
            String icao) {
        String uri = null;

        SimpleDateFormat datef = new SimpleDateFormat(datePattern);
        datef.setTimeZone(TimeZone.getTimeZone("GMT"));
        String time = datef.format(date);
        String sql = "select datauri from scan where tilt = " + angle
                + " and type = '" + type.name() + "' and icao = '" + icao
                + "' ";

        if (type == ScanTables.CELL) {
            sql = sql.concat("order by reftime desc limit 1");
        } else {
            sql = sql.concat("and reftime = '" + time
                    + "' order by reftime desc limit 1");
        }

        try {
            List<Object[]> results = DirectDbQuery.executeQuery(sql,
                    "metadata", QueryLanguage.SQL);

            if (results.size() > 0) {
                uri = (String) results.get(0)[0];
            }

        } catch (VizException e) {
            statusHandler.error("Unable to get URI", e);
        }

        return uri;
    }

    /**
     * Get available URIs for the type and icao
     * 
     * @param type
     *            the Type
     * @param icao
     *            the icao
     * @return list of URIs
     */
    public List<String> getAvailableUris(ScanTables type, String icao) {
        List<String> uriList = null;
        String sql = null;
        if (type == ScanTables.DMD) {
            sql = "(select datauri from scan where type = '"
                    + type.name()
                    + "' and icao = '"
                    + icao
                    + "' and lastelevationangle ='t')"
                    + " union (select datauri from scan where type = '"
                    + type.name()
                    + "' and icao = '"
                    + icao
                    + "' and lastelevationangle = 'f' order by reftime desc, tilt desc limit 1)";
        } else {
            sql = "(select datauri from scan where type = '" + type.name()
                    + "' and icao = '" + icao + "')";
        }

        try {
            List<Object[]> results = DirectDbQuery.executeQuery(sql,
                    "metadata", QueryLanguage.SQL);

            uriList = new ArrayList<String>(results.size());
            if (results.size() > 0) {
                for (Object[] ob : results) {
                    uriList.add((String) ob[0]);
                }
            }
        } catch (VizException e) {
            statusHandler.error("Error retrieving scan uris", e);
        }

        return uriList;
    }

    /**
     * Gets you the entire volume for Time Height Graphs, by column graphed
     * 
     * @param tableCol
     *            Table column.
     * @param dmdIdent
     *            Ident.
     * @param currentTime
     *            Time currently displayed in the DMD table dialog
     * @return The time height graph data or null if empty.
     */
    public TreeMap<Long, DMDTableDataRow> getTimeHeightGraphData(String icao,
            SCANConfigEnums.DMDTable tableCol, String dmdIdent, Date currentTime) {
        TreeMap<Long, DMDTableDataRow> mapData = new TreeMap<Long, DMDTableDataRow>();
        DMDScanData dmdScanData;

        for (Date date : getTimeOrderedKeys(this, ScanTables.DMD.name(), icao)) {
            // Current time plus 4 minutes
            if (date.getTime() <= currentTime.getTime() + 240000) {
                dmdScanData = (DMDScanData) getData(ScanTables.DMD, icao).get(
                        date.getTime());

                TreeMap<Long, DMDTableDataRow> tmp = dmdScanData
                        .getTimeHeightData(tableCol, dmdIdent);
                if (tmp == null) {
                    continue;
                }

                mapData.putAll(tmp);
            }
        }

        if (mapData.isEmpty()) {
            return null;
        }

        return mapData;
    }

    /**
     * Sort by Date
     * 
     * @author dhladky
     * 
     */
    public class SortByDate implements Comparator<Date> {

        @Override
        public int compare(Date o1, Date o2) {

            return o1.compareTo(o2);
        }
    }

    /**
     * Order the dates
     * 
     * @param monitor
     * @param type
     * @param icao
     * @return
     */
    public List<Date> getTimeOrderedKeys(IMonitor monitor, String type,
            String icao) {
        List<Date> dates = new ArrayList<Date>();
        for (Long date : getData(ScanTables.valueOf(type), icao).keySet()) {
            dates.add(new Date(date));
        }
        Collections.sort(dates, new SortByDate());
        return dates;
    }

    /**
     * Create a list of ScanRecords containing only the maximum tilt angle for
     * each volume scan and the number of records to match the number of frames
     * being displayed.
     * 
     * @param rec
     *            The ScanRecord
     */
    public void addDmdScanRecord(ScanRecord rec) {
        boolean added = false;
        for (Date d : dmdRecordList.keySet()) {
            ScanRecord record = dmdRecordList.get(d);
            if (rec.getDataTime() == record.getDataTime()) {
                dmdRecordList.remove(d);
            } else {
                added = true;
            }
        }

        this.dmdRecordList.put(rec.getDataTime().getRefTime(), rec);

        if (added && (dmdRecordList.size() >= frames)) {
            for (Date d : dmdRecordList.keySet()) {
                dmdRecordList.remove(d);
                break;
            }
        }
    }

    /**
     * @return the dmdRecordList
     */
    public List<ScanRecord> getDmdRecordList() {
        List<ScanRecord> returnList = new ArrayList<ScanRecord>();

        for (Date d : dmdRecordList.keySet()) {
            returnList.add(dmdRecordList.get(d));
        }

        return returnList;
    }

    /**
     * Set the number of frames.
     * 
     * @param frames
     *            number of frames
     */
    public void setFrames(int frames) {
        this.frames = frames;
    }

    /**
     * Get the number of frames
     * 
     * @return number of frames
     */
    public int getFrames() {
        return this.frames;
    }

    /**
     * Gets a list of unwarned cells, if any exist
     * 
     * @param icao
     * @param time
     * @param severe
     * @param tvs
     * @return
     */
    public Map<String, UnwarnedCell> retrieveWarnings(String icao, Date time,
            boolean severe, boolean tvs) {

        List<String> removeList = new ArrayList<String>();
        HashMap<String, UnwarnedCell> cells = new HashMap<String, UnwarnedCell>();
        List<AbstractWarningRecord> tvswarnings = new ArrayList<AbstractWarningRecord>();
        List<AbstractWarningRecord> severewarnings = new ArrayList<AbstractWarningRecord>();

        DbQueryRequest request = new DbQueryRequest();
        // TODO: There should be a smarter way to cap data requested using time
        // passed in possibly. Should investigate into it
        request.setLimit(NUMBER_OF_IMAGES);
        request.setEntityClass(CAVEMode.getMode() == CAVEMode.PRACTICE ? PracticeWarningRecord.class
                : WarningRecord.class);
        if (icao != null) {
            request.addConstraint("officeid",
                    new RequestConstraint(icao.toUpperCase()));
        }

        GeometryFactory factory = new GeometryFactory();

        try {
            ScanTableData<?> cellTable = getTableData(ScanTables.CELL, icao,
                    time);

            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            for (AbstractWarningRecord rec : response
                    .getEntityObjects(AbstractWarningRecord.class)) {
                if (rec.getPhensig() != null) {
                    if (tvs
                            && rec.getPhensig().equals(
                                    ScanUtils.TORNADO_WARNING_PHENSIG)) {
                        tvswarnings.add(rec);
                    }
                    if (severe
                            && rec.getPhensig().equals(
                                    ScanUtils.SEVERE_THUNDERSTORM_PHENSIG)) {
                        severewarnings.add(rec);
                    }
                }
            }

            // check all of the cells for ones that meet Unwarned criteria
            for (String key : cellTable.getTableData().keySet()) {
                CellTableDataRow ctdr = (CellTableDataRow) cellTable
                        .getRow(key);
                // find cells which meet criteria for warnings
                if (severe && tvs) {
                    boolean sts = false;
                    boolean tor = false;
                    if (isUnwarned(ctdr, WARN_TYPE.TVS)) {
                        tor = true;
                    }
                    if (isUnwarned(ctdr, WARN_TYPE.SEVERE)) {
                        sts = true;
                    }

                    if (sts && !tor) {
                        cells.put(key, new UnwarnedCell(key, WARN_TYPE.SEVERE));
                    } else if (!sts && tor) {
                        cells.put(key, new UnwarnedCell(key, WARN_TYPE.TVS));
                    } else if (sts && tor) {
                        // more critical cell gets added by when you find both
                        // cells.add(new UnwarnedCell(key, WARN_TYPE.TVS));
                        cells.put(key, new UnwarnedCell(key, WARN_TYPE.TVS));
                    }
                }
                if (severe && !tvs) {
                    if (isUnwarned(ctdr, WARN_TYPE.SEVERE)) {
                        cells.put(key, new UnwarnedCell(key, WARN_TYPE.SEVERE));
                    }
                }
                if (!severe && tvs) {
                    if (isUnwarned(ctdr, WARN_TYPE.TVS)) {
                        cells.put(key, new UnwarnedCell(key, WARN_TYPE.TVS));
                    }
                }
            }

            // run over tvs first, eliminate already warned events
            for (String key : cells.keySet()) {
                CellTableDataRow ctdr = (CellTableDataRow) cellTable
                        .getRow(cells.get(key).getCellId());
                Point point = factory.createPoint(new Coordinate(ctdr.getLon(),
                        ctdr.getLat()));

                // Looking for an unwarned TOR
                if (tvs && (tvswarnings.size() > 0)) {
                    boolean warned = false;
                    for (AbstractWarningRecord rec : tvswarnings) {
                        if (rec.getGeometry().contains(point)) {
                            // has a SVR but not a TVS warning
                            if ((cells.get(key).getWarnType() == WARN_TYPE.TVS)
                                    && rec.getPhensig().equals(
                                            ScanUtils.TORNADO_WARNING_PHENSIG)) {
                                warned = true;
                            }
                        }
                    }

                    if (warned) {
                        if (!removeList.contains(cells.get(key).getCellId())) {
                            removeList.add(cells.get(key).getCellId());
                        }
                    }
                }

                // Looking for an unwarned SVR
                if (tvs && (severewarnings.size() > 0)) {
                    boolean warned = false;
                    for (AbstractWarningRecord rec : severewarnings) {
                        if (rec.getGeometry().contains(point)) {
                            // has a SVR but not a TVS warning
                            if ((cells.get(key).getWarnType() == WARN_TYPE.SEVERE)
                                    && rec.getPhensig()
                                            .equals(ScanUtils.SEVERE_THUNDERSTORM_PHENSIG)) {
                                warned = true;
                            }
                        }
                    }
                    if (warned) {
                        if (!removeList.contains(cells.get(key).getCellId())) {
                            removeList.add(cells.get(key).getCellId());
                        }
                    }
                }
            }

            // get rid of warnings already accounted for
            for (String cellId : removeList) {
                cells.remove(cellId);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error retreiving warnings",
                    e);
        }

        return cells;
    }

    /**
     * Run over the unwarned criteria checks
     * 
     * @param row
     *            row to check
     * @param warnType
     *            warn type
     * @return true if unwarned
     */
    public boolean isUnwarned(CellTableDataRow row, WARN_TYPE warnType) {

        boolean isUnwarned = false;
        boolean dbz = false;
        boolean vil = false;
        boolean mdasr = false;
        boolean tvs = false;
        boolean hail = false;

        final String NONE = "NONE";
        if (warnType == WARN_TYPE.SEVERE) {

            // actual value checks
            if (getScanConfig().getUnwarnedConfig().getSvrMaxDbz()) {
                if (getScanConfig().getUnwarnedConfig().getSvrMaxDbzVal() <= row
                        .getDbz()) {
                    dbz = true;
                } else {
                    dbz = false;
                }
            }
            if (getScanConfig().getUnwarnedConfig().getSvrMaxVil()) {
                if (getScanConfig().getUnwarnedConfig().getSvrMaxVilVal() <= row
                        .getVil()) {
                    vil = true;
                } else {
                    vil = false;
                }
            }
            if (getScanConfig().getUnwarnedConfig().getSvrMesoStRank()
                    && !row.getMdaSR().equals(NONE)) {
                if (getScanConfig().getUnwarnedConfig().getSvrMesoStRankVal() <= ScanUtils
                        .parseMDASR(row.getMdaSR())) {
                    mdasr = true;
                } else {
                    mdasr = false;
                }
            }
            if (getScanConfig().getUnwarnedConfig().getSvrTvs()) {
                if (!row.getTvs().equals(NONE)) {
                    tvs = true;
                } else {
                    tvs = false;
                }
            }
            if (getScanConfig().getUnwarnedConfig().getHailSize()) {
                if (getScanConfig().getUnwarnedConfig().getHailSizeVal() <= row
                        .getHsize()) {
                    hail = true;
                } else {
                    hail = false;
                }
            }

            if (tvs || mdasr || hail || vil || dbz) {
                isUnwarned = true;
            }

        } else if (warnType == WARN_TYPE.TVS) {

            if (getScanConfig().getUnwarnedConfig().getTorMaxDbz()) {
                if (getScanConfig().getUnwarnedConfig().getTorMaxDbzVal() <= row
                        .getDbz()) {
                    dbz = true;
                } else {
                    dbz = false;
                }
            }
            if (getScanConfig().getUnwarnedConfig().getTorMaxVil()) {
                if (getScanConfig().getUnwarnedConfig().getTorMaxVilVal() <= row
                        .getVil()) {
                    vil = true;
                } else {
                    vil = false;
                }
            }
            if (getScanConfig().getUnwarnedConfig().getTorMesoStRank()
                    && !row.getMdaSR().equals(NONE)) {
                if (getScanConfig().getUnwarnedConfig().getTorMesoStRankVal() <= ScanUtils
                        .parseMDASR(row.getMdaSR())) {
                    mdasr = true;
                } else {
                    mdasr = false;
                }
            }
            if (getScanConfig().getUnwarnedConfig().getTorTvs()) {
                if (!row.getTvs().equals(NONE)) {
                    tvs = true;
                } else {
                    tvs = false;
                }
            }

            if (tvs || mdasr || vil || dbz) {
                isUnwarned = true;
            }
        }

        return isUnwarned;
    }

    /**
     * @return the dataUpdateFlag
     */
    public boolean isDataUpdated() {
        return dataUpdated;
    }

    /**
     * @param dataUpdated
     *            the dataUpdateFlag to set
     */
    public void setDataUpdated(boolean dataUpdated) {
        this.dataUpdated = dataUpdated;
    }

    /**
     * Is this class instantiated.
     * 
     * @return true if instantiated
     */
    public boolean isInstantiated() {
        return instantiated;
    }

    /**
     * Set instantiation flag.
     * 
     * @param instantiated
     *            true if instantiated
     */
    public void setInstantiated(boolean instantiated) {
        this.instantiated = instantiated;
    }

    @Override
    public ArrayList<Date> getTimeOrderedKeys(IMonitor monitor, String type) {
        // No-op
        return null;
    }

    /**
     * Gets a list of the site keys
     * 
     * @param type
     *            Table type
     * @return Set of sites
     */
    public Set<String> getSites(ScanTables type) {
        Set<String> data = null;
        if (type.equals(ScanTables.CELL)) {
            data = cellData.keySet();
        } else if (type.equals(ScanTables.DMD)) {
            data = dmdData.keySet();
        } else if (type.equals(ScanTables.MESO)) {
            data = mdData.keySet();
        } else if (type.equals(ScanTables.TVS)) {
            data = tvsData.keySet();
        }
        return data;
    }

    /**
     * Get the time
     * 
     * @param icao
     * @param scanTable
     * @param timeType
     * @return
     */
    public Date getTime(String icao, ScanTables scanTable, ScanTimeType timeType) {
        return times.get(icao).get(scanTable).getTime(timeType);
    }

    /**
     * Set the time
     * 
     * @param icao
     * @param scanTable
     * @param timeType
     * @param date
     */
    public void setTime(String icao, ScanTables scanTable,
            ScanTimeType timeType, Date date) {
        if (times.get(icao) == null) {
            Map<ScanTables, ScanTime> map = new HashMap<ScanTables, ScanTime>();
            times.put(icao, map);
        }
        if (times.get(icao).get(scanTable) == null) {
            ScanTime time = new ScanTime();
            times.get(icao).put(scanTable, time);
        }
        times.get(icao).get(scanTable).setTime(date, timeType);
    }

    /**
     * @param latestElevation
     *            the latestElevation to set
     */
    public void setLatestElevation(Double latestElevation) {
        this.latestElevation = latestElevation;
    }

    /**
     * @return the latestElevation
     */
    public Double getLatestElevation() {
        return latestElevation;
    }

    /**
     * fire off a cleaner
     * 
     * @param icao
     * @param table
     * @param date
     */
    public void purgeSCANData(String icao, ScanTables table, Date date) {
        PurgeSCANData psd = new PurgeSCANData(icao, table, date);
        psd.purge();
    }

    /**
     * Purge old data from the cache
     * 
     * @author dhladky
     * 
     */
    private class PurgeSCANData implements Runnable {

        private final String icao;

        private final ScanTables table;

        private final Date date;

        public PurgeSCANData(String icao, ScanTables table, Date date) {

            this.icao = icao;
            this.table = table;
            this.date = date;
        }

        @Override
        public void run() {
            try {
                purge();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to purge Data from SCAN cache, ", e);
            }
        }

        public void purge() {
            long dateTime = date.getTime();

            ConcurrentMap<Long, ?> data = getData(table, icao);

            for (Long idate : data.keySet()) {
                if (idate < dateTime) {
                    data.remove(idate);
                }
            }

            // also check MESO and TVS
            if (table == ScanTables.CELL) {

                ConcurrentMap<Long, ?> mesodata = getData(ScanTables.MESO, icao);

                for (Long idate : mesodata.keySet()) {
                    if (idate < dateTime) {
                        mesodata.remove(idate);
                    }
                }

                ConcurrentMap<Long, ?> tvsdata = getData(ScanTables.TVS, icao);

                for (Long idate : tvsdata.keySet()) {
                    if (idate < dateTime) {
                        tvsdata.remove(idate);
                    }
                }
            }
        }
    }
}
