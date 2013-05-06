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

import com.raytheon.uf.common.dataplugin.scan.ScanRecord;
import com.raytheon.uf.common.dataplugin.scan.data.CellTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.DMDTableDataRow;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableData;
import com.raytheon.uf.common.dataplugin.scan.data.ScanTableDataRow;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.geospatial.ISpatialQuery;
import com.raytheon.uf.common.geospatial.ISpatialQuery.SearchMode;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.geospatial.SpatialQueryFactory;
import com.raytheon.uf.common.geospatial.SpatialQueryResult;
import com.raytheon.uf.common.monitor.scan.ScanUtils;
import com.raytheon.uf.common.monitor.scan.config.SCANConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.DMDTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.WARN_TYPE;
import com.raytheon.uf.common.monitor.scan.config.SCANMonitorConfig;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.catalog.ScriptCreator;
import com.raytheon.uf.viz.core.comm.Connector;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.ResourceMonitor;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.scan.ScanTime.ScanTimeType;
import com.raytheon.uf.viz.monitor.scan.commondialogs.SCANSplash;
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
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class ScanMonitor extends ResourceMonitor implements IScanDialogListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ScanMonitor.class);

    /** boolean for initialization **/
    // public static boolean isInitialized = false;
    public ArrayList<String> icaos = new ArrayList<String>();

    private HashMap<String, String> cwas = new HashMap<String, String>();

    private HashMap<String, Date> cellDialogTime = new HashMap<String, Date>();

    private HashMap<String, Date> dmdDialogTime = new HashMap<String, Date>();

    private HashMap<String, Date> cellScanTime = new HashMap<String, Date>();

    private HashMap<String, Date> dmdScanTime = new HashMap<String, Date>();

    private Map<String, Map<ScanTables, ScanTime>> times = new HashMap<String, Map<ScanTables, ScanTime>>();

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
    private final ArrayList<IScanRadarListener> scanListeners = new ArrayList<IScanRadarListener>();

    /** Scan splash **/
    public SCANSplash scanSplashDlg = null;

    /** Cell table Dialog **/
    // /public SCANCellTableDlg cellDialog = null;

    public ConcurrentHashMap<String, SCANCellTableDlg> cellDialogs = new ConcurrentHashMap<String, SCANCellTableDlg>();

    /** Meso Table Dialog **/
    // public SCANMesoTableDlg mesoDialog = null;

    public ConcurrentHashMap<String, SCANMesoTableDlg> mesoDialogs = new ConcurrentHashMap<String, SCANMesoTableDlg>();

    /** TVS Table Dialog **/
    // public SCANTvsTableDlg tvsDialog = null;

    public ConcurrentHashMap<String, SCANTvsTableDlg> tvsDialogs = new ConcurrentHashMap<String, SCANTvsTableDlg>();

    /** DMD Table Dialog **/
    // public SCANDmdTableDlg dmdDialog = null;

    public ConcurrentHashMap<String, SCANDmdTableDlg> dmdDialogs = new ConcurrentHashMap<String, SCANDmdTableDlg>();

    /** SCAN Monitor Configuration object **/
    private SCANMonitorConfig config = null;

    /** SCAN table Configuration object **/
    private SCANConfig scanConfig = null;

    public HashMap<String, Double> cellTilts = new HashMap<String, Double>();

    public HashMap<String, Double> dmdTilts = new HashMap<String, Double>();

    public GeometryFactory factory = new GeometryFactory();

    private HashMap<String, Coordinate> stationCoors = new HashMap<String, Coordinate>();

    /** Pattern for dates in radar */
    public static String datePattern = "yyyy-MM-dd HH:mm:ss";

    private final ConcurrentSkipListMap<Date, ScanRecord> dmdRecordList = new ConcurrentSkipListMap<Date, ScanRecord>();

    /**
     * Number of frames for display in cave.
     */
    private int frames = 12;

    /**
     * Set to true when new records come in.
     */
    private boolean dataUpdated = false;

    private boolean instantiated = false;

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
     * Actual initialization if necessary
     * 
     * @return
     */
    public static synchronized ScanMonitor getInstance() {

        if (monitor == null) {
            monitor = new ScanMonitor();
            monitor.createDataStructures();
            // isInitialized = true;
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
     */
    public void setConfig(SCANMonitorConfig config) {
        this.config = config;
    }

    /**
     * gets the config
     * 
     * @return
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
     * @return
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
        if (dmdData != null) {
            dmdData.clear();
        }
        if (cellData != null) {
            cellData.clear();
        }
        if (tvsData != null) {
            tvsData.clear();
        }
        if (mdData != null) {
            mdData.clear();
        }
    }

    /**
     * Initial setup of monitor
     * 
     * @param stations
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
        // TODO Auto-generated method stub

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
        }
        if (dmdDialogs.get(icao) == null) {
            monitor.removeMonitorListener(dmdDialogs.get(icao));
            monitor.dmdData.remove(icao);
        }

        if ((dmdDialogs == null) && (cellDialogs == null)) {
            // kill this monitor
            monitor = null;
        }
    }

    @Override
    protected void nullifyMonitor() {
        monitor = null;
    }

    /**
     * Gets the radar station ICAOs
     * 
     * @return
     */
    public ArrayList<String> getIcaos() {
        return icaos;
    }

    public void addIcao(String icao) {
        icaos.add(icao);

    }

    public void setCwa(String icao) {
        cwas.put(icao, getCWABySpatialQuery(getStationCoordinate(icao)));
    }

    /**
     * Gets the cell tilt angle
     * 
     * @return
     */
    public double getCellTilt(String icao) {
        if (cellTilts.get(icao) == null) {
            return 0.0;
        }
        return cellTilts.get(icao);
    }

    public void setCellTilt(double cellTilt, String icao) {
        cellTilts.put(icao, cellTilt);
    }

    /**
     * Gets the dmd tilt angle
     * 
     * @return
     */
    public double getDmdTilt(String icao) {
        return dmdTilts.get(icao);
    }

    public void setDmdTilt(double dmdTilt, String icao) {
        dmdTilts.put(icao, dmdTilt);
    }

    /**
     * Gets the latest records
     * 
     * @param type
     * @param date
     * @param tilt
     *            (aka azimuth)
     * @return
     */
    public ScanRecord getScanRecord(ScanTables type, String icao, Date date,
            double tilt) throws VizException {
        String uri = getAvailableUri(tilt, date, type, icao);
        ScanRecord scanRec = null;
        Map<String, Object> vals = new HashMap<String, Object>();
        vals.put("pluginName", "scan");
        vals.put("dataURI", uri);
        if (uri != null) {
            scanRec = (ScanRecord) Loader.loadData(vals);
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
     * @param icao
     * @return
     */
    public Double getTiltAngle(ScanTables type, String icao) {
        double angle = 0.0;
        if (type == ScanTables.DMD) {
            angle = getDmdTilt(icao);
        } else if (type == ScanTables.CELL) {
            angle = getCellTilt(icao);
        } else if (type == ScanTables.TVS) {
            angle = getCellTilt(icao);
        } else if (type == ScanTables.MESO) {
            angle = getCellTilt(icao);
        }

        return angle;
    }

    /**
     * Get most recent time
     * 
     * @param data
     * @return
     */
    public DataTime getMostRecent(IMonitor monitor, String type, String icao) {

        DataTime time = null;
        ScanTables scanType = ScanTables.valueOf(type);
        if ((scanType == ScanTables.MESO) || (scanType == ScanTables.TVS)) {
            scanType = ScanTables.CELL;
        }
        Set<Long> ds = getData(scanType, icao).keySet();
        DataTime[] times = new DataTime[ds.size()];
        int i = 0;
        for (Long d : ds) {
            times[i] = new DataTime(new Date(d));
            i++;
        }
        java.util.Arrays.sort(times);
        if (times.length > 0) {
            time = times[times.length - 1]; // most recent
        }

        return time;
    }

    /**
     * Get the Keys for my table
     * 
     * @param type
     * @param time
     * @return
     */

    public Set<String> getTableKeys(ScanTables type, String icao, Date time) {
        Set<String> keySet = null;
        try {
            if (getTableData(type, icao, time) != null) {
                keySet = getTableData(type, icao, time).getTableData().keySet();
            } else {
                return null;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return keySet;
    }

    /**
     * Sends a String from the TABLE enum for which table data to grab
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
            if (tableData == null
                    && (table == ScanTables.TVS || table == ScanTables.MESO)) {
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
     */
    public ScanTableData<?> getDmdTableData(String icao, Date time,
            double tiltAngle) {
        ScanTables dmd = ScanTables.DMD;
        ScanTableData<?> tableData = null;
        ConcurrentMap<Long, ?> data = getData(dmd, icao);
        DMDScanData dmdsd = (DMDScanData) data.get(time.getTime());
        if (dmdsd != null) {
            tableData = dmdsd.getTableData(time.getTime());
        }

        return tableData;
    }

    /**
     * gets the newest tableData by time, type
     */

    public ScanTableData<?> getNewTableData(ScanTables type, String icao,
            Date date, double tilt) {
        ScanTableData<?> data = null;
        try {
            ScanRecord rec = getScanRecord(type, icao, date, tilt);
            if (rec != null) {
                data = getScanRecord(type, icao, date, tilt).getTableData();
            }
        } catch (VizException ve) {
            System.out.println("Couldn't load new ScanRecord: " + type.name());
        }

        return data;
    }

    /**
     * Set new row data
     * 
     * @param data
     * @param date
     * @param angle
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
     * @param table
     */
    public void launchDialog(Shell shell, String icao, ScanTables table) {
        if (table.equals(ScanTables.CELL)) {
            if (cellDialogs.size() == 0) {
                getScanConfig().reload(ScanTables.CELL);
            }
            if (cellDialogs.get(icao) == null) {
                SCANCellTableDlg cellDialog = new SCANCellTableDlg(shell, icao,
                        new SCANTableData(ScanTables.CELL));
                cellDialogs.put(icao, cellDialog);
                monitor.addMonitorListener(cellDialog);
                cellDialog.addMonitorControlListener(monitor);
            }
        } else if (table.equals(ScanTables.DMD)) {
            if (dmdDialogs.size() == 0) {
                getScanConfig().reload(ScanTables.DMD);
            }
            if (dmdDialogs.get(icao) == null) {
                SCANDmdTableDlg dmdDialog = new SCANDmdTableDlg(shell, icao,
                        new SCANTableData(ScanTables.DMD));
                dmdDialogs.put(icao, dmdDialog);
                monitor.addMonitorListener(dmdDialog);
                dmdDialog.addMonitorControlListener(monitor);
            }
        } else if (table.equals(ScanTables.MESO)) {
            if (mesoDialogs.size() == 0) {
                getScanConfig().reload(ScanTables.MESO);
            }
            if (mesoDialogs.get(icao) == null) {
                SCANMesoTableDlg mesoDialog = new SCANMesoTableDlg(shell, icao,
                        new SCANTableData(ScanTables.MESO));
                mesoDialogs.put(icao, mesoDialog);
                monitor.addMonitorListener(mesoDialog);
                mesoDialog.addMonitorControlListener(monitor);
            } else {
                return;
            }
        } else if (table.equals(ScanTables.TVS)) {
            if (tvsDialogs.size() == 0) {
                getScanConfig().reload(ScanTables.TVS);
            }
            if (tvsDialogs.get(icao) == null) {
                SCANTvsTableDlg tvsDialog = new SCANTvsTableDlg(shell, icao,
                        new SCANTableData(ScanTables.TVS));
                tvsDialogs.put(icao, tvsDialog);
                monitor.addMonitorListener(tvsDialog);
                tvsDialog.addMonitorControlListener(monitor);
            } else {
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
     */
    public void addScanRadarListener(IScanRadarListener isrl) {
        scanListeners.add(isrl);
    }

    /**
     * remove a listener
     * 
     * @param isrl
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
     * @param type
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
     * @return
     */
    public AbstractTableDlg getDialog(
            com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables scanTables,
            String icao) {
        if (scanTables.equals(ScanTables.CELL)) {
            return cellDialogs.get(icao);
        } else if (scanTables.equals(ScanTables.DMD)) {
            return dmdDialogs.get(icao);
        } else if (scanTables.equals(ScanTables.MESO)) {
            return mesoDialogs.get(icao);
        } else if (scanTables.equals(ScanTables.TVS)) {
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
     * @param dialogTime
     * @param tilt
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
     * Dialogs retrieve the drawTime=
     * 
     * @return
     */
    public Date getDialogTime(ScanTables type, String icao) {
        Date dialogTime = null;
        if (type.equals(ScanTables.CELL)) {
            if (cellDialogTime.get(icao) == null) {
                return null;
            } else {
                dialogTime = cellDialogTime.get(icao);
            }
        } else if (type.equals(ScanTables.DMD)) {
            if (dmdDialogTime.get(icao) == null) {
                return null;
            } else {
                dialogTime = dmdDialogTime.get(icao);
            }
        } else if (type.equals(ScanTables.MESO)) {
            dialogTime = cellDialogTime.get(icao);
        } else if (type.equals(ScanTables.TVS)) {
            dialogTime = cellDialogTime.get(icao);
        }

        return dialogTime;
    }

    /**
     * Dialogs retrieve the drawTime=
     * 
     * @return
     */
    public Date getScanTime(ScanTables type, String icao) {
        Date scanTime = null;
        if (type.equals(ScanTables.CELL)) {
            if (cellScanTime.get(icao) == null) {
                return null;
            } else {
                scanTime = cellScanTime.get(icao);
            }
        } else if (type.equals(ScanTables.DMD)) {
            if (dmdScanTime.get(icao) == null) {
                return null;
            } else {
                scanTime = dmdScanTime.get(icao);
            }
        } else if (type.equals(ScanTables.MESO)) {
            scanTime = cellScanTime.get(icao);
        } else if (type.equals(ScanTables.TVS)) {
            scanTime = cellScanTime.get(icao);
        }

        return scanTime;
    }

    /**
     * launches the splash screen
     * 
     * @param shell
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
                    && (mesoDialogs.get(icao).getCurrentShell().isDisposed() == false)) {
                mesoDialogs.get(icao).updateThresh(attr);
            }
            break;
        case CELL:
            if ((cellDialogs.get(icao) != null)
                    && (cellDialogs.get(icao).getCurrentShell().isDisposed() == false)) {
                cellDialogs.get(icao).updateThresh(attr);
            }
            break;

        case DMD:
            if ((dmdDialogs.get(icao) != null)
                    && (dmdDialogs.get(icao).getCurrentShell().isDisposed() == false)) {
                dmdDialogs.get(icao).updateThresh(attr);
            }
            break;
        case TVS:
            if ((tvsDialogs.get(icao) != null)
                    && (tvsDialogs.get(icao).getCurrentShell().isDisposed() == false)) {
                tvsDialogs.get(icao).updateThresh(attr);
            }
            break;
        }
    }

    /**
     * Get CWA Query
     * 
     * @param c
     * @return
     */
    public String getCWABySpatialQuery(Coordinate c) {
        ISpatialQuery sq = null;
        try {
            sq = SpatialQueryFactory.create();
        } catch (SpatialException e1) {
            e1.printStackTrace();
        }
        // convert coordinate to point geometry
        Point geometry = factory.createPoint(c);
        SpatialQueryResult[] results = null;
        try {
            results = sq.query("cwa", new String[] { "cwa" }, geometry, null,
                    false, SearchMode.WITHIN);
        } catch (Exception e) {
            e.printStackTrace();
        }
        String cwa = (String) results[0].attributes.get("cwa");

        return cwa;
    }

    /**
     * Gets the station Coordinate
     * 
     * @return
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
            e.printStackTrace();
        } catch (ParseException pe) {
            pe.printStackTrace();
        }
    }

    /**
     * Gets the StationCoordinate
     * 
     * @return
     */
    public Coordinate getStationCoordinate(String icao) {
        return stationCoors.get(icao);
    }

    /**
     * Gets the CWA
     * 
     * @return
     */
    public String getCwa(String icao) {
        return cwas.get(icao);
    }

    /**
     * Get the DMD frames for the max angle for the Volume Scan Time.
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

            if (results.size() > 0) {
                recordList = new long[results.size()];
                for (int i = 0; i < recordList.length; i++) {
                    Object[] oa = results.get(i);
                    Timestamp reftime = (Timestamp) oa[0];
                    recordList[i] = reftime.getTime();
                }
            }
        } catch (VizException e) {
            e.printStackTrace();
        }
        return recordList;
    }

    /**
     * Getting the available uri
     * 
     * @param angle
     *            (aka azimuth)
     * @param date
     * @param type
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
            e.printStackTrace();
        }

        return uri;
    }

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
     * @return The time height graph data.
     */
    public TreeMap<Long, DMDTableDataRow> getTimeHeightGraphData(String icao,
            SCANConfigEnums.DMDTable tableCol, String dmdIdent, Date currentTime) {
        TreeMap<Long, DMDTableDataRow> mapData = new TreeMap<Long, DMDTableDataRow>();
        DMDScanData dmdScanData;

        for (Date date : getTimeOrderedKeys(this, ScanTables.DMD.name(), icao)) {
            if (date.getTime() <= currentTime.getTime() + 240000) { // currenttime
                                                                    // plus 4
                                                                    // minutes
                // dmdScanData = new DMDScanData();
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

        if (mapData.isEmpty() == true) {
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
     * @param set
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

    public void setFrames(int frames) {
        this.frames = frames;
    }

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

        Object[] resp;
        List<String> removeList = new ArrayList<String>();
        HashMap<String, UnwarnedCell> cells = new HashMap<String, UnwarnedCell>();
        List<AbstractWarningRecord> tvswarnings = new ArrayList<AbstractWarningRecord>();
        List<AbstractWarningRecord> severewarnings = new ArrayList<AbstractWarningRecord>();
        HashMap<String, RequestConstraint> vals = new HashMap<String, RequestConstraint>();
        LayerProperty lp = new LayerProperty();
        GeometryFactory factory = new GeometryFactory();

        try {
            ScanTableData<?> cellTable = getTableData(ScanTables.CELL, icao,
                    time);

            vals.put("pluginName", new RequestConstraint("warning"));
            if (icao != null) {
                vals.put("officeid", new RequestConstraint(icao.toUpperCase()));
            }
            lp.setDesiredProduct(ResourceType.PLAN_VIEW);
            lp.setEntryQueryParameters(vals, false);

            lp.setNumberOfImages(NUMBER_OF_IMAGES);

            String script = ScriptCreator.createScript(lp);
            if (script != null) {
                resp = Connector.getInstance().connect(script, null, 60000);

                // System.out.println("Number of active CWA warnings: "+resp.length);

                for (int i = 0; i < resp.length; i++) {

                    AbstractWarningRecord rec = (AbstractWarningRecord) resp[i];

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
                        // cells.add(new UnwarnedCell(key, WARN_TYPE.SEVERE));
                        cells.put(key, new UnwarnedCell(key, WARN_TYPE.SEVERE));
                    }
                    if (!sts && tor) {
                        // cells.add(new UnwarnedCell(key, WARN_TYPE.TVS));
                        cells.put(key, new UnwarnedCell(key, WARN_TYPE.TVS));
                    }
                    // more critical cell gets added by when you find both
                    if (sts && tor) {
                        // cells.add(new UnwarnedCell(key, WARN_TYPE.TVS));
                        cells.put(key, new UnwarnedCell(key, WARN_TYPE.TVS));
                    }
                }
                if (severe && !tvs) {
                    if (isUnwarned(ctdr, WARN_TYPE.SEVERE)) {
                        // cells.add(new UnwarnedCell((String)key,
                        // WARN_TYPE.SEVERE));
                        cells.put(key, new UnwarnedCell(key, WARN_TYPE.SEVERE));
                    }
                }
                if (!severe && tvs) {
                    if (isUnwarned(ctdr, WARN_TYPE.TVS)) {
                        // cells.add(new UnwarnedCell((String)key,
                        // WARN_TYPE.TVS));
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

            // System.out.println("Number of cells unwarned: "+cells.size());

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
     * @param warnType
     * @return
     */
    public boolean isUnwarned(CellTableDataRow row, WARN_TYPE warnType) {

        boolean isUnwarned = false;
        boolean dbz = false;
        boolean vil = false;
        boolean mdasr = false;
        boolean tvs = false;
        boolean hail = false;

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
                    && !row.getMdaSR().equals("NONE")) {
                if (getScanConfig().getUnwarnedConfig().getSvrMesoStRankVal() <= ScanUtils
                        .parseMDASR(row.getMdaSR())) {
                    mdasr = true;
                } else {
                    mdasr = false;
                }
            }
            if (getScanConfig().getUnwarnedConfig().getSvrTvs()) {
                if (!row.getTvs().equals("NONE")) {
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
                // System.out.println("CEll: "+row.getIdent()+" meets SEVERE criteria");
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
                    && !row.getMdaSR().equals("NONE")) {
                if (getScanConfig().getUnwarnedConfig().getTorMesoStRankVal() <= ScanUtils
                        .parseMDASR(row.getMdaSR())) {
                    mdasr = true;
                } else {
                    mdasr = false;
                }
            }
            if (getScanConfig().getUnwarnedConfig().getTorTvs()) {
                if (!row.getTvs().equals("NONE")) {
                    tvs = true;
                } else {
                    tvs = false;
                }
            }

            if (tvs || mdasr || vil || dbz) {
                isUnwarned = true;
                // System.out.println("CEll: "+row.getIdent()+" meets TVS criteria");
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
     * @param dataUpdateFlag
     *            the dataUpdateFlag to set
     */
    public void setDataUpdated(boolean dataUpdated) {
        this.dataUpdated = dataUpdated;
    }

    public boolean isInstantiated() {
        return instantiated;
    }

    public void setInstantiated(boolean instantiated) {
        this.instantiated = instantiated;
    }

    @Override
    public ArrayList<Date> getTimeOrderedKeys(IMonitor monitor, String type) {
        // // TODO Auto-generated method stub
        return null;
    }

    /**
     * Gets a list of the site keys
     * 
     * @param type
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

    public Date getTime(String icao, ScanTables scanTable, ScanTimeType timeType) {
        return times.get(icao).get(scanTable).getTime(timeType);
    }

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
     * @param product
     * @param source
     * @param siteKey
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

        private String icao;

        private ScanTables table;

        private Date date;

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
