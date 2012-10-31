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
package com.raytheon.uf.viz.monitor.safeseas;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.uf.common.dataplugin.fog.FogRecord;
import com.raytheon.uf.common.dataplugin.fog.FogRecord.FOG_THREAT;
import com.raytheon.uf.common.monitor.MonitorAreaUtils;
import com.raytheon.uf.common.monitor.config.SSMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.AdjacentWfoMgr;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.CellType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.Monitor;
import com.raytheon.uf.viz.monitor.ObsMonitor;
import com.raytheon.uf.viz.monitor.data.MonitoringArea;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.data.TableCellData;
import com.raytheon.uf.viz.monitor.data.TableData;
import com.raytheon.uf.viz.monitor.data.TableRowData;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.safeseas.listeners.ISSResourceListener;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.safeseas.ui.dialogs.SSMonitoringAreaConfigDlg;
import com.raytheon.uf.viz.monitor.safeseas.ui.dialogs.SSZoneTableDlg;
import com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg;
import com.raytheon.uf.viz.monitor.util.MonitorThresholdConfiguration;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * SafeSeasMonitor, monitor Data that triggers changes to the SafeSeas display.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009 1981       dhladky     Initial creation.
 * 3/2/2009     2047       grichard    Made pluginName an array.
 * 3/2/2009     2047       grichard    Added stationName array.
 * 11/30/2009   3424       Zhidong/Slav/wkwock Use real station data.
 * Dec 30, 2009 3424       zhao        use ObMultiHrsReports for obs data archive over time
 * July 20,2010 4891       skorolev    Added resource listener
 * May 15, 2012 14510      zhao        Modified processing at startup
 * Oct 26, 2012 1280       skorolev    Clean code and made changes for non-blocking dialog
 * Oct 30, 2012 1297       skorolev    Changed HashMap to Map
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class SafeSeasMonitor extends ObsMonitor implements ISSResourceListener {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SafeSeasMonitor.class);

    /** Singleton instance of this class */
    private static SafeSeasMonitor monitor = null;

    /** zone table dialog **/
    private SSZoneTableDlg zoneDialog;

    /** monitoring area config dialog **/
    private SSMonitoringAreaConfigDlg areaDialog = null;

    /** configuration manager **/
    private SSMonitorConfigurationManager safeseasConfig = null;

    /**
     * This object contains all observation data necessary for the table dialogs
     * and trending plots
     */
    private ObMultiHrsReports obData;

    /** table data for the zone table **/
    private final TableData zoneTableData = new TableData(
            CommonConfig.AppName.SAFESEAS);

    /** table data for all table data **/
    private final TableData stationTableData = new TableData(
            CommonConfig.AppName.SAFESEAS);

    /** All SafeSeas datauri start with this */
    private final String OBS = "fssobs";

    /** regex wild card filter */
    private final String wildCard = "[\\w\\(\\)-_:.]+";

    /** List of SAFESEAS resource listeners **/
    private final List<ISSResourceListener> safeSeasResources = new ArrayList<ISSResourceListener>();

    /** Time which Zone/County dialog shows. **/
    private Date dialogTime = null;

    /** list of coordinates for each zone **/
    private Map<String, Geometry> zoneGeometries = null;

    /** data holder for FOG data **/
    private Map<Date, Map<String, FOG_THREAT>> algorithmData = null;

    /** Adjacent areas for current cwa **/
    private Geometry geoAdjAreas;

    /** List of fogAlg listeners **/
    private final List<ISSResourceListener> fogResources = new ArrayList<ISSResourceListener>();

    /** Pattern for SAFESEAS **/
    private final Pattern ssPattern = Pattern
            .compile(URIFilter.uriSeperator + OBS + URIFilter.uriSeperator
                    + wildCard + URIFilter.uriSeperator + wildCard
                    + URIFilter.uriSeperator + cwa + URIFilter.uriSeperator
                    + wildCard + URIFilter.uriSeperator + wildCard
                    + URIFilter.uriSeperator + wildCard
                    + URIFilter.uriSeperator + "ss");

    /**
     * Private constructor, singleton
     */
    private SafeSeasMonitor() {
        pluginPatterns.add(ssPattern);
        readTableConfig(MonitorThresholdConfiguration.SAFESEAS_THRESHOLD_CONFIG);
        initObserver(OBS, this);
    }

    /**
     * @return instance of monitor
     */
    public static synchronized SafeSeasMonitor getInstance() {
        if (monitor == null) {
            monitor = new SafeSeasMonitor();
            // Pre-populate dialog with an observation (METAR) for KOMA
            monitor.createDataStructures();
            monitor.getAdjAreas();
            monitor.processProductAtStartup("ss");
            monitor.fireMonitorEvent(monitor);
        }
        return monitor;
    }

    // TODO: Provide the changes in EDEX URIFilters when area configuration file
    // has been changed.
    /**
     * DR#11279: When monitor area configuration is changed, this module is
     * called to re-initialize monitor using new monitor area configuration
     */
    public static void reInitialize() {
        if (monitor != null) {
            monitor = null;
            monitor = new SafeSeasMonitor();
        }
    }

    /**
     * Creates the maps
     */
    private void createDataStructures() {
        obData = new ObMultiHrsReports(CommonConfig.AppName.SAFESEAS);
        obData.setThresholdMgr(SSThresholdMgr.getInstance());
        algorithmData = new HashMap<Date, Map<String, FOG_THREAT>>();
    }

    /**
     * Launch SAFESEAS Zone Dialog
     * 
     * @param type
     * @param shell
     */
    public void launchDialog(String type, Shell shell) {

        if (type.equals("zone")) {
            if (zoneDialog == null || zoneDialog.getShell() == null
                    || zoneDialog.isDisposed()) {
                zoneDialog = new SSZoneTableDlg(shell, obData);
                addMonitorListener(zoneDialog);
                zoneDialog.addMonitorControlListener(this);
                fireMonitorEvent(zoneDialog.getClass().getName());
                zoneDialog.open();
            } else {
                zoneDialog.bringToTop();
            }
        } else if (type.equals("area")) {
            if (areaDialog == null) {
                areaDialog = new SSMonitoringAreaConfigDlg(shell,
                        "Safe Seas Monitor Area Configuration");
                areaDialog.open();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ObsMonitor#filterNotifyMessage(com.raytheon
     * .uf.viz.core.notification.NotificationMessage)
     */
    @Override
    public boolean filterNotifyMessage(NotificationMessage alertMessage) {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ObsMonitor#processNotifyMessage(com.raytheon
     * .uf.viz.core.notification.NotificationMessage)
     */
    @Override
    public void processNotifyMessage(NotificationMessage filtered) {
        // Not used
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ObsMonitor#processProductMessage(com.raytheon
     * .uf.viz.core.alerts.AlertMessage)
     */
    @Override
    public void processProductMessage(final AlertMessage filtered) {
        if (ssPattern.matcher(filtered.dataURI).matches()) {
            processURI(filtered.dataURI, filtered);
        }
    }

    /**
     * Adds data to station table
     * 
     * @param trd
     * @param stationID
     */
    public void addToStationDataTable(TableRowData trd, String stationID) {
        int numberCell = trd.getNumberOfCellData();
        TableRowData stationRowData = new TableRowData(numberCell + 1);
        RGB bgColor = new RGB(220, 175, 55);
        stationRowData.setTableCellData(0, new TableCellData(stationID,
                stationID, CellType.StationID, false, bgColor));
        for (int i = 0; i < numberCell; i++) {
            stationRowData.setTableCellData(i + 1, trd.getTableCellData(i));
        }
        stationTableData.addReplaceDataRow(stationRowData);
    }

    /**
     * Method that reads the table configuration and updates the zone monitor
     * threshold map
     * 
     * @param file
     *            -- the xml configuration filename
     */
    public void readTableConfig(String file) {
        Map<String, List<String>> zones = new HashMap<String, List<String>>();
        // create zones and station list
        try {
            SSMonitorConfigurationManager areaConfig = getMonitorAreaConfig();
            for (String zone : areaConfig.getAreaList()) {
                List<String> stations = areaConfig.getAreaStations(zone);
                zones.put(zone, stations);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.CRITICAL,
                    "SafeSeas failed to load configuration..."
                            + this.getClass().getName(), e);
        }
        MonitoringArea.setPlatformMap(zones);
    }

    /**
     * Gets configuration manager
     * 
     * @return safeseasConfig
     */
    private SSMonitorConfigurationManager getMonitorAreaConfig() {
        if (safeseasConfig == null) {
            LocalizationManager mgr = LocalizationManager.getInstance();
            String siteScope = mgr.getCurrentSite();

            safeseasConfig = SSMonitorConfigurationManager.getInstance();
            safeseasConfig.readConfigXml(siteScope);
        }
        return safeseasConfig;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.Monitor#initObserver(java.lang.String,
     * com.raytheon.uf.viz.monitor.Monitor)
     */
    @Override
    public void initObserver(String pluginName, Monitor monitor) {
        ProductAlertObserver.addObserver(pluginName, this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ObsMonitor#thresholdUpdate(com.raytheon.uf
     * .viz.monitor.events.IMonitorThresholdEvent)
     */
    @Override
    public void thresholdUpdate(IMonitorThresholdEvent me) {
        fireMonitorEvent(zoneDialog.getClass().getName());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ObsMonitor#configUpdate(com.raytheon.uf.viz
     * .monitor.events.IMonitorConfigurationEvent)
     */
    @Override
    public void configUpdate(IMonitorConfigurationEvent me) {
        // Not used
    }

    /**
     * Kills this monitor by nullifying the monitor's private instance variable.
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ObsMonitor#nullifyMonitor()
     */
    @Override
    public void nullifyMonitor() {
        monitor.removeMonitorListener(zoneDialog);
        monitor.fogResources.removeAll(getMonitorListeners());
        stopObserver(OBS, this);
        monitor = null;
    }

    /**
     * @return zoneTableData
     */
    public TableData getZoneTableData() {
        return zoneTableData;
    }

    /**
     * @return stationTableData
     */
    public TableData getStationTableData() {
        return stationTableData;
    }

    /**
     * Gets data
     * 
     * @return obData
     */
    public ObMultiHrsReports getObData() {
        return obData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ObsMonitor#process(com.raytheon.uf.viz.monitor
     * .data.ObReport)
     */
    @Override
    protected void process(ObReport result) throws Exception {
        obData.addReport(result);
        fireMonitorEvent(this);
    }

    /**
     * SSResource updates the dialogTime
     * 
     * @param dialogTime
     */
    public void updateDialogTime(Date dialogTime) {
        this.dialogTime = dialogTime;
        fireMonitorEvent(zoneDialog.getClass().getName());
    }

    /**
     * @return dialogTime
     */
    public Date getDialogTime() {
        return dialogTime;
    }

    /**
     * Sets the dialogTime
     * 
     * @param dialogTime
     */
    public void setDialogTime(Date dialogTime) {
        this.dialogTime = dialogTime;
    }

    /**
     * Add recourse listener
     * 
     * @param issr
     */
    public void addSSResourceListener(ISSResourceListener issr) {
        safeSeasResources.add(issr);
    }

    /**
     * Remove recourse listener
     * 
     * @param issr
     */
    public void removeSSResourceListener(ISSResourceListener issr) {
        safeSeasResources.remove(issr);
    }

    /**
     * Close dialog
     */
    public void closeDialog() {
        if (zoneDialog != null) {
            monitor.nullifyMonitor();

            zoneDialog.removeMonitorContorlListener(this);
            zoneDialog.close();
            zoneDialog = null;
        }
        if (areaDialog != null) {
            areaDialog.shellDisposeDialog();
            areaDialog = null;
        }
    }

    /**
     * 
     * @param type
     * @return
     * 
     *         (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.IMonitor#getTimeOrderedKeys(com.raytheon.
     *      uf.viz.monitor.IMonitor, java.lang.String)
     */
    @Override
    public ArrayList<Date> getTimeOrderedKeys(IMonitor monitor, String type) {
        // Not used
        return null;
    }

    /**
     * Gets area geometries
     * 
     * @return zoneGeometries
     */
    public Map<String, Geometry> getMonitoringAreaGeometries() {
        if (zoneGeometries == null) {
            ArrayList<String> zones = getMonitorAreaConfig().getAreaList();
            zoneGeometries = new HashMap<String, Geometry>();
            for (String zone : zones) {
                try {
                    zoneGeometries.put(zone,
                            MonitorAreaUtils.getZoneGeometry(zone));
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error get Monitoring Area Config", e);
                }
            }
        }
        return zoneGeometries;
    }

    /**
     * Gets threats
     * 
     * @param refTime
     * @param zoneThreats
     */
    public void setAlgorithmData(Date refTime,
            Map<String, FOG_THREAT> zoneThreats) {
        if (algorithmData.containsKey(refTime)) {
            algorithmData.remove(refTime);
        }
        algorithmData.put(refTime, zoneThreats);
    }

    /**
     * Gets the algorithm threat by time
     * 
     * @param time
     * @return algData
     */
    public Map<String, FOG_THREAT> getAlgorithmData(Date time) {

        Map<String, FOG_THREAT> algData = new HashMap<String, FOG_THREAT>();

        if ((algorithmData != null) && algorithmData.containsKey(time)) {
            algData = algorithmData.get(time);
        } else {
            // by default is nothing in the Fog column
            for (String zone : MonitoringArea.getPlatformMap().keySet()) {
                algData.put(zone, FOG_THREAT.GRAY);
            }
        }
        return algData;
    }

    /**
     * Gets Fog threat types.
     * 
     * @param fogAlgThreats
     * @return types
     */
    public Map<String, CellType> getAlgCellTypes(
            Map<String, FOG_THREAT> fogAlgThreats) {
        Map<String, CellType> types = new HashMap<String, CellType>();
        for (String zone : fogAlgThreats.keySet()) {
            CellType type = getAlgorithmCellType(fogAlgThreats.get(zone));
            types.put(zone, type);
        }
        return types;
    }

    /**
     * Gets cell threat type
     * 
     * @param fog_THREAT
     * @return type
     */
    private CellType getAlgorithmCellType(FOG_THREAT fog_THREAT) {
        CellType type = CellType.NotDetermined;
        if (fog_THREAT == FogRecord.FOG_THREAT.GREEN) {
            type = CellType.G;
        } else if (fog_THREAT == FogRecord.FOG_THREAT.YELLOW) {
            type = CellType.Y;
        } else if (fog_THREAT == FogRecord.FOG_THREAT.RED) {
            type = CellType.R;
        }
        return type;
    }

    /**
     * Gets zone dialog
     * 
     * @return zoneDialog
     */
    public ZoneTableDlg getDialog() {
        return zoneDialog;
    }

    /**
     * Gets adjacent areas
     */
    public void getAdjAreas() {
        this.setGeoAdjAreas(AdjacentWfoMgr.getAdjacentAreas(cwa));
    }

    /**
     * Sets geometry of adjacent areas
     * 
     * @param geoAdjAreas
     *            the geoAdjAreas to set
     */
    public void setGeoAdjAreas(Geometry geoAdjAreas) {
        this.geoAdjAreas = geoAdjAreas;
    }

    /**
     * Gets geometry of adjacent areas
     * 
     * @return the geoAdjAreas
     */
    public Geometry getGeoAdjAreas() {
        return geoAdjAreas;
    }

    /*
     * Updates data of Fog monitor
     * 
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.safeseas.listeners.ISSResourceListener#fogUpdate
     * ()
     */
    @Override
    public void fogUpdate() {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                Iterator<ISSResourceListener> iter = fogResources.iterator();
                while (iter.hasNext()) {
                    ISSResourceListener listener = iter.next();
                    listener.fogUpdate();
                }
            }
        });

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ObsMonitor#processAtStartup(com.raytheon.
     * uf.viz.monitor.data.ObReport)
     */
    @Override
    protected void processAtStartup(ObReport report) {
        obData.addReport(report);
    }

    /**
     * Gets SAFESEAS zone table dialog
     * 
     * @return
     */
    public SSZoneTableDlg getZoneDialog() {
        return zoneDialog;
    }

}
