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
package com.raytheon.uf.viz.monitor.fog;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.uf.common.dataplugin.fog.FogRecord.FOG_THREAT;
import com.raytheon.uf.common.monitor.MonitorAreaUtils;
import com.raytheon.uf.common.monitor.config.FogMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.AdjacentWfoMgr;
import com.raytheon.uf.common.monitor.data.CommonConfig;
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
import com.raytheon.uf.viz.monitor.data.ObsData;
import com.raytheon.uf.viz.monitor.data.TableData;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.fog.listeners.IFogResourceListener;
import com.raytheon.uf.viz.monitor.fog.threshold.FogThresholdMgr;
import com.raytheon.uf.viz.monitor.fog.ui.dialogs.FogMonitoringAreaConfigDlg;
import com.raytheon.uf.viz.monitor.fog.ui.dialogs.FogZoneTableDlg;
import com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg;
import com.raytheon.uf.viz.monitor.util.MonitorThresholdConfiguration;
import com.raytheon.viz.alerts.Activator;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * FogMonitor, monitor Data that triggers changes to the Fog display.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009 1981       dhladky     Initial creation.
 * 3/2/2009     2047       grichard    Made pluginName an array.
 * 3/2/2009     2047       grichard    Added stationName array.
 * 10/7/2009    ****       dhladky     reworked
 * 11/30/2009   3424       Zhidong/Slav/Wen Adds stationTableData to keep station info.
 * May 15, 2012 14510      zhao        Modified processing at startup
 * Jun 16, 2012 14386      zhao        Auto update County/Zone Table when new fog threat data arrives
 * Oct 26, 2012 1280       skorolev    Made changes for non-blocking dialog and changed HashMap to Map
 * Oct.31  2012 1297       skorolev    Clean code
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class FogMonitor extends ObsMonitor implements IFogResourceListener {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FogMonitor.class);

    /** Singleton instance of this class */
    private static FogMonitor monitor = null;

    /**
     * This object contains all observation data necessary for the table dialogs
     * and trending plots [this replaces the objects of ObsData and TableData
     * below Jan 21, 2010, zhao]
     */
    private ObMultiHrsReports obData;

    /** data holder for FOG **/
    private ObsData obsData = null;

    /** data holder for FOG ALG data **/
    private SortedMap<Date, Map<String, FOG_THREAT>> algorithmData = null;

    private Date dialogTime = null;

    /** list of coordinates for each zone **/
    private Map<String, Geometry> zoneGeometries = null;

    /** zone table dialog **/
    private FogZoneTableDlg zoneDialog;

    /** zone table dialog **/
    private MonitoringAreaConfigDlg areaDialog;

    /** area config manager **/
    private FogMonitorConfigurationManager fogConfig = null;

    /** table data for the station table **/
    private final TableData stationTableData = new TableData(
            CommonConfig.AppName.FOG);

    /** All FOG's datauri start with this **/
    private final String OBS = "fssobs";

    /** List of fogAlg listeners **/
    private final List<IFogResourceListener> fogResources = new ArrayList<IFogResourceListener>();

    /** regex wild card filter **/
    private final String wildCard = "[\\w\\(\\)\\-_:.]+";

    /** Geometry of adjacent areas **/
    private Geometry geoAdjAreas = null;

    /** Data URI pattern for fog **/
    private final Pattern fogPattern = Pattern.compile(URIFilter.uriSeperator
            + OBS + URIFilter.uriSeperator + wildCard + URIFilter.uriSeperator
            + wildCard + URIFilter.uriSeperator + cwa + URIFilter.uriSeperator
            + wildCard + URIFilter.uriSeperator + wildCard
            + URIFilter.uriSeperator + wildCard + URIFilter.uriSeperator
            + "fog");

    /**
     * Private constructor, singleton
     */
    private FogMonitor() {
        pluginPatterns.add(fogPattern);
        readTableConfig(MonitorThresholdConfiguration.FOG_THRESHOLD_CONFIG);
        initObserver(OBS, this);
    }

    /**
     * Static factory
     * 
     * @return fog monitor
     */
    public static synchronized FogMonitor getInstance() {
        if (monitor == null) {
            monitor = new FogMonitor();
            // Pre-populate dialog with an observations from DB
            monitor.createDataStructures();
            monitor.getAdjAreas();
            monitor.processProductAtStartup("fog");
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
            monitor = new FogMonitor();
        }
    }

    /**
     * Creates the maps
     */
    private void createDataStructures() {
        // [Jan 21, 2010, zhao]
        obData = new ObMultiHrsReports(CommonConfig.AppName.FOG);
        obData.setThresholdMgr(FogThresholdMgr.getInstance());

        obsData = new ObsData();
        algorithmData = new TreeMap<Date, Map<String, FOG_THREAT>>();

        for (String zone : MonitoringArea.getPlatformMap().keySet()) {
            obsData.addArea(zone, MonitoringArea.getPlatformMap().get(zone));
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
        if (fogPattern.matcher(filtered.dataURI).matches()) {
            processURI(filtered.dataURI, filtered);
        }
    }

    /**
     * Method that reads the table configuration and updates the zone monitor
     * threshold map
     * 
     * @param file
     *            -- the xml configuration filename
     */
    public void readTableConfig(String file) {
        // TODO update for Maritime
        Map<String, List<String>> zones = new HashMap<String, List<String>>();
        // create zones and stations list
        try {
            FogMonitorConfigurationManager areaConfig = getMonitorAreaConfig();
            for (String zone : areaConfig.getAreaList()) {
                // add the unique
                List<String> stations = areaConfig.getAreaStations(zone);
                zones.put(zone, stations);
            }
        } catch (Exception ve) {
            String msg = "FOG Monitor failed to load configuration..."
                    + this.getClass().getName();
            ErrorDialog.openError(Display.getCurrent().getActiveShell(),
                    "FOG Monitor failed to load configuration", msg,
                    new Status(IStatus.ERROR, Activator.PLUGIN_ID, msg, ve));

        }
        MonitoringArea.setPlatformMap(zones);
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
        fireMonitorEvent(zoneDialog.getClass().getName());
    }

    /**
     * Kill this monitor by nullifying the monitor's private instance variable.
     */
    @Override
    public void nullifyMonitor() {
        ProductAlertObserver.removeObserver(OBS, this);
        monitor = null;
    }

    /**
     * Finds the zone based on the icao passed into it
     * 
     * @param icao
     * @return zone
     */
    public String findZone(String icao) {
        for (String zone : MonitoringArea.getPlatformMap().keySet()) {
            if (MonitoringArea.getPlatformMap().get(zone).contains(icao)) {
                return zone;
            }
        }
        return null;
    }

    /**
     * get the main map
     * 
     * @return obsData
     */
    public ObsData getTableData() {
        return obsData;
    }

    /**
     * This method processes the incoming messages
     * 
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ObsMonitor#process(com.raytheon.uf.viz.monitor.data.ObReport)
     */
    @Override
    protected void process(ObReport result) throws Exception {
        obData.addReport(result);

        String zone = findZone(result.getPlatformId());
        getTableData().getArea(zone).addReport(result.getObservationTime(),
                result);

        fireMonitorEvent(this);

    }

    /**
     * gets the station Table Data
     * 
     * @return stationTableData
     */
    public TableData getStationTableData() {
        return stationTableData;
    }

    /**
     * launch the zone table dialog
     * 
     * @param type
     * @param shell
     */
    public void launchDialog(String type, Shell shell) {
        if (type.equals("zone")) {
            if (zoneDialog == null) {
                zoneDialog = new FogZoneTableDlg(shell, obData);
                addMonitorListener(zoneDialog);
                zoneDialog.addMonitorControlListener(this);
                fireMonitorEvent(zoneDialog.getClass().getName());
            }
            zoneDialog.open();
        } else if (type.equals("area")) {
            if (areaDialog == null) {
                areaDialog = new FogMonitoringAreaConfigDlg(shell,
                        "Fog Monitor Area Configuration");
            }
            areaDialog.open();
        }
    }

    /**
     * Set the algorithm threat by time and zone
     * 
     * @param time
     * @param algData
     */
    public void setAlgorithmData(Date time, Map<String, FOG_THREAT> algData) {
        if (algorithmData.containsKey(time)) {
            algorithmData.remove(time);
        }
        algorithmData.put(time, algData);
        updateDialogTime(time);
    }

    /**
     * Gets algorithm threat by time
     * 
     * @param time
     * @return algData
     */
    public Map<String, FOG_THREAT> getAlgorithmData(Date time) {
        Map<String, FOG_THREAT> algData = new HashMap<String, FOG_THREAT>();

        if (algorithmData.containsKey(time)) {
            algData = algorithmData.get(time);
        } else {
            // by default is nothing in the ALG column
            for (String zone : MonitoringArea.getPlatformMap().keySet()) {
                algData.put(zone, FOG_THREAT.GRAY);
            }
        }
        return algData;
    }

    /**
     * Gets the monitoring geometries
     * 
     * @return zoneGeometries
     */
    public Map<String, Geometry> getMonitoringAreaGeometries() {

        if (zoneGeometries == null) {

            List<String> zones = getMonitorAreaConfig().getAreaList();
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
     * Gets the fog configuration manager
     * 
     * @return fogConfig
     */
    public FogMonitorConfigurationManager getMonitorAreaConfig() {
        if (fogConfig == null) {
            LocalizationManager mgr = LocalizationManager.getInstance();
            String siteScope = mgr.getCurrentSite();

            fogConfig = FogMonitorConfigurationManager.getInstance();
            fogConfig.readConfigXml(siteScope);
        }
        return fogConfig;
    }

    /**
     * Gets observation data
     * 
     * @return
     */
    public ObMultiHrsReports getObData() {
        return obData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.fog.listeners.IFogResourceListener#
     * algorithmUpdate()
     */
    @Override
    public void algorithmUpdate() {

        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                Iterator<IFogResourceListener> iter = fogResources.iterator();

                while (iter.hasNext()) {
                    IFogResourceListener listener = iter.next();
                    listener.algorithmUpdate();
                }
            }
        });
    }

    /**
     * add a listener
     * 
     * @param ifru
     */
    public void addFogResourceListener(IFogResourceListener ifru) {
        fogResources.add(ifru);
    }

    /**
     * remove a listener
     * 
     * @param ifru
     */
    public void removeFogResourceListener(IFogResourceListener ifru) {
        fogResources.remove(ifru);
    }

    /**
     * FogResource sets the Drawtime
     * 
     * @param drawTime
     */
    public void updateDialogTime(Date dialogTime) {
        this.dialogTime = dialogTime;
        fireMonitorEvent(zoneDialog.getClass().getName());
    }

    /**
     * The date for the dialog to stay in step with
     * 
     * @return
     */
    public Date getDialogDate() {
        return dialogTime;
    }

    /**
     * close down the dialog
     * 
     */
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.fog.listeners.IFogResourceListener#closeDialog
     * ()
     */
    public void closeDialog() {
        if (zoneDialog != null) {
            monitor.removeMonitorListener(zoneDialog);
            monitor.nullifyMonitor();

            zoneDialog.close();
            zoneDialog = null;
        }

        if (areaDialog != null) {
            areaDialog.close();
            areaDialog = null;
        }
    }

    /**
     * Order the dates
     * 
     * @param type
     * @return
     */
    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.IMonitor#getTimeOrderedKeys(com.raytheon.
     * uf.viz.monitor.IMonitor, java.lang.String)
     */
    @Override
    public ArrayList<Date> getTimeOrderedKeys(IMonitor monitor, String type) {
        return null;
    }

    /**
     * Get adjacent areas.
     */
    public void getAdjAreas() {
        this.geoAdjAreas = AdjacentWfoMgr.getAdjacentAreas(cwa);
    }

    /**
     * Get geometry of adjacent areas.
     * 
     * @return the geoAdjAreas
     */
    public Geometry getGeoAdjAreas() {
        return geoAdjAreas;
    }

    /**
     * Sets the geoAdjAreas
     * 
     * @param geoAdjAreas
     */
    public void setGeoAdjAreas(Geometry geoAdjAreas) {
        this.geoAdjAreas = geoAdjAreas;
    }

    /**
     * First start
     */
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
        String zone = findZone(report.getPlatformId());
        getTableData().getArea(zone).addReport(report.getObservationTime(),
                report);
    }

    /**
     * Gets Fog zone table dialog
     * 
     * @return zoneDialog
     */
    public FogZoneTableDlg getZoneDialog() {
        return zoneDialog;
    }

}
