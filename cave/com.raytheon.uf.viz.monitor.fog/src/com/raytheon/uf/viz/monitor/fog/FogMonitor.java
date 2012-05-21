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
import java.util.Set;
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
import com.raytheon.uf.common.monitor.data.ObConst.ChosenAppKey;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.Monitor;
import com.raytheon.uf.viz.monitor.ObsMonitor;
import com.raytheon.uf.viz.monitor.data.AreaContainer;
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
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class FogMonitor extends ObsMonitor implements IFogResourceListener {

    /** Singleton instance of this class */
    private static FogMonitor monitor = null;

    /**
     * This object contains all observation data necessary for the table dialogs
     * and trending plots [this replaces the objects of ObsData and TableData
     * below Jan 21, 2010, zhao]
     */
    private ObMultiHrsReports obData;

    /** data holder for FOG **/
    public ObsData obsData = null;

    public ArrayList<Date> resourceTimes = null;

    /** data holder for FOG Algo data **/
    public HashMap<Date, HashMap<String, FOG_THREAT>> algorithmData = null;

    public Date dialogTime = null;

    /** list of coordinates for each zone **/
    public HashMap<String, Geometry> zoneGeometries = null;

    /** zone table dialog **/
    public FogZoneTableDlg zoneDialog = null;

    /** zone table dialog **/
    public MonitoringAreaConfigDlg areaDialog = null;

    /** area config manager **/
    public FogMonitorConfigurationManager fogConfig = null;

    /** The application key */
    ChosenAppKey chosenAppKey = ChosenAppKey.FOG;

    /** table data for the station table **/
    private final TableData stationTableData = new TableData(
            CommonConfig.AppName.FOG);

    /** All FOG's plugs start with this **/
	private static String OBS = "fssobs";

    /** Array of fogAlg listeners **/
    private final ArrayList<IFogResourceListener> fogResources = new ArrayList<IFogResourceListener>();

    /** regex wild card filter */
    protected static String wildCard = "[\\w\\(\\)\\-_:.]+";

    public Geometry geoAdjAreas = null;
    
    /**
     * Private constructor, singleton
     */
    private FogMonitor() {
		pluginPatterns.add(fogPattern);
        readTableConfig(MonitorThresholdConfiguration.FOG_THRESHOLD_CONFIG);
		initObserver("fssobs", this);
    }

    /**
     * Static factory
     * 
     * @return
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

    /**
     * DR#11279:
     * When monitor area configuration is changed, 
     * this module is called to re-initialize monitor
     * using new monitor area configuration 
     */
    public static void reInitialize() {
    	if ( monitor != null ) {
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
        algorithmData = new HashMap<Date, HashMap<String, FOG_THREAT>>();

        for (String zone : MonitoringArea.getPlatformMap().keySet()) {
            obsData.addArea(zone, MonitoringArea.getPlatformMap().get(zone));
        }
    }

    @Override
    public boolean filterNotifyMessage(NotificationMessage alertMessage) {

        return false;
    }

    @Override
    public void processNotifyMessage(NotificationMessage filtered) {

    }

    @Override
    public void processProductMessage(final AlertMessage filtered) {
		if (fogPattern.matcher(filtered.dataURI).matches()) {
            // System.out.println("Found match: " + fogPattern + " URI: "
            // + filtered.dataURI);
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
        HashMap<String, ArrayList<String>> zones = new HashMap<String, ArrayList<String>>();
        // create zones and stations list
        try {
            FogMonitorConfigurationManager areaConfig = getMonitorAreaConfig();
            for (String zone : areaConfig.getAreaList()) {
                // add the unique
                ArrayList<String> stations = areaConfig.getAreaStations(zone);
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

    @Override
    public void initObserver(String pluginName, Monitor monitor) {
        ProductAlertObserver.addObserver(pluginName, this);
    }

    @Override
    public void thresholdUpdate(IMonitorThresholdEvent me) {
        fireMonitorEvent(zoneDialog.getClass().getName());
    }

    @Override
    public void configUpdate(IMonitorConfigurationEvent me) {
        fireMonitorEvent(zoneDialog.getClass().getName());
    }

    /**
     * Kill this monitor by nullifying the monitor's private instance variable.
     */
    @Override
    public void nullifyMonitor() {
        // /**
        // * Before making the monitor null, remove observers
        // */
        // for (String p : pluginName) {
        // if (!pluginName.equals("")) {
        // stopObserver(p, this);
        // }
        // }

        // monitor.fogResources.removeAll(getMonitorListeners());
        ProductAlertObserver.removeObserver("fssobs", this);
        monitor = null;
    }

	private Pattern fogPattern = Pattern.compile(URIFilter.uriSeperator + OBS
			+ URIFilter.uriSeperator + wildCard + URIFilter.uriSeperator
			+ wildCard + URIFilter.uriSeperator + cwa + URIFilter.uriSeperator
			+ wildCard + URIFilter.uriSeperator + wildCard
			+ URIFilter.uriSeperator + wildCard + URIFilter.uriSeperator
			+ "fog");

    /**
     * Finds the zone based on the icao passed into it
     * 
     * @param icao
     * @return
     */
    public String findZone(String icao) {
        String rzone = null;
        for (String zone : MonitoringArea.getPlatformMap().keySet()) {
            if (MonitoringArea.getPlatformMap().get(zone).contains(icao)) {
                rzone = zone;
                break;
            }
        }
        return rzone;
    }

    /**
     * get the main map
     * 
     * @return
     */
    public ObsData getTableData() {
        return obsData;
    }

    /**
     * get your table row data
     * 
     * @param areaID
     * @return
     */
    public AreaContainer getObs(String areaID) {
        AreaContainer ac = null;
        if (getTableData().getContainer().containsKey(areaID)) {
            ac = getTableData().getArea(areaID);
        }
        return ac;
    }

    /**
     * This method processes the incoming messages
     * 
     * @param objectToSend
     * @param pluginname
     */
    @Override
    protected void process(ObReport result)
            throws Exception {

        // ProcessFogReport fog = new ProcessFogReport(result);
        // fog.processFogReport();

        // use ObMultiHrsReports for data archive
        // [Jan 21, 2010, zhao]
        obData.addReport(result);
        fireMonitorEvent(this);

        String zone = findZone(result.getPlatformId());
        getTableData().getArea(zone).addReport(result.getObservationTime(),
                result);
    }

    /**
     * gets the station Table Data
     * 
     * @return
     */
    public TableData getStationTableData() {
        return stationTableData;
    }

    /**
     * launch the zone table dialog
     * 
     * @param table
     */
    public void launchDialog(String type, Shell shell) {

        if (type.equals("zone")) {
            if (zoneDialog == null) {
				zoneDialog = new FogZoneTableDlg(shell, obData);
                addMonitorListener(zoneDialog);
                zoneDialog.addMonitorControlListener(this);
            }

            zoneDialog.open();
			fireMonitorEvent(zoneDialog.getClass().getName());
        }

        else if (type.equals("area")) {
            areaDialog = new FogMonitoringAreaConfigDlg(shell,
                    "Fog Monitor Area Configuration");
            areaDialog.open();
        }
    }

    /**
     * Set the algorithm threat by zone, time
     * 
     * @param zone
     * @param threat
     */
    public void setAlgorithmData(Date time, HashMap<String, FOG_THREAT> algData) {
        if (algorithmData.containsKey(time)) {
            algorithmData.remove(time);
        }
        algorithmData.put(time, algData);
    }

    /**
     * Gets the algorithm threat
     * 
     * @param zone
     * @return
     */
    public HashMap<String, FOG_THREAT> getAlgorithmData(Date time) {

        HashMap<String, FOG_THREAT> algData = new HashMap<String, FOG_THREAT>();

        if (algorithmData.containsKey(time)) {
            algData = algorithmData.get(time);
        } else {
            // default nothing in algo column
            for (String zone : MonitoringArea.getPlatformMap().keySet()) {
                algData.put(zone, FOG_THREAT.GRAY);
            }
        }
        return algData;
    }

    /**
     * Gets the monitoring geometries
     * 
     * @return
     */
    public HashMap<String, Geometry> getMonitoringAreaGeometries() {

        if (zoneGeometries == null) {

            ArrayList<String> zones = getMonitorAreaConfig().getAreaList();
            zoneGeometries = new HashMap<String, Geometry>();

            for (String zone : zones) {
                try {
                    zoneGeometries.put(zone, MonitorAreaUtils
                            .getZoneGeometry(zone));
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }

        return zoneGeometries;
    }

    /**
     * Gets the fog configuration manager
     * 
     * @return
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

    public ObMultiHrsReports getObData() {
        return obData;
    }

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
     * @param type
     */
    public void closeDialog() {
        if (zoneDialog != null) {
            monitor.removeMonitorListener(zoneDialog);
            monitor.nullifyMonitor();

            zoneDialog.shellDisposeDialog();
            zoneDialog = null;
        }

        if (areaDialog != null) {
            areaDialog.shellDisposeDialog();
            areaDialog = null;
        }
    }

    /**
     * Order the dates
     * 
     * @param type
     * @return
     */
    public ArrayList<Date> getTimeOrderedKeys(IMonitor monitor, String type) {
        return null;
    }

    /**
     * Get most recent time
     * 
     * @param type
     * @return
     */
    public DataTime getMostRecent() {
		DataTime time = null;
		Set<Date> ds = this.algorithmData.keySet();
		DataTime[] times = new DataTime[ds.size()];
		int i = 0;
		for (Date d : ds) {
			times[i] = new DataTime(d);
			i++;
		}
		java.util.Arrays.sort(times);
		if (times.length > 0) {
			time = times[times.length - 1]; // most recent
		}

		return time;
    }

	public FogZoneTableDlg getDialog() {
		return zoneDialog;
	}

    public void getAdjAreas() {
        this.geoAdjAreas = AdjacentWfoMgr.getAdjacentAreas(cwa);
    }

    /**
     * @return the geoAdjAreas
     */
    public Geometry getGeoAdjAreas() {
        return geoAdjAreas;
    }

    /**
     * @param geoAdjAreas
     *            the geoAdjAreas to set
     */
    public void setGeoAdjAreas(Geometry geoAdjAreas) {
        this.geoAdjAreas = geoAdjAreas;
    }

	@Override
	protected void processAtStartup(ObReport report) {
		obData.addReport(report);
        String zone = findZone(report.getPlatformId());
        getTableData().getArea(zone).addReport(report.getObservationTime(),
                report);
	}

}
