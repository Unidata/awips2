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
package com.raytheon.uf.viz.monitor.snow;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.regex.Pattern;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.ObConst.ChosenAppKey;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.Monitor;
import com.raytheon.uf.viz.monitor.ObsMonitor;
import com.raytheon.uf.viz.monitor.data.MonitoringArea;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.snow.listeners.ISnowResourceListener;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.snow.ui.dialogs.SnowMonitoringAreaConfigDlg;
import com.raytheon.uf.viz.monitor.snow.ui.dialogs.SnowZoneTableDlg;
import com.raytheon.uf.viz.monitor.util.MonitorThresholdConfiguration;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;

/**
 * 
 * SnowMonitor, monitor Data that triggers changes to the Snow display.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009 1981       dhladky     Initial creation.
 * 3/2/2009     2047       grichard    Made pluginName an array.
 * 3/2/2009     2047       grichard    Added stationName array.
 * 11/6/2009    3424       zhao/wkwock Display data from files instead of datagenerator.
 * 11/30/09     3424       zhao/wkwock/slav Automatically updates snow display. Display station data.
 * Dec 18, 2009 3424       zhao        use ObMultiHrsReports for obs data archive over time
 * Dec 22, 2009 3424       zhao        revised processProductAtStartup method to retrieve all data
 * July 20,2010 4891       skorolev    Added resource listener
 * May 15, 2012 14510      zhao        Modified processing at startup
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class SnowMonitor extends ObsMonitor {

    /** Singleton instance of this class */
    private static SnowMonitor monitor = null;

    /**
     * the zone table dialog
     */
    public SnowZoneTableDlg zoneDialog = null;

    /**
     * the monitoring area configure dialog
     */
    public SnowMonitoringAreaConfigDlg areaDialog = null;

    private SnowMonitorConfigurationManager snowConfig = null;

    /**
     * This object contains all observation data necessary for the table dialogs
     * and trending plots
     */
    private final ObMultiHrsReports obData;

    /** The application key */
    ChosenAppKey chosenAppKey = ChosenAppKey.SNOW;

    /** All SNOW plugins start with this */
	private static String OBS = "fssobs";

    /** regex wild card filter */
    protected static String wildCard = "[\\w\\(\\)-_:.]+";

    /** date of the data sent **/
    // private Date dataDate = null;
    /**
     * Time which Zone/County dialog shows.
     */
    public Date dialogTime = null;

    /** Array of snow listeners **/
    private final ArrayList<ISnowResourceListener> snowResources = new ArrayList<ISnowResourceListener>();


    /**
     * Private constructor, singleton
     */
	/**
     * 
     */
    private SnowMonitor() {

		pluginPatterns.add(snowPattern);
        readTableConfig(MonitorThresholdConfiguration.SNOW_THRESHOLD_CONFIG);
		initObserver("fssobs", this);
        obData = new ObMultiHrsReports(CommonConfig.AppName.SNOW);
        obData.setThresholdMgr(SnowThresholdMgr.getInstance());
        // Pre-populate dialog with an observation (METAR) for KOMA
        }

    public static synchronized SnowMonitor getInstance() {
        if (monitor == null) {
            monitor = new SnowMonitor();
    		monitor.processProductAtStartup("snow");
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
    		monitor = new SnowMonitor();
    	}
    }
    
    /**
     * 
     * @param shell
     */
    public void launchDialog(String type, Shell shell) {

        if (type.equals("zone")) {
            if (zoneDialog == null) {
                zoneDialog = new SnowZoneTableDlg(shell, obData);
                addMonitorListener(zoneDialog);
                zoneDialog.addMonitorControlListener(this);
            }
            zoneDialog.open();
            fireMonitorEvent(zoneDialog.getClass().getName());
        } else if (type.equals("area")) {
            areaDialog = new SnowMonitoringAreaConfigDlg(shell,
                    "SNOW Monitor Area Configuration");
            areaDialog.open();
        }
    }

    public ObMultiHrsReports getObData() {
        return obData;
    }

	private Pattern snowPattern = Pattern.compile(URIFilter.uriSeperator + OBS
			+ URIFilter.uriSeperator + wildCard + URIFilter.uriSeperator
			+ wildCard + URIFilter.uriSeperator + cwa + URIFilter.uriSeperator
			+ wildCard + URIFilter.uriSeperator + wildCard
			+ URIFilter.uriSeperator + wildCard + URIFilter.uriSeperator
			+ "snow");

    @Override
    public boolean filterNotifyMessage(NotificationMessage alertMessage) {
        return false;
    }

    @Override
    public void processNotifyMessage(NotificationMessage filtered) {

    }

    @Override
    public void processProductMessage(final AlertMessage filtered) {
		if (snowPattern.matcher(filtered.dataURI).matches()) {
            // System.out.println("Found match: " + snowPattern + " URI: "
            // + filtered.dataURI);
                processURI(filtered.dataURI, filtered);
            }
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
     * Method that reads the table configuration and updates the zone monitor
     * threshold map
     * 
     * @param file
     *            -- the xml configuration filename
     */
    public void readTableConfig(String file) {
        HashMap<String, ArrayList<String>> zones = new HashMap<String, ArrayList<String>>();
        // create zones and station list
        try {
            SnowMonitorConfigurationManager areaConfig = getMonitorAreaConfig();
            for (String zone : areaConfig.getAreaList()) {
                ArrayList<String> stations = areaConfig.getAreaStations(zone);
                zones.put(zone, stations);
            }
        } catch (Exception e) {
            System.out.println("Snow failed to load configuration..."
                    + this.getClass().getName());
        }
        MonitoringArea.setPlatformMap(zones);
    }

    public SnowMonitorConfigurationManager getMonitorAreaConfig() {
        if (snowConfig == null) {
            LocalizationManager mgr = LocalizationManager.getInstance();
            String siteScope = mgr.getCurrentSite();

            snowConfig = SnowMonitorConfigurationManager.getInstance();
            snowConfig.readConfigXml(siteScope);
        }
        return snowConfig;
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
        monitor.removeMonitorListener(zoneDialog);
        stopObserver("fssobs", this);
        monitor = null;
    }

    @Override
    protected void process(ObReport result)
            throws Exception {

        //final ProcessSnowReport snow = new ProcessSnowReport(result);
        //snow.processSnowReport();
        // add data to obData
        obData.addReport(result);
        fireMonitorEvent(this);
    }

    public Date getDialogTime() {
        return dialogTime;
    }

    public void setDialogTime(Date dialogTime) {
        this.dialogTime = dialogTime;
    }

    /**
     * add a listener
     * 
     * @param isru
     */
    public void addSnowResourceListener(ISnowResourceListener isru) {
        snowResources.add(isru);
    }

    /**
     * remove a listener
     * 
     * @param isru
     */
    public void removeSnowResourceListener(ISnowResourceListener isru) {
        snowResources.remove(isru);
    }

    /**
     * SnowResource sets the Drawtime
     * 
     * @param drawTime
     */
    public void updateDialogTime(Date dialogTime) {
        this.dialogTime = dialogTime;
        fireMonitorEvent(this);
    }

    public void closeDialog() {
        if (zoneDialog != null) {
            monitor.nullifyMonitor();

            zoneDialog.removeMonitorContorlListener(this);
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
    public DataTime getMostRecent(IMonitor monitor, String type) {
        return null;
    }

    /**
     * @return the zoneDialog
     */
    public SnowZoneTableDlg getZoneDialog() {
        return zoneDialog;
    }

    /**
     * @param zoneDialog
     *            the zoneDialog to set
     */
    public void setZoneDialog(SnowZoneTableDlg zoneDialog) {
        this.zoneDialog = zoneDialog;
    }

	@Override
	protected void processAtStartup(ObReport report) {
		obData.addReport(report);
	}
}
