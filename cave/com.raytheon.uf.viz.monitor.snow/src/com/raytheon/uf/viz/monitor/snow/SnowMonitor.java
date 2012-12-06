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
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.edex.urifilter.URIFilter;
import com.raytheon.uf.common.monitor.config.SnowMonitorConfigurationManager;
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
 * Oct 26, 2012 1280       skorolev    Clean code and made changes for non-blocking ZoneTableDlg
 * Nov. 1, 2012 1297       skorolev    Changed HashMap to Map and clean code
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class SnowMonitor extends ObsMonitor {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SnowMonitor.class);

    /** Singleton instance of this class */
    private static SnowMonitor monitor = null;

    /** Zone table dialog **/
    private SnowZoneTableDlg zoneDialog;

    /** Monitoring area configure dialog **/
    private SnowMonitoringAreaConfigDlg areaDialog = null;

    /** SNOW configuration manager **/
    private SnowMonitorConfigurationManager snowConfig = null;

    /**
     * This object contains all observation data necessary for the table dialogs
     * and trending plots
     */
    private final ObMultiHrsReports obData;

    /** All SNOW datauri start with this */
    private final String OBS = "fssobs";

    /** regex wild card filter */
    private final String wildCard = "[\\w\\(\\)-_:.]+";

    /** Time which Zone/County dialog shows. **/
    private Date dialogTime = null;

    /** Array of snow listeners **/
    private final List<ISnowResourceListener> snowResources = new ArrayList<ISnowResourceListener>();

    /** Pattern for SNOW **/
    private final Pattern snowPattern = Pattern.compile(URIFilter.uriSeperator
            + OBS + URIFilter.uriSeperator + wildCard + URIFilter.uriSeperator
            + wildCard + URIFilter.uriSeperator + cwa + URIFilter.uriSeperator
            + wildCard + URIFilter.uriSeperator + wildCard
            + URIFilter.uriSeperator + wildCard + URIFilter.uriSeperator
            + "snow");

    /**
     * Private constructor, singleton
     */
    private SnowMonitor() {
        pluginPatterns.add(snowPattern);
        readTableConfig(MonitorThresholdConfiguration.SNOW_THRESHOLD_CONFIG);
        initObserver(OBS, this);
        obData = new ObMultiHrsReports(CommonConfig.AppName.SNOW);
        obData.setThresholdMgr(SnowThresholdMgr.getInstance());
        // Pre-populate dialog with an observation (METAR) for KOMA
    }

    /**
     * Gets instance of monitor
     * 
     * @return monitor
     */
    public static synchronized SnowMonitor getInstance() {
        if (monitor == null) {
            monitor = new SnowMonitor();
            monitor.processProductAtStartup("snow");
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
            monitor = new SnowMonitor();
        }
    }

    /**
     * Launches SNOW zone table dialog
     * 
     * @param type
     * @param shell
     */
    public void launchDialog(String type, Shell shell) {
        if (type.equals("zone")) {
            if (zoneDialog == null) {
                zoneDialog = new SnowZoneTableDlg(shell, obData);
                addMonitorListener(zoneDialog);
                zoneDialog.addMonitorControlListener(this);
                fireMonitorEvent(zoneDialog.getClass().getName());
            }
            zoneDialog.open();
        } else if (type.equals("area")) {
            if (areaDialog == null) {
                areaDialog = new SnowMonitoringAreaConfigDlg(shell,
                        "SNOW Monitor Area Configuration");
            }
            areaDialog.open();
        }
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
        if (snowPattern.matcher(filtered.dataURI).matches()) {
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
        Map<String, List<String>> zones = new HashMap<String, List<String>>();
        // create zones and station list
        try {
            SnowMonitorConfigurationManager areaConfig = getMonitorAreaConfig();
            for (String zone : areaConfig.getAreaList()) {
                List<String> stations = areaConfig.getAreaStations(zone);
                zones.put(zone, stations);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Snow failed to load configuration..."
                            + this.getClass().getName());
        }
        MonitoringArea.setPlatformMap(zones);
    }

    /**
     * Gets configuration manager
     * 
     * @return snowConfig
     */
    public SnowMonitorConfigurationManager getMonitorAreaConfig() {
        if (snowConfig == null) {
            LocalizationManager mgr = LocalizationManager.getInstance();
            String siteScope = mgr.getCurrentSite();
            snowConfig = SnowMonitorConfigurationManager.getInstance();
            snowConfig.readConfigXml(siteScope);
        }
        return snowConfig;
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
     * Kills this monitor by nullifying the monitor's private instance variable.
     */
    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ObsMonitor#nullifyMonitor()
     */
    @Override
    public void nullifyMonitor() {
        monitor.removeMonitorListener(zoneDialog);
        stopObserver(OBS, this);
        monitor = null;
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
     * @return dialogTime
     */
    public Date getDialogTime() {
        return dialogTime;
    }

    /**
     * Sets dialog time
     * 
     * @param dialogTime
     */
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

    /**
     * Close SNOW zone table dialog
     */
    public void closeDialog() {
        if (zoneDialog != null) {
            monitor.nullifyMonitor();

            zoneDialog.removeMonitorContorlListener(this);
            zoneDialog.close();
            zoneDialog = null;
        }
        if (areaDialog != null) {
            areaDialog.close();
            areaDialog = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.IMonitor#getTimeOrderedKeys(com.raytheon.
     * uf.viz.monitor.IMonitor, java.lang.String)
     */
    @Override
    public ArrayList<Date> getTimeOrderedKeys(IMonitor monitor, String type) {
        // not used
        return null;
    }

    /**
     * @return zoneDialog
     */
    public SnowZoneTableDlg getZoneDialog() {
        return zoneDialog;
    }

    /**
     * Sets the zoneDialog
     * 
     * @param zoneDialog
     */
    public void setZoneDialog(SnowZoneTableDlg zoneDialog) {
        this.zoneDialog = zoneDialog;
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
    }
}
