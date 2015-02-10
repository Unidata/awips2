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
import java.util.Date;
import java.util.List;
import java.util.regex.Pattern;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.monitor.config.FSSObsMonitorConfigurationManager;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.monitor.IMonitor;
import com.raytheon.uf.viz.monitor.Monitor;
import com.raytheon.uf.viz.monitor.ObsMonitor;
import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;
import com.raytheon.uf.viz.monitor.snow.listeners.ISnowResourceListener;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.snow.ui.dialogs.SnowMonitoringAreaConfigDlg;
import com.raytheon.uf.viz.monitor.snow.ui.dialogs.SnowZoneTableDlg;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.ui.dialogs.ICloseCallback;

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
 * Feb 15, 2013 1638       mschenke    Changed code to reference DataURI.SEPARATOR instead of URIFilter
 * Apr 28, 2014 3086       skorolev    Removed local getMonitorAreaConfig method.
 * Jan 27, 2015 3220       skorolev    Updated configUpdate method and added updateMonitoringArea.
 *                                     Corrected snowConfig assignment.Corrected snowConfig assignment.
 *                                     Moved refreshing of table in the UI thread.Replaced MonitoringArea with snowAreaConfig.
 *                                     Updated code for better performance.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public class SnowMonitor extends ObsMonitor implements ISnowResourceListener {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SnowMonitor.class);

    /** Singleton instance of this class */
    private static SnowMonitor monitor = null;

    /** Zone table dialog **/
    private SnowZoneTableDlg zoneDialog;

    /** Monitoring area configure dialog **/
    private SnowMonitoringAreaConfigDlg areaDialog = null;

    /** SNOW configuration manager **/
    private static FSSObsMonitorConfigurationManager snowAreaConfig = null;

    /** All SNOW datauri start with this */
    private final String OBS = "fssobs";

    /** regex wild card filter */
    private final String wildCard = "[\\w\\(\\)-_:.]+";

    /** Array of snow listeners **/
    private final List<ISnowResourceListener> snowResources = new ArrayList<ISnowResourceListener>();

    /** Pattern for SNOW **/
    private final Pattern snowPattern = Pattern.compile(DataURI.SEPARATOR + OBS
            + DataURI.SEPARATOR + wildCard + DataURI.SEPARATOR + wildCard
            + DataURI.SEPARATOR + wildCard + DataURI.SEPARATOR + wildCard
            + DataURI.SEPARATOR + wildCard);

    /**
     * Private constructor, singleton
     */
    private SnowMonitor() {
        pluginPatterns.add(snowPattern);
        snowAreaConfig = FSSObsMonitorConfigurationManager.getSnowObsManager();
        initObserver(OBS, this);
        obData = new ObMultiHrsReports(CommonConfig.AppName.SNOW);
        obData.setThresholdMgr(SnowThresholdMgr.getInstance());
    }

    /**
     * Gets instance of monitor
     * 
     * @return monitor
     */
    public static synchronized SnowMonitor getInstance() {
        if (monitor == null) {
            monitor = new SnowMonitor();
            monitor.createDataStructures();
            monitor.processProductAtStartup(snowAreaConfig);
        }
        return monitor;
    }

    private void createDataStructures() {
        obData = new ObMultiHrsReports(CommonConfig.AppName.SNOW);
        obData.setThresholdMgr(SnowThresholdMgr.getInstance());
    }

    /**
     * Re-initialization of monitor.
     * 
     * DR#11279: When monitor area configuration is changed, this module is
     * called to re-initialize monitor using new monitor area configuration
     */
    public void reInitialize() {
        if (monitor != null)
            monitor.nullifyMonitor();
        SnowMonitor.getInstance();
    }

    /**
     * Launches SNOW zone table dialog
     * 
     * @param type
     * @param shell
     */
    public void launchDialog(String type, Shell shell) {
        if (type.equals("zone")) {
            zoneDialog = new SnowZoneTableDlg(shell, obData);
            addMonitorListener(zoneDialog);
            zoneDialog.addMonitorControlListener(this);
            zoneDialog.open();
        } else if (type.equals("area")) {
            areaDialog = new SnowMonitoringAreaConfigDlg(shell,
                    "SNOW Monitor Area Configuration");
            areaDialog.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    areaDialog = null;
                }

            });
            areaDialog.open();
        }
    }

    /**
     * Gets data.
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
            processURI(filtered.dataURI, filtered, snowAreaConfig);
        }
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
        snowAreaConfig = (FSSObsMonitorConfigurationManager) me.getSource();
        obData.getObHourReports().updateZones(snowAreaConfig);
        if (zoneDialog != null && !zoneDialog.isDisposed()) {
            obData.updateTableCache();
            fireMonitorEvent(zoneDialog.getClass().getName());
        }
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
        obData.getZoneTableData(result.getRefHour());
        fireMonitorEvent(this);
    }

    /**
     * Adds a listener.
     * 
     * @param isru
     *            listener
     */
    public void addSnowResourceListener(ISnowResourceListener isru) {
        snowResources.add(isru);
    }

    /**
     * Removes a listener.
     * 
     * @param isru
     *            listener
     */
    public void removeSnowResourceListener(ISnowResourceListener isru) {
        snowResources.remove(isru);
    }

    /**
     * Close SNOW zone table dialog.
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
     * Gets SNOW Zone Dialog.
     * 
     * @return zoneDialog
     */
    public SnowZoneTableDlg getZoneDialog() {
        return zoneDialog;
    }

    /**
     * Gets SNOW Area configuration dialog
     * 
     * @return
     */
    public SnowMonitoringAreaConfigDlg getAreaDialog() {
        return areaDialog;
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
