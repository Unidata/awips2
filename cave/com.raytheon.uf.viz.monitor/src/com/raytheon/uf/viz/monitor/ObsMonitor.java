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
package com.raytheon.uf.viz.monitor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecordTransform;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.uf.viz.monitor.data.MonitoringArea;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;

/**
 * 
 * obsMonitor, common for observation types
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 25, 2010 4759       dhladky     Initial creation.
 * Mar 15, 2012 14510      zhao        modified processProductAtStartup()
 * Sep 11, 2013 2277       mschenke    Got rid of ScriptCreator references
 * Feb 04, 2014 2757       skorolev    Added filter for removed stations
 * May 08, 2014 3086       skorolev    Added current site definition.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 * 
 */

public abstract class ObsMonitor extends Monitor {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ObsMonitor.class);

    /** Current Site name */
    protected String currentSite = LocalizationManager.getInstance()
            .getCurrentSite();

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.Monitor#filterNotifyMessage(com.raytheon.
     * uf.viz.core.notification.NotificationMessage)
     */
    @Override
    protected abstract boolean filterNotifyMessage(
            NotificationMessage alertMessage);

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.Monitor#nullifyMonitor()
     */
    @Override
    protected abstract void nullifyMonitor();

    /**
     * these are the over-arching "First line" checked patterns by plugin, first
     * letter of icao
     **/
    protected ArrayList<Pattern> pluginPatterns = new ArrayList<Pattern>();

    /** these are the patterns for the stations **/
    protected ArrayList<Pattern> stationPatterns = new ArrayList<Pattern>();

    /** Current CWA **/
    public static String cwa = LocalizationManager.getInstance().getSite();

    /**
     * This method processes the incoming messages
     * 
     * @param result
     */
    protected abstract void process(ObReport result) throws Exception;

    /**
     * This method processes the incoming messages at startup
     * 
     * @param report
     */
    protected abstract void processAtStartup(ObReport report);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.Monitor#processNotifyMessage(com.raytheon
     * .uf.viz.core.notification.NotificationMessage)
     */
    @Override
    protected abstract void processNotifyMessage(NotificationMessage filtered);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.Monitor#processProductMessage(com.raytheon
     * .uf.viz.core.alerts.AlertMessage)
     */
    @Override
    protected abstract void processProductMessage(AlertMessage filtered);

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.listeners.IMonitorThresholdListener#
     * thresholdUpdate
     * (com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent)
     */
    @Override
    public abstract void thresholdUpdate(IMonitorThresholdEvent me);

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.listeners.IMonitorConfigurationListener#
     * configUpdate
     * (com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent)
     */
    @Override
    public abstract void configUpdate(IMonitorConfigurationEvent me);

    /**
     * use this to do the initial filtering
     * 
     * @see com.raytheon.uf.viz.monitor.Monitor#filterProductMessage(com.raytheon.uf.viz.core.alerts.AlertMessage)
     */
    public boolean filterProductMessage(AlertMessage alertMessage) {
        // Determine whether or not there is a station ID present in the decoded
        // alert notification for this particular plugin name.
        try {
            boolean b = false;
            for (Pattern p : pluginPatterns) {
                if (p.matcher(alertMessage.dataURI).find()) {
                    b = true;
                    break;
                }
            }
            return b;
        } catch (NullPointerException e) {
            return false;
        }
    }

    /**
     * Process the incoming dataURI
     * 
     * @param dataURI
     * @param filtered
     */
    public void processURI(String dataURI, AlertMessage filtered) {
        try {
            Map<String, RequestConstraint> constraints = RequestConstraint
                    .toConstraintMapping(DataURIUtil.createDataURIMap(dataURI));
            FSSObsRecord[] pdos = requestFSSObs(constraints, null);
            if (pdos.length > 0 && pdos[0].getTimeObs() != null) {
                final FSSObsRecord objectToSend = pdos[0];
                try {
                    Display.getDefault().asyncExec(new Runnable() {
                        public void run() {
                            try {
                                // Filter removed stations
                                ArrayList<String> zones = MonitoringArea
                                        .getZoneIds(objectToSend
                                                .getPlatformId());
                                if (!zones.isEmpty()) {
                                    ObReport result = GenerateFSSObReport
                                            .generateObReport(objectToSend);
                                    statusHandler
                                            .handle(Priority.INFO,
                                                    "New FSSrecord ===> "
                                                            + objectToSend
                                                                    .getDataURI());
                                    process(result);
                                }
                            } catch (Exception e) {
                                statusHandler
                                        .handle(Priority.PROBLEM,
                                                "An error has occured processing the incoming messages.",
                                                e);
                            }
                        }
                    });
                } catch (Exception e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "An error has occured processing incoming dataURIs.",
                                    e);
                }
            }
        } catch (final Exception e) {
            statusHandler.handle(Priority.PROBLEM, "ObsMonitor: URI: "
                    + dataURI + " failed to process. " + e.getMessage());
        }
    }

    /**
     * Process products at startup
     * 
     * @param monitorUse
     * 
     */
    public void processProductAtStartup(String monitorUse) {

        /**
         * Assume this number for MaxNumObsTimes is larger enough to cover data
         * of all observations (at least 24 hours' worth of data) in database
         * [changed from 10 to 240 on May, 18, 2010 for DR #6015, zhao]
         */
        int MaxNumObsTimes = 240;
        Map<String, RequestConstraint> vals = new HashMap<String, RequestConstraint>();
        try {
            vals.put("cwa", new RequestConstraint(cwa));
            vals.put(FSSObsRecord.PLUGIN_NAME_ID, new RequestConstraint(
                    FSSObsRecord.PLUGIN_NAME));
            vals.put("monitorUse", new RequestConstraint(monitorUse));

            DataTime[] dataTimesAvailable = DataCubeContainer.performTimeQuery(
                    vals, false);
            DataTime[] selectedTimes = dataTimesAvailable;

            // Ensure that the latest product is retrieved.
            // [Modified: retrieve at most MaxNumObsTimes data
            // points, Feb
            // 19, 2010, zhao]
            if (dataTimesAvailable.length > 0) {
                Arrays.sort(dataTimesAvailable);
                // at most, MaxNumObsTimes observation times are
                // considered
                if (dataTimesAvailable.length > MaxNumObsTimes) {
                    selectedTimes = new DataTime[MaxNumObsTimes];
                    System.arraycopy(dataTimesAvailable,
                            dataTimesAvailable.length - MaxNumObsTimes,
                            selectedTimes, 0, MaxNumObsTimes);
                }

                FSSObsRecord[] obsRecords = requestFSSObs(vals, selectedTimes);
                for (FSSObsRecord objectToSend : obsRecords) {
                    // Filter removed stations
                    ArrayList<String> zones = MonitoringArea
                            .getZoneIds(objectToSend.getPlatformId());
                    if (!zones.isEmpty()) {
                        ObReport result = GenerateFSSObReport
                                .generateObReport(objectToSend);
                        processAtStartup(result);
                    }
                }
            }
        } catch (DataCubeException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "No data in database at startup.  " + monitorUse);
        }
    }

    /**
     * Gets array of FSSObs records.
     * 
     * @param constraints
     * @param times
     * @return FSSObsRecord[]
     * @throws VizException
     * @throws DataCubeException
     */
    private FSSObsRecord[] requestFSSObs(
            Map<String, RequestConstraint> constraints, DataTime[] times)
            throws DataCubeException {
        if (times != null) {
            String[] timeStrs = new String[times.length];
            for (int i = 0; i < times.length; ++i) {
                timeStrs[i] = times[i].toString();
            }
            constraints.put(PluginDataObject.DATATIME_ID,
                    new RequestConstraint(timeStrs));
        }
        PointDataContainer pdc = DataCubeContainer.getPointData(
                FSSObsRecord.PLUGIN_NAME, FSSObsRecordTransform.FSSOBS_PARAMS,
                constraints);
        return FSSObsRecordTransform.toFSSObsRecords(pdc);
    }
}
