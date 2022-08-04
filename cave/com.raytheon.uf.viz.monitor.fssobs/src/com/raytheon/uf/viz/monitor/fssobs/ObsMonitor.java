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
package com.raytheon.uf.viz.monitor.fssobs;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecord;
import com.raytheon.uf.common.dataplugin.fssobs.FSSObsRecordTransform;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.monitor.data.AdjacentWfoMgr;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.uf.viz.monitor.Monitor;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.ui.dialogs.MonitoringAreaConfigDlg;
import com.raytheon.uf.viz.monitor.ui.dialogs.ZoneTableDlg;
import org.locationtech.jts.geom.Geometry;

/**
 *
 * obsMonitor, common for observation types
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 25, 2010  4759     dhladky   Initial creation.
 * Mar 15, 2012  14510    zhao      modified processProductAtStartup()
 * Sep 11, 2013  2277     mschenke  Got rid of ScriptCreator references
 * Feb 04, 2014  2757     skorolev  Added filter for removed stations
 * May 08, 2014  3086     skorolev  Added current site definition.
 * Sep 04, 2014  3220     skorolev  Removed cwa and monitorUsefrom vals.
 * Sep 18, 2015  3873     skorolev  Included common definitions.
 * Oct 21, 2015  3873     dhladky   Get Obs load off UI thread.
 * Dec 17, 2015  3873     dhladky   Abstracted handling of dialogTime and Zone
 *                                  dialog events.
 * Jan 06, 2018  5934     njensen   Moved to new
 *                                  com.raytheon.uf.viz.monitor.fssobs plugin
 * Jul 10, 2018  6766     randerso  Moved dialog ownership and closeDialog
 *                                  method into base class
 *
 * </pre>
 *
 * @author dhladky
 */

public abstract class ObsMonitor extends Monitor {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ObsMonitor.class);

    /** Current CWA **/
    protected static final String cwa = LocalizationManager.getInstance()
            .getSite();

    /** All FSSObs datauri start with this **/
    protected static final String OBS = "fssobs";

    /** regex wild card filter **/
    protected static final String wildCard = "[\\w\\(\\)\\-_:.]+";

    /** Pattern for FSSObs **/
    protected static final Pattern fssPattern = Pattern
            .compile(DataURI.SEPARATOR + OBS + DataURI.SEPARATOR + wildCard
                    + DataURI.SEPARATOR + wildCard + DataURI.SEPARATOR
                    + wildCard + DataURI.SEPARATOR + wildCard
                    + DataURI.SEPARATOR + wildCard);

    /** Thread job to load obs with. **/
    protected ProcessObsJob obsJob = null;

    /** Time which Zone/County dialog shows. **/
    protected Date dialogTime = null;

    /** zone table dialog **/
    protected ZoneTableDlg zoneDialog;

    /** zone table dialog **/
    protected MonitoringAreaConfigDlg areaDialog = null;

    /**
     * these are the over-arching "First line" checked patterns by plugin, first
     * letter of icao
     **/
    protected List<Pattern> pluginPatterns = new ArrayList<>();

    /** these are the patterns for the stations **/
    protected List<Pattern> stationPatterns = new ArrayList<>();

    /** Adjacent areas for current cwa **/
    private Geometry geoAdjAreas;

    /**
     * Constructor.
     */
    public ObsMonitor() {
        this.getAdjAreas();
    }

    /**
     * Gets adjacent areas
     */
    public void getAdjAreas() {
        try {
            this.setGeoAdjAreas(AdjacentWfoMgr.getAdjacentAreas(cwa));
        } catch (SpatialException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

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

    /**
     * use this to do the initial filtering
     */
    @Override
    public boolean filterProductMessage(AlertMessage alertMessage) {
        // Determine whether or not there is a station ID present in the decoded
        // alert notification for this particular plugin name.
        boolean b = false;
        for (Pattern p : pluginPatterns) {
            if (p.matcher(alertMessage.dataURI).find()) {
                b = true;
                break;
            }
        }
        return b;
    }

    /**
     * Process the incoming dataURI
     *
     * @param dataURI
     */
    public void processURI(String dataURI) {
        try {
            Map<String, RequestConstraint> constraints = RequestConstraint
                    .toConstraintMapping(DataURIUtil.createDataURIMap(dataURI));
            FSSObsRecord[] pdos = requestFSSObs(constraints);
            if (pdos.length > 0 && pdos[0].getTimeObs() != null) {
                final FSSObsRecord objectToSend = pdos[0];
                try {
                    VizApp.runAsync((new Runnable() {
                        @Override
                        public void run() {
                            try {
                                ObReport result = GenerateFSSObReport
                                        .generateObReport(objectToSend);
                                statusHandler.info("New FSSrecord ===> "
                                        + objectToSend.getDataURI());
                                process(result);
                            } catch (Exception e) {
                                statusHandler.error(
                                        "An error has occured processing the incoming messages.",
                                        e);
                            }
                        }
                    }));
                } catch (Exception e) {
                    statusHandler.error(
                            "An error has occured processing incoming dataURIs.",
                            e);
                }
            }
        } catch (final Exception e) {
            statusHandler.error(
                    "ObsMonitor: URI: " + dataURI + " failed to process. ", e);
        }
    }

    /**
     * Process products at startup
     */
    public void processProductAtStartup() {

        final long start = System.currentTimeMillis();
        obsJob = null;
        obsJob = new ProcessObsJob(this);
        obsJob.addJobChangeListener(new JobChangeAdapter() {
            @Override
            public void done(IJobChangeEvent event) {
                // do nothing at this point
                final long end = System.currentTimeMillis();
                statusHandler.info("Obs Load took: " + (end - start) + " ms");
                obsJob = null;
            }
        });
        obsJob.schedule();
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
    protected FSSObsRecord[] requestFSSObs(
            Map<String, RequestConstraint> constraints)
            throws DataCubeException {
        PointDataContainer pdc = DataCubeContainer.getPointData(
                FSSObsRecord.PLUGIN_NAME, FSSObsRecordTransform.FSSOBS_PARAMS,
                constraints);
        return FSSObsRecordTransform.toFSSObsRecords(pdc);
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

    /**
     * Sets the Resource Drawtime
     *
     * @param dialogTime
     */
    public void updateDialogTime(Date dialogTime) {
        if (getZoneDialog().linkedToFrame) {
            this.dialogTime = dialogTime;
            fireMonitorEvent(zoneDialog.getClass().getName());
        }
    }

    /**
     * The date for the dialog to stay in step with
     *
     * @return
     */
    public Date getDialogTime() {
        return dialogTime;
    }

    /**
     * @return the zone table dialog
     */
    public ZoneTableDlg getZoneDialog() {
        return zoneDialog;
    }

    /**
     * Close dialog.
     */
    public void closeDialog() {
        if (zoneDialog != null) {
            nullifyMonitor();

            zoneDialog.removeMonitorContorlListener(this);
            zoneDialog.closeDialog();
            zoneDialog = null;
        }
        if (areaDialog != null) {
            areaDialog.close();
            areaDialog = null;
        }
    }

}
