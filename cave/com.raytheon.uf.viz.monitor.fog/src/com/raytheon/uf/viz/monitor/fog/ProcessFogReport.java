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
import java.util.List;

import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.ChosenAppKey;
import com.raytheon.uf.common.monitor.data.ObConst.ReportType;
import com.raytheon.uf.common.monitor.data.ObConst.ThreatLevel;
import com.raytheon.uf.viz.monitor.ProcessNewReport;
import com.raytheon.uf.viz.monitor.data.MonitorAreaThresholds;
import com.raytheon.uf.viz.monitor.data.MonitoringArea;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.util.ObUtil;

/**
 * The ProcessFogReport class contains the business logic to process a FOG
 * report.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2009 1999       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class ProcessFogReport extends ProcessNewReport {

    // Instance of Fog Report Model
    private FogReportModel model = FogReportModel.getInstance();

    // The list of ObReports to processes
    private ObReport[] report;

    // Indicator that report was dropped (not processed)
    private List<Boolean> droppedReport = new ArrayList<Boolean>();

    // Public Constructor
    public ProcessFogReport(ObReport[] report) {
        this.report = report;
    }

    /**
     * Process the Fog Report
     */
    public void processFogReport() {
        if (model.isRedoFullThreatInterval()) {
            model.setRedoFullThreatInterval(false);
            // completely empty out the green, yellow, and red lists; we must
            // re-process all reports within the SAFESEAS button's time
            // interval.
            model.getGreenList().clearObTimeList();
            model.getYellowList().clearObTimeList();
            model.getRedList().clearObTimeList();
        } else {
            // remove from the green, yellow, and red lists all observation
            // times two or more hours old.
            model.getGreenList().pruneObTimeList(ObUtil.getDropTime());
            model.getYellowList().pruneObTimeList(ObUtil.getDropTime());
            model.getRedList().pruneObTimeList(ObUtil.getDropTime());
        }
        String latestObscurationString = "";
        String obscurationString = "";
        float minVisThisRun = 1e34f;
        float minVis = 1e34f;
        Date minVisTimeThisRun = ObUtil.getDropTime();
        Date minVisTime = ObUtil.getDropTime();
        for (int i = 0; i < report.length; i++) {

            // DO NOT PROCESS SYNOPTIC FIXED LAND OR SYNOPTIC MOBILE LAND
            if (report[i].getReportType() == ReportType.SYNOPTIC_FIXED_LAND
                    || report[i].getReportType() == ReportType.SYNOPTIC_MOBILE_LAND) {
                droppedReport.add(true);
                continue;
            }
            newDataReceived = true;
            obTime = report[i].getObservationTime();
            if (obTime.after(dropTime)) {
                // Initialize threat level for observation.
                threatLevel = MonitorAreaThresholds.getThreatLevel(report[i],
                        ChosenAppKey.FOG);

                // insert the report's observation time into the correct
                // threat list based on the report's threat level.
                switch (threatLevel) {
                case GREEN:
                    model.getGreenList().storeDateTime(obTime);
                    break;
                case YELLOW:
                    model.getYellowList().storeDateTime(obTime);
                    break;
                case RED:
                    model.getRedList().storeDateTime(obTime);
                    break;
                default:
                    break;
                } // end switch

                // Search the report's present weather string for
                // obscurations.

                String presentWx = report[i].getPresentWx();

                if (MonitorAreaThresholds.isObstructionReported(presentWx)) {
                    // The present weather contains an obscuration.
                    // Now check the obscuration string for the same
                    // obscuration.
                    if (!latestObscurationString.contains(presentWx)) {
                        // The presentWx obscuration is not in the obscuration
                        // string, so append the two-character obscuration
                        // portion of presentWx onto the larger string.
                        latestObscurationString += presentWx + "  ";
                    }
                }

                // Now find the minimum visibility.
                // The vis >=0 check prevents running afoul of the MISSING -9999
                // value.

                float vis = report[i].getVisibility();
                if (vis >= 0) {
                    // Vis comes in in 16ths of a statute mile. Convert to
                    // statute miles.
                    // vis /= 16.0f;
                    // Update: in AWIPS II, visibility is in statute miles
                    // due to processing upstream.

                    // Find the minimum vis from the current update (and the ob
                    // time). The "<=" instead of "<" allows for
                    // minVisTime_this_run updates if the lowest vis stays
                    // static for awhile.
                    if (vis <= minVisThisRun) {
                        minVisThisRun = vis;
                        minVisTimeThisRun = obTime;
                    }
                }
            } // end if (report is not too old)
        } // end report loop

        if (droppedReport.size() == report.length) {
            return; // there are no reports that were not dropped
        }

        // If there have been station updates, then update the class member
        // Otherwise, if there are no updates (as is often the case, don't
        // let the "latest" empty string blot out valid obscuration text
        // from a previous run.
        if (latestObscurationString.length() != 0) {
            obscurationString = latestObscurationString;
        }

        // Check for empty string
        if (obscurationString.length() == 0) {
            obscurationString = "N/A";
        }

        // If the latest min vis (from the latest run) is valid and is lower
        // than or equal to the stored min vis for the whole time duration,
        // then update the min vis for the whole time duration ( >= instead of
        // > allows for more frequent obtime updates.) Min vis time comparison
        // to drop time checks of _minVis is too old.

        if (minVisThisRun < 1e20 && minVisThisRun >= 0) {
            if (minVisThisRun <= minVis || minVisTime.before(dropTime)) {
                minVis = minVisThisRun;
                if (minVisTimeThisRun.after(dropTime)) {
                    minVisTime = minVisTimeThisRun;
                }
            }
        }

        String minVisStr = "";
        if (minVis >= 0 && minVis < 1e20f) {
            minVisStr = ObUtil.getDisplayString("%.2f mi.", minVis);
        } else {
            minVisStr = "N/A";
        }

        // Now determine the new overall threat level.
        threatLevel = ThreatLevel.GRAY;

        String threatMessage = "";
        synchronized(ObConst.DATE_FORMAT){
            threatMessage += "Lowest Vis : " + minVisStr + " observed at "
                    + ObUtil.getDisplayString(minVisTime, ObConst.DATE_FORMAT)
                    + "Z\n";
        }
        threatMessage += "Obscurations: " + obscurationString + "\n";

        if (!model.getRedList().isObTimeListEmpty()) {
            threatLevel = ThreatLevel.RED;
            threatMessage = "Monitored observations at RED threshold level.";
        } else if (!model.getYellowList().isObTimeListEmpty()) {
            threatLevel = ThreatLevel.YELLOW;
            threatMessage = "Monitored observations at YELLOW threshold level.";
        } else if (!model.getGreenList().isObTimeListEmpty()) {
            threatLevel = ThreatLevel.GREEN;
            threatMessage = "Monitored observations at GREEN threshold level.";
        } else {
            threatMessage = "No monitoring information from any observation platform.";
        }

        // If Fog Monitor algorithm output is to be considered in the
        // obs app threat level, then evaluate the max fog threat level.

        if (model.isUsingFogThreatLevel()) {
            // Get the fog threat level.
            ThreatLevel fogThreatLevel = ObUtil.readFogThreatLevel();

            // Convert threat level number into string for text output.
            String fogThreatString = ObConst.THREAT_LEVEL_STRINGS[fogThreatLevel
                    .ordinal()];

            // Convert to upper case.
            String upcaseFogThreatString = fogThreatString.toUpperCase();

            // Define extra Fog Monitor messages to be delivered to Guardian.
            if (fogThreatLevel == ThreatLevel.GRAY) {
                threatMessage += "\nNo Fog Monitor threat levels available";
            } else if (fogThreatLevel == ThreatLevel.BLACK) {
                threatMessage += "\nFog Monitor error.  Check FMprocessor on server. ";
            } else {
                threatMessage += "\nFog Monitor at " + upcaseFogThreatString
                        + " threshold level.";
            }

            // Choose the higher of the two threats as the level to go forward.
            if ((fogThreatLevel.compareTo(threatLevel)) > 0) {
                threatLevel = fogThreatLevel;
            }
        } else {
            threatMessage += "\nFog Monitor satellite algorithms "
                    + "configured not to provide Guardian info.";
        }

        // Setup string for latest obstime; N/A if none available.
        String obTimeString = "";
        if (latestObTime.before(dropTime)) {
            obTimeString = "N/A";
        } else {
            synchronized(ObConst.DATE_FORMAT){
                obTimeString = ObUtil.getDisplayString(latestObTime,
                        ObConst.DATE_FORMAT);
            }
        }
        synchronized(ObConst.DATE_FORMAT){
            String dropTimeString = ObUtil.getDisplayString(dropTime,
                    ObConst.DATE_FORMAT);
        }

        // Set up time window character string.
        String timeCharStr = ObUtil.getDisplayString("%5.2f",
                (float) MonitoringArea.getTimeWindow());

        // Add time window info to Guardian message.
        threatMessage += "\nTime Window (hours): " + timeCharStr + "\n";

        // Send the message to AlertViz f/k/a Guardian.
        String mssg = "";
        int priority = 0;
        switch (threatLevel) {
        case RED:
            priority = 1;
            break;
        case YELLOW:
            priority = 2;
            break;
        case GREEN:
            priority = 3;
            break;
        case BLACK:
            priority = 4;
            break;
        default:
            priority = 5;
            break;
        }

        String type = "";
        type = "Fog/Vis Status";
        synchronized(ObConst.DATE_FORMAT){
            mssg = type
                    + " "
                    + ObUtil.getDisplayString(ObUtil.getThreatTime(),
                            ObConst.DATE_FORMAT) + "\n" + threatMessage;
        }
        String key = "";
        key = "FOG_MONITOR";

        // TODO Send the message (mssg) to AlertViz f/k/a Guardian.
		// System.out.println(mssg + " " + "MONITOR" + " " + key + " " +
        // priority);
        //model.getFogDisplay().putMessage(
		// mssg + " " + "MONITOR:" + " " + key + " " + "RT1: "
		// + report[0].getReportType().toString() + " " + "STN1: "
		// + report[0].getPlatformId() + " " + "PRIORITY: "
		// + priority);
        //model.getFogDisplay().putPriority(threatLevel);

    }
}
