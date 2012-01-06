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
import java.util.List;

import com.raytheon.uf.common.monitor.data.ObConst;
import com.raytheon.uf.common.monitor.data.ObConst.ReportType;
import com.raytheon.uf.common.monitor.data.ObConst.ThreatLevel;
import com.raytheon.uf.viz.monitor.ProcessNewReport;
import com.raytheon.uf.viz.monitor.data.MonitorAreaThresholds;
import com.raytheon.uf.viz.monitor.data.MonitoringArea;
import com.raytheon.uf.viz.monitor.data.ObReport;
import com.raytheon.uf.viz.monitor.util.ObUtil;

/**
 * The ProcessSafeSeasReport class contains the business logic to process a
 * SAFESEAS report.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2009 1999       grichard    Initial creation.
 * Nov 30, 2009 3424       zhao/wkwock/slav display station data.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */
public class ProcessSafeSeasReport extends ProcessNewReport {

    // Instance of Safe Seas Report Model
    private SafeSeasReportModel model = SafeSeasReportModel.getInstance();

    // The list of ObReports to processes
    private ObReport[] report;

    // Indicator that report was dropped (not processed)
    private List<Boolean> droppedReport = new ArrayList<Boolean>();

    // Public Constructor
    public ProcessSafeSeasReport(ObReport[] report) {
        this.report = report;
    }

    /**
     * Process the Safe Seas Report
     */
    public void processSafeSeasReport() {
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
        for (int i = 0; i < report.length; i++) {

            // DO NOT PROCESS SYNOPTIC FIXED LAND OR SYNOPTIC MOBILE LAND
            if (report[i].getReportType() == ReportType.SYNOPTIC_FIXED_LAND
                    || report[i].getReportType() == ReportType.SYNOPTIC_MOBILE_LAND) {
                droppedReport.add(true);
                continue;
            }
            newDataReceived = true;
            obTime = report[i].getObservationTime();
            if (latestObTime.before(obTime)) {
                latestObTime = obTime;
            }
            if (obTime.after(dropTime)) {
                // Initialize threat level for observation.
                ObConst.ThreatLevel threatTemp = ObConst.ThreatLevel.GRAY;
                // Begin worst case value and threat level evaluations for each
                // parameter. Note that worst case values and threat levels are
                // handled independently, because the county-by-county, zone-by-
                // zone threshold settings mean that the worst-case value may
                // not be generating the highest threat level (so you can't just
                // plug the worst-case value into get_threat_level()). Checks
                // against drop time allow expired values (older than the time
                // window specifies) to be discarded.

                // Wind speed evaluations.

                if (model.getWorstValueReport().getWindSpeed() < report[i]
                        .getWindSpeed()
                        || model.getWorstValueTimeReport().getWindSpeed()
                                .before(dropTime)) {
                    model.getWorstValueReport().setWindSpeed(
                            report[i].getWindSpeed());
                    model.getWorstValueTimeReport().setWindSpeed(obTime);
                }

                threatTemp = MonitorAreaThresholds.getThreatLevel(report[i],
                        ObConst.VarName.WIND_SPEED);

                if (threatLevel.ordinal() < threatTemp.ordinal()) {
                    threatLevel = threatTemp;
                }
                if ((model.getWorstThreatReport().getWindSpeed().ordinal() < threatTemp
                        .ordinal())
                        || model.getWorstThreatTimeReport().getWindSpeed()
                                .before(dropTime)) {
                    model.getWorstThreatReport().setWindSpeed(threatTemp);
                    model.getWorstThreatTimeReport().setWindSpeed(obTime);
                }

                // Wind gust evaluations.

                if (model.getWorstValueReport().getWindGust() < report[i]
                        .getWindGust()
                        || model.getWorstValueTimeReport().getWindGust()
                                .before(dropTime)) {
                    model.getWorstValueReport().setWindGust(
                            report[i].getWindGust());
                    model.getWorstValueTimeReport().setWindGust(obTime);
                }

                threatTemp = MonitorAreaThresholds.getThreatLevel(report[i],
                        ObConst.VarName.GUST_SPEED);

                if (threatLevel.ordinal() < threatTemp.ordinal()) {
                    threatLevel = threatTemp;
                }
                if ((model.getWorstThreatReport().getWindGust().ordinal() < threatTemp
                        .ordinal())
                        || model.getWorstThreatTimeReport().getWindGust()
                                .before(dropTime)) {
                    model.getWorstThreatReport().setWindGust(threatTemp);
                    model.getWorstThreatTimeReport().setWindGust(obTime);
                }

                // Peak wind evaluations.

                if (model.getWorstValueReport().getMaxWindSpeed() < report[i]
                        .getMaxWindSpeed()
                        || model.getWorstValueTimeReport().getMaxWindSpeed()
                                .before(dropTime)) {
                    model.getWorstValueReport().setMaxWindSpeed(
                            report[i].getMaxWindSpeed());
                    model.getWorstValueTimeReport().setMaxWindSpeed(obTime);
                }

                threatTemp = MonitorAreaThresholds.getThreatLevel(report[i],
                        ObConst.VarName.MAX_WIND_SPEED);

                if (threatLevel.ordinal() < threatTemp.ordinal()) {
                    threatLevel = threatTemp;
                }
                if ((model.getWorstThreatReport().getMaxWindSpeed().ordinal() < threatTemp
                        .ordinal())
                        || model.getWorstThreatTimeReport().getMaxWindSpeed()
                                .before(dropTime)) {
                    model.getWorstThreatReport().setMaxWindSpeed(threatTemp);
                    model.getWorstThreatTimeReport().setMaxWindSpeed(obTime);
                }

                // Visibility evaluations.

                float vis = report[i].getVisibility();
                // The vis >=0 check prevents running afoul of the MISSING -999
                // value.
                if ((model.getWorstValueReport().getVisibility() > vis && vis >= 0)
                        || (vis >= 0 && model.getWorstValueTimeReport()
                                .getVisibility().before(dropTime))) {
                    // Divide by 16.0 to get statute miles.
                    // model.getWorstValueReport().setVisibility(vis / 16.0f);
                    // Update: in AWIPS II, visibility is in statute miles
                    // due to processing upstream.
                    model.getWorstValueTimeReport().setVisibility(obTime);
                }

                threatTemp = MonitorAreaThresholds.getThreatLevel(report[i],
                        ObConst.VarName.VISIBILITY);

                if (threatLevel.ordinal() < threatTemp.ordinal()) {
                    threatLevel = threatTemp;
                }
                if ((model.getWorstThreatReport().getVisibility().ordinal() < threatTemp
                        .ordinal())
                        || model.getWorstThreatTimeReport().getVisibility()
                                .before(dropTime)) {
                    model.getWorstThreatReport().setVisibility(threatTemp);
                    model.getWorstThreatTimeReport().setVisibility(obTime);
                }

                // Wave height evaluations.

                if (model.getWorstValueReport().getHighResWaveHeight() < report[i]
                        .getHighResWaveHeight()
                        || model.getWorstValueTimeReport()
                                .getHighResWaveHeight().before(dropTime)) {
                    model.getWorstValueReport().setHighResWaveHeight(
                            report[i].getHighResWaveHeight());
                    model.getWorstValueTimeReport()
                            .setHighResWaveHeight(obTime);
                }

                threatTemp = MonitorAreaThresholds.getThreatLevel(report[i],
                        ObConst.VarName.WAVE_HEIGHT);

                if (threatLevel.ordinal() < threatTemp.ordinal()) {
                    threatLevel = threatTemp;
                }
                if ((model.getWorstThreatReport().getHighResWaveHeight()
                        .ordinal() < threatTemp.ordinal())
                        || model.getWorstThreatTimeReport()
                                .getHighResWaveHeight().before(dropTime)) {
                    model.getWorstThreatReport().setHighResWaveHeight(
                            threatTemp);
                    model.getWorstThreatTimeReport().setHighResWaveHeight(
                            obTime);
                }

                // Primary swell height evaluations.

                if (model.getWorstValueReport().getPSwellHeight() < report[i]
                        .getPSwellHeight()
                        || model.getWorstValueTimeReport().getPSwellHeight()
                                .before(dropTime)) {
                    model.getWorstValueReport().setPSwellHeight(
                            report[i].getPSwellHeight());
                    model.getWorstValueTimeReport().setPSwellHeight(obTime);
                }

                threatTemp = MonitorAreaThresholds.getThreatLevel(report[i],
                        ObConst.VarName.PRIM_SWELL_HT);

                if (threatLevel.ordinal() < threatTemp.ordinal()) {
                    threatLevel = threatTemp;
                }
                if ((model.getWorstThreatReport().getPSwellHeight().ordinal() < threatTemp
                        .ordinal())
                        || model.getWorstThreatTimeReport().getPSwellHeight()
                                .before(dropTime)) {
                    model.getWorstThreatReport().setPSwellHeight(threatTemp);
                    model.getWorstThreatTimeReport().setPSwellHeight(obTime);
                }

                // Primary swell period evaluations. There's an extra check
                // to see whether higher or lower values should be emphasized
                // (the check is MonitorAreaThresholds.isRankHighSwellPeriods())

                if ((MonitorAreaThresholds.isRankHighSwellPeriods() && model
                        .getWorstValueReport().getPSwellPeriod() < report[i]
                        .getPSwellPeriod())
                        || (!MonitorAreaThresholds.isRankHighSwellPeriods() && model
                                .getWorstValueReport().getPSwellPeriod() > report[i]
                                .getPSwellPeriod())
                        || (model.getWorstValueTimeReport().getPSwellPeriod()
                                .before(dropTime))) {
                    model.getWorstValueReport().setPSwellPeriod(
                            report[i].getPSwellPeriod());
                    model.getWorstValueTimeReport().setPSwellPeriod(obTime);
                }

                threatTemp = MonitorAreaThresholds.getThreatLevel(report[i],
                        ObConst.VarName.PRIM_SWELL_PD);

                if (threatLevel.ordinal() < threatTemp.ordinal()) {
                    threatLevel = threatTemp;
                }
                if ((model.getWorstThreatReport().getPSwellPeriod().ordinal() < threatTemp
                        .ordinal())
                        || model.getWorstThreatTimeReport().getPSwellPeriod()
                                .before(dropTime)) {
                    model.getWorstThreatReport().setPSwellPeriod(threatTemp);
                    model.getWorstThreatTimeReport().setPSwellPeriod(obTime);
                }

                // Primary swell direction evaluations.
                // Omit primary worst swell value calculation.

                threatTemp = MonitorAreaThresholds.getThreatLevel(report[i],
                        ObConst.VarName.PRIM_SWELL_DIR);

                if (threatLevel.ordinal() < threatTemp.ordinal()) {
                    threatLevel = threatTemp;
                }
                if ((model.getWorstThreatReport().getPSwellDir().ordinal() < threatTemp
                        .ordinal())
                        || model.getWorstThreatTimeReport().getPSwellDir()
                                .before(dropTime)) {
                    model.getWorstThreatReport().setPSwellDir(threatTemp);
                    model.getWorstThreatTimeReport().setPSwellDir(obTime);
                }

                // Secondary swell height evaluations.

                if (model.getWorstValueReport().getSSwellHeight() < report[i]
                        .getSSwellHeight()
                        || model.getWorstValueTimeReport().getSSwellHeight()
                                .before(dropTime)) {
                    model.getWorstValueReport().setSSwellHeight(
                            report[i].getSSwellHeight());
                    model.getWorstValueTimeReport().setSSwellHeight(obTime);
                }

                threatTemp = MonitorAreaThresholds.getThreatLevel(report[i],
                        ObConst.VarName.SEC_SWELL_HT);

                if (threatLevel.ordinal() < threatTemp.ordinal()) {
                    threatLevel = threatTemp;
                }
                if ((model.getWorstThreatReport().getSSwellHeight().ordinal() < threatTemp
                        .ordinal())
                        || model.getWorstThreatTimeReport().getSSwellHeight()
                                .before(dropTime)) {
                    model.getWorstThreatReport().setSSwellHeight(threatTemp);
                    model.getWorstThreatTimeReport().setSSwellHeight(obTime);
                }

                // Secondary swell period evaluations. There's an extra check
                // to see whether higher or lower values should be emphasized
                // (the check is MonitorAreaThresholds.isRankHighSwellPeriods())

                if ((MonitorAreaThresholds.isRankHighSwellPeriods() && model
                        .getWorstValueReport().getSSwellPeriod() < report[i]
                        .getSSwellPeriod())
                        || (!MonitorAreaThresholds.isRankHighSwellPeriods() && model
                                .getWorstValueReport().getSSwellPeriod() > report[i]
                                .getSSwellPeriod())
                        || (model.getWorstValueTimeReport().getSSwellPeriod()
                                .before(dropTime))) {
                    model.getWorstValueReport().setSSwellPeriod(
                            report[i].getSSwellPeriod());
                    model.getWorstValueTimeReport().setSSwellPeriod(obTime);
                }

                threatTemp = MonitorAreaThresholds.getThreatLevel(report[i],
                        ObConst.VarName.SEC_SWELL_PD);

                if (threatLevel.ordinal() < threatTemp.ordinal()) {
                    threatLevel = threatTemp;
                }
                if ((model.getWorstThreatReport().getSSwellPeriod().ordinal() < threatTemp
                        .ordinal())
                        || model.getWorstThreatTimeReport().getSSwellPeriod()
                                .before(dropTime)) {
                    model.getWorstThreatReport().setSSwellPeriod(threatTemp);
                    model.getWorstThreatTimeReport().setSSwellPeriod(obTime);
                }

                // Secondary swell direction evaluations.
                // Omit primary worst swell value calculation.

                threatTemp = MonitorAreaThresholds.getThreatLevel(report[i],
                        ObConst.VarName.SEC_SWELL_DIR);

                if (threatLevel.ordinal() < threatTemp.ordinal()) {
                    threatLevel = threatTemp;
                }
                if ((model.getWorstThreatReport().getSSwellDir().ordinal() < threatTemp
                        .ordinal())
                        || model.getWorstThreatTimeReport().getSSwellDir()
                                .before(dropTime)) {
                    model.getWorstThreatReport().setSSwellDir(threatTemp);
                    model.getWorstThreatTimeReport().setSSwellDir(obTime);
                }

                // insert the report's observation time into the correct threat
                // list based on the report's threat level.
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
            } // end if (report is not too old)
        } // end report loop

        if (droppedReport.size() == report.length) {
            return; // there are no reports that were not dropped
        }

        // Initialize the boolean matching vector (which is true if there's a
        // match (if we've seen the basic string, without intensity, earlier
        // in the upcoming loop. It is assigned the size of the presentWxVec
        // vector (one T/F for each present weather element in the vector).
        List<Boolean> presentWxMatchVec = new ArrayList<Boolean>();
        // Initialize the present weather match vector.
        for (int i = 0; i < presentWxVec.size(); i++) {
            presentWxMatchVec.add(false);
        }
        String maxIntensityFrozenPrecip = "";

        // This loop sorts through the frozen precip vector collected above.
        // The filter above identified the unique frozen precip strings. Now
        // that we have unique strings in a vector, we can sort through them
        // again, looking for the highest intensity. Only the highest intensity
        // of each unique present weather string will be listed in the Guardian
        // output (e.g., if -SNFZFG, SNFZFG, and +SNFZFG have been reported,
        // the +SNFZFG string will be displayed in the Guardian cursor outputi).
        for (int i = 0; i < presentWxVec.size(); i++) {
            // Check to see if this string (sans intensity) has been matched
            // in a prior iteration. If so, continue.
            if (presentWxMatchVec.get(i)) {
                continue;
            }
            String stringI = "";
            ObConst.IntensityLevel maxIntensityLevel;

            // Get the present weather strings without the intensity
            // prefix. This is straightforward for moderate events
            // (because there is no prefix), but for light or heavy
            // events, the TextString::right() method is used to lop
            // off initial +/- character.
            if (intensityLevelVec.get(i) == ObConst.IntensityLevel.MODERATE) {
                stringI = presentWxVec.get(i);
            } else {
                stringI = presentWxVec.get(i).replaceFirst("(\\+|-)", "");
            }

            // Initialize the max intensity level for this event.
            maxIntensityLevel = intensityLevelVec.get(i);

            // Initialize the string which will hold the event of the current
            // type that has the highest intensity.
            String maxIntensityPresentWx = "";

            // Start a second loop to compare the rest of the vector to
            // the current weather event.
            for (int j = i + 1; j < presentWxVec.size(); j++) {
                // As was done above, skip over vector elements which have
                // already been matched to the same weather event, and thus
                // have been accounted for.
                if (presentWxMatchVec.get(j)) {
                    continue;
                }

                // Get the present weather strings without the intensity
                // prefix (as was done in the i-loop.)
                String stringJ = "";
                if (intensityLevelVec.get(j) == ObConst.IntensityLevel.MODERATE) {
                    stringJ = presentWxVec.get(j);
                } else {
                    stringJ = presentWxVec.get(j).replaceFirst("(\\+|-)", "");
                }

                // Compare the strings from the nested loops...
                if (stringI.equals(stringJ)) {
                    // If there is a match, flag the corresponding boolean
                    // vector element as true.
                    presentWxMatchVec.set(j, true);

                    // Compare the current max intensity found for this event
                    // type with the intensity of the current inner-loop
                    // vector[i] element. Change if necessary.
                    if (maxIntensityLevel.compareTo(intensityLevelVec.get(j)) < 0) {
                        maxIntensityLevel = intensityLevelVec.get(j);
                    }
                }
            }

            // Append the appropriate maximum intensity prefix (for
            // light or heavy) the max event type string.
            if (maxIntensityLevel == ObConst.IntensityLevel.HEAVY) {
                maxIntensityPresentWx = "+" + stringI;
            } else if (maxIntensityLevel == ObConst.IntensityLevel.LIGHT) {
                maxIntensityPresentWx = "-" + stringI;
            } else {
                maxIntensityPresentWx = stringI;
            }

            maxIntensityFrozenPrecip += maxIntensityPresentWx = "  ";
            // Now that the parent loop will iterate, assign the matching
            // vector element as true -- this weather event type does not
            // need to be evaluated again.
            presentWxMatchVec.set(i, true);
        }

        // Update max intensity frozen precip values to Guardian output
        // string provided the time they were reported is within the drop
        // time. Also check for length to ensure that quick, back-to
        // back updates don't blot out the values (the only way the
        // maxIntensityFrozenPrecipString would be 0 sized if the
        // _latestFrozenPrecipTime > drop_time is if notifications came
        // in rapidly back to back, and no new obs were reported in that
        // time.

        if (!latestFrozenPrecipTime.before(dropTime)) {
            guardianFrozenPrecip = maxIntensityFrozenPrecip;
        } else {
            guardianFrozenPrecip = "";
        }

        // now determine the new overall threat level.
        threatLevel = ThreatLevel.GRAY;
        String threatMessage = "";
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

        String worstVal = "";

        if (threatLevel != ThreatLevel.GRAY) {
            threatMessage += "\n\nMONITORED VALUES \t  HIGHEST THREAT LEVELS\n ";

            // Visibility threat message
            if (model.getWorstValueReport().getVisibility() < 0
                    || model.getWorstValueReport().getVisibility() > 1e20f) {
                worstVal = "N/A";
            } else {
                worstVal = ObUtil.getDisplayString("%.2f mi", model
                        .getWorstValueReport().getVisibility());
            }
            threatMessage += "Min Vis: "
                    + worstVal
                    + "\t\t "
                    + ObConst.THREAT_LEVEL_STRINGS[model.getWorstThreatReport()
                            .getVisibility().ordinal()] + "\n ";

            // Wind speed threat message
            if (model.getWorstValueReport().getWindSpeed() < 0
                    || model.getWorstValueReport().getWindSpeed() > 1e20f) {
                worstVal = "N/A";
            } else {
                worstVal = ObUtil.getDisplayString("%.1f kts", model
                        .getWorstValueReport().getWindSpeed());
            }
            threatMessage += "Max Wind Speed: "
                    + worstVal
                    + "\t "
                    + ObConst.THREAT_LEVEL_STRINGS[model.getWorstThreatReport()
                            .getWindSpeed().ordinal()] + "\n ";

            // Wind gust threat message
            if (model.getWorstValueReport().getWindGust() < 0
                    || model.getWorstValueReport().getWindGust() > 1e20f) {
                worstVal = "N/A";
            } else {
                worstVal = ObUtil.getDisplayString("%.1f kts", model
                        .getWorstValueReport().getWindGust());
            }
            threatMessage += "Max Wind Gust: "
                    + worstVal
                    + "\t "
                    + ObConst.THREAT_LEVEL_STRINGS[model.getWorstThreatReport()
                            .getWindGust().ordinal()] + "\n ";

            // Peak Wind speed threat message
            if (model.getWorstValueReport().getMaxWindSpeed() < 0
                    || model.getWorstValueReport().getMaxWindSpeed() > 1e20f) {
                worstVal = "N/A";
            } else {
                worstVal = ObUtil.getDisplayString("%.1f kts", model
                        .getWorstValueReport().getMaxWindSpeed());
            }
            threatMessage += "Max Peak Wind: "
                    + worstVal
                    + "\t "
                    + ObConst.THREAT_LEVEL_STRINGS[model.getWorstThreatReport()
                            .getMaxWindSpeed().ordinal()] + "\n ";

            // High resolution wave height threat message
            if (model.getWorstValueReport().getHighResWaveHeight() < 0
                    || model.getWorstValueReport().getHighResWaveHeight() > 1e20f) {
                worstVal = "N/A";
            } else {
                worstVal = ObUtil.getDisplayString("%.1f ft", model
                        .getWorstValueReport().getHighResWaveHeight());
            }
            threatMessage += "Max Wave Ht: "
                    + worstVal
                    + "\t\t "
                    + ObConst.THREAT_LEVEL_STRINGS[model.getWorstThreatReport()
                            .getHighResWaveHeight().ordinal()] + "\n ";

            // Primary swell height threat message
            if (model.getWorstValueReport().getPSwellHeight() < 0
                    || model.getWorstValueReport().getPSwellHeight() > 1e20f) {
                worstVal = "N/A";
            } else {
                worstVal = ObUtil.getDisplayString("%.1f ft", model
                        .getWorstValueReport().getPSwellHeight());
            }
            threatMessage += "Max Prim Swell Ht: "
                    + worstVal
                    + "\t\t "
                    + ObConst.THREAT_LEVEL_STRINGS[model.getWorstThreatReport()
                            .getPSwellHeight().ordinal()] + "\n ";

            // Primary swell period threat message
            if (model.getWorstValueReport().getPSwellPeriod() < 0
                    || model.getWorstValueReport().getPSwellPeriod() > 1e20f) {
                worstVal = "N/A";
            } else {
                worstVal = ObUtil.getDisplayString("%.1f sec", model
                        .getWorstValueReport().getPSwellPeriod());
            }
            threatMessage += "Max Prim Swell Pd: "
                    + worstVal
                    + "\t\t "
                    + ObConst.THREAT_LEVEL_STRINGS[model.getWorstThreatReport()
                            .getPSwellPeriod().ordinal()] + "\n ";

            // Primary swell direction threat message
            worstVal = "NM";
            threatMessage += "Prim Swell Dir: "
                    + worstVal
                    + "\t\t "
                    + ObConst.THREAT_LEVEL_STRINGS[model.getWorstThreatReport()
                            .getPSwellDir().ordinal()] + "\n ";

            // Secondary swell height threat message
            if (model.getWorstValueReport().getSSwellHeight() < 0
                    || model.getWorstValueReport().getSSwellHeight() > 1e20f) {
                worstVal = "N/A";
            } else {
                worstVal = ObUtil.getDisplayString("%.1f ft", model
                        .getWorstValueReport().getSSwellHeight());
            }
            threatMessage += "Max Sec Swell Ht: "
                    + worstVal
                    + "\t\t "
                    + ObConst.THREAT_LEVEL_STRINGS[model.getWorstThreatReport()
                            .getSSwellHeight().ordinal()] + "\n ";

            // Secondary swell period threat message
            if (model.getWorstValueReport().getSSwellPeriod() < 0
                    || model.getWorstValueReport().getSSwellPeriod() > 1e20f) {
                worstVal = "N/A";
            } else {
                worstVal = ObUtil.getDisplayString("%.1f sec", model
                        .getWorstValueReport().getSSwellPeriod());
            }
            threatMessage += "Max Sec Swell Pd: "
                    + worstVal
                    + "\t\t "
                    + ObConst.THREAT_LEVEL_STRINGS[model.getWorstThreatReport()
                            .getSSwellPeriod().ordinal()] + "\n ";

            // Secondary swell direction threat message
            worstVal = "NM";
            threatMessage += "Sec Swell Dir: "
                    + worstVal
                    + "\t\t "
                    + ObConst.THREAT_LEVEL_STRINGS[model.getWorstThreatReport()
                            .getSSwellDir().ordinal()] + "\n ";

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
            threatMessage += "\nFog Monitor input not contributing to SAFESEAS icon.\n";
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
        String dropTimeString = null;
        synchronized(ObConst.DATE_FORMAT){
            dropTimeString = ObUtil.getDisplayString(dropTime,
                    ObConst.DATE_FORMAT);
        }

        // Set up time window character string.
        String timeCharStr = ObUtil.getDisplayString("%5.2f",
                (float) MonitoringArea.getTimeWindow());

        threatMessage += "Latest Observation (GMT): " + obTimeString
                + "\nEarliest Report Time Considered (GMT): " + dropTimeString
                + "\nTime Window (hours): " + timeCharStr + "\n";

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
        type = "MaritimeThreat";
        synchronized(ObConst.DATE_FORMAT){
            mssg = type
                    + " "
                    + ObUtil.getDisplayString(ObUtil.getThreatTime(),
                            ObConst.DATE_FORMAT) + "\n" + threatMessage;
        }
        String key = "";
        key = "SAFESEAS";

        // TODO Send the message (mssg) to AlertViz f/k/a Guardian.
        // System.out.println(mssg + " " + "MONITOR" + " " + key + " " +
        // priority);
/*        model.getSafeSeasDisplay().putMessage(
                mssg + " " + "MONITOR:" + " " + key + " " + "RT1: "
                        + report[0].getReportType().toString() + " " + "STN1: "
                        + report[0].getPlatformId() + " " + "PRIORITY: "
                        + priority);
        model.getSafeSeasDisplay().putPriority(threatLevel);
*/
    }
}
