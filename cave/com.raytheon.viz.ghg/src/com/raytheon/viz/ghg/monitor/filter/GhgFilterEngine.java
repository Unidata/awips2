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
package com.raytheon.viz.ghg.monitor.filter;

import java.util.Arrays;
import java.util.Calendar;
import java.util.List;

import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.ghg.monitor.data.GhgAlertCheckData;
import com.raytheon.viz.ghg.monitor.data.GhgAlertData;
import com.raytheon.viz.ghg.monitor.data.GhgAlertsConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.AlertsEnum;
import com.raytheon.viz.ghg.monitor.data.GhgConfigData.SelectionEnum;
import com.raytheon.viz.ghg.monitor.data.GhgData;
import com.raytheon.viz.ghg.monitor.data.GhgDataFilter;

/**
 * GHG Monitor filtering engine.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 26May2010               mpduff      Initial creation.
 * 11Apr2014    15769      ryu         Promote delta minutes if within a few seconds.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GhgFilterEngine {
    /** Milliseconds per minute */
    private static final int MILLIS_PER_MINUTE = 60 * 1000;

    /**
     * Compare this record to the filter and see if it should be included or
     * not.
     * 
     * @param gd
     *            The GhgData object to check
     * @return true if included, false if filtered out
     */
    public static boolean filterCheck(GhgData gd) {
        boolean include = true;

        GhgConfigData config = GhgConfigData.getInstance();
        GhgDataFilter filter = config.getCurrentFilter();

        // Initial checks for "Include Alerts"
        // return true if record is in "Alert Situation" regardless of filter
        // settings
        boolean includeAlerts = filter.includeAlerts;
        if (includeAlerts
                && (GhgFilterEngine.alertCheck(gd).getAlertType() != AlertsEnum.NoAlerts)) {
            return true;
        }

        // Technically, only the map selection is supposed to get past this
        // filter. However, to view a specific row in the text tab, users click
        // on it, making it the monitor selection. If it's filtered out by that,
        // then the user can't view its text and gets frustrated.
        if (filter.includeMapSelections
                && (SelectionEnum.NoSelection != gd.getSelection())) {
            return true;
        }

        if (filter.includePastEvents == false) {
            if (gd.getEndDate().getTime() < SimulatedTime.getSystemTime()
                    .getTime().getTime()) {
                return false;
            }
        }

        // Check the list values

        // combined actions may be like "CON,EXA".
        // Accept if:
        // (1) filter.actions is empty
        // (2) any filter action matches any sub-action.
        if (filter.actions.length != 0) {
            boolean actionFound = false;
            String[] gdActions = gd.getAction().split(",");
            for (String filterAction : filter.actions) {
                if (Arrays.binarySearch(gdActions, filterAction) >= 0) {
                    actionFound = true;
                    break;
                }
            }
            if (!actionFound) {
                return false;
            }
        }

        String phenSig = gd.getPhenSig();
        if ((filter.phenSigs.length != 0)
                && !Arrays.asList(filter.phenSigs).contains(phenSig)) {
            return false;
        }

        String pil = gd.getPil();
        if ((filter.pils.length != 0)
                && !Arrays.asList(filter.pils).contains(pil)) {
            return false;
        }

        String wfo = gd.getWfo();
        if ((filter.wfos.length != 0)
                && !Arrays.asList(filter.wfos).contains(wfo)) {
            return false;
        }

        String geoId = gd.getGeoId();
        if ((filter.geoids.length != 0)
                && !Arrays.asList(filter.geoids).contains(geoId)) {
            return false;
        }

        String etn = gd.getEtn();
        if ((filter.etns.length != 0)
                && !Arrays.asList(filter.etns).contains(etn)) {
            return false;
        }

        String seg = gd.getSegNum();
        if ((filter.segs.length != 0)
                && !Arrays.asList(filter.segs).contains(seg)) {
            return false;
        }

        return include;
    }

    public static GhgAlertCheckData alertCheck(GhgData gd) {
        String wfo = DataManager.getCurrentInstance().getSiteID();
        wfo = SiteMap.getInstance().getSite4LetterId(wfo);
        GhgConfigData config = GhgConfigData.getInstance();
        GhgAlertsConfigData alertFilter = config.getAlerts();

        // Alerts for my WFO
        if (alertFilter.isLocal() && !gd.getWfo().equals(wfo)) {
            return new GhgAlertCheckData(AlertsEnum.NoAlerts);
        }

        // Alerts for Test products
        boolean alertTestProducts = alertFilter.isTest();
        if (alertTestProducts == false) {
            // Check the VTECstr for a "T"
            if ((gd.getVtecString().length() > 1)
                    && (gd.getVtecString().indexOf("T") == 1)) {
                return new GhgAlertCheckData(AlertsEnum.NoAlerts);
            }
        }

        // pil check
        String[] pils = alertFilter.getPils();
        if ((pils != null)
                && ((pils.length != 0) && !Arrays.asList(pils).contains(
                        gd.getPil()))) {
            return new GhgAlertCheckData(AlertsEnum.NoAlerts);
        }

        // Action check
        List<String> filterActionList = Arrays.asList(alertFilter.getActions());
        boolean found = false;
        String[] recordActions = gd.getAction().split(",");
        if (filterActionList != null) {
            if (filterActionList.size() != 0) {
                for (String action : recordActions) {

                    if (filterActionList.contains(action)) {
                        found = true;
                        break;
                    }
                }
            } else {
                found = true;
            }

            if (found == false) {
                return new GhgAlertCheckData(AlertsEnum.NoAlerts);
            }
        }

        // phensig check
        List<String> phenSigList = Arrays.asList(alertFilter.getPhenSigs());
        if ((phenSigList.size() != 0) && !phenSigList.contains(gd.getPhenSig())) {
            return new GhgAlertCheckData(AlertsEnum.NoAlerts);
        }

        // timing until purge time
        long now = SimulatedTime.getSystemTime().getTime().getTime();

        // minutes until purge time
        int margin = 4999; // promote the deltas if within 5 seconds
        int deltaP = (int) ((gd.getPurgeDate().getTime() - now + margin) / MILLIS_PER_MINUTE);

        // minutes until end time
        int deltaE = (int) ((gd.getEndDate().getTime() - now + margin) / MILLIS_PER_MINUTE);

        long earlierT = Math.min(gd.getPurgeDate().getTime(), gd.getEndDate()
                .getTime());

        if (deltaE < -30) {
            // After purge time, after event ending time (+30 minutes)
            return new GhgAlertCheckData(AlertsEnum.NoAlerts);
        }

        // no time limit for Expired alerts
        GhgAlertData expiredAlert = config.getAlerts().getAlert(
                AlertsEnum.ExpiredAlert);
        int tval = expiredAlert.getTime();
        long triggerTime = earlierT - (tval * MILLIS_PER_MINUTE);

        if (expiredAlert.isEnabled() && (now >= triggerTime)) {
            return new GhgAlertCheckData(AlertsEnum.ExpiredAlert, deltaP,
                    deltaE);
        }

        // time limit for non-Expired alerts
        GhgAlertData alert2 = config.getAlerts().getAlert(AlertsEnum.AlertLvl2);
        tval = alert2.getTime();
        triggerTime = earlierT - (tval * MILLIS_PER_MINUTE);

        if (alert2.isEnabled() && (now >= triggerTime)) {
            return new GhgAlertCheckData(AlertsEnum.AlertLvl2, deltaP, deltaE);
        }

        GhgAlertData alert1 = config.getAlerts().getAlert(AlertsEnum.AlertLvl1);
        tval = alert1.getTime();
        triggerTime = earlierT - (tval * MILLIS_PER_MINUTE);

        if (alert1.isEnabled() && (now >= triggerTime)) {
            return new GhgAlertCheckData(AlertsEnum.AlertLvl1, deltaP, deltaE);
        }

        return new GhgAlertCheckData(AlertsEnum.NoAlerts);
    }
}
