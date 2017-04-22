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
package com.raytheon.viz.hydro.pointdatacontrol.engine;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.hydro.pointdatacontrol.PDCConstants;
import com.raytheon.viz.hydro.pointdatacontrol.PointDataControlManager;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDBUtils;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.GageData;
import com.raytheon.viz.hydrocommon.data.GageData.ThreatIndex;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;

/**
 * Point Data Control Add Missing Reports
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2008            mpduff     Initial creation
 * Jul 28, 2016 4623       skorolev   Cleanup.
 * 
 * </pre>
 * 
 * @author mpduff
 */

public class PointDataControlAddMissing {
    private static String previousWhere = "";

    private static PointDataControlManager pdcManager = PointDataControlManager
            .getInstance();

    /**
     * Adds multiple missing reports to a linked list of report structures
     * containing observed/forecast data. This is done using repetitive calls to
     * the add_missing_report routine.
     * 
     * A list of unique Lids is loaded from the IngestFilter table based on the
     * PE and TS elements selected by the user in the Point Data Control GUI.
     * The list of observed/forecast reports is compared against this list of
     * unique lids to determine if reports for one or more lids are missing. If
     * there are any missing reports, then they are inserted into to the
     * observed/forecast data report list.
     * 
     * At the beginning of this routine, the observed/forecast report list
     * contains only reports with data. At the end of this routine, the list
     * contains both reports with data and reports with missing data.
     */
    public static List<GageData> addMissingReports(List<GageData> obsReportList) {
        if ((obsReportList == null) || (obsReportList.size() == 0)) {
            return obsReportList;
        }

        PDCOptionData pcOptions = PDCOptionData.getInstance();
        StringBuilder where = new StringBuilder();
        List<String> missingLids = new ArrayList<String>();

        /*
         * if the user requests that missing reports be shown, then get a list
         * of stations from ingest filter that match the requested physical
         * element, and if specified, the type-source code also. except for
         * retrievals of observed and forecast data, as occurs when retrieving
         * the latest river data, do not consider type-source entries for
         * forecast data.
         */

        /* build the where class for the IngestFilter list. */
        if (pcOptions.getPrimary() == 1) {
            where.append("where (pe like 'H%%' or pe like 'Q%%') ");
        } else if (pcOptions.getPcAndpp() == 1) {
            where.append("where pe in ('PC', 'PP') ");
        } else {
            where.append("where pe = '"
                    + pcOptions.getSelectedAdHocElementString() + "' ");
        }

        if (pcOptions.getFilterByTypeSource() == 1) {
            where.append(PDCDBUtils.buildTypeSourceWhereFilter());
        } else {
            if ((pcOptions.getElementType() == HydroConstants.AdHocDataElementType.RIVER_AD_HOC_TYPE
                    .getAdHocDataElementType())
                    && (pcOptions.getTimeMode() == PDCConstants.TimeModeType.LATEST
                            .getTimeMode())) {
                where.append(" and ts not like 'C%' ");
            } else {
                where.append(" and ts not like 'C%' and ts not like 'F%' ");
            }
        }

        where.append(" and ingest = 'T' order by 1 ");

        /*
         * only load the data anew if this request is different from the
         * previous one
         */
        previousWhere = where.toString();

        PDCDataManager dataManager = PDCDataManager.getInstance();
        List<Object[]> results = dataManager.getUnique("lid", "ingestfilter",
                where.toString());

        /* Create a HashMap for quick search of missing LID */
        Map<String, Object[]> ingestMap = new HashMap<String, Object[]>();
        for (int i = 0; i < results.size(); i++) {
            ingestMap.put((String) results.get(i)[0], results.get(i));
        }

        /*
         * loop on the unique stations in the ingestfilter filter and see if
         * there is a corresponding report already there. note that this loop on
         * ingest filter actually has a simultaneous loop on the report list
         * going on
         */
        Set<String> obsReportLidSet = new HashSet<String>();

        /* Create a set for searching */
        for (int i = 0; i < obsReportList.size(); i++) {
            obsReportLidSet.add(obsReportList.get(i).getLid());
        }

        Set<String> ingestLidSet = ingestMap.keySet();
        Iterator<String> it = ingestLidSet.iterator();
        while (it.hasNext()) {
            String lid = it.next();
            if (!obsReportLidSet.contains(lid)) {
                missingLids.add(lid);
            }
        }
        obsReportList = addMissingReport(missingLids, obsReportList);

        return obsReportList;
    }

    /**
     * Adds Missing Report
     * 
     * @param missingList
     * @param obsReportList
     * @return
     */
    private static List<GageData> addMissingReport(List<String> missingList,
            List<GageData> obsReportList) {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        for (String lid : missingList) {
            GageData report = new GageData();
            report.setLid(lid);

            if (pcOptions.getPrimary() == 1) {
                report.setPe("--");
            } else if (pcOptions.getPcAndpp() == 1) {
                report.setPe("P-");
            } else {
                report.setPe(pcOptions.getSelectedAdHocElementString());
            }
            report.setDur(0);
            report.setTs("R-");
            report.setExtremum("Z");
            report.setProbability(-1);
            report.setShefQualCode("Z");
            report.setQuality_code(PDCConstants.DEFAULT_QC_VALUE);
            report.setValue(PDCConstants.MISSING_VALUE);
            report.setValue2(PDCConstants.MISSING_VALUE);
            report.setValidtime(pcOptions.getValidTime());
            Date d = SimulatedTime.getSystemTime().getTime();
            d.setTime(0);
            report.setBasistime(d);
            report.setThreatIndex(ThreatIndex.THREAT_MISSING_DATA);

            obsReportList.add(report);
        }

        return obsReportList;
    }
}