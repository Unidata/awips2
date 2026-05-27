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
package com.raytheon.viz.aviation.monitor;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import com.raytheon.uf.common.aviation.avnconfig.ITafSiteConfig;
import com.raytheon.uf.common.aviation.avnconfig.TafSiteConfigFactory;
import com.raytheon.uf.common.aviation.avnconfig.TafSiteData;
import com.raytheon.uf.common.dataplugin.ccfp.CcfpRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;

/**
 * Holds the current state of CCFP data related to a set of sites
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2009            njensen     Initial creation
 * Jun  3, 2014 16289      zhao        Use "end time" instead of "start time" for CCFP report
 * Feb 16, 2016 4935       mapeters    Account for CcfpRecords with null geometry
 * Feb 09, 2018 6763       njensen     Use empty string when there are records without geometries
 *                                     to signify there was data but no convection
 * Feb 13, 2018 6860       njensen     Fix date format to have two digit years
 * May 15, 2019 20693   mgamazaychikov ITafSiteConfig, TafSiteConfigFactory, TafSiteData refactor
 * 
 * 
 * </pre>
 * 
 * @author njensen
 */

public class CcfpData {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CcfpData.class);

    private static final SimpleDateFormat FORMAT = new SimpleDateFormat(
            "yyMMddHH00");

    private static final GeometryFactory geomFactory = new GeometryFactory();

    private static Map<String, List<String>> siteReportMap = new HashMap<>();

    static {
        FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Ported from TextThreadP.py _mkCCFPReport()
     * 
     * @param ident
     * @param vtime
     * @param area
     * @return
     */
    private static String mkCCFPReport(String ident, Date vtime, CcfpRecord area) {
        StringBuilder sb = new StringBuilder();
        sb.append(ident);
        sb.append(" ");
        sb.append(FORMAT.format(vtime));
        sb.append(" TOPS ");
        sb.append(area.getTops());
        sb.append(" GWTH ");
        sb.append(area.getGrowth());
        sb.append(" CONF ");
        sb.append(area.getConf());
        sb.append(" CVRG ");
        sb.append(area.getCoverage());

        return sb.toString();
    }

    public static synchronized void checkData() {
        Set<String> sites = siteReportMap.keySet();
        long time = SimulatedTime.getSystemTime().getTime().getTime();
        // within last 5 hours
        time = time - (5 * TimeUtil.MILLIS_PER_HOUR);
        CcfpRecord[] records = MonitorDataUtil.getCcfpData(time);
        Map<Date, List<CcfpRecord>> timeMap = new HashMap<>();
        for (CcfpRecord cc : records) {
            Date issueTime = cc.getDataTime().getRefTime();
            List<CcfpRecord> list = timeMap.get(issueTime);
            if (list == null) {
                list = new ArrayList<>();
            }
            list.add(cc);
            timeMap.put(issueTime, list);
        }

        Date[] times = timeMap.keySet().toArray(new Date[timeMap.size()]);
        Arrays.sort(times);
        try {
            ITafSiteConfig config = TafSiteConfigFactory.getInstance();
            for (String site : sites) {
                // clear it out and update with the latest
                siteReportMap.get(site).clear();
                TafSiteData tsd = config.getSite(site);
                Coordinate c = new Coordinate(Double.valueOf(tsd.longitude),
                        Double.valueOf(tsd.latitude));
                for (Date dt : times) {
                    List<CcfpRecord> list = timeMap.get(dt);
                    CcfpRecord area = findMostRelevantArea(c, list);
                    List<String> reports = siteReportMap.get(site);
                    if (area != null) {
                        // use valid time, i.e. the end of the valid period
                        String report = mkCCFPReport(site,
                                area.getDataTime().getValidPeriod().getEnd(),
                                area);
                        reports.add(report);
                    } else {
                        /*
                         * The Python code will recognize this as there was no
                         * convection over the area. If reports is instead an
                         * empty list (by having no times) it will be recognized
                         * as missing data.
                         */
                        reports.add("");
                    }
                    siteReportMap.put(site, reports);
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error updating ccfp data",
                    e);
        }

    }

    /**
     * Finds the CCFPRecord where the site fits within the record's polygons (if
     * any). If more than two polygons cover the area, finds the one with the
     * highest confidence, and if they have the same confidence, then finds the
     * one with more coverage.
     * 
     * @param site
     * @param list
     * @return
     */
    private static CcfpRecord findMostRelevantArea(Coordinate site,
            List<CcfpRecord> list) {
        CcfpRecord best = null;
        for (CcfpRecord cc : list) {
            Geometry geom = cc.getLocation().getGeometry();
            if (geom != null && geom.contains(geomFactory.createPoint(site))) {
                if ((best == null)
                        || (cc.getConf() > best.getConf())
                        || (cc.getConf() == best.getConf() && cc.getCoverage() > best
                                .getCoverage())) {
                    best = cc;
                }
            }
        }

        return best;
    }

    /**
     * Called from CCFPData.py to retrieve the reports for a site in a String
     * 
     * @param siteID
     * @return
     */
    public static List<String> getReports(String siteID) {
        List<String> reports = siteReportMap.get(siteID);
        if (reports == null) {
            reports = new ArrayList<>();
            siteReportMap.put(siteID, reports);
        }
        return reports;
    }

}
