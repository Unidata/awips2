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

import com.raytheon.uf.common.dataplugin.ccfp.CcfpRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.avnconfig.ITafSiteConfig;
import com.raytheon.viz.avnconfig.TafSiteConfigFactory;
import com.raytheon.viz.avnconfig.TafSiteData;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

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
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class CcfpData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CcfpData.class);

    private static final SimpleDateFormat FORMAT = new SimpleDateFormat(
            "yyyyMMddHH00");

    private static final GeometryFactory geomFactory = new GeometryFactory();

    private static Map<String, List<String>> siteReportMap = new HashMap<String, List<String>>();

    static {
        FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

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
        time = time - 5 * 60 * 60 * 1000; // within last 5 hours
        CcfpRecord[] records = MonitorDataUtil.getCcfpData(time);
        Map<Date, List<CcfpRecord>> timeMap = new HashMap<Date, List<CcfpRecord>>();
        for (CcfpRecord cc : records) {
            Date issueTime = cc.getDataTime().getRefTime();
            List<CcfpRecord> list = timeMap.get(issueTime);
            if (list == null) {
                list = new ArrayList<CcfpRecord>();
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
                        String report = mkCCFPReport(site, area.getDataTime().getValidPeriod().getEnd(), area);
                        reports.add(report);
                    }
                    siteReportMap.put(site, reports);
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error updating ccfp data",
                    e);
        }

    }

    private static CcfpRecord findMostRelevantArea(Coordinate site,
            List<CcfpRecord> list) {
        CcfpRecord best = null;
        for (CcfpRecord cc : list) {
            if (cc.getLocation().getGeometry()
                    .contains(geomFactory.createPoint(site))) {
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

    public static List<String> getReports(String siteID) {
        List<String> reports = siteReportMap.get(siteID);
        if (reports == null) {
            reports = new ArrayList<String>();
            siteReportMap.put(siteID, reports);
        }
        return reports;
    }
}
