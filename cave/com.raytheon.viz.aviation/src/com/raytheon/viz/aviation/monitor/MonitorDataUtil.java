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
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.edex.plugin.ccfp.CcfpRecord;
import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingRecord;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.CatalogQuery;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.comm.Loader;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Utility functions for data requesting
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class MonitorDataUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MonitorDataUtil.class);

    private static final SimpleDateFormat SDF = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss.S");

    static {
        SDF.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Get the lightning data newer than a particular time
     * 
     * @param time
     * @return
     */
    public static BinLightningRecord[] getLightningData(long time) {
        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        map.put("pluginName", new RequestConstraint("binlightning"));
        map.put("startTime", new RequestConstraint(SDF.format(new Date(time)),
                RequestConstraint.ConstraintType.GREATER_THAN_EQUALS));
        LayerProperty lp = new LayerProperty();
        lp.setNumberOfImages(999);
        try {
            lp.setEntryQueryParameters(map);
            List<Object> objs = Loader.loadData(lp, "select", 10000);
            BinLightningRecord[] records = new BinLightningRecord[objs.size()];
            for (int i = 0; i < records.length; i++) {
                records[i] = (BinLightningRecord) objs.get(i);
            }
            return records;
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving lightning data", e);
        }
        return new BinLightningRecord[0];
    }

    public static CcfpRecord[] getCcfpData(long time) {
        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        map.put("pluginName", new RequestConstraint("ccfp"));
        map.put("dataTime.refTime",
                new RequestConstraint(SDF.format(new Date(time)),
                        RequestConstraint.ConstraintType.GREATER_THAN_EQUALS));
        try {
            String[] catalog = CatalogQuery.performQuery("dataTime", map);
            String[] times = catalog;
            DataTime[] dts = null;
            Arrays.sort(catalog);
            if (catalog.length > 3) {
                dts = new DataTime[3];
                dts[0] = new DataTime(catalog[catalog.length - 1]);
                dts[1] = new DataTime(catalog[catalog.length - 2]);
                dts[2] = new DataTime(catalog[catalog.length - 3]);
            } else {
                dts = new DataTime[times.length];
                for (int i = 0; i < catalog.length; i++) {
                    dts[i] = new DataTime(catalog[catalog.length - i - 1]);
                }
            }
            map.remove("dataTime.refTime");
            LayerProperty lp = new LayerProperty();
            lp.setNumberOfImages(999);
            lp.setSelectedEntryTimes(dts);

            lp.setEntryQueryParameters(map);
            List<Object> objs = Loader.loadData(lp, "select", 10000);
            CcfpRecord[] records = new CcfpRecord[objs.size()];
            for (int i = 0; i < records.length; i++) {
                records[i] = (CcfpRecord) objs.get(i);
            }
            return records;
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving ccfp data", e);
        }

        return new CcfpRecord[0];
    }

    public static RadarRecord[] getVerticalWindProfile(String radar, long time) {
        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        map.put("pluginName", new RequestConstraint("radar"));
        map.put("dataTime.refTime",
                new RequestConstraint(SDF.format(new Date(time)),
                        RequestConstraint.ConstraintType.GREATER_THAN_EQUALS));
        map.put("icao", new RequestConstraint(radar.toLowerCase()));
        map.put("mnemonic", new RequestConstraint("VWP"));

        LayerProperty lp = new LayerProperty();
        try {
            lp.setEntryQueryParameters(map);
            DataTime[] dt = lp.getEntryTimes();
            if (dt.length > 0) {
                lp.setSelectedEntryTimes(new DataTime[] { dt[dt.length - 1] });
            }

            List<Object> objs = Loader.loadData(lp, "select", 10000);

            RadarRecord[] records = new RadarRecord[objs.size()];
            for (int i = 0; i < records.length; i++) {
                records[i] = (RadarRecord) objs.get(i);
            }
            return records;
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return null;
    }

    public static ACARSSoundingRecord[] getAcarsSoundingRecords(
            String stationId, long time) {
        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        map.put("pluginName", new RequestConstraint("acarssounding"));
        map.put("dataTime.refTime",
                new RequestConstraint(SDF.format(new Date(time)),
                        RequestConstraint.ConstraintType.GREATER_THAN_EQUALS));
        map.put("location.stationId",
                new RequestConstraint(stationId.substring(1)));

        LayerProperty lp = new LayerProperty();
        try {
            lp.setEntryQueryParameters(map);
            DataTime[] dt = lp.getEntryTimes();
            if (dt.length > 0) {
                lp.setSelectedEntryTimes(new DataTime[] { dt[dt.length - 1] });
            }

            List<Object> objs = Loader.loadData(lp, "select", 10000);

            ACARSSoundingRecord[] records = new ACARSSoundingRecord[objs.size()];
            for (int i = 0; i < records.length; i++) {
                records[i] = (ACARSSoundingRecord) objs.get(i);
            }
            return records;
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return null;
    }
}
