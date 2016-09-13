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
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.acarssounding.ACARSSoundingRecord;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.ccfp.CcfpRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest.OrderMode;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTimeComparator;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Utility functions for data requesting.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 10, 2009            njensen     Initial creation
 * Apr 10, 2013 1735       rferrel     Convert to ThinClient and DbQueryRequests.
 * Sep 16, 2015 4880       njensen     Optimized requests for data
 * Dec 01, 2015 5156       rferrel     {@linkplain #getCcfpData(long)} do not shrink dtList 
 *                                      when it contains 3 or fewer elements.
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
     * @return records
     */
    public static BinLightningRecord[] getLightningData(long time) {
        DbQueryRequest request = new DbQueryRequest();
        request.setEntityClass(BinLightningRecord.class);

        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        map.put("pluginName", new RequestConstraint("binlightning"));
        map.put("startTime", new RequestConstraint(SDF.format(new Date(time)),
                RequestConstraint.ConstraintType.GREATER_THAN_EQUALS));
        request.setConstraints(map);
        request.setLimit(999);

        try {
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            BinLightningRecord[] records = response
                    .getEntityObjects(BinLightningRecord.class);
            return records;
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving lightning data", e);
        }
        return new BinLightningRecord[0];
    }

    /**
     * Obtain ccfp records greater then or equal to time.
     * 
     * @param time
     * @return records
     */
    @SuppressWarnings("unchecked")
    public static CcfpRecord[] getCcfpData(long time) {
        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        map.put("dataTime.refTime",
                new RequestConstraint(SDF.format(new Date(time)),
                        RequestConstraint.ConstraintType.GREATER_THAN_EQUALS));

        /*
         * CCFPs are issued on a periodic basis and for each issue time there
         * can be multiple CCFPs covering different areas. Following the
         * original AvnFPS implementation, we need to get all the CCFPs of the
         * three most recent periods.
         */
        try {
            TimeQueryRequest tqRequest = new TimeQueryRequest();
            tqRequest.setPluginName("ccfp");
            tqRequest.setQueryTerms(map);
            List<DataTime> dtList = (List<DataTime>) ThriftClient
                    .sendRequest(tqRequest);

            // don't bother requesting data if there's no times
            if (dtList.isEmpty()) {
                return new CcfpRecord[0];
            }

            // filter so no more then the three most recent periods are included
            Collections.sort(dtList,
                    Collections.reverseOrder(new DataTimeComparator()));
            if (dtList.size() > 3) {
                dtList = dtList.subList(0, 3);
            }
            String[] dts = new String[dtList.size()];
            for (int index = 0; index < dts.length; ++index) {
                dts[index] = dtList.get(index).toString();
            }

            map.put("pluginName", new RequestConstraint("ccfp"));
            map.remove("dataTime.refTime");
            DbQueryRequest request = new DbQueryRequest();
            request.setEntityClass(CcfpRecord.class);
            request.setLimit(999);
            request.setConstraints(map);
            RequestConstraint dataTimeRC = new RequestConstraint();
            dataTimeRC.setConstraintType(ConstraintType.IN);
            dataTimeRC.setConstraintValueList(dts);
            request.addConstraint("dataTime", dataTimeRC);
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            CcfpRecord[] records = response.getEntityObjects(CcfpRecord.class);
            return records;
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error retrieving ccfp data", e);
        }

        return new CcfpRecord[0];
    }

    /**
     * Get the most recent radar vertical wind profile.
     * 
     * @param radar
     * @param time
     * @return records
     */
    public static RadarRecord[] getVerticalWindProfile(String radar, long time) {
        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        map.put("pluginName", new RequestConstraint("radar"));
        map.put("dataTime.refTime",
                new RequestConstraint(SDF.format(new Date(time)),
                        RequestConstraint.ConstraintType.GREATER_THAN_EQUALS));
        map.put("icao", new RequestConstraint(radar.toLowerCase()));
        map.put("mnemonic", new RequestConstraint("VWP"));

        try {
            DbQueryRequest request = new DbQueryRequest();
            request.setEntityClass(RadarRecord.class);
            request.setOrderByField("dataTime.refTime", OrderMode.DESC);
            request.setLimit(1);
            request.setConstraints(map);

            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            RadarRecord[] records = response
                    .getEntityObjects(RadarRecord.class);
            return records;
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR,
                    "Error requesting radar vertical wind profile data", e);
        }
        return null;
    }

    /**
     * Get ACARS most recent sounding record for a station that is greater then
     * or equal to time.
     * 
     * @param stationId
     * @param time
     * @return records
     */
    public static ACARSSoundingRecord[] getAcarsSoundingRecords(
            String stationId, long time) {
        Map<String, RequestConstraint> map = new HashMap<String, RequestConstraint>();
        map.put("pluginName", new RequestConstraint("acarssounding"));
        map.put("dataTime.refTime",
                new RequestConstraint(SDF.format(new Date(time)),
                        RequestConstraint.ConstraintType.GREATER_THAN_EQUALS));
        map.put("location.stationId",
                new RequestConstraint(stationId.substring(1)));

        try {
            DbQueryRequest request = new DbQueryRequest();
            request.setEntityClass(ACARSSoundingRecord.class);
            request.setLimit(1);
            request.setOrderByField("dataTime.refTime", OrderMode.DESC);
            request.setConstraints(map);
            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            ACARSSoundingRecord[] records = response
                    .getEntityObjects(ACARSSoundingRecord.class);
            return records;
        } catch (VizException e) {
            statusHandler.handle(Priority.ERROR,
                    "Error requesting acarssounding data", e);
        }
        return null;
    }
}
