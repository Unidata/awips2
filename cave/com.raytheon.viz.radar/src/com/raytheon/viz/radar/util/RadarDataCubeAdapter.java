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
package com.raytheon.viz.radar.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequestSet;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponseSet;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterGenerator;
import com.raytheon.viz.pointdata.util.AbstractPointDataInventory;
import com.raytheon.viz.pointdata.util.PointDataCubeAdapter;

/**
 * 
 * DataCubeAdapter for Radar Data. Passes the work for point data to radar point
 * data adapters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2009             bsteffen    Initial creation
 * Nov 21, 2009 #3576      rjpeter     Refactored use of DerivParamDesc.
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RadarDataCubeAdapter extends PointDataCubeAdapter {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarDataCubeAdapter.class);

    private static final String DATA_TIME_FIELD = "dataTime";

    private static final String LATEST_DATA_TIME_FIELD = "dataTime.refTime";

    private static final String LEVEL_FIELD = "primaryElevationAngle";

    @Override
    public String[] getSupportedPlugins() {
        return new String[] { "radar" };
    }

    @Override
    public void initInventory() {
        if (inventory == null) {
            AbstractPointDataInventory pointInventory = new VwpInventory();
            try {
                pointInventory.initTree(DerivedParameterGenerator
                        .getDerParLibrary());
                this.inventory = pointInventory;
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /**
     * @param queryParams
     * @return
     * @throws VizException
     */
    @Override
    public String getType(Map<String, RequestConstraint> queryParams)
            throws VizException {
        String type = super.getType(queryParams);
        if (VwpInventory.ProductCode.toString().equals(type)) {
            return VwpInventory.Mnemonic;
        }
        return type;
    }

    @Override
    public PointDataContainer getBaseRecords(
            Collection<String> baseParameters,
            Map<String, RequestConstraint> queryParams) throws VizException {
        return ((VwpInventory) inventory).getBaseRecords(baseParameters,
                queryParams);
    }

    private Collection<DataTime> processTimeQueryResponse(
            DbQueryResponse response, boolean latestOnly, BinOffset binOffset) {
        String dataTimefield = DATA_TIME_FIELD;
        if (latestOnly) {
            dataTimefield = LATEST_DATA_TIME_FIELD;
        }
        Collection<DataTime> results = new HashSet<DataTime>();
        int i = 0;
        for (Map<String, Object> map : response.getResults()) {
            DataTime time = null;
            if (latestOnly) {
                time = new DataTime((Date) map.get(dataTimefield), 0);
            } else {
                time = (DataTime) map.get(dataTimefield);
                time.setLevelValue((Double) map.get(LEVEL_FIELD));
            }
            // Best res requests need this because they span a time period
            if (time.getRefTime().before(
                    SimulatedTime.getSystemTime().getTime())) {
                results.add(time);
                ++i;
            }
        }

        if (binOffset != null) {
            Set<DataTime> scaledDates = new TreeSet<DataTime>();
            for (DataTime dt : results) {
                scaledDates.add(binOffset.getNormalizedTime(dt));
            }
            results = scaledDates;
        }

        return results;
    }

    private DbQueryRequest getTimeQueryRequest(
            Map<String, RequestConstraint> queryParams, boolean latestOnly) {
        DbQueryRequest request = new DbQueryRequest();
        request.setConstraints(queryParams);

        String dataTimefield = DATA_TIME_FIELD;
        if (latestOnly) {
            dataTimefield = LATEST_DATA_TIME_FIELD;
        }
        request.addRequestField(dataTimefield, latestOnly);
        if (!latestOnly) {
            request.addRequestField(LEVEL_FIELD);
        }
        request.setDistinct(true);
        return request;
    }

    @Override
    public List<List<DataTime>> timeQuery(List<TimeQueryRequest> requests)
            throws VizException {
        List<DbQueryRequest> dbRequests = new ArrayList<DbQueryRequest>(
                requests.size());
        for (TimeQueryRequest request : requests) {
            dbRequests.add(getTimeQueryRequest(request.getQueryTerms(),
                    request.isMaxQuery()));
        }
        DbQueryRequestSet requestSet = new DbQueryRequestSet();
        requestSet.setQueries(dbRequests.toArray(new DbQueryRequest[0]));
        DbQueryResponseSet responseSet = (DbQueryResponseSet) ThriftClient
                .sendRequest(requestSet);
        List<List<DataTime>> result = new ArrayList<List<DataTime>>(
                requests.size());
        for (int i = 0; i < requests.size(); i++) {
            DbQueryResponse response = responseSet.getResults()[i];
            TimeQueryRequest request = requests.get(i);
            Collection<DataTime> times = processTimeQueryResponse(response,
                    request.isMaxQuery(), request.getBinOffset());

            result.add(new ArrayList<DataTime>(times));
        }
        return result;
    }
}
