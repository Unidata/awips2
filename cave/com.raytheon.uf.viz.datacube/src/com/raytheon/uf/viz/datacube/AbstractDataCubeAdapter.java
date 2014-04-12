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
package com.raytheon.uf.viz.datacube;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequestSet;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponseSet;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.derivparam.inv.AvailabilityContainer;
import com.raytheon.uf.common.derivparam.inv.MetadataContainer;
import com.raytheon.uf.common.inventory.TimeAndSpace;
import com.raytheon.uf.common.inventory.data.AbstractRequestableData;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;

/**
 * Abstract data cube adapter for standard data type that uses derived
 * parameters
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 30, 2012           mschenke    Initial creation
 * Feb 25, 2013  1659     bsteffen    Stop derived parameters from sending
 *                                    empty requests for cached times
 * Jan 30, 2014  2725     ekladstrup  Remove usage of ThriftClient
 * Apr 11, 2014  2947     bsteffen    Fix binoffset time queries and getRecord.
 * 
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractDataCubeAdapter extends DefaultDataCubeAdapter {

    private String[] supportedPlugins;

    protected AbstractDataCubeAdapter(String[] supportedPlugins) {
        super(null);
        this.supportedPlugins = supportedPlugins;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getSupportedPlugins
     * ()
     */
    @Override
    public String[] getSupportedPlugins() {
        return supportedPlugins;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#timeQuery(java
     * .util.List)
     */
    @Override
    public List<List<DataTime>> timeQuery(List<TimeQueryRequest> requests)
            throws DataCubeException {
        List<AvailabilityContainer> containers = new ArrayList<AvailabilityContainer>(
                requests.size());
        List<List<DbQueryRequest>> requestLists = new ArrayList<List<DbQueryRequest>>(
                requests.size());
        List<DbQueryRequest> fullList = new ArrayList<DbQueryRequest>(
                requests.size());
        for (TimeQueryRequest request : requests) {
            AvailabilityContainer container = createAvailabilityContainer(request
                    .getQueryTerms());
            List<AbstractRequestableNode> requestNodes = evaluateRequestConstraints(request
                    .getQueryTerms());
            // pull out time queries and bulk submit
            for (AbstractRequestableNode requestNode : requestNodes) {
                container.prepareRequests(requestNode);
            }
            containers.add(container);
            List<DbQueryRequest> containerRequests = container
                    .getAvailabilityRequests();
            requestLists.add(containerRequests);
            fullList.addAll(containerRequests);
        }

        // bulk up all the requests.
        DbQueryResponse[] responses = new DbQueryResponse[0];
        if (!fullList.isEmpty()) {
            DbQueryRequestSet requestSet = new DbQueryRequestSet();
            requestSet.setQueries(fullList.toArray(new DbQueryRequest[0]));
            DbQueryResponseSet responseSet;
            try {
                responseSet = (DbQueryResponseSet) RequestRouter
                        .route(requestSet);
            } catch (Exception e) {
                throw new DataCubeException(e);
            }
            responses = responseSet.getResults();
        }
        int responseIndex = 0;
        List<List<DataTime>> finalResponse = new ArrayList<List<DataTime>>(
                requests.size());
        for (int i = 0; i < requests.size(); i += 1) {
            TimeQueryRequest request = requests.get(i);
            AvailabilityContainer container = containers.get(i);
            // set the bulked responses back into the container
            List<DbQueryRequest> containerRequests = requestLists.get(i);
            Map<DbQueryRequest, DbQueryResponse> responseMap = new HashMap<DbQueryRequest, DbQueryResponse>(
                    (int) (containerRequests.size() / 0.75) + 1, 0.75f);
            for (int j = 0; j < containerRequests.size(); j += 1) {
                responseMap.put(containerRequests.get(j),
                        responses[responseIndex++]);
            }
            container.setAvailabilityResponses(responseMap);
            List<AbstractRequestableNode> requestNodes = evaluateRequestConstraints(request
                    .getQueryTerms());
            // pull the actual results from the cache
            Set<DataTime> results = new HashSet<DataTime>(64);
            for (AbstractRequestableNode requestNode : requestNodes) {
                Set<TimeAndSpace> avialability = container
                        .getAvailability(requestNode);
                for (TimeAndSpace ast : avialability) {
                    if (ast.isTimeAgnostic()) {
                        List<DataTime> temp = timeAgnosticQuery(request
                                .getQueryTerms());
                        if (temp != null) {
                            results.addAll(temp);
                        }
                        break;
                    } else {
                        results.add(ast.getTime());
                    }
                }
            }
            if (request.getBinOffset() != null) {
                BinOffset bin = request.getBinOffset();
                results = bin.getNormalizedTimes(results
                        .toArray(new DataTime[0]));
            }
            if (!request.isMaxQuery() || results.isEmpty()) {
                finalResponse.add(new ArrayList<DataTime>(results));
            } else {
                ArrayList<DataTime> response = new ArrayList<DataTime>(results);
                Collections.sort(response);
                finalResponse
                        .add(Arrays.asList(response.get(response.size() - 1)));
            }
        }
        return finalResponse;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getData(java.
     * util.Map, com.raytheon.uf.common.time.DataTime[])
     */
    @Override
    public PluginDataObject[] getData(
            Map<String, RequestConstraint> constraints, DataTime[] selectedTimes)
            throws DataCubeException {
        List<AbstractRequestableNode> requests = evaluateRequestConstraints(new HashMap<String, RequestConstraint>(
                constraints));
        Set<TimeAndSpace> availability = null;
        if (selectedTimes != null) {
            availability = new HashSet<TimeAndSpace>();
            for (DataTime time : selectedTimes) {
                availability.add(new TimeAndSpace(time));
            }
        } else {
            availability = AvailabilityContainer.AGNOSTIC_SET;
        }

        // pull the actual results from the cache
        List<AbstractRequestableData> requesters = new ArrayList<AbstractRequestableData>();
        MetadataContainer container = createMetadataContainer(new HashMap<String, RequestConstraint>(
                constraints));
        for (AbstractRequestableNode request : requests) {
            container.prepareRequests(request, availability);
        }
        for (AbstractRequestableNode request : requests) {
            requesters.addAll(container.getData(request, availability));
        }

        return getData(constraints, selectedTimes, requesters).toArray(
                new PluginDataObject[0]);
    }

    protected MetadataContainer createMetadataContainer(
            Map<String, RequestConstraint> constraints) {
        return new MetadataContainer(constraints,
                createAvailabilityContainer(constraints));
    }

    protected AvailabilityContainer createAvailabilityContainer(
            Map<String, RequestConstraint> constraints) {
        return new AvailabilityContainer(constraints);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getRecord(com
     * .raytheon.uf.common.dataplugin.PluginDataObject)
     */
    @Override
    public IDataRecord[] getRecord(PluginDataObject obj)
            throws DataCubeException {
        return getRecord(obj, Request.ALL, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getRecord(com
     * .raytheon.uf.common.dataplugin.PluginDataObject,
     * com.raytheon.uf.common.datastorage.Request, java.lang.String)
     */
    @Override
    public IDataRecord[] getRecord(PluginDataObject obj, Request req,
            String dataset) throws DataCubeException {
        getRecords(Arrays.asList(obj), req, dataset);
        IDataRecord[] result = null;
        Object message = obj.getMessageData();
        if (message instanceof IDataRecord[]) {
            result = (IDataRecord[]) message;
        } else if (message instanceof IDataRecord) {
            result = new IDataRecord[] { (IDataRecord) message };
        }
        obj.setMessageData(null);
        return result;
    }

    /**
     * Scan the inventory for AbstractRequestableLevelNodes that match the
     * request constraints
     * 
     * @param constraints
     * @return
     */
    protected abstract List<AbstractRequestableNode> evaluateRequestConstraints(
            Map<String, RequestConstraint> constraints);

    /**
     * @param queryTerms
     * @return
     */
    protected abstract List<DataTime> timeAgnosticQuery(
            Map<String, RequestConstraint> queryTerms) throws DataCubeException;

    /**
     * @param requesters
     * @return
     */
    protected abstract List<PluginDataObject> getData(
            Map<String, RequestConstraint> constraints,
            DataTime[] selectedTimes, List<AbstractRequestableData> requesters)
            throws DataCubeException;
}
