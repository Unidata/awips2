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
package com.raytheon.uf.viz.core.datastructure;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequestSet;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Default implementation of IDataCubeAdapter, function implementations were
 * moved from DataCubeContainer into here
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec  7, 2011            mschenke    Initial creation
 * Sep  9, 2013       2277 mschenke    Got rid of ScriptCreator references
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DefaultDataCubeAdapter implements IDataCubeAdapter {

    private String pluginName;

    public DefaultDataCubeAdapter(String pluginName) {
        this.pluginName = pluginName;
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
        return new String[] { pluginName };
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
            throws VizException {
        TimeQueryRequestSet set = new TimeQueryRequestSet();
        set.setRequests(requests.toArray(new TimeQueryRequest[0]));

        @SuppressWarnings("unchecked")
        List<List<DataTime>> result = (List<List<DataTime>>) ThriftClient
                .sendRequest(set);
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getPoints(java
     * .lang.String, java.lang.String[], java.util.Map)
     */
    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            Map<String, RequestConstraint> queryParams) throws VizException {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getPoints(java
     * .lang.String, java.lang.String[], java.lang.String, java.util.Map)
     */
    @Override
    public PointDataContainer getPoints(String plugin, String[] parameters,
            String levelKey, Map<String, RequestConstraint> queryParams)
            throws VizException {
        return null;
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
            throws VizDataCubeException {
        IDataRecord record = null;
        try {
            record = CubeUtil.retrieveData(obj, pluginName);
        } catch (VizException e) {
            throw new VizDataCubeException("Error retrieving 2D data record.",
                    e);
        }
        return new IDataRecord[] { record };
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
            String dataset) throws VizDataCubeException {
        IDataRecord record = null;
        try {
            record = CubeUtil.retrieveData(obj, pluginName, req, dataset);
        } catch (VizException e) {
            throw new VizDataCubeException("Error retrieving 2D data record.",
                    e);
        }
        return new IDataRecord[] { record };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getRecords(java
     * .util.List, com.raytheon.uf.common.datastorage.Request, java.lang.String)
     */
    @Override
    public void getRecords(List<PluginDataObject> objs, Request req,
            String dataset) throws VizDataCubeException {
        for (PluginDataObject obj : objs) {
            IDataRecord record = null;
            try {
                record = CubeUtil.retrieveData(obj, pluginName, req, dataset);
            } catch (VizException e) {
                throw new VizDataCubeException(
                        "Error retrieving 2D grid record.", e);
            }
            obj.setMessageData(record);
        }
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
            throws VizException {
        DbQueryRequest request = new DbQueryRequest(constraints);
        if (selectedTimes != null && selectedTimes.length > 0) {
            RequestConstraint timeConstraint = new RequestConstraint();
            if (selectedTimes.length == 1) {
                timeConstraint.setConstraintType(ConstraintType.EQUALS);
                timeConstraint.setConstraintValue(selectedTimes[0].toString());
            } else {
                timeConstraint.setConstraintType(ConstraintType.IN);
                String[] times = new String[selectedTimes.length];
                for (int i = 0; i < times.length; ++i) {
                    times[i] = selectedTimes[i].toString();
                }
                timeConstraint.setConstraintValueList(times);
            }
            request.addConstraint(PluginDataObject.DATATIME_ID, timeConstraint);
        }
        DbQueryResponse response = (DbQueryResponse) ThriftClient
                .sendRequest(request);
        return response.getEntityObjects(PluginDataObject.class);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#initInventory()
     */
    @Override
    public void initInventory() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#getInventory()
     */
    @Override
    public Object getInventory() {
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.datastructure.IDataCubeAdapter#
     * getBaseUpdateConstraints(java.util.Map)
     */
    @Override
    public List<Map<String, RequestConstraint>> getBaseUpdateConstraints(
            Map<String, RequestConstraint> constraints) {
        List<Map<String, RequestConstraint>> result = new ArrayList<Map<String, RequestConstraint>>(
                1);
        result.add(constraints);
        return result;
    }

}
