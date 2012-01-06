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
package com.raytheon.viz.grid.inv;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamField;
import com.raytheon.uf.viz.derivparam.tree.AbstractCubeLevelNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode;
import com.raytheon.uf.viz.derivparam.tree.CubeLevel;
import com.raytheon.viz.grid.data.RadarRequestableData;

/**
 * 
 * Provided a mechanism for requesting data for an entire 3D cube. If a Level is
 * set it will request records for all Standard levels within that composite
 * level, otherwise it will request all MB records. It will respond to time
 * queries with the Union of all levels it represents, although in the future
 * this may need to be changed to the intersection, or a limited intersection
 * when at least 3 levels are available. It returns all the GribRecords from all
 * the level nodes it represents, these should be sorted by the requesting node.
 * Finally it attempts to merge any requests to avoid the overhead of multiple
 * requests to EDEX.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RadarCubeLevelNode extends AbstractCubeLevelNode {
    private String paramAbbrev = null;

    public RadarCubeLevelNode(AbstractCubeLevelNode that) {
        super(that);
    }

    public RadarCubeLevelNode(
            List<CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode>> levels,
            String modelName, DerivParamField field) {
        super(levels, modelName);
        paramAbbrev = field.getParam();
    }

    protected List<AbstractRequestableData> wrapRawRecord(List<Object> objs)
            throws VizException {
        List<AbstractRequestableData> gribResults = new ArrayList<AbstractRequestableData>(
                objs.size());
        for (Object obj : objs) {
            AbstractRequestableData record = new RadarRequestableData(
                    (RadarRecord) obj, paramAbbrev);
            gribResults.add(record);
        }
        return gribResults;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.derivparam.tree.AbstractCubeLevelNode#mergedTimeQuery
     * (java.util.List, boolean)
     */
    @Override
    protected Set<DataTime> mergedTimeQuery(
            List<Map<String, RequestConstraint>> requests, boolean latestOnly)
            throws VizException {
        // Make sure we have data on at least two levels
        Map<DataTime, Double> single = new HashMap<DataTime, Double>();
        Set<DataTime> results = new HashSet<DataTime>();
        for (Map<String, RequestConstraint> mergeMap : requests) {
            DbQueryRequest request = new DbQueryRequest();
            request.setConstraints(mergeMap);
            String field = "dataTime";
            String levelField = "primaryElevationAngle";
            request.addFields(new String[] { field, levelField });

            DbQueryResponse response = (DbQueryResponse) ThriftClient
                    .sendRequest(request);
            if (response.getResults() == null) {
                continue;
            }
            for (Map<String, Object> map : response.getResults()) {
                DataTime time = (DataTime) map.get(field);
                Double level = (Double) map.get(levelField);
                if (results.contains(time)) {
                    continue;
                }
                if (!single.containsKey(time)) {
                    single.put(time, level);
                } else if (!level.equals(single.get(time))) {
                    single.remove(time);
                    results.add(time);
                }
            }
        }
        return results;
    }

    @Override
    protected void filter(
            Map<String, RequestConstraint> baseRequestConstraints,
            Map<String, RequestConstraint> requestContraintsToFilter) {
        // do nothing, no filtering necessary for radar
    }
}
