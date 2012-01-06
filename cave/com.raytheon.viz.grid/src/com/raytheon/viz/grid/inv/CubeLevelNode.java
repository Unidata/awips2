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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.tree.AbstractCubeLevelNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode;
import com.raytheon.uf.viz.derivparam.tree.CubeLevel;
import com.raytheon.viz.grid.data.GribRequestableDataFactory;

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
public class CubeLevelNode extends AbstractCubeLevelNode {
    public CubeLevelNode(AbstractCubeLevelNode that) {
        super(that);
    }

    public CubeLevelNode(
            List<CubeLevel<AbstractRequestableLevelNode, AbstractRequestableLevelNode>> levels,
            String modelName) {
        super(levels, modelName);
    }

    protected List<AbstractRequestableData> wrapRawRecord(List<Object> objs)
            throws VizException {
        List<AbstractRequestableData> gribResults = new ArrayList<AbstractRequestableData>(
                objs.size());
        GribRequestableDataFactory factory = GribRequestableDataFactory
                .getInstance();
        for (Object obj : objs) {
            AbstractRequestableData record = factory
                    .getGribRequestableData((GribRecord) obj);
            gribResults.add(record);
        }

        return gribResults;
    }

    @Override
    protected void filter(
            Map<String, RequestConstraint> baseRequestConstraints,
            Map<String, RequestConstraint> requestContraintsToFilter) {
        for (Entry<String, RequestConstraint> e : baseRequestConstraints
                .entrySet()) {
            if (!requestContraintsToFilter.containsKey(e.getKey())) {
                requestContraintsToFilter.put(e.getKey(), e.getValue());
            }
        }
    }
}
