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
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.AggregateRequestableData;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.tree.AbstractAliasLevelNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableLevelNode;

/**
 * 
 * Builds AggregateRecords which contain all perturbations for a given record
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 26, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GatherLevelNode extends AbstractAliasLevelNode {

    public GatherLevelNode(GatherLevelNode that) {
        super(that);
    }

    public GatherLevelNode(AbstractRequestableLevelNode sourceNode,
            DerivParamDesc desc, DerivParamMethod method, String modelName,
            Level level) {
        super(sourceNode, desc, method, modelName, level);
    }

    public List<AbstractRequestableData> getDataInternal(
            LayerProperty property,
            int timeOut,
            Map<AbstractRequestableLevelNode, List<AbstractRequestableData>> cache)
            throws VizException {
        Map<DataTime, List<AbstractRequestableData>> recordMap = new HashMap<DataTime, List<AbstractRequestableData>>();
        HashMap<String, RequestConstraint> rcMap = property
                .getEntryQueryParameters(false);
        for (Integer pert : GridInventory.getPerts(sourceNode)) {
            rcMap.put(GridInventory.PERT_QUERY, new RequestConstraint(pert
                    .toString()));
            property.setEntryQueryParameters(rcMap, false);
            cache.clear();
            for (AbstractRequestableData record : sourceNode.getData(property,
                    timeOut, cache)) {
                List<AbstractRequestableData> records = recordMap.get(record
                        .getDataTime());
                if (records == null) {
                    records = new ArrayList<AbstractRequestableData>();
                    recordMap.put(record.getDataTime(), records);
                }
                records.add(record);
            }
        }
        rcMap.remove(GridInventory.PERT_QUERY);
        property.setEntryQueryParameters(rcMap, false);
        List<AbstractRequestableData> result = new ArrayList<AbstractRequestableData>(
                recordMap.size());
        for (List<AbstractRequestableData> records : recordMap.values()) {
            AggregateRequestableData record = new AggregateRequestableData(
                    records);
            modifyRequest(record);
            result.add(record);
        }
        return result;
    }

    @Override
    public GatherLevelNode clone() {
        return new GatherLevelNode(this);
    }

}
