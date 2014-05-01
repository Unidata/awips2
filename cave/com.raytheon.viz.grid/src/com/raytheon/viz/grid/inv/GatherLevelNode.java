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

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.AggregateRequestableData;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;
import com.raytheon.uf.viz.derivparam.tree.AbstractAliasLevelNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode;

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

    public GatherLevelNode(AbstractRequestableNode sourceNode,
            DerivParamDesc desc, DerivParamMethod method, String modelName,
            Level level) {
        super(sourceNode, desc, method, modelName, level);
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws VizException {
        Map<TimeAndSpace, List<AbstractRequestableData>> availMap = new HashMap<TimeAndSpace, List<AbstractRequestableData>>();
        for (AbstractRequestableData data : dependencyData.get(sourceNode)) {
            TimeAndSpace ast = data.getTimeAndSpace();
            List<AbstractRequestableData> avail = availMap.get(ast);
            if (avail == null) {
                avail = new ArrayList<AbstractRequestableData>();
                availMap.put(ast, avail);
            }
            avail.add(data);
        }
        Set<AbstractRequestableData> result = new HashSet<AbstractRequestableData>();
        for (List<AbstractRequestableData> records : availMap.values()) {
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
