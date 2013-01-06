/*****************************************************************************************
 * COPYRIGHT (c), 2006-2009, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.uf.viz.derivparam.tree;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.AggregateRequestableData;
import com.raytheon.uf.viz.derivparam.data.DerivedRequestableData;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterRequest;

/**
 * 
 * This node provides a mechanism for Averaging on a composite level. It is
 * based off a cube node, with a limited level. GribRecords are converted from
 * the list that CubeNodes return to DerivedRecords that will Average. In theory
 * this behaves exactly like a UnionLevelNode being passed to an
 * DerivedLevelnode that Averages and in the future may be replaced with
 * that.(UnionLevelNodes do not exist at the time of this comment)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 23, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class CompositeAverageLevelNode extends UnionLevelNode {

    public CompositeAverageLevelNode(CompositeAverageLevelNode that) {
        super(that);
    }

    public CompositeAverageLevelNode(Level level, DerivParamDesc desc,
            String modelName, List<AbstractRequestableNode> nodes) {
        super(level, desc, null, modelName, nodes);
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws VizException {
        Set<AbstractRequestableData> result = new HashSet<AbstractRequestableData>();

        for (AbstractRequestableData record : super.getData(availability,
                dependencyData)) {
            AggregateRequestableData aRec = (AggregateRequestableData) record;
            if (aRec.getSourceRecords().size() == nodes.size()) {
                DerivedParameterRequest request = new DerivedParameterRequest();
                request.setMethod("Average");
                for (AbstractRequestableData baseRec : aRec.getSourceRecords()) {
                    request.addBaseParam(baseRec);
                }
                AbstractRequestableData newRecord = new DerivedRequestableData(
                        aRec.getSourceRecords().get(0), request);
                request.setParameterAbbreviation(aRec.getSourceRecords().get(0)
                        .getParameter());
                modifyRequest(newRecord);
                result.add(newRecord);
            }
        }
        return result;
    }

    @Override
    public Set<TimeAndSpace> getAvailability(
            Map<AbstractRequestableNode, Set<TimeAndSpace>> availability)
            throws VizException {
        Set<TimeAndSpace> results = null;

        List<AbstractRequestableNode> requests = new ArrayList<AbstractRequestableNode>(
                nodes);
        for (AbstractRequestableNode request : requests) {
            // Do not request just latest only because if two nodes have
            // different latests than this will return no times
            Set<TimeAndSpace> times = availability.get(request);
            if (times == null) {
                continue;
            } else if (results == null) {
                results = new HashSet<TimeAndSpace>(times);
            } else {
                results.retainAll(times);
            }
        }
        return results;
    }

    @Override
    public CompositeAverageLevelNode clone() {
        return new CompositeAverageLevelNode(this);
    }

}
