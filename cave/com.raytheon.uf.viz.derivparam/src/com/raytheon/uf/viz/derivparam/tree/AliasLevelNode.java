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
package com.raytheon.uf.viz.derivparam.tree;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.AliasRequestableData;
import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;
import com.raytheon.uf.viz.derivparam.library.DerivParamDesc;
import com.raytheon.uf.viz.derivparam.library.DerivParamMethod;

/**
 * This node handles all Alias derived parameters which includes Import in AWIPS
 * I. Data requests and Time queries are simply forwarded to the source nodes.
 * Returned records are wrapped in an ALiasRecord which can handle unit
 * conversion and model conversion if necessary.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2010 #3965      rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class AliasLevelNode extends AbstractAliasLevelNode {

    public AliasLevelNode(AliasLevelNode that) {
        super(that);
    }

    public AliasLevelNode(AbstractRequestableNode sourceNode,
            DerivParamDesc desc, DerivParamMethod method, String modelName,
            Level level) {
        super(sourceNode, desc, method, modelName, level);
    }

    @Override
    public Set<AbstractRequestableData> getData(
            Set<TimeAndSpace> availability,
            Map<AbstractRequestableNode, Set<AbstractRequestableData>> dependencyData)
            throws VizException {
        Set<AbstractRequestableData> origs = dependencyData.get(sourceNode);
        Set<AbstractRequestableData> results = new HashSet<AbstractRequestableData>(
                origs.size());
        for (AbstractRequestableData orig : origs) {
            AbstractRequestableData result = new AliasRequestableData(orig);
            modifyRequest(result);
            results.add(result);
        }
        return results;
    }

    @Override
    public AliasLevelNode clone() {
        return new AliasLevelNode(this);
    }
}
