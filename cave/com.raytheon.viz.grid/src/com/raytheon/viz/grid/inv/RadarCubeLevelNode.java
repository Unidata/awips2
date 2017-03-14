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

import java.util.List;

import com.raytheon.uf.common.derivparam.tree.AbstractCubeLevelNode;
import com.raytheon.uf.common.inventory.tree.AbstractRequestableNode;
import com.raytheon.uf.common.inventory.tree.CubeLevel;

/**
 * 
 * Implementation of {@link AbstractCubeLevelNode} that is intended specifically
 * for cubes of data from a radar source.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------
 * Jan 19, 2010  4126     bsteffen  Initial creation
 * Mar 22, 2016  5439     bsteffen  Remove unneccessary constructor arg.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class RadarCubeLevelNode extends AbstractCubeLevelNode {

    public RadarCubeLevelNode(AbstractCubeLevelNode that) {
        super(that);
    }

    public RadarCubeLevelNode(
            List<CubeLevel<AbstractRequestableNode, AbstractRequestableNode>> levels,
            String modelName) {
        super(levels, modelName);
    }
}
