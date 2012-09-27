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
package com.raytheon.viz.grid.util;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.tree.LevelNode;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.derivparam.data.AbstractRequestableData;
import com.raytheon.uf.viz.derivparam.data.FloatRequestableData;
import com.raytheon.uf.viz.derivparam.library.IDerivParamField;
import com.raytheon.uf.viz.derivparam.tree.AbstractDerivedDataNode;
import com.raytheon.uf.viz.derivparam.tree.AbstractRequestableNode.Dependency;
import com.raytheon.uf.viz.derivparam.tree.DerivedLevelNode;
import com.raytheon.uf.viz.derivparam.tree.StaticDataLevelNode;
import com.raytheon.viz.grid.inv.GridInventory;
import com.raytheon.viz.grid.inv.GridRequestableNode;

/**
 * This is a tool intended only for use by developers to help to visualize the
 * necessary data for a derived parameter. printGraph prints a graph to standard
 * out that is formated to be used by graphViz to generate a data tree showing
 * all derived parameter operations.
 * 
 * To use this call printGraph on a LevelNode and copy the results from standard
 * out to a file, for example "test.dot". Then run graphviz to convert it to an
 * image, for example "dot -T png -o test.png test.dot" That should create a
 * file "test.png" than can be viewed to see the derivation tree for that level
 * node.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GridTreeGrapher {

    private static Map<Object, Integer> dupMap = new HashMap<Object, Integer>();

    private static int next = 0;

    public static void printGraph(LevelNode node) {
        System.out.print("digraph G {\n  graph [\n    rankdir=\"UR\"\n"
                + "    nodesep=\"0.18\" \n    ranksep=\"0.46\"\n"
                + "    fontname=\"Helvetica\"\n    fontsize=\"11\"\n"
                + "    size=\"300,300\"\n" + "  ];\n  node [\n"
                + "    fontname=\"Helvetica\" \n   fontsize=\"11\"\n"
                + "    shape=\"box\"\n" + "  ];\n  edge [\n"
                + "    arrowsize=\"0.8\"\n  ];");
        try {
            printGraphInternal(node);
        } catch (VizException e) {
            e.printStackTrace();
        }
        System.out.println("}");
        dupMap.clear();
        next = 0;
    }

    public static int printGraphInternal(Object node) throws VizException {
        int i = next++;
        if (dupMap.containsKey(node)) {
            return dupMap.get(node);
        } else {
            dupMap.put(node, i);
        }
        if (node instanceof DerivedLevelNode) {
            DerivedLevelNode dNode = (DerivedLevelNode) node;
            String label = dNode.getModelName() + "\\n"
                    + dNode.getDesc().getAbbreviation() + "\\n"
                    + dNode.getLevel();
            System.out.println("node" + i + " [label=\"" + label + "\"];");
            for (IDerivParamField ifield : dNode.getMethod().getFields()) {
                Object fieldValue = null;
                if (dNode.getFieldsStaticData() != null) {
                    fieldValue = dNode.getFieldsStaticData().get(ifield);
                }
                if (fieldValue == null) {
                    fieldValue = dNode.getFields().get(ifield);
                }
                int that = printGraphInternal(fieldValue);
                System.out.println("node" + i + " -> node" + that);
            }
        } else if (node instanceof StaticDataLevelNode) {
            StaticDataLevelNode cNode = (StaticDataLevelNode) node;
            AbstractRequestableData staticData = cNode.getData(null, null)
                    .iterator().next();
            if (staticData instanceof FloatRequestableData) {
                String abbr = node.getClass().getSimpleName();
                if (cNode.getDesc() != null) {
                    abbr = cNode.getDesc().getAbbreviation();
                }
                String label = "Constant: " + staticData.getDataValue(null)
                        + "\\n" + cNode.getModelName() + "\\n" + abbr + "\\n"
                        + cNode.getLevel();
                System.out.println("node" + i + " [label=\"" + label + "\"];");
            } else {
                String abbr = node.getClass().getSimpleName();
                if (cNode.getDesc() != null) {
                    abbr = cNode.getDesc().getAbbreviation();
                }
                String label = "StaticData\\n" + cNode.getModelName() + "\\n"
                        + abbr + "\\n" + cNode.getLevel();
                System.out.println("node" + i + " [label=\"" + label + "\"];");
                for (Dependency dep : cNode.getDependencies()) {
                    int that = printGraphInternal(dep.node);
                    System.out.println("node" + i + " -> node" + that);
                }
            }
        } else if (node instanceof AbstractDerivedDataNode) {
            AbstractDerivedDataNode cNode = (AbstractDerivedDataNode) node;
            String abbr = node.getClass().getSimpleName();
            if (cNode.getDesc() != null) {
                abbr = cNode.getDesc().getAbbreviation();
            }
            String label = cNode.getModelName() + "\\n" + abbr + "\\n"
                    + cNode.getLevel();
            System.out.println("node" + i + " [label=\"" + label + "\"];");
            for (Dependency dep : cNode.getDependencies()) {
                int that = printGraphInternal(dep.node);
                System.out.println("node" + i + " -> node" + that);
            }
        } else if (node instanceof FloatRequestableData) {
            System.out.println("node" + i + " [label=\"Constant: "
                    + ((FloatRequestableData) node).getDataValue() + "\"];");
        } else if (node instanceof GridRequestableNode) {
            Map<String, RequestConstraint> rcMap = ((GridRequestableNode) node)
                    .getRequestConstraintMap();
            Level level = ((GridRequestableNode) node).getLevel();
            String label = rcMap.get(GridInventory.MODEL_NAME_QUERY)
                    .getConstraintValue()
                    + "\\n"
                    + rcMap.get(GridInventory.PARAMETER_QUERY)
                            .getConstraintValue() + "\\n" + level;
            System.out.println("node" + i + " [label=\"" + label + "\"];");
        } else {
            System.out.println("node" + i + " [label=\""
                    + node.getClass().getSimpleName() + "\"];");
        }
        return i;
    }
}
