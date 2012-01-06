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
package com.raytheon.uf.common.derivparam.tree;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 17, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@DynamicSerialize
public class DataTree implements ISerializableObject, Cloneable {

    @DynamicSerializeElement
    private Map<String, SourceNode> sourceNodes = new HashMap<String, SourceNode>();

    public Map<String, SourceNode> getSourceNodes() {
        return sourceNodes;
    }

    public void setSourceNodes(Map<String, SourceNode> sourceNodes) {
        this.sourceNodes = sourceNodes;
    }

    public DataTree clone() {
        DataTree tree = new DataTree();
        Map<String, SourceNode> clonedNodes = tree.getSourceNodes();

        for (SourceNode node : sourceNodes.values()) {
            clonedNodes.put(node.getValue(), node.clone());
        }

        return tree;
    }

    public int mergeGridTree(DataTree tree) {
        for (SourceNode sourceNode : tree.getSourceNodes().values()) {
            SourceNode thisSourceNode = sourceNodes.get(sourceNode.getValue());

            if (thisSourceNode == null) {
                sourceNodes.put(sourceNode.getValue(), sourceNode);
            } else {
                thisSourceNode.mergeNode(sourceNode);
            }
        }

        return tree.getSourceNodes().size();
    }

    public void addBranch(String source, String parameterAbbreviation,
            String parameterName, String parameterUnit, String levelId) {
        addBranch(source, -1, parameterAbbreviation, parameterName,
                parameterUnit, levelId);
    }

    /**
     * A convenience method that adds a new branch to the grid tree. This is for
     * the GridCatalog script task and expects the list to contain the following
     * metadata parameters in the same order:
     * <p>
     * modelName, parameterAbbreviation, parameterName, parameterUnit, level.id
     * </p>
     * 
     * @param gridInfo
     *            A List object that contains the above list
     */
    public void addBranch(String source, Integer dT,
            String parameterAbbreviation, String parameterName,
            String parameterUnit, String levelId) {
        SourceNode sourceNode = sourceNodes.get(source);
        if (sourceNode == null) {
            sourceNode = new SourceNode();
            sourceNode.setValue(source);
            sourceNode.setDt(dT);
            sourceNodes.put(source, sourceNode);
        }

        ParameterNode paramNode = sourceNode
                .getChildNode(parameterAbbreviation);
        if (paramNode == null) {
            paramNode = new ParameterNode();
            paramNode.setValue(parameterAbbreviation);
            paramNode.setParameterName(parameterName);
            paramNode.setParameterUnit(parameterUnit);
            sourceNode.addChildNode(paramNode);
        }

        LevelNode levelNode = paramNode.getChildNode(levelId);
        if (levelNode == null) {
            levelNode = new LevelNode();
            levelNode.setValue(levelId);
            paramNode.addChildNode(levelNode);
        }
    }

    /**
     * Returns a list of available model names.
     * 
     * @return The center id
     */
    public Set<String> getSources() {
        return sourceNodes.keySet();
    }

    /**
     * Returns the specified model name node.
     * 
     * @param modelNameId
     * @return The center node
     */
    public SourceNode getSourceNode(String modelNameId) {
        return sourceNodes.get(modelNameId);
    }

    /**
     * Returns a list of available parameters for the specified criteria. If not
     * found, an empty string array is returned.
     * 
     * @param modelName
     *            The target model name
     * @return A string array of available parameter nodes
     */
    public Set<String> getParameters(String modelName) {
        SourceNode node = getSourceNode(modelName);
        return node != null ? node.getChildNodeIds() : new HashSet<String>();
    }

    /**
     * Returns the specified parameter node.
     * 
     * @param modelName
     *            The target model name
     * @param param
     *            The target parameter
     * @return The parameter node
     */
    public ParameterNode getParameterNode(String modelName, String parameter) {
        ParameterNode rval = null;
        SourceNode node = getSourceNode(modelName);

        if (node != null) {
            rval = node.getChildNode(parameter);
        }

        return rval;
    }

    /**
     * Returns a list of available levels for the specified criteria. If not
     * found, an empty string array is returned.
     * 
     * @param modelName
     *            The target model name
     * @param param
     *            The target parameter
     * @return A string array of available level nodes
     */
    public Set<String> getLevels(String modelName, String param) {
        ParameterNode parameter = getParameterNode(modelName, param);
        return parameter != null ? parameter.getChildNodeIds()
                : new HashSet<String>();
    }

    /**
     * Returns the specified level node.
     * 
     * @param modelName
     *            The target model name
     * @param param
     *            The target parameter
     * @param level
     *            The target level
     * @return The level node or null if not found
     */
    public LevelNode getLevelNode(String modelName, String param, String level) {
        LevelNode rval = null;
        ParameterNode parameter = getParameterNode(modelName, param);

        if (parameter != null) {
            rval = parameter.getChildNode(level);
        }

        return rval;
    }

    @Override
    public String toString() {
        StringBuilder tmp = new StringBuilder();

        for (SourceNode node : sourceNodes.values()) {
            tmp.append(node.toString());
        }

        return tmp.toString();
    }
}
