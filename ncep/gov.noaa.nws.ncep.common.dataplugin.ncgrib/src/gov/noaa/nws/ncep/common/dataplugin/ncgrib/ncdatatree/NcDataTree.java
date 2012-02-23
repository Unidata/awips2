package gov.noaa.nws.ncep.common.dataplugin.ncgrib.ncdatatree;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

//import com.raytheon.uf.common.derivparam.tree.DataTree;
//import com.raytheon.uf.common.derivparam.tree.LevelNode;
//import com.raytheon.uf.common.derivparam.tree.ParameterNode;
//import com.raytheon.uf.common.derivparam.tree.SourceNode;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@DynamicSerialize
public class NcDataTree implements ISerializableObject,
		Cloneable {
	
	@DynamicSerializeElement
    private Map<String, NcSourceNode> ncSourceNodes = new HashMap<String, NcSourceNode>();


    public Map<String, NcSourceNode> getNcSourceNodes() {
		return ncSourceNodes;
	}

	public void setNcSourceNodes(Map<String, NcSourceNode> ncSourceNodes) {
		this.ncSourceNodes = ncSourceNodes;
	}

	public NcDataTree clone() {
        NcDataTree tree = new NcDataTree();
        Map<String, NcSourceNode> clonedNodes = tree.getNcSourceNodes();

        for (NcSourceNode node : ncSourceNodes.values()) {
            clonedNodes.put(node.getValue(), node.clone());
        }

        return tree;
    }

//    public int mergeGridTree(NcDataTree tree) {
//        for (NcSourceNode sourceNode : tree.getNcSourceNodes().values()) {
//            NcSourceNode thisNcSourceNode = ncSourceNodes.get(sourceNode.getValue());
//
//            if (thisNcSourceNode == null) {
//            	ncSourceNodes.put(sourceNode.getValue(), (NcSourceNode) ncSourceNodes);
//            } else {
//                thisNcSourceNode.mergeNode(ncSourceNodes);
//            }
//        }
//
//        return tree.getNcSourceNodes().size();
//    }

//    public void addBranch(String source,
//            String parameterName, 
//            String levelName, String levelValue, Map<String, RequestConstraint> rcMap) {
//        addBranch(source, null, parameterName,
//        		levelName, levelValue, null, rcMap);
//    }

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
    public void addBranch(String source, String eventName,
            String parameterName, String levelName,
            String level1Value, String level2Value, Map<String, RequestConstraint> rcMap) {
        NcSourceNode sourceNode = ncSourceNodes.get(source);
        if (sourceNode == null) {
            sourceNode = new NcSourceNode();
            sourceNode.setValue(source);
            ncSourceNodes.put(source, sourceNode);
        }
        
        NcEventNode eventNode = sourceNode.getChildNode(eventName);
        if (eventNode == null) {
        	eventNode = new NcEventNode();
        	eventNode.setValue(eventName);
            sourceNode.addChildNode(eventNode);
        }
        

        NcParameterNode paramNode = eventNode
                .getChildNode(parameterName);
        if (paramNode == null) {
            paramNode = new NcParameterNode();
            paramNode.setValue(parameterName);
            eventNode.addChildNode(paramNode);
        }

        String levelId = null;
        if ( level2Value.equals("-9999") )
        	levelId = levelName + ":" + level1Value;
        else
        	levelId = levelName + ":" + level1Value + ":" + level2Value;
        NcLevelNode levelNode = paramNode.getChildNode(levelId);
        if (levelNode == null) {
            levelNode = new NcLevelNode();
            levelNode.setValue(levelId);
            //levelNode.setLevelName(levelName);
            levelNode.setRcmap(rcMap);
            paramNode.addChildNode(levelNode);
        }
        
    }

    /**
     * Returns a list of available model names.
     * 
     * @return The center id
     */
    public Set<String> getNcSources() {
        return ncSourceNodes.keySet();
    }

    /**
     * Returns the specified model name node.
     * 
     * @param modelNameId
     * @return The center node
     */
    public NcSourceNode getNcSourceNode(String modelNameId) {
        return ncSourceNodes.get(modelNameId);
    }
    
    public Set<String> getNcEvents(String modelName) {
        NcSourceNode node = getNcSourceNode(modelName);
        return node != null ? node.getChildNodeIds() : new HashSet<String>();
    }

    public NcEventNode getNcEventNode(String modelName, String event) {
    	NcEventNode rval = null;
        NcSourceNode node = getNcSourceNode(modelName);

        if (node != null) {
            rval = node.getChildNode(event);
        }

        return rval;
    }
    /**
     * Returns a list of available parameters for the specified criteria. If not
     * found, an empty string array is returned.
     * 
     * @param modelName
     *            The target model name
     * @return A string array of available parameter nodes
     */
    public Set<String> getNcParameters(String modelName, String eventName) {
        NcEventNode node = getNcEventNode(modelName, eventName);
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
    public NcParameterNode getNcParameterNode(String modelName, String eventName, String parameterName) {
        NcParameterNode rval = null;
        NcEventNode node = getNcEventNode(modelName, eventName);

        if (node != null) {
            rval = node.getChildNode(parameterName);
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
    public Set<String> getNcLevels(String modelName, String eventName, String paramName) {
        NcParameterNode parameter = getNcParameterNode(modelName, eventName, paramName);
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
    public NcLevelNode getNcLevelNode(String modelName, String eventName, String paramName, String level) {
        NcLevelNode rval = null;
        NcParameterNode parameter = getNcParameterNode(modelName, eventName, paramName);

        if (parameter != null) {
            rval = parameter.getChildNode(level);
        }

        return rval;
    }

    @Override
    public String toString() {
        StringBuilder tmp = new StringBuilder();

        for (NcSourceNode node : ncSourceNodes.values()) {
            tmp.append(node.toString());
        }

        return tmp.toString();
    }

}
