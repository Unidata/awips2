package gov.noaa.nws.ncep.common.dataplugin.ncgrib.ncdatatree;

import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.tree.AbstractNode;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@DynamicSerialize
public class NcLevelNode extends AbstractNode<NcLevelNode> {

	@DynamicSerializeElement
    private String levelName;
	
	@DynamicSerializeElement
	private Map<String, RequestConstraint> rcmap;
	
	public Map<String, RequestConstraint> getRcmap() {
		return rcmap;
	}

	public void setRcmap(Map<String, RequestConstraint> rcmap) {
		this.rcmap = rcmap;
	}

	public String getLevelName() {
		return levelName;
	}

	public void setLevelName(String levelName) {
		this.levelName = levelName;
	}	
	
//	public NcLevelNode(NcLevelNode that) {
//        this.value = that.value;
//        this.levelName = that.levelName;
//
//        for (NcLevelNode child : that.getChildNodes().values()) {
//            addChildNode((NcLevelNode) child.clone());
//        }
//    }
	
	public NcLevelNode() {
    }

	@Override
	public NcLevelNode clone() {
		NcLevelNode node = new NcLevelNode();
        node.setValue(this.getValue());
        node.setLevelName(levelName);
        node.setRcmap(rcmap);

        for (NcLevelNode child : this.getChildNodes().values()) {
            node.addChildNode((NcLevelNode) child.clone());
        }

        return node;
	}

}
