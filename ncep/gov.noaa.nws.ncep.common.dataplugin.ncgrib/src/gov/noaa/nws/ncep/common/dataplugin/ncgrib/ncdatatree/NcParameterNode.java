package gov.noaa.nws.ncep.common.dataplugin.ncgrib.ncdatatree;

import com.raytheon.uf.common.derivparam.tree.AbstractNode;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@DynamicSerialize
public class NcParameterNode extends AbstractNode<NcLevelNode> {
	
	@DynamicSerializeElement
    private String parameterName;

	public String getParameterName() {
		return parameterName;
	}

	public void setParameterName(String parameterName) {
		this.parameterName = parameterName;
	}

	@Override
	public NcParameterNode clone() {
		NcParameterNode node = new NcParameterNode();
        node.setValue(this.getValue());
        node.setParameterName(parameterName);

        for (NcLevelNode child : this.getChildNodes().values()) {
            node.addChildNode((NcLevelNode) child.clone());
        }

        return node;
	}

}
