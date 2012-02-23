package gov.noaa.nws.ncep.common.dataplugin.ncgrib.ncdatatree;

import com.raytheon.uf.common.derivparam.tree.AbstractNode;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

@DynamicSerialize
public class NcSourceNode extends AbstractNode<NcEventNode> {

	@Override
	public NcSourceNode clone() {
		NcSourceNode node = new NcSourceNode();
		node.setValue(this.getValue());

		for (NcEventNode child : this.getChildNodes().values()) {
			node.addChildNode((NcEventNode) child.clone());
		}
		return node;
	}

}
