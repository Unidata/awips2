package gov.noaa.nws.ncep.common.dataplugin.ncgrib.ncdatatree;

import com.raytheon.uf.common.derivparam.tree.AbstractNode;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@DynamicSerialize
public class NcEventNode extends AbstractNode<NcParameterNode> {

	@DynamicSerializeElement
    private String eventName;


	public String getEventName() {
		return eventName;
	}


	public void setEventName(String eventName) {
		this.eventName = eventName;
	}


	@Override
	public NcEventNode clone() {
		NcEventNode node = new NcEventNode();
        node.setValue(this.getValue());
        node.setEventName(eventName);

        for (NcParameterNode child : this.getChildNodes().values()) {
            node.addChildNode((NcParameterNode) child.clone());
        }

        return node;
	}

}
