package gov.noaa.nws.ncep.viz.resources.manager;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;


@XmlRootElement(name = "AttrSetGroupList")
@XmlAccessorType(XmlAccessType.NONE)
public class AttrSetGroupList implements ISerializableObject {
		
	@XmlElement(name="AttrSetGroup")
	private List<AttrSetGroup> attrSetGroupList;

	public AttrSetGroupList( ) {
	}

	public List<AttrSetGroup> getAttrSetGroupList() {
		return attrSetGroupList;
	}

	public void setAttrSetGroupList(List<AttrSetGroup> attrSetGroupList) {
		this.attrSetGroupList = attrSetGroupList;
	}
	
	public void addAttrSetGroup( AttrSetGroup asg ) {
		if( attrSetGroupList == null ) {
			attrSetGroupList = new ArrayList<AttrSetGroup>();
		}
		if( asg != null ) {
			attrSetGroupList.add( asg );
		}
	}
}
