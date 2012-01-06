package gov.noaa.nws.ncep.viz.resources.manager;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;


@XmlRootElement(name = "ResourceDefinitionTable")
@XmlAccessorType(XmlAccessType.NONE)
public class ResourceDefinitionTable implements ISerializableObject {
		
	@XmlElement(name="ResourceDefinition")
	private List<ResourceDefinition> resourceDefinitionsList;

	public ResourceDefinitionTable( ) {
	}

	public List<ResourceDefinition> getResourceDefinitionsList() {
		return resourceDefinitionsList;
	}
	
	public void setResourceDefinitionsList( ArrayList<ResourceDefinition> rdlist ) {
		resourceDefinitionsList = rdlist;
	}
}
