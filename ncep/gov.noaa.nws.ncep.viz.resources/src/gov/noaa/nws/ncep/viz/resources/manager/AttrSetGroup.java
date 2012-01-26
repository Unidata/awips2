package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.common.StringListAdapter;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.core.exception.VizException;

@XmlRootElement(name = "AttributeSetGroup")
@XmlAccessorType(XmlAccessType.NONE)
public class AttrSetGroup {
	
	@XmlElement
    private String resource;

	@XmlElement
	private String attrSetGroupName;
	
	@XmlElement
	@XmlJavaTypeAdapter(StringListAdapter.class)
	private ArrayList<String> attrSetNames;
		
	private LocalizationFile lclFile;
		
	private boolean isModified;

	public AttrSetGroup() {
		attrSetNames = new ArrayList<String>();
		resource = "";
		attrSetGroupName = "";
		isModified = false;
	}
		
	public AttrSetGroup( AttrSetGroup asg ) {
		attrSetNames = new ArrayList<String>( asg.getAttrSetNames() );
		resource = asg.getResource();
		attrSetGroupName = asg.getAttrSetGroupName();
		isModified = false;
	}
	
	public boolean isModified() {
		return isModified;
	}
	
	public String getResource() {
		return resource;
	}

	public void setResource(String resource) {
		this.resource = resource;
	}

	public String getAttrSetGroupName() {
		return attrSetGroupName;
	}

	public void setAttrSetGroupName(String attrSetGroupName) {
		this.attrSetGroupName = attrSetGroupName;
	}

	public List<String> getAttrSetNames() {
		return attrSetNames;
//		return attrSetFilesMap.keySet();
	}

	public void setAttrSetNames(ArrayList<String> attrSetNames) {
		this.attrSetNames = attrSetNames;
	}	
		
	// add the name and find the file.
	public boolean addAttrSetName( String asName ) {
		if( !attrSetNames.contains( asName ) ) {
			attrSetNames.add( asName );
			return true;
		}
		return false;
	}

	public boolean removeAttrSet( String asName ) {
		
		return attrSetNames.remove( asName );
		
	}
	
	public void removeAllAttrSets( ) {
		attrSetNames.clear();
		return;
	}
	
	public LocalizationFile getLocalizationFile() {
		return lclFile;
	}

	public void setLocalizationFile( LocalizationFile lclFile) {
		this.lclFile = lclFile;
	}

	public String getMapKey( ) {
		if( resource.equals("PGEN") ) {
			return "PGEN";
		}
		else {
			return resource+File.separator+attrSetGroupName;
		}
	}
}
