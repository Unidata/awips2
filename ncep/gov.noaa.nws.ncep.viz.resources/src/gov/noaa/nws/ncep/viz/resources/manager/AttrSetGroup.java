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
	
//	@XmlElement
//    private String resource;
//
//	@XmlElement
//	private String attrSetGroupName;
	
	// This class would be called AttrSetGroupName except that this was the name 
	// of the old member for just the group name and the set method needs to stay the
	// same so that the existing jaxb files are still compatible.
	public static class RscAndGroupName implements Comparable<RscAndGroupName> {
	    private String resource;
		private String groupName;
		
		public RscAndGroupName( ) {
			this( "","");
		}
		public RscAndGroupName( String r, String g ) {
			resource = r;
			groupName = g;
		}
		public String getResource() {
			return resource;
		}
		public String getGroupName() {
			return groupName;
		}
		public Boolean isPGEN() {
			return getGroupName().equals("PGEN");
		}
		@Override
		public String toString() {
			return (isPGEN() ? "PGEN" : resource+"-"+groupName );
		}
		@Override
		public int compareTo(RscAndGroupName o) {		
			return toString().compareTo( o.toString() );
		}		
	}

	private RscAndGroupName rscAndGroupName;
	
	@XmlElement
	@XmlJavaTypeAdapter(StringListAdapter.class)
	private ArrayList<String> attrSetNames;
		
	private LocalizationFile lclFile;
		
	private boolean isModified;

	public AttrSetGroup() {
		attrSetNames = new ArrayList<String>();
		rscAndGroupName = new RscAndGroupName();
		
//		resource = "";
//		attrSetGroupName = "";
		isModified = false;
	}
		
	public AttrSetGroup( AttrSetGroup asg ) {
		attrSetNames = new ArrayList<String>( asg.getAttrSetNames() );
		rscAndGroupName = new RscAndGroupName( asg.getResource(), asg.getAttrSetGroupName() );
//		resource = asg.getResource();
//		attrSetGroupName = asg.getAttrSetGroupName();
		isModified = false;
	}
	
	public boolean isModified() {
		return isModified;
	}
	
	public String getResource() {
		return rscAndGroupName.getResource();
	}

	@XmlElement
	public void setResource(String resource) {
		rscAndGroupName = new RscAndGroupName( resource, rscAndGroupName.getGroupName() );
	}

	public RscAndGroupName getRscAndGroupName() {
		return rscAndGroupName;
	}
	
	public String getAttrSetGroupName() {
		return rscAndGroupName.getGroupName();
//		return attrSetGroupName;
	}

	@XmlElement
	public void setAttrSetGroupName(String group ) {
		rscAndGroupName = new RscAndGroupName( rscAndGroupName.getResource(), group );
//		this.attrSetGroupName = attrSetGroupName;
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

}
