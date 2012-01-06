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

import com.raytheon.uf.viz.core.exception.VizException;

@XmlRootElement(name = "AttributeSetGroup")
@XmlAccessorType(XmlAccessType.NONE)
public class AttrSetGroup {

	public static final String ATTR_SET_FILE_EXT = ".attr";
	
	@XmlElement
    private String resource;

	@XmlElement
	private String attrSetGroupName;
	
	@XmlElement
	@XmlJavaTypeAdapter(StringListAdapter.class)
	private ArrayList<String> attrSetNames;
		
	// Not used. not sure we need this
	private HashMap<String,File> attrSetFilesMap;

	private File attrSetsDir=null;
	
	private boolean isModified;

	public AttrSetGroup() {
		attrSetNames = new ArrayList<String>();
		attrSetFilesMap = new HashMap<String,File>();
		resource = "";
		attrSetGroupName = "";
		isModified = false;
		attrSetsDir=null;
	}
		
	public AttrSetGroup( AttrSetGroup asg ) {
		attrSetNames = new ArrayList<String>( asg.getAttrSetNames() );
		attrSetFilesMap = new HashMap<String,File>();
		resource = asg.getResource();
		attrSetGroupName = asg.getAttrSetGroupName();
		isModified = false;
		
		// set the attrSetsDir and the attrSetFilesMap
		validateAttrSets( asg.attrSetsDir );
	}

	// use the unmarshalled attrSetNames to load the attrSetFilesMap
	//
	public boolean validateAttrSets( File asGroupDir ) {
		attrSetsDir = asGroupDir; // asGroupFile.getParentFile();
		
		if( attrSetsDir == null || !attrSetsDir.exists() ) {
			attrSetNames.clear();
			attrSetFilesMap.clear();
			attrSetsDir = null;
			return false;
		}
		
		ArrayList<String> removeList = new ArrayList<String>();

		// For now just check for the existence of the file.
		for( String asName : attrSetNames ) {
			File attrSetFile = new File( attrSetsDir, asName+ATTR_SET_FILE_EXT );
			if( !attrSetFile.exists() ) {
				removeList.add( asName );
			}
			attrSetFilesMap.put( asName, attrSetFile );
		}
			
		if( !removeList.isEmpty() ) {
			attrSetNames.removeAll( removeList );
			// should we return false here? 
		}
		
//		isModified = false;
		return true;
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

	public Set<String> getAttrSetNames() {
//		return attrSetNames;
		return attrSetFilesMap.keySet();
	}

	public void setAttrSetNames(ArrayList<String> attrSetNames) {
		this.attrSetNames = attrSetNames;
	}	
	
	// add the name and the file.
	public void addAttrSet( String asName, File asFile ) {
		if( attrSetFilesMap.containsKey( asName ) ) {
			return;
		}
		attrSetFilesMap.put( asName, asFile );
		attrSetNames.add( asName );		
	}
	
	// add the name and find the file.
	public boolean addAttrSetName( String asName ) {
		
		// check for the existence of the attrset file.		
		File attrSetFile = new File( attrSetsDir, asName+ATTR_SET_FILE_EXT );
		if( attrSetFile.exists() ) {
			addAttrSet( asName, attrSetFile );
			return true;
		}
		return false;
	}

	public boolean removeAttrSet( String asName ) {
		
		attrSetNames.remove( asName );
		
		if( attrSetFilesMap.containsKey( asName ) ) {
			attrSetFilesMap.remove( asName );
			return true;
		}
		return false;
	}
	
	public void removeAllAttrSets( ) {
		attrSetNames.clear();
		attrSetFilesMap.clear();
		return;
	}
	
	public File getAttrSetFile( String asName ) {
		if( attrSetFilesMap.containsKey( asName ) ) {
			return attrSetFilesMap.get( asName );			
		}
		else if( attrSetNames.contains( asName ) ) {
			attrSetNames.remove( asName );
		}
		return null;
	}
}
