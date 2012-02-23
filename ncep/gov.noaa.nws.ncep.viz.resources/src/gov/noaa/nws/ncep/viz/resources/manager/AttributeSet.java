package gov.noaa.nws.ncep.viz.resources.manager;

import java.util.HashMap;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.core.exception.VizException;

// TODO : beef this up later when changing format of files to xml.
// 
public class AttributeSet {

	private String applicableResource; // either the rsc type or implementation
	private String attrSetName;
	private LocalizationFile lFile;
	private HashMap<String, String> attrsMap;


	private  AttributeSet( String rsc, LocalizationFile lf ) throws VizException {
		applicableResource = rsc;
		lFile = lf;
		attrSetName = lFile.getFile().getName();
		attrSetName = attrSetName.substring(0, attrSetName.length()-".attr".length() );
		
		attrsMap  = ResourceDefnsMngr.readAttrSetFile( lFile.getFile() );
	}
	
//	public AttributeSet( String rsc, LocalizationFile lf ) {
//		applicableResource = rsc;
//		lFile = lf;
//		attrsMap  = new HashMap<String,String>();
//	}

	public static AttributeSet createAttributeSet(
										String rsc, LocalizationFile lf ) throws VizException {		
		return new AttributeSet( rsc, lf );	 
	}
	
	
	public String getApplicableResource() {
		return applicableResource;
	}

	public void setApplicableResource(String applicableResource) {
		this.applicableResource = applicableResource;
	}

	public LocalizationFile getFile() {
		return lFile;
	}

	public void setFile(LocalizationFile lFile) {
		this.lFile = lFile;
	}
	
	public LocalizationLevel getLocalizationLevel( ) {
		if( lFile == null ) {
			return LocalizationLevel.UNKNOWN;
		}
		else {
			return lFile.getContext().getLocalizationLevel();
		}
	}
	
	public String getName() {
		return attrSetName;
	}

	public void setName(String attrSetName) {
		this.attrSetName = attrSetName;
	}

	public void setAttributes( HashMap<String, String> attrs ) {
		attrsMap =  new HashMap<String,String>( attrs );
	}

	public HashMap<String, String> getAttributes() {
		return attrsMap;
	}
}
