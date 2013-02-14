package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;

import java.io.File;
import java.util.Date;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 *  09/13/10      #307        Greg Hull   add cycleTime.
 *  09/16/10      #307        Greg Hull   add forecast flag
 *  10/14/10	  #277		  M. Li		  add ensemble category
 *  10/20/10                  Xilin Guo   Rename getCycleTimeStringFromDataTime to getTimeStringFromDataTime
 *  02/16/11      #408        Greg Hull   add 'backup' categories for obs/fcst surface/uair
 *  01/09/11      #561        Greg Hull   generated equals()
 *  09/13/12      #860        Greg Hull   trim()
 *  11/19/12      #630        Greg Hull   getAbbrName() (for satellite-area names)
 *
 * </pre>
 * 
 * @author 
 * @version 1
 */

public class ResourceName {
	
	public static final String SatelliteRscCategory = "SATELLITE";
	public static final String RadarRscCategory =     "RADAR";
	public static final String GridRscCategory =      "GRID";
	public static final String SurfaceRscCategory =   "SURFACE";
	public static final String UpperAirRscCategory =  "UPPER_AIR";
	public static final String PGENRscCategory =      "PGEN";
	public static final String MiscRscCategory =      "MISC";
	public static final String EnsembleRscCategory =  "ENSEMBLE";
	public static final String OverlayRscCategory =   "OVERLAY";

	// NOTE : These are available if the users don't like the SURFACE/UPPER_AIR
	// categories as configured in the resourceDefinitions file.
	// 
	public static final String SurfaceFcstRscCategory =   "SURF_FCST";
	public static final String UpperAirFcstRscCategory =  "UAIR_FCST";
	public static final String SurfaceObsRscCategory =   "SURF_OBS";
	public static final String UpperAirObsRscCategory =  "UAIR_OBS";

	// IMAGE, MODEL, WATCH/WARNING
	
	public static class ResourceNameAdapter extends XmlAdapter<String, ResourceName> {

		@Override
		public String marshal(ResourceName rscName) throws Exception {
			return (rscName != null ? rscName.toString() : "");
		}

		@Override
		public ResourceName unmarshal(String rscNameStr) throws Exception {
			return new ResourceName( rscNameStr );
		} 	
	}

	public static final String generatedTypeDelimiter = ":";
	
	public static final String dfltAttrSetName = "default";
	
	// public String rscName; // ex. (SURFACE/METAR/standard)
	private String rscCategory;
	private String rscType;
	private String rscGroup; // this could be either a subType or attrSetGroup
	private String rscAttrSetName;

	// for forecast resources. If this is not null then this is a forecast resource. 
	private DataTime cycleTime; 
	
	// This may be used to set the cycle time
	public static final String LatestCycleTime = "LATEST";
	
	private static final DataTime LatestDataTime = new DataTime( new Date(0) ); 
	
	public ResourceName() {
		rscCategory = "";
		rscType = "";
		rscGroup = "";
		rscAttrSetName = "";
		cycleTime = null;
	}
	
	// parse the full resource name
	public ResourceName( String rName ) {
		if( rName == null || rName.isEmpty() ) 
			return;
		
		setFullResourceName( rName );
	}
	
	public ResourceName( String cat, String type, String attrSet ) {
		
		setFullResourceName( cat + File.separator + 
							 type + File.separator + 
							 (attrSet == null ? dfltAttrSetName : attrSet ) );				
	}

	public ResourceName( ResourceName rn ) {
		super();
		if( rn == null ) {
			return;
		}
		
		rscCategory = rn.getRscCategory();
		rscType     = rn.getRscType();
		rscGroup    = rn.getRscGroup();
		rscAttrSetName = rn.getRscAttrSetName();
		if( rn.getCycleTime() == null ) {
			cycleTime = null;
		}
		else if( rn.getCycleTime() == LatestDataTime ) {
			cycleTime = LatestDataTime;
		}
		else { 
			cycleTime = new DataTime( rn.getCycleTime().getRefTime() );
		}
	}
		
	public ResourceName( String cat, String type, String group, String attrSet ) {
		
		setFullResourceName( cat.trim() + File.separator + 
							 type.trim() + File.separator + 
							 group.trim() + File.separator +
							 (attrSet == null ? dfltAttrSetName : attrSet.trim() ) );				
	}

	public void setFullResourceName(String rscName) {
		String[] parts = rscName.split( File.separator );
		// at minimum we need a c
		if( parts == null || parts.length < 3 ) {
			System.out.println("Unrecognized Resource Name : " + rscName );
			return;
		}
		
		rscCategory = parts[0].trim();
		rscType     = parts[1].trim();
		rscAttrSetName = parts[ parts.length-1 ].trim();
		
		// forecast resources have the cycle time after the attrSet without a separator
		//
		int parenIndx = rscAttrSetName.indexOf('(');
		
		if( parenIndx != -1 ) {
			String cycleTimeStr = rscAttrSetName.substring( parenIndx+1, 
					                              rscAttrSetName.indexOf(')' ) );
			setCycleTimeFromString( cycleTimeStr );
			
			rscAttrSetName = rscAttrSetName.substring(0, parenIndx );
			rscAttrSetName = rscAttrSetName.trim();
		}
		else {
			cycleTime = null;
		}
		
		if( parts.length > 3 ) {
			rscGroup = parts[2].trim();
		}
		else {
			rscGroup = "";
		}
		
//		if( parts.length > 5 ) {
//			System.out.println("Unrecognized Resource Name : " + rscName );
//		}
	}
	
	// ??? call method on the ResourceDefnsMngr to check that this is valid
	//
	public boolean isValid() {
		try {
			return  ResourceDefnsMngr.getInstance().isResourceNameValid( this );
		}
		catch( VizException vizex ) {
			return false;
		}
	}

	public String getRscCategory() {
		return (rscCategory == null ? "" : rscCategory);
	}

	public void setRscCategory(String rscCategory) {
		this.rscCategory = (rscCategory == null ? "" : rscCategory.trim() );
	}

	public String getRscType() {
		return (rscType == null ? "" : rscType);
	}

	public void setRscType(String rscType) {
		this.rscType = (rscType == null ? "" : rscType.trim() );
	}

	public String getRscGroup() {
		return (rscGroup == null ? "" : rscGroup);
	}

	public void setRscGroup(String rscGroup) {
		this.rscGroup = (rscGroup == null ? "" : rscGroup.trim());
	}

	public String getRscAttrSetName() {
		return (rscAttrSetName == null ? "" : rscAttrSetName);
	}

	public void setRscAttrSetName(String rscAttrSetName) {
		this.rscAttrSetName = (rscAttrSetName == null ? "" : rscAttrSetName.trim() );
	}
	
	public DataTime getCycleTime() {
		return cycleTime;
	}

	public String getCycleTimeString() {
		if( cycleTime == LatestDataTime ) {
			return LatestCycleTime;
		}
		return NmapCommon.getTimeStringFromDataTime( cycleTime, "_" );
	}

	public void setCycleTime( DataTime cycleTime) {
		this.cycleTime = cycleTime;
	}

	public void setCycleTimeLatest() {
		this.cycleTime = LatestDataTime;
	}

	public boolean isLatestCycleTime() {
		return (cycleTime == LatestDataTime);
	}
	
	public void setCycleTimeFromString( String cycTimeStr ) {
		if( cycTimeStr == null || cycTimeStr.isEmpty() ) {
			cycleTime = null;
			return;
		}
		if( cycTimeStr.equals( LatestCycleTime ) ) {
			cycleTime = LatestDataTime;
			return;
		}
		// else this will be in the format as created from NmapCommon.
		cycleTime = NmapCommon.parseDataTimeFromCycleTimeString( cycTimeStr );		
	}
	
	public boolean isForecastResource() {
		return (cycleTime != null);
	}
	
	// public boolean isForecastResource() return if cycleTimes is set?
	
	public boolean isPgenResource() {
		return (rscCategory != null && rscCategory.equals( PGENRscCategory ) );
	}

	public boolean isOverlayResource() {
		return (rscCategory != null && rscCategory.equals( OverlayRscCategory ) );
	}

	public String getAbbreviatedName() {
		if( !isValid() ) {
			return "";
		}
		if( !getRscGroup().isEmpty() ) {
			return rscType + File.separator + rscGroup;
		}
		
		return rscCategory + File.separator + rscType;
	}
	
	public String toString() {
		if( rscCategory == null || rscCategory.isEmpty() ||
			rscType     == null || rscType.isEmpty() ||
			rscAttrSetName == null || rscAttrSetName.isEmpty() ) {
			return "";
		}
		else {
//			String str = rscCategory + File.separator + rscType + File.separator +
//			        (!rscGroup.isEmpty() ? rscGroup + File.separator : "" ) +
//			            rscAttrSetName +
//			        (!cycleTime.isEmpty() ? "("+cycleTime +")" : "" ) ;
			String str = rscCategory + File.separator + rscType + File.separator +
	        		(!rscGroup.isEmpty() ? rscGroup + File.separator : "" ) +
	            		rscAttrSetName;
			if( cycleTime  == null ) {
				return str;
			}
			else if( cycleTime == LatestDataTime ) {
				return str + '(' + LatestCycleTime + ')';
			}
			else {
				return str + '(' +NmapCommon.getTimeStringFromDataTime( cycleTime, "_" ) + ')';
			}
		}
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((cycleTime == null) ? 0 : cycleTime.hashCode());
		result = prime * result
				+ ((rscAttrSetName == null) ? 0 : rscAttrSetName.hashCode());
		result = prime * result
				+ ((rscCategory == null) ? 0 : rscCategory.hashCode());
		result = prime * result
				+ ((rscGroup == null) ? 0 : rscGroup.hashCode());
		result = prime * result + ((rscType == null) ? 0 : rscType.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		ResourceName other = (ResourceName) obj;
		if (cycleTime == null) {
			if (other.cycleTime != null)
				return false;
		} else if (!cycleTime.equals(other.cycleTime))
			return false;
		if (rscAttrSetName == null) {
			if (other.rscAttrSetName != null)
				return false;
		} else if (!rscAttrSetName.equals(other.rscAttrSetName))
			return false;
		if (rscCategory == null) {
			if (other.rscCategory != null)
				return false;
		} else if (!rscCategory.equals(other.rscCategory))
			return false;
		if (rscGroup == null) {
			if (other.rscGroup != null)
				return false;
		} else if (!rscGroup.equals(other.rscGroup))
			return false;
		if (rscType == null) {
			if (other.rscType != null)
				return false;
		} else if (!rscType.equals(other.rscType))
			return false;
		return true;
	}
	
	
}