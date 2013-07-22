package gov.noaa.nws.ncep.viz.common.area;


import java.io.File;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

// beef this up and use it more.....
public  class AreaName {
	
	// TODO : could have an extention point to define AreaSources and factory classes to 
	// create them.
	@XmlRootElement
	@XmlAccessorType(XmlAccessType.NONE)
	public static class AreaSource {

		@XmlElement
		private String sourceName="";
		
		private Boolean isImageBased = false;

		private static Map<String,AreaSource> areaSources=new HashMap<String,AreaSource>();

		// the area is a jaxb representation of a PredefinedArea object in PredefinedAreas localization
		public static AreaSource PREDEFINED_AREA = createAreaSource( "PREDEFINED_AREA");  // saved as a PredefinedArea

		// the area is defined by the current area of a loaded display pane (ie MapRenderableDisplay)
		public static AreaSource DISPLAY_AREA = createAreaSource( "DISPLAY_AREA");
		
		// the area is the initial area of a pane saved in an RBD. (also a MapRenderableDisplay object)
		// (this is used when the user creates a 'custom' area by selecting a DISPLAY_AREA area when creating an RBD.) 
		public static AreaSource INITIAL_DISPLAY_AREA = createAreaSource( "INITIAL_DISPLAY_AREA" );
		
		public static AreaSource UNKNOWN_SOURCE = createAreaSource( "UNKNOWN_SOURCE");

		// other sources defined in the areaProviders extention point. 
//		MCIDAS_AREA_NAME,
//		GINI_SECTOR_ID,
//		GEMPAK_GEOG_AREA,
//		GEMPAK_SFSTN_AREA
//      METAR_STATION_ID, stations from the common_obs_spatial db (also SYNOP_STATION_ID, UAIR_STATION_ID...etc)
				
		public AreaSource() {
			// needed for Jaxb, but should not be called otherwise.
		}
		
		private AreaSource( String srcName, Boolean imgBased ) {
			sourceName = srcName;
			isImageBased = imgBased;
		}
		
		public static AreaSource createAreaSource( String src ) {
			src = src.trim();
			if( !areaSources.containsKey( src ) ) {
				areaSources.put( src, new AreaSource( src, false ) );
			}
			return areaSources.get( src );
		}
		
		public static AreaSource createImagaeBasedAreaSource( String src ) {
			src = src.trim();
			if( !areaSources.containsKey( src ) ) {
				areaSources.put( src, new AreaSource( src, true ) );
			}
			return areaSources.get( src );
		}
		
		public static AreaSource getAreaSource( String src ) {
			if( src == null || src.trim().isEmpty() || !areaSources.containsKey(src.trim()) ) {
				return UNKNOWN_SOURCE;
			}
			return areaSources.get( src.trim() );
		}
		
		public Boolean isImagedBased() {
			return isImageBased;
		}
		
		public String toString() {
			return sourceName;
		}
		
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((isImageBased == null) ? 0 : isImageBased.hashCode());
			result = prime * result + ((sourceName == null) ? 0 : sourceName.hashCode());
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
			AreaSource other = (AreaSource) obj;
			if (isImageBased == null) {
				if (other.isImageBased != null)
					return false;
			} else if (!isImageBased.equals(other.isImageBased))
				return false;
			if (sourceName == null) {
				if (other.sourceName != null)
					return false;
			} else if (!sourceName.equals(other.sourceName))
				return false;
			return true;
		}
	}

	private String name;
	private AreaSource source;
	public AreaName( AreaSource s, String n ) { name = n; source = s; }
	public static AreaName parseAreaNaem( String str ) {
		int i = str.indexOf( File.separator );
		if( i == -1 ) { return null; }
		return new AreaName( 
				AreaSource.getAreaSource( str.substring(0, i)), str.substring( i+1 ) );
	}
	
	public String     getName()   { return name; }
	public AreaSource getSource() { return source; }

	public String toString() { return source.toString()+File.separator+name; }
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj) return true;
		if (obj == null) return false;
		if (getClass() != obj.getClass()) return false;
		AreaName other = (AreaName) obj;
		if (name == null) {
			if (other.name != null) return false;
		} else if (!name.equals(other.name)) return false;
		if (source == null) {
			if (other.source != null) return false;
		} else if (!source.equals(other.source))
			return false;
		return true;
	}
}
