package gov.noaa.nws.ncep.viz.common.area;

import java.util.ArrayList;
import java.util.List;

import gov.noaa.nws.ncep.viz.common.area.AreaName.AreaSource;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class AreaMenus implements ISerializableObject {

	@XmlAttribute
	private Boolean showDisplayAreas=false;
	
	@XmlAttribute
	private Boolean showImageBasedResourceAreas=true;
	
//	@XmlAttribute 
//	private Boolean  ;
	
	@XmlRootElement
	@XmlAccessorType(XmlAccessType.NONE)
	public static class AreaMenuItem implements ISerializableObject {

		@XmlAttribute 
		private String menuName;

		// Optional
		@XmlAttribute
		private String subMenuName="";

		// optional defaulting to false. If true then the current display area is recentered 
		// on the mapCenter of the given area. The current projection is kept but may have
		// its central meridian or latitude_of_origin set based on the center point.
		//  
		// NOT IMPLEMENTED. 
		// would like to add the capability to set/override either the projection (with flag to indicate the current projection),
		// and or the extents/width of the area. 
		// would need to add one or more extra attributes here and extra cmd parameters to the area command and handler.
		@XmlAttribute
		private Boolean recenterOnly;

		// must be a valid area source provider as given by the areaProvider extension.
		// 
		@XmlElement
		private String source;

		// 
		// if not given, then the name of the menuName.
		// 
		@XmlElement
		private String areaName;

		public AreaMenuItem() {
		}

		public AreaMenuItem( AreaName aName ) {
			subMenuName = "";
			areaName = aName.getName();
			source = aName.getSource().toString();
			menuName = (aName.getSource() == AreaSource.INITIAL_DISPLAY_AREA ? "Custom" : aName.getName() );
		}

		public AreaMenuItem( String menu, String subMenu, String area, String src ) {
			setSubMenuName( subMenu );
			areaName = area;
			setSource( src );
			menuName = (AreaSource.INITIAL_DISPLAY_AREA.toString().equals( source ) ? "Custom" :menu );
		}


		public String getMenuName() {
			return (menuName == null || menuName.isEmpty() ? areaName : menuName);
		}

		public void setMenuName(String menuName) {
			this.menuName = menuName.trim();
		}

		public String getSubMenuName() {
			return (subMenuName==null ? "" : subMenuName);
		}

		public void setSubMenuName(String subMenuName) {
			this.subMenuName = subMenuName.trim();
		}

		public String getSource() {
			return (source == null ? AreaSource.PREDEFINED_AREA.toString() : source);
		}

		public void setSource(String source) {
			this.source = source.trim();
		}

		public String getAreaName() {
			return areaName;
		}

		public void setAreaName(String areaName) {
			this.areaName = areaName.trim();
		}
		
		public Boolean getRecenterOnly() {
			return recenterOnly;
		}

		public void setRecenterOnly(Boolean recenterOnly) {
			this.recenterOnly = recenterOnly;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result
					+ ((areaName == null) ? 0 : areaName.hashCode());
			result = prime * result
					+ ((menuName == null) ? 0 : menuName.hashCode());
			result = prime * result
					+ ((source == null) ? 0 : source.hashCode());
			result = prime * result
					+ ((subMenuName == null) ? 0 : subMenuName.hashCode());
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
			AreaMenuItem other = (AreaMenuItem) obj;
			if (areaName == null) {
				if (other.areaName != null)
					return false;
			} else if (!areaName.equals(other.areaName))
				return false;
			if (menuName == null) {
				if (other.menuName != null)
					return false;
			} else if (!menuName.equals(other.menuName))
				return false;
			if (source == null) {
				if (other.source != null)
					return false;
			} else if (!source.equals(other.source))
				return false;
			if (subMenuName == null) {
				if (other.subMenuName != null)
					return false;
			} else if (!subMenuName.equals(other.subMenuName))
				return false;
			return true;
		}		
	}
	
	@XmlElement(name = "AreaMenuItem")
	private List<AreaMenuItem> areaMenuItems;

	public AreaMenus( ) {
		areaMenuItems = new ArrayList<AreaMenuItem>();
	}
	
	public List<AreaMenuItem> getAreaMenuItems() {
		return areaMenuItems;
	}

	public void addAreaMenuItem( AreaMenuItem ami ) {
		areaMenuItems.add( ami );
	}

	public Boolean getShowImageBasedResourceAreas() {
		return showImageBasedResourceAreas;
	}

	public void setShowImageBasedResourceAreas(Boolean showImageBasedResourceAreas) {
		this.showImageBasedResourceAreas = showImageBasedResourceAreas;
	}

	public Boolean getShowDisplayAreas() {
		return showDisplayAreas;
	}

	public void setShowDisplayAreas(Boolean showDisplayAreas) {
		this.showDisplayAreas = showDisplayAreas;
	}

	public void setAreaMenuItems(List<AreaMenuItem> areaMenuItems) {
		this.areaMenuItems = areaMenuItems;
	}	
}
