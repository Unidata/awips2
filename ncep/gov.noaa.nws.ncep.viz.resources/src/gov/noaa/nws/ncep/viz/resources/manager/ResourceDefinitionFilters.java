package gov.noaa.nws.ncep.viz.resources.manager;

import gov.noaa.nws.ncep.viz.common.StringListAdapter;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;


@XmlRootElement(name = "ResourceDefinitionFilters")
@XmlAccessorType(XmlAccessType.NONE)
public class ResourceDefinitionFilters {

	@XmlRootElement(name = "ResourceDefinitionFilter")
	@XmlAccessorType(XmlAccessType.NONE)
	public static class ResourceDefinitionFilter {

		@XmlAttribute
		private String rscDefnName="";

		@XmlAttribute
		private Boolean isEnabled;
		
		@XmlElement 
		@XmlJavaTypeAdapter(StringListAdapter.class)
		private ArrayList<String> filters;
		
		private 
		LocalizationLevel locLevel = LocalizationLevel.USER;

		public ResourceDefinitionFilter() {
			filters = new ArrayList<String>();
		}

		public ResourceDefinitionFilter( 
				String rscType, Boolean enabled, List<String> fList, LocalizationLevel lvl ) {
			rscDefnName = rscType;
			isEnabled = enabled;
			locLevel = lvl;
			filters = ( fList == null ? new ArrayList<String>() : new ArrayList<String>( fList ) );
		}

		public String getRscDefnName() {
			return rscDefnName;
		}

		public void setRscDefnName(String rscDefnName) {
			this.rscDefnName = rscDefnName;
		}

		public Boolean getIsEnabled() {
			return isEnabled;
		}

		public void setIsEnabled(Boolean isEnabled) {
			this.isEnabled = isEnabled;
		}

		public ArrayList<String> getFilters() {
			return filters;
		}

		public void setFilters(ArrayList<String> filters) {
			this.filters = filters;
		}
		
		public Boolean testFilter( String filtStr ) {
			return filters.contains( filtStr );
		}

		public LocalizationLevel getLocLevel() {
			return locLevel;
		}

		public void setLocLevel(LocalizationLevel locLevel) {
			this.locLevel = locLevel;
		}
	}
	
	// for RDs that may not have an entry in the list.
	@XmlAttribute
	private Boolean defaultRscDefnEnableStatus = false;
	
    @XmlElement(name = "ResourceDefinitionFilter", required = true)
    private List<ResourceDefinitionFilter> rscDefnFilterList;
   	
    public ResourceDefinitionFilters() {
    	defaultRscDefnEnableStatus = false;
		rscDefnFilterList = new ArrayList<ResourceDefinitionFilter>();
	}
	
    public List<ResourceDefinitionFilter> getResourceDefinitionFiltersList() {
    	return rscDefnFilterList;
    }

	public Boolean getDefaultRscDefnEnableStatus() {
		return defaultRscDefnEnableStatus;
	}

	public void setDefaultRscDefnEnableStatus(Boolean defaultRscDefnEnableStatus) {
		this.defaultRscDefnEnableStatus = defaultRscDefnEnableStatus;
	}
    
}