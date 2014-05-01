/*
 * TcaAttrInfo
 * 
 * Date created 15 SEPTEMBER 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Contains possible selections for each of the pulldown menus in the 
 * TCA Attributes GUI Dialog
 * @author sgilbert
 *
 */
@XmlRootElement(name="TCAAttributeInfo")
@XmlAccessorType(XmlAccessType.NONE)
public class TcaAttrInfo implements ISerializableObject {

	@XmlElementWrapper(name="issuingStatus")
	@XmlElements( @XmlElement(name="entry", type=String.class) )
	private String[] statusList;

	@XmlElementWrapper(name="stormTypes")
	@XmlElements( @XmlElement(name="entry", type=String.class) )
	private String[] typeList;

	@XmlElementWrapper(name="basins")
	@XmlElements( @XmlElement(name="entry", type=String.class) )
	private String[] basinList;	

	@XmlElementWrapper(name="timeZones")
	@XmlElements( @XmlElement(name="entry", type=String.class) )
	private String[] timezones;

	@XmlElementWrapper(name="advisorySeverity")
	@XmlElements( @XmlElement(name="entry", type=String.class) )
	private String[] severityList;

	@XmlElementWrapper(name="advisoryTypes")
	@XmlElements( @XmlElement(name="entry", type=String.class) )
	private String[] advisoryList;	

	@XmlElementWrapper(name="breakpointTypes")
	@XmlElements( @XmlElement(name="entry", type=String.class) )
	private String[] breakpointTypeList;	

	@XmlElementWrapper(name="geographyTypes")
	@XmlElements( @XmlElement(name="entry", type=String.class) )
	private String[] geographyTypeList;

	/**
	 * Default no-arg constructor
	 */
	public TcaAttrInfo() {
		//no op
	}

	/**
	 * @return the list of Issuing statuses that can be used for the TCV 
	 */
	public String[] getStatusList() {
		return statusList;
	}

	/**
	 * @param statusList the list of Issuing statuses that can be used for the TCV
	 */
	public void setStatusList(String[] statusList) {
		this.statusList = statusList;
	}

	/**
	 * @return the list of tropical cyclone types
	 */
	public String[] getTypeList() {
		return typeList;
	}

	/**
	 * @param typeList the list of tropical cyclone types
	 */
	public void setTypeList(String[] typeList) {
		this.typeList = typeList;
	}

	/**
	 * @return the list of Storm basins
	 */
	public String[] getBasinList() {
		return basinList;
	}

	/**
	 * @param basinList the list of storm basins
	 */
	public void setBasinList(String[] basinList) {
		this.basinList = basinList;
	}

	/**
	 * @return the list of time zones
	 */
	public String[] getTimezones() {
		return timezones;
	}

	/**
	 * @param timezones the list of time zones
	 */
	public void setTimezones(String[] timezones) {
		this.timezones = timezones;
	}

	/**
	 * @return the list of watch/warning severities
	 */
	public String[] getSeverityList() {
		return severityList;
	}

	/**
	 * @param severityList the list of watch/warning severities
	 */
	public void setSeverityList(String[] severityList) {
		this.severityList = severityList;
	}

	/**
	 * @return the list of advisory types
	 */
	public String[] getAdvisoryList() {
		return advisoryList;
	}

	/**
	 * @param advisoryList the list of advisory types
	 */
	public void setAdvisoryList(String[] advisoryList) {
		this.advisoryList = advisoryList;
	}

	/**
	 * @return the list of breakpoint types
	 */
	public String[] getBreakpointTypeList() {
		return breakpointTypeList;
	}

	/**
	 * @param breakpointTypeList the list of breakpoint Types
	 */
	public void setBreakpointTypeList(String[] breakpointTypeList) {
		this.breakpointTypeList = breakpointTypeList;
	}

	/**
	 * @return the list of special geography Types
	 */
	public String[] getGeographyTypeList() {
		return geographyTypeList;
	}

	/**
	 * @param geographyTypeList the list of special geography Types
	 */
	public void setGeographyTypeList(String[] geographyTypeList) {
		this.geographyTypeList = geographyTypeList;
	}
	
	
}
