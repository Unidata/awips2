package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Defines a location (latitude and longitude) of a station.
 * 
 *<pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ----------- --------------------------
 * 03/29/2013   975        sgurung     Initial Creation 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
@XmlRootElement(name = "headerFormat")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "")
public class HeaderFormat {
	
	@XmlElement(name = "regex")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
	protected String pattern;
	
	@XmlElement(name = "group")
	protected Group[] group;
	
	public HeaderFormat() {
		
	}
	
	public String getPattern() {
		return pattern;
	}
	
	public void setPattern(String pattern) {
		this.pattern = pattern;
	}	
	
	public Group[] getGroup() {
		return group;
	}
	
	public void setGroup(Group[] group) {
		this.group = group;
	}	

}
