package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

@XmlRootElement(name = "dataFormat")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "")
public class DataFormat {
	
	@XmlAttribute
	protected Boolean conversionRequired;
	
	@XmlElement(name = "regex")
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
	protected String pattern;
	
	@XmlElement(name = "group")
	protected Group[] group;
	
	public DataFormat() {
		
	}
	
	public String getPattern() {
		return pattern;
	}
	
	public void setPattern(String pattern) {
		this.pattern = pattern;
	}
	
	public Boolean getConversionRequired() {
		return conversionRequired;
	}
	
	public void setConversionRequired(Boolean conversionRequired) {
		this.conversionRequired = conversionRequired;
	}	

	public Group[] getGroup() {
		return group;
	}
	
	public void setGroup(Group[] group) {
		this.group = group;
	}	
}
