package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/*
 * The FitTime.
 * 
 * <pre>
 * SOFTWARE HISTORY
 *                   
 * ate          Ticket#     Engineer   Description
 * -----------  ----------  ---------- --------------------------
 * 05/14/2013   #989        qzhou      Initial Creation
 * </pre>
 * 
 * @author qzhou
 * @version 1
 */

@XmlRootElement(name = "fitTime")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "")
public class KFitTime {

	@XmlAttribute
	private String key;
	
	@XmlAttribute
	private float coeffA;
	
	@XmlAttribute
	private float coeffB;
	
	@XmlAttribute
	private float coeffW;
	
	
	public String getKey() {
		return key;
	}
	
	public void setKey(String key) {
		this.key = key;
	}
		
	public float getCoeffA() {
		return coeffA;
	}
	
	public void setCoeffA(float coeffA) {
		this.coeffA = coeffA;
	}
	
	public float getCoeffB() {
		return coeffB;
	}
	
	public void setCoeffB(float coeffB) {
		this.coeffB = coeffB;
	}		
	
	public float getCoeffW() {
		return coeffW;
	}
	
	public void setCoeffW(float coeffW) {
		this.coeffW = coeffW;
	}

}
