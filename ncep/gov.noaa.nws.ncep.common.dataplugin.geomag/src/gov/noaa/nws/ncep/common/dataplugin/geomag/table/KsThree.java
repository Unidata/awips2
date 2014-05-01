package gov.noaa.nws.ncep.common.dataplugin.geomag.table;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
/*
 * The KStationCoeffTable Reader.
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
@XmlRootElement(name = "ksThree")
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(name = "")
public class KsThree {

	@XmlAttribute
	private String period;
	
	@XmlAttribute
	private String season;
	
	@XmlAttribute
	private int k0;
	
	@XmlAttribute
	private int k1;
	
	@XmlAttribute
	private int k2;
	
	@XmlAttribute
	private int k3;
	
	@XmlAttribute
	private int k4;
	
	@XmlAttribute
	private int k5;
	
	@XmlAttribute
	private int k6;
	
	@XmlAttribute
	private int k7;
	
	@XmlAttribute
	private int k8;
	
	@XmlAttribute
	private int k9;
	
	
	public String getSeason() {
		return season;
	}	
	public void setSeason(String season) {
		this.season = season;
	}
		
	public String getPeriod() {
		return period;
	}	
	public void setPeriod(String period) {
		this.period = period;
	}

	public int getK0() {
		return k0;
	}	
	public void setK0(int k0) {
		this.k0 = k0;
	}
	
	public int getK1() {
		return k1;
	}	
	public void setK1(int k1) {
		this.k1 = k1;
	}
	
	public int getK2() {
		return k2;
	}	
	public void setK2(int k2) {
		this.k2 = k2;
	}
	
	public int getK3() {
		return k3;
	}	
	public void setK3(int k3) {
		this.k3 = k3;
	}
	
	public int getK4() {
		return k4;
	}	
	public void setK4(int k4) {
		this.k4 = k4;
	}
	
	public int getK5() {
		return k5;
	}	
	public void setK5(int k5) {
		this.k5 = k5;
	}
	
	public int getK6() {
		return k6;
	}	
	public void setK6(int k6) {
		this.k6 = k6;
	}
	
	public int getK7() {
		return k7;
	}	
	public void setK7(int k7) {
		this.k7 = k7;
	}
	
	public int getK8() {
		return k8;
	}	
	public void setK8(int k8) {
		this.k8 = k8;
	}
	
	public int getK9() {
		return k9;
	}	
	public void setK9(int k9) {
		this.k9 = k9;
	}	
}
