package gov.noaa.nws.ncep.viz.rsc.ncgrid.util;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlAccessorType(XmlAccessType.NONE)
public class GempakGridVcrdInfo implements ISerializableObject{
	@XmlElement
    private String name;
	
	@XmlElement
    private String units;
	
	@XmlElement
    private String gnam;
	
	@XmlElement
    private int scale;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getUnits() {
		return units;
	}

	public void setUnits(String units) {
		this.units = units;
	}

	public String getGnam() {
		return gnam;
	}

	public void setGnam(String gnam) {
		this.gnam = gnam;
	}

	public int getScale() {
		return scale;
	}

	public void setScale(int scale) {
		this.scale = scale;
	}
}
