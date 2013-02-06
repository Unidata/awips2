package gov.noaa.nws.ncep.viz.gempak.grid.units;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "gempakGridParmUnitSet")
@XmlAccessorType(XmlAccessType.NONE)
public class GempakGridParmInfoSet implements ISerializableObject{
	/**
     * List of parameter for/from the XML.
     */
    @XmlElements( { @XmlElement(name = "parmeterinfo", type = GempakGridParmInfo.class) })
    private ArrayList<GempakGridParmInfo> parmeterinfo;

	public ArrayList<GempakGridParmInfo> getParmeterinfo() {
		return parmeterinfo;
	}

	public void setParmeterinfo(ArrayList<GempakGridParmInfo> parmeterinfo) {
		this.parmeterinfo = parmeterinfo;
	}
}
