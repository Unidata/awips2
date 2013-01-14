package gov.noaa.nws.ncep.viz.rsc.ncgrid.util;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "gempakGridVcordUnitSet")
@XmlAccessorType(XmlAccessType.NONE)
public class GempakGridVcrdInfoSet implements ISerializableObject{
	/**
     * List of vcord for/from the XML.
     */
    @XmlElements( { @XmlElement(name = "vcordinfo", type = GempakGridVcrdInfo.class) })
    private ArrayList<GempakGridVcrdInfo> vcordinfo;

	public ArrayList<GempakGridVcrdInfo> getVcordinfo() {
		return vcordinfo;
	}

	public void setVcordinfo(ArrayList<GempakGridVcrdInfo> vcordinfo) {
		this.vcordinfo = vcordinfo;
	}
}
