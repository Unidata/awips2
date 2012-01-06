package gov.noaa.nws.ncep.edex.common.sounding;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;
import java.util.ArrayList;
import java.util.List;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcSoundingModel implements  ISerializableObject{
	@DynamicSerializeElement
    private static final long serialVersionUID = 1324632469L;
	
	@DynamicSerializeElement
    private List<String> mdlList;

	public List<String> getMdlList() {
		return mdlList;
	}

	public void setMdlList(List<String> mdlList) {
		this.mdlList = mdlList;
	}

	public NcSoundingModel() {
		super();
		mdlList = new ArrayList<String>();
	}
	
}
