package gov.noaa.nws.ncep.edex.common.sounding;
/**
 * 
 * gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfoCollection
 * 
 * This java class provides sounding data data structure for used with NC sounding query.
 * Each NcSoundingStnInfoCollection contains a list (size could be zero) of NcSoundingStnInfo.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 11/15/2010	TBD			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import java.sql.Timestamp;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcSoundingStnInfoCollection implements  ISerializableObject{
	@DynamicSerializeElement
    private static final long serialVersionUID = 1324632468L;

	@DynamicSerializeElement
    private NcSoundingStnInfo[] stationInfo;

	public NcSoundingStnInfo[] getStationInfo() {
		return stationInfo;
	}

	public void setStationInfo(NcSoundingStnInfo[] stationInfo) {
		this.stationInfo = stationInfo;
	}
	
	public NcSoundingStnInfo getNewStnInfo(){
		return new NcSoundingStnInfo();
	}

	
}
