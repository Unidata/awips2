package gov.noaa.nws.ncep.edex.common.sounding;
/**
 * 
 * gov.noaa.nws.ncep.edex.common.sounding.NcSoundingCube
 * 
 * This java class provides sounding data data structure for used with NC sounding query.
 * Each NcSoundingCube contains a list (its size could be one, or more elements) of NcSoundingProfile.
 * Each NcSoundingProfile is for one point (one point represent one location with lat/lon specified) at one particular request time (timeLine).
 * The size of NcSoundingProfile-list will be the size of request point-list.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 11/18/2010	TBD			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcSoundingCube implements ISerializableObject {
	public enum QueryStatus {
		OK, FAILED, LOCATION_NOT_FOUND
	}
	
	@DynamicSerializeElement
    private static final long serialVersionUID = 1324632468L;

	@DynamicSerializeElement
	private List<NcSoundingProfile> soundingProfileList;
	
	@DynamicSerializeElement
	private QueryStatus rtnStatus;
	
	public QueryStatus getRtnStatus() {
		return rtnStatus;
	}

	public void setRtnStatus(QueryStatus rtnStatus) {
		this.rtnStatus = rtnStatus;
	}

	public List<NcSoundingProfile> getSoundingProfileList() {
		return soundingProfileList;
	}

	public void setSoundingProfileList(List<NcSoundingProfile> soundingProfileList) {
		this.soundingProfileList = soundingProfileList;
	}

	public NcSoundingCube() {
		super();
		this.soundingProfileList = new ArrayList<NcSoundingProfile>();
	}

	public NcSoundingCube(List<NcSoundingProfile> soundingProfileList) {
		super();
		this.soundingProfileList = soundingProfileList;
	}

	

}
