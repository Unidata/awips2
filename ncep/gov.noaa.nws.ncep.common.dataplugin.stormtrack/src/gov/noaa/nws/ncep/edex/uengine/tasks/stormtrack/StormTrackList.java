/**
 * StormTrackList
 * 
 * Date created 07 Sep 2010
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.edex.uengine.tasks.stormtrack;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * StormTrackList
 * 
  * <pre>
 * SOFTWARE HISTORY
 *    Date        		Ticket#		Engineer			Description
 * ------------------	---------- 	-----------------	-------------------
 * 07- Sep-2010         284      	mgamazaychikov     	Initial creation
 * 8/2011							T. Lee				Renamed from AtcfCyclone
 * 
 * @author mgamazaychikov
 *</pre>
 */



@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class StormTrackList implements ISerializableObject {
	
	@XmlElement
	@DynamicSerializeElement
	private String cyclone;
	
	@XmlElement
	@DynamicSerializeElement
	private List<StormTrack> trackList;
	
	public StormTrackList() {
		trackList = new ArrayList<StormTrack>(0);
	}
	
	public StormTrackList(String cyclone) {
		super();
		trackList = new ArrayList<StormTrack>(0);
		this.cyclone = cyclone;
	}
	public String getCyclone() {
		return cyclone;
	}
	public void setCyclone(String cyclone) {
		this.cyclone = cyclone;
	}
	public List<StormTrack> getTrackList() {
		return trackList;
	}

	public void setTrackList(List<StormTrack> aList) {
		this.trackList = new ArrayList<StormTrack>(aList);
	}	
	
	public void addTrack(StormTrack newTrack){
		this.trackList.add(newTrack);
	}

}

