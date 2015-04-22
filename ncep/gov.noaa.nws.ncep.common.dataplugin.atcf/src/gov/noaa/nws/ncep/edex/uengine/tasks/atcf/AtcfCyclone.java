/**
 * AtcfCyclone
 * 
 * Date created 07 Sep 2010
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.edex.uengine.tasks.atcf;

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
 * Atcf Cyclone
 * 
  * <pre>
 * SOFTWARE HISTORY
 *    Date                  Ticket#     Engineer                  Description
 * ------------------      ----------   ----------------------- --------------------------
 * 07- Sep-2010         284      mgamazaychikov     Initial creation.
 * @author mgamazaychikov
 *</pre>
 */



@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AtcfCyclone implements ISerializableObject {
	
	@XmlElement
	@DynamicSerializeElement
	private String cyclone;
	
	@XmlElement
	@DynamicSerializeElement
	private List<AtcfTrack> trackList;
	
	public AtcfCyclone() {
		trackList = new ArrayList<AtcfTrack>(0);
	}
	
	public AtcfCyclone(String cyclone) {
		super();
		trackList = new ArrayList<AtcfTrack>(0);
		this.cyclone = cyclone;
	}
	public String getCyclone() {
		return cyclone;
	}
	public void setCyclone(String cyclone) {
		this.cyclone = cyclone;
	}
	public List<AtcfTrack> getTrackList() {
		return trackList;
	}

	public void setTrackList(List<AtcfTrack> aList) {
		this.trackList = new ArrayList<AtcfTrack>(aList);
	}	
	
	public void addTrack(AtcfTrack newTrack){
		this.trackList.add(newTrack);
	}
//	public void setTrack(AtcfTrack track) {
//		this.track = track;
//	}

}

