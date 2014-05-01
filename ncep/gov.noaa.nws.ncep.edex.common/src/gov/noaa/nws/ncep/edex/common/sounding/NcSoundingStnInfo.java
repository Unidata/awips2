package gov.noaa.nws.ncep.edex.common.sounding;
/**
 * 
 * gov.noaa.nws.ncep.edex.common.sounding.NcSoundingStnInfo
 * 
 * This java class provides sounding data data structure for used with NC sounding query.
 * Each NcSoundingStnInfo contains lat/lon/elv/stnId/synopTine for a station.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 11/15/2010	TBD			Chin Chen	Initial coding
 * 12/16/2010   362         Chin Chen   add support of BUFRUA observed sounding and PFC (NAM and GFS) model sounding data
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
public class NcSoundingStnInfo implements  ISerializableObject{
	@DynamicSerializeElement
    private static final long serialVersionUID = 1324632468L;

	@DynamicSerializeElement
    private float stationElevation;
    @DynamicSerializeElement
    //private float	stationLatitude;
    private double	stationLatitude;
    @DynamicSerializeElement
    //private float	stationLongitude;
    private double	stationLongitude;
    @DynamicSerializeElement
    private String stnId;
    @DynamicSerializeElement
    private Timestamp synopTime; // same as retTime 
    @DynamicSerializeElement
    private Timestamp rangeStartTime;
    
	public Timestamp getRangeStartTime() {
		return rangeStartTime;
	}
	public void setRangeStartTime(Timestamp rangeStartTime) {
		this.rangeStartTime = rangeStartTime;
	}
	public NcSoundingStnInfo() {
		super();
		// TODO Auto-generated constructor stub
	}
	public float getStationElevation() {
		return stationElevation;
	}
	public void setStationElevation(float stationElevation) {
		this.stationElevation = stationElevation;
	}
	public double getStationLatitude() {
		return stationLatitude;
	}
	public void setStationLatitude(double stationLatitude) {
		this.stationLatitude = stationLatitude;
	}
	public double getStationLongitude() {
		return stationLongitude;
	}
	public void setStationLongitude(double stationLongitude) {
		this.stationLongitude = stationLongitude;
	}
	public String getStnId() {
		return stnId;
	}
	public void setStnId(String stnId) {
		this.stnId = stnId;
	}
	public Timestamp getSynopTime() {
		return synopTime;
	}
	public void setSynopTime(Timestamp synopTime) {
		this.synopTime = synopTime;
	}
}

