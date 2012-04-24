package gov.noaa.nws.ncep.edex.common.sounding;
/**
 * 
 * gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile
 * 
 * This java class provides sounding data data structure for used with NC sounding query.
 * Each NcSoundingProfile contains a list (its size could be zero, one, or more elements) of NcSoundingLayer
 * for one point (one point represent one location with lat/lon specified) at one particular request time (timeLine).
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date             Ticket#    	Engineer       Description
 * -------		      ---------   ---------------    ------------------
 * 09/13/2010	362		   Chin Chen	 Initial coding
 * 12/16/2010   362         Chin Chen    add support of BUFRUA observed sounding and PFC (NAM and GFS) model sounding data
 * 09/14/2011   457         S. Gurung     Renamed ObsSndType.H5UAIR to ObsSndType.NCUAIR
 *10/06/2011    465         Archana        Added a list of NcSoundingLayer2 objects to the sounding profile
 * 02/15/2012               Chin Chen      added fcsTime to support pfc sounding query
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import java.io.Serializable;
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
public class NcSoundingProfile implements ISerializableObject{
	/**
	 * 
	 */
	private static final long serialVersionUID = 6858474095965608817L;

	@DynamicSerializeElement
    public static final float MISSING = -9999.f;
	public static enum PfcSndType {
        NAMSND, GFSSND, RUC2SND, RUCPTYPSND, BROWSE, NONE
    };
    public static enum MdlSndType {
        ANY,  NONE
    };
	public static enum ObsSndType {
        NCUAIR, UAIR, DROP, TAMDAR, BUFRUA //same as uair but using bufrua decoder and data is saved in HDF5 
        ,BROWSE, NONE
    };
    //Important Note:
    //Chin: definition is based on UAIR record. stnid is string of character, stnnum is string of number
    //BUFRUA has different definition. stnid is string of number which is stnnum in UAIR, 
    //and stnname is string of character which is stnid in UAIR..
    //type conversion is done at BUFRUA code.
    public static enum SndQueryKeyType {
    	LATLON, STNID, STNNUM, NONE
    };
    @DynamicSerializeElement
    private List<NcSoundingLayer> soundingLyLst;

    @DynamicSerializeElement
    private List<NcSoundingLayer2> soundingLyLst2;
    
	@DynamicSerializeElement
    private float stationElevation;
    //@DynamicSerializeElement
    //private String stationId;
    @DynamicSerializeElement
    private double	stationLatitude;
    @DynamicSerializeElement
    private double	stationLongitude;
    @DynamicSerializeElement
    private float sfcPress;
    
    @DynamicSerializeElement
    private String stationId;

    @DynamicSerializeElement
    private int stationNum;
    
    @DynamicSerializeElement
    private long fcsTime;

    @DynamicSerializeElement
    private NcSoundingCube.QueryStatus rtnStatus = NcSoundingCube.QueryStatus.OK;
    

	public NcSoundingCube.QueryStatus getRtnStatus() {
		return rtnStatus;
	}

	public void setRtnStatus(NcSoundingCube.QueryStatus rtnStatus) {
		this.rtnStatus = rtnStatus;
	}

	public String getStationId() {
		return stationId;
	}

	public void setStationId(String stnId) {
		this.stationId = stnId;
	}

	public int getStationNum() {
		return stationNum;
	}

	public void setStationNum(int stnNum) {
		this.stationNum = stnNum;
	}

	
	public long getFcsTime() {
		return fcsTime;
	}

	public void setFcsTime(long fcsTime) {
		this.fcsTime = fcsTime;
	}

	public List<NcSoundingLayer> getSoundingLyLst() {
		return soundingLyLst;
	}

	public void setSoundingLyLst(List<NcSoundingLayer> soundingLyLst) {
		this.soundingLyLst = soundingLyLst;
	}

    /**
	 * @return the soundingLyLst2
	 */
	public List<NcSoundingLayer2> getSoundingLyLst2() {
		return soundingLyLst2;
	}

	/**
	 * @param soundingLyLst2 the soundingLyLst2 to set
	 */
	public void setSoundingLyLst2(List<NcSoundingLayer2> soundingLyLst2) {
		this.soundingLyLst2 = soundingLyLst2;
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

	public float getSfcPress() {
		return sfcPress;
	}

	public void setSfcPress(float sfcPress) {
		this.sfcPress = sfcPress;
	}

	public static long getSerialVersionUID() {
		return serialVersionUID;
	}

	// TO-DO: Add station number (stationNumber)
	public NcSoundingProfile(List<NcSoundingLayer2> soundingLyLst2, 
			                                    List<NcSoundingLayer> soundingLyLst, 
			float stationElevation, String stationId, float stationLatitude,
			float stationLongitude, float sfcPress, int stnNum) {
		super();
		this.soundingLyLst2 = soundingLyLst2;
		this.soundingLyLst = soundingLyLst;
		this.stationElevation = stationElevation;
		this.stationId = stationId;
		this.stationLatitude = stationLatitude;
		this.stationLongitude = stationLongitude;
		this.sfcPress = sfcPress;
		this.stationNum = stnNum;
	}

	public NcSoundingProfile() {
		super();
		this.soundingLyLst = new ArrayList<NcSoundingLayer>();
		this.soundingLyLst2 = new ArrayList<NcSoundingLayer2>();
		this.stationElevation = MISSING;
		this.stationId = "";
		this.stationLatitude = MISSING;
		this.stationLongitude = MISSING;
		this.sfcPress = MISSING;
		this.stationNum= 0;
	}

	/*
	@Override
	protected AbstractStorageRecord cloneInternal() {
		// TODO Auto-generated method stub
		System.out.println("NcSoundingProfile cloneInternal()");
		return null;
	}

	@Override
	public Object getDataObject() {
		// TODO Auto-generated method stub
		System.out.println("NcSoundingProfile getDataObject()");
		return null;
	}

	@Override
	public int getSizeInBytes() {
		// TODO Auto-generated method stub
		System.out.println("NcSoundingProfile getSizeInBytes()");
		return 0;
	}

	@Override
	public void reduce(int[] indices) {
		// TODO Auto-generated method stub
		System.out.println("NcSoundingProfile ()");
	}

	@Override
	public boolean validateDataSet() {
		// TODO Auto-generated method stub
		System.out.println("NcSoundingProfile validateDataSet()");
		return false;
	}
    */
    
}

