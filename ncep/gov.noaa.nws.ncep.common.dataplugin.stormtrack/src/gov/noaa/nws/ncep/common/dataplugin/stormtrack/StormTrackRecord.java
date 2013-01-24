/*
 * 
 * StormTrackRecord
 * 
 * This class performs the mapping to the database tables for the Automated
 * Tropical Cyclone Forecast (ATCF) and ensemble storm tracks Decoder plug-in.
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer		Description
 * ------------	----------- --------------	-----------------------------------
 * 06/23/10 	283			F. J. Yen		Initial creation
 * *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * </pre>
 * 
 * @author F. J. Yen, SIB
 * @version 1
 
 */

package gov.noaa.nws.ncep.common.dataplugin.stormtrack;

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

/**
 * StormTrackRecord is the Data Access component for Automated Tropical Cyclone
 * Forecast (ATCF) and ensemble cyclone tracks. This class contains getters and 
 * setters for the main parent table stormtrack.
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 07/2011					T. Lee		ATCF and Ensemble storm tracks
 * 10/19/2011    858        Greg Hull   remove forecastHr
 * 
 * </pre>
 * 
 * @author tlee
 * @version 1.0
 */
@Entity
@Table(name = "stormtrack", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class StormTrackRecord extends PluginDataObject {

	private static final long serialVersionUID = 1L;
	private static final float RMISSD = IDecoderConstantsN.FLOAT_MISSING;
	private static final Integer IMISSD = IDecoderConstantsN.INTEGER_MISSING;

	/** Report type */
	@Column(length = 32)
	@XmlElement
	@DynamicSerializeElement
	@DataURI(position = 6)
	private String reportType;

	/**
	 * Basin, e.g. WP, IO, SH, CP, EP, AL, SL, ML
	 */
	@DataURI(position = 1)
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String basin;

	/**
	 * Annual cyclone number; 1 through 99
	 */
	@DataURI(position = 2)
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String cycloneNum;

	/**
	 * Warning Date-Time
	 */
	@Column
	@DynamicSerializeElement
	@XmlElement
	private Calendar warnTime;

	/**
	 * Objective technique sorting number/Minutes for best track: 00-99
	 */
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private int techniqueNum;

	/**
	 * Objective Technique or CARQ or WRNG, BEST for best Track
	 */
	@DataURI(position = 3)
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String model;

//	@DataURI(position = 4)
//	@Column
//	@DynamicSerializeElement
//	@XmlElement
//	private int fcstHour;

	/**
	 * Latitude (degrees) for the DTG: -900 through 900
	 */
	@DynamicSerializeElement
	@XmlElement
	private float clat;

	/**
	 * Longitude (degrees) for the DTG: -1800 through 1800
	 */
	@DynamicSerializeElement
	@XmlElement
	private float clon;

	/**
	 * Maximum sustained wind speed in knots: 0 through 300
	 */
	@DynamicSerializeElement
	@XmlElement
	private float windMax;

	/**
	 * Minimum sea level pressure, 1 through 1100MB
	 */
	@DynamicSerializeElement
	@XmlElement
	private float mslp;

	/**
	 * Level of tropical cyclone development; such as DB, TD, TS, TY, ...
	 */
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String stormType;

	/**
	 * Wind intensity (kts) for the radii defined i this record: 34, 50, 64
	 */
	@DataURI(position = 4)
	@DynamicSerializeElement
	@XmlElement
	private float windCategory;

	/**
	 * Radius Code for wind intensity: AAA = full circle; QQQ = quadrant (NNQ,
	 * NEQ, EEQ, SEQ, SSQ, SWQ, WWQ, NWQ)
	 */
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String windCode;

	/**
	 * If full circle, radius of specified wind intensity. If semicircle or
	 * quadrant, radius of specified wind intensity of circle portion specified
	 * in radius code. 0 - 1200 nm.
	 */
	@DynamicSerializeElement
	@XmlElement
	private float quad1WindRad;

	/**
	 * If full circle, this field not used. If semicircle, radius (nm) of
	 * specified wind intensity for semicircle not specified in radius code. If
	 * quadrant, radius (nm) of specified wind intensity for 2nd quadrant
	 * (counting clockwise from quadrant specified in radius code). 0 - 1200 nm.
	 */
	@DynamicSerializeElement
	@XmlElement
	private float quad2WindRad;

	/**
	 * If full circle or semicircle this field not used. If quadrant, radius
	 * (nm) of specified wind intensity for 3rd quadrant (counting clockwise
	 * from quadrant specified in radius code). 0 - 1200 nm.
	 */
	@DynamicSerializeElement
	@XmlElement
	private float quad3WindRad;

	/**
	 * If full circle or semicircle this field not used. If quadrant, radius
	 * (nm) of specified wind intensity for 4th quadrant (counting clockwise
	 * from quadrant specified in radius code). 0 - 1200 nm.
	 */
	@DynamicSerializeElement
	@XmlElement
	private float quad4WindRad;

	/**
	 * Pressure in millibars of the last closed isobar. 900 - 1050 mb
	 */
	@DynamicSerializeElement
	@XmlElement
	private float closedP;

	/**
	 * Radius of the last closed isobar in nm. 0 - 9999 nm
	 */
	@DynamicSerializeElement
	@XmlElement
	private float radClosedP;

	/**
	 * Radius of max winds. 0 - 999 nm
	 */
	@DynamicSerializeElement
	@XmlElement
	private float maxWindRad;

	/**
	 * Gusts. 0 - 995 kts.
	 */
	@DynamicSerializeElement
	@XmlElement
	private float gust;

	/**
	 * Eye diameter. 0 - 999nm
	 */
	@DynamicSerializeElement
	@XmlElement
	private float eyeSize;

	/**
	 * Subregion code: A, B, C, E, L, P, Q, S, W
	 */
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String subRegion;

	/**
	 * Max seas: 0 through 999 ft.
	 */
	@DynamicSerializeElement
	@XmlElement
	private float maxSeas;

	/**
	 * Forecaster's initials, used for forecastHour 0 WRNG, up to 3 chars
	 */
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String forecaster;

	/**
	 * Storm direction in compass coordinates, 0 - 359 degrees
	 */
	@DynamicSerializeElement
	@XmlElement
	private float stormDrct;

	/**
	 * Storm speed. 0 - 999 kts
	 */
	@DynamicSerializeElement
	@XmlElement
	private float stormSped;

	/**
	 * Literal storm name, NONAME, or INVEST
	 */
	@DataURI(position = 5)
	@Column(length = 32)
	@DynamicSerializeElement
	@XmlElement
	private String stormName;

	/**
	 * System depth, D=deep, M=medium, S=shallow, X=unknown
	 */
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String stormDepth;

	/**
	 * Wave height for radii defined in seas1 - seas4. 0 - 99 ft.
	 */
	@DynamicSerializeElement
	@XmlElement
	private float waveHght;

	/**
	 * Radius code for seas wave height
	 */
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String waveCode;

	/**
	 * First quadrant seas radius as defined by waveCode, 0 - 999 nm
	 */
	@DynamicSerializeElement
	@XmlElement
	private float quad1WaveRad;

	/**
	 * Second quadrant seas radius as defined by waveCode, 0 - 999 nm
	 */
	@DynamicSerializeElement
	@XmlElement
	private float quad2WaveRad;

	/**
	 * Third quadrant seas radius as defined by waveCode, 0 - 999 nm
	 */
	@DynamicSerializeElement
	@XmlElement
	private float quad3WaveRad;

	/**
	 * Fourth quadrant seas radius as defined by waveCode, 0 - 999 nm
	 */
	@DynamicSerializeElement
	@XmlElement
	private float quad4WaveRad;

	/*
	 * 20 character description of format to follow in userData
	 */
	@Column(length = 20)
	@DynamicSerializeElement
	@XmlElement
	private String userDefined;

	/*
	 * User data section as indicated by userDefined parameter
	 */
	@Column(length = 30)
	@DynamicSerializeElement
	@XmlElement
	private String userData;

	/**
	 * Default Constructor
	 */
	public StormTrackRecord() {
		this.reportType= " ";
		this.basin = "XX";
		this.cycloneNum = "M";
		this.warnTime = null;
		this.techniqueNum = IDecoderConstantsN.INTEGER_MISSING;
		this.model = " ";
//		this.fcstHour = IMISSD;
		this.clat = RMISSD;
		this.clon = RMISSD;
		this.windMax = RMISSD;
		this.mslp = RMISSD;
		this.stormType = "XX";
		this.windCategory = RMISSD;
		this.windCode = "M";
		this.quad1WindRad = RMISSD;
		this.quad2WindRad = RMISSD;
		this.quad3WindRad = RMISSD;
		this.quad4WindRad = RMISSD;
		this.closedP = RMISSD;
		this.radClosedP = RMISSD;
		this.maxWindRad = RMISSD;
		this.gust = RMISSD;
		this.eyeSize = RMISSD;
		this.subRegion = " ";
		this.maxSeas = RMISSD;
		this.forecaster = "";
		this.stormDrct = RMISSD;
		this.stormSped = RMISSD;
		this.stormName = " ";
		this.stormDepth = " ";
		this.waveHght = RMISSD;
		this.waveCode = " ";
		this.quad1WaveRad = RMISSD;
		this.quad2WaveRad = RMISSD;
		this.quad3WaveRad = RMISSD;
		this.quad4WaveRad = RMISSD;
		this.userDefined = " ";
		this.userData = " ";
	}

	/**
	 * Constructs a StormTrack record from a dataURI
	 * 
	 * @param uri
	 *            The dataURI
	 */
	public StormTrackRecord(String uri) {
		super(uri);
	}

	@Override
	public IDecoderGettable getDecoderGettable() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getReportType() {
		return reportType;
	}

	public void setReportType(String reportType) {
		this.reportType = reportType;
	}

	public String getBasin() {
		return basin;
	}

	public void setBasin(String basin) {
		this.basin = basin;
	}

	public String getCycloneNum() {
		return cycloneNum;
	}

	public void setCycloneNum(String cycloneNum) {
		this.cycloneNum = cycloneNum;
	}

	public Calendar getWarnTime() {
		return warnTime;
	}

	public void setWarnTime(Calendar warnTime) {
		this.warnTime = warnTime;
	}

	public int getTechniqueNum() {
		return techniqueNum;
	}

	public void setTechniqueNum(int techniqueNum) {
		this.techniqueNum = techniqueNum;
	}

	public String getModel() {
		return model;
	}

	public void setModel(String model) {
		this.model = model;
	}

	public int getFcstHour() {
		return dataTime.getFcstTime()/3600; //fcstHour;
	}

	public float getClat() {
		return clat;
	}

	public void setClat(float clat) {
		this.clat = clat;
	}

	public float getClon() {
		return clon;
	}

	public void setClon(float clon) {
		this.clon = clon;
	}

	public float getWindMax() {
		return windMax;
	}

	public void setWindMax(float windMax) {
		this.windMax = windMax;
	}

	public float getMslp() {
		return mslp;
	}

	public void setMslp(float mslp) {
		this.mslp = mslp;
	}

	public String getStormType() {
		return stormType;
	}

	public void setStormType(String stormType) {
		this.stormType = stormType;
	}

	public float getWindCategory() {
		return windCategory;
	}

	public void setWindCategory(float windCategory) {
		this.windCategory = windCategory;
	}

	public String getWindCode() {
		return windCode;
	}

	public void setWindCode(String windCode) {
		this.windCode = windCode;
	}

	public float getQuad1WindRad() {
		return quad1WindRad;
	}

	public void setQuad1WindRad(float quad1WindRad) {
		this.quad1WindRad = quad1WindRad;
	}

	public float getQuad2WindRad() {
		return quad2WindRad;
	}

	public void setQuad2WindRad(float quad2WindRad) {
		this.quad2WindRad = quad2WindRad;
	}

	public float getQuad3WindRad() {
		return quad3WindRad;
	}

	public void setQuad3WindRad(float quad3WindRad) {
		this.quad3WindRad = quad3WindRad;
	}

	public float getQuad4WindRad() {
		return quad4WindRad;
	}

	public void setQuad4WindRad(float quad4WindRad) {
		this.quad4WindRad = quad4WindRad;
	}

	public float getClosedP() {
		return closedP;
	}

	public void setClosedP(float closedP) {
		this.closedP = closedP;
	}

	public float getRadClosedP() {
		return radClosedP;
	}

	public void setRadClosedP(float radClosedP) {
		this.radClosedP = radClosedP;
	}

	public float getMaxWindRad() {
		return maxWindRad;
	}

	public void setMaxWindRad(float maxWindRad) {
		this.maxWindRad = maxWindRad;
	}

	public float getGust() {
		return gust;
	}

	public void setGust(float gust) {
		this.gust = gust;
	}

	public float getEyeSize() {
		return eyeSize;
	}

	public void setEyeSize(float eyeSize) {
		this.eyeSize = eyeSize;
	}

	public String getSubRegion() {
		return subRegion;
	}

	public void setSubRegion(String subRegion) {
		this.subRegion = subRegion;
	}

	public float getMaxSeas() {
		return maxSeas;
	}

	public void setMaxSeas(float maxSeas) {
		this.maxSeas = maxSeas;
	}

	public String getForecaster() {
		return forecaster;
	}

	public void setForecaster(String forecaster) {
		this.forecaster = forecaster;
	}

	public float getStormDrct() {
		return stormDrct;
	}

	public void setStormDrct(float stormDrct) {
		this.stormDrct = stormDrct;
	}

	public float getStormSped() {
		return stormSped;
	}

	public void setStormSped(float stormSped) {
		this.stormSped = stormSped;
	}

	public String getStormName() {
		return stormName;
	}

	public void setStormName(String stormName) {
		this.stormName = stormName;
	}

	public String getStormDepth() {
		return stormDepth;
	}

	public void setStormDepth(String stormDepth) {
		this.stormDepth = stormDepth;
	}

	public float getWaveHght() {
		return waveHght;
	}

	public void setWaveHght(float waveHght) {
		this.waveHght = waveHght;
	}

	public String getWaveCode() {
		return waveCode;
	}

	public void setWaveCode(String waveCode) {
		this.waveCode = waveCode;
	}

	public float getQuad1WaveRad() {
		return quad1WaveRad;
	}

	public void setQuad1WaveRad(float quad1WaveRad) {
		this.quad1WaveRad = quad1WaveRad;
	}

	public float getQuad2WaveRad() {
		return quad2WaveRad;
	}

	public void setQuad2WaveRad(float quad2WaveRad) {
		this.quad2WaveRad = quad2WaveRad;
	}

	public float getQuad3WaveRad() {
		return quad3WaveRad;
	}

	public void setQuad3WaveRad(float quad3WaveRad) {
		this.quad3WaveRad = quad3WaveRad;
	}

	public float getQuad4WaveRad() {
		return quad4WaveRad;
	}

	public void setQuad4WaveRad(float quad4WaveRad) {
		this.quad4WaveRad = quad4WaveRad;
	}

	public String getUserDefined() {
		return userDefined;
	}

	public void setUserDefined(String userDefined) {
		this.userDefined = userDefined;
	}

	public String getUserData() {
		return userData;
	}

	public void setUserData(String userData) {
		this.userData = userData;
	}

}
