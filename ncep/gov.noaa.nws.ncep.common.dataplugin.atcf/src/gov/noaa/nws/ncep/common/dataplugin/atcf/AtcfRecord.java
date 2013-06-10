/*
 * 
 * AtcfRecord
 * 
 * This class performs the mapping to the database tables for the Automated
 * Tropical Cyclone Forecast (ATCF) Decoder Plug-In
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

package gov.noaa.nws.ncep.common.dataplugin.atcf;

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.Calendar;

import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * AtcfRecord is the Data Access component for ATCF Automated Tropical Cyclone
 * Forecast. This contains getters and setters for the main parent table atcf.
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/23/10        208    F. J. Yen    Initial Coding.
 * 03/10/12        606    G. Hull      added reportType to URI
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * 
 * </pre>
 * 
 * @author F. J. Yen, SIB
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "atcfseq")
@Table(name = "atcf", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "atcf",
		indexes = {
				@Index(name = "atcf_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class AtcfRecord extends PluginDataObject {

	private static final long serialVersionUID = 1L;
	private static final float RMISSD = IDecoderConstantsN.FLOAT_MISSING;
	private static final Integer IMISSD = IDecoderConstantsN.INTEGER_MISSING;

	/** Report type */
	@DataURI(position = 8)
	@Column(length = 32)
	@XmlElement
	@DynamicSerializeElement
	private String reportType;

	/**
	 * Basin, e.g. WP, IO, SH, CP, EP, AL, SL
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
	private int cycloneNum;

	/**
	 * Warning Date-Time
	 */
	@DataURI(position = 3)
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
	@DataURI(position = 4)
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String technique;

	/**
	 * TAU -- forecast Period: -24 through 120 hours, 0 for best-track, negative
	 * forecastHours used for CARQ and WRNG records.
	 */
	@DataURI(position = 5)
	@Column
	@DynamicSerializeElement
	@XmlElement
	private int fcstHour;

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
	private String intensity;

	/**
	 * Wind intensity (kts) for the radii defined i this record: 34, 50, 64
	 */
	@DataURI(position = 6)
	@DynamicSerializeElement
	@XmlElement
	private float radWind;

	/**
	 * Radius Code for wind intensity: AAA = full circle; QQQ = quadrant (NNQ,
	 * NEQ, EEQ, SEQ, SSQ, SWQ, WWQ, NWQ)
	 */
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String radWindQuad;

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
	@DataURI(position = 7)
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
	private float radWave;

	/**
	 * Radius code for seas wave height
	 */
	@Column(length = 8)
	@DynamicSerializeElement
	@XmlElement
	private String radWaveQuad;

	/**
	 * First quadrant seas radius as defined by radWaveQuad, 0 - 999 nm
	 */
	@DynamicSerializeElement
	@XmlElement
	private float quad1WaveRad;

	/**
	 * Second quadrant seas radius as defined by radWaveQuad, 0 - 999 nm
	 */
	@DynamicSerializeElement
	@XmlElement
	private float quad2WaveRad;

	/**
	 * Third quadrant seas radius as defined by radWaveQuad, 0 - 999 nm
	 */
	@DynamicSerializeElement
	@XmlElement
	private float quad3WaveRad;

	/**
	 * Fourth quadrant seas radius as defined by radWaveQuad, 0 - 999 nm
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
	public AtcfRecord() {
		this.reportType="ATCF";
		this.basin = " ";
		this.cycloneNum = IMISSD;
		this.warnTime = null;
		this.techniqueNum = IMISSD;
		this.technique = " ";
		this.fcstHour = IMISSD;
		this.clat = RMISSD;
		this.clon = RMISSD;
		this.windMax = RMISSD;
		this.mslp = RMISSD;
		this.intensity = " ";
		this.radWind = RMISSD;
		this.radWindQuad = " ";
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
		this.forecaster = " ";
		this.stormDrct = RMISSD;
		this.stormSped = RMISSD;
		this.stormName = " ";
		this.stormDepth = " ";
		this.radWave = RMISSD;
		this.radWaveQuad = " ";
		this.quad1WaveRad = RMISSD;
		this.quad2WaveRad = RMISSD;
		this.quad3WaveRad = RMISSD;
		this.quad4WaveRad = RMISSD;
		this.userDefined = " ";
		this.userData = " ";
	}

	/**
	 * Constructs a atcf record from a dataURI
	 * 
	 * @param uri
	 *            The dataURI
	 */
	public AtcfRecord(String uri) {
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

	public int getCycloneNum() {
		return cycloneNum;
	}

	public void setCycloneNum(int cycloneNum) {
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

	public String getTechnique() {
		return technique;
	}

	public void setTechnique(String technique) {
		this.technique = technique;
	}

	public int getFcstHour() {
		return fcstHour;
	}

	public void setFcstHour(int fcstHour) {
		this.fcstHour = fcstHour;
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

	public String getIntensity() {
		return intensity;
	}

	public void setIntensity(String intensity) {
		this.intensity = intensity;
	}

	public float getRadWind() {
		return radWind;
	}

	public void setRadWind(float radWind) {
		this.radWind = radWind;
	}

	public String getRadWindQuad() {
		return radWindQuad;
	}

	public void setRadWindQuad(String radWindQuad) {
		this.radWindQuad = radWindQuad;
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

	public float getRadWave() {
		return radWave;
	}

	public void setRadWave(float radWave) {
		this.radWave = radWave;
	}

	public String getRadWaveQuad() {
		return radWaveQuad;
	}

	public void setRadWaveQuad(String radWaveQuad) {
		this.radWaveQuad = radWaveQuad;
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

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
