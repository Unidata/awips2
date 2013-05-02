package gov.noaa.nws.ncep.common.dataplugin.ncpirep;

/**
 * This software was modified from Raytheon's pirep plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/

import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;

import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import javax.measure.quantity.Angle;
import javax.measure.quantity.Length;
import javax.measure.quantity.Pressure;
import javax.measure.quantity.Temperature;
import javax.measure.quantity.Velocity;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.IDecoderGettable;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.AircraftObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * NcPirepRecord is the Data Access component for pirep observation data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20110421				   F. J. Yen   Initial creation from Raytheon's pirep.
 * 									   Change temp, windSpeed from Double to Float.
 * 									   Change windDirection from Integer to Float
 * 08/30/2011    286        qzhou      Use IDecoderConstantsN.INTEGER_MISSING instead -9999 in visibility. 
 * 08/31/2011    286        qzhou      Created project and moved this from ~edex.plugin.pirep
 * 09/19/2011    286        Q.Zhou      Changed reportType to string,
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * 04/08/2013    1293       bkowal     Removed references to hdffileid.
 * Apr 12, 2013  1857      bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ncpirepseq")
@Table(name = "ncpirep", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "ncpirep",
		indexes = {
				@Index(name = "ncpirep_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NcPirepRecord extends PluginDataObject implements ISpatialEnabled,
		IDecoderGettable, IPointData, IPersistable {

	private static final long serialVersionUID = 1L;

	public static final Unit<Temperature> TEMPERATURE_UNIT = SI.CELSIUS;

	public static final Unit<Length> ALTITUDE_UNIT = NonSI.FOOT;

	public static final Unit<Velocity> WIND_SPEED_UNIT = NonSI.KNOT;

	public static final Unit<Angle> WIND_DIR_UNIT = NonSI.DEGREE_ANGLE;

	public static final Unit<Pressure> PRESSURE_UNIT = SI.PASCAL;

	public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

	private static final HashMap<String, String> PARM_MAP = new HashMap<String, String>();
	private static final HashMap<String, Integer> ICING_MAP = new HashMap<String, Integer>();
	private static final HashMap<String, Integer> TURB_MAP = new HashMap<String, Integer>();
	static {
		PARM_MAP.put("T", SFC_TEMP);
		PARM_MAP.put("WS", SFC_WNDSPD);
		PARM_MAP.put("WD", SFC_WNDDIR);
		PARM_MAP.put("NLAT", STA_LAT);
		PARM_MAP.put("NLON", STA_LON);
		PARM_MAP.put("FLT_LVL", UA_FLTLVL);
		PARM_MAP.put("ICT", UA_ICETYPE);
		PARM_MAP.put("ICI", UA_ICEINTENSE);
		PARM_MAP.put("TBF", UA_TURBFREQ);
		PARM_MAP.put("TBI", UA_TURBINTENSE);
		PARM_MAP.put("TOP_HGT", UA_TOPHGT);
		PARM_MAP.put("BOT_HGT", UA_BOTHGT);

		ICING_MAP.put("", new Integer(0));
		ICING_MAP.put("NEG", new Integer(1));
		ICING_MAP.put("TRACE", new Integer(2));
		ICING_MAP.put("TRACELGT", new Integer(3));
		ICING_MAP.put("LGT", new Integer(4));
		ICING_MAP.put("LGTMOD", new Integer(5));
		ICING_MAP.put("MOD", new Integer(6));
		ICING_MAP.put("MODSEV", new Integer(7));
		ICING_MAP.put("SEV", new Integer(8));

		TURB_MAP.put("", new Integer(0));
		TURB_MAP.put("NEG", new Integer(1));
		TURB_MAP.put("SMOOTHLGT", new Integer(2));
		TURB_MAP.put("LGT", new Integer(3));
		TURB_MAP.put("LGTMOD", new Integer(4));
		TURB_MAP.put("MOD", new Integer(5));
		TURB_MAP.put("MODSEV", new Integer(6));
		TURB_MAP.put("SEV", new Integer(7));
		TURB_MAP.put("EXTRM", new Integer(8));
	}

	@Transient
	private NcPirepLayerData maxPirepLayerData = null;

	@Transient
	private boolean display = true;

	@Transient
	@XmlAttribute
	@DynamicSerializeElement
	private Integer obsId;

	// Time of the observation.
	@Transient
	@XmlAttribute
	@DynamicSerializeElement
	private Calendar timeObs;

	// Time of the observation to the nearest hour.
	@Column
	@XmlElement
	@DynamicSerializeElement
	private Calendar refHour;

	//
	@Column(length = 8)
	@DataURI(position = 1)
	@XmlAttribute
	@DynamicSerializeElement
	private String reportType;

	// Text of the WMO header
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private String wmoHeader;

	@Transient
	@DynamicSerializeElement
	@XmlElement
	private String suspectTimeFlag;

	// Correction indicator from wmo header
	@Column(length = 8)
	@DataURI(position = 2)
	@XmlElement
	@DynamicSerializeElement
	private String corIndicator;

	@Transient
	@XmlElement
	@DynamicSerializeElement
	private String aircraftType;

	// Observation air temperature in degrees Celsius, if not converted.
	// Observation air temperature in degrees Kelvin.
	// Decimal(5,2)
	@Transient
	@XmlElement
	@DynamicSerializeElement
	// private Double temp;
	private Float temp;

	// Observation wind direction in angular degrees. Integer
	@Transient
	@XmlElement
	@DynamicSerializeElement
	// private Integer windDirection;
	private Float windDirection;

	// Observation wind speed in knots, if not converted.
	// Observation wind speed in meters per second.
	// Decimal(5,2)
	@Transient
	@XmlElement
	@DynamicSerializeElement
	// private Double windSpeed;
	private Float windSpeed;

	@Transient
	@XmlElement
	@DynamicSerializeElement
	// private Integer horzVisibility;
	private Integer horzVisibility = IDecoderConstantsN.INTEGER_MISSING; // -9999

	@Transient
	@XmlElement
	@DynamicSerializeElement
	private String obsText;

	// @Column(length = 16)
	@Transient
	@XmlElement
	@DynamicSerializeElement
	private String weatherGroup;

	@DynamicSerializeElement
	@XmlElement
	// @OneToMany(cascade = CascadeType.ALL, mappedBy = "parent", fetch =
	// FetchType.EAGER)
	// private Set<NcPirepLayerData> ancPirepData;
	@Transient
	private Set<NcPirepLayerData> ancPirepData = new HashSet<NcPirepLayerData>();

	@Embedded
	@DataURI(position = 3, embedded = true)
	@XmlElement
	@DynamicSerializeElement
	private AircraftObsLocation location;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	/**
     * 
     */
	public NcPirepRecord() {
	}

	/**
	 * Constructor for DataURI construction through base class. This is used by
	 * the notification service.
	 * 
	 * @param uri
	 *            A data uri applicable to this class.
	 * @param tableDef
	 *            The table definitions for this class.
	 */
	public NcPirepRecord(String uri) {
		super(uri);
	}

	public Integer getObsId() {
		return obsId;
	}

	public void setObsId(Integer obsId) {
		this.obsId = obsId;
	}

	/**
	 * @return the wmoHeader
	 */
	public String getWmoHeader() {
		return wmoHeader;
	}

	/**
	 * @param wmoHeader
	 *            the wmoHeader to set
	 */
	public void setWmoHeader(String wmoHeader) {
		this.wmoHeader = wmoHeader;
	}

	/**
	 * Get the report correction indicator.
	 * 
	 * @return The corIndicator
	 */
	public String getCorIndicator() {
		return corIndicator;
	}

	/**
	 * Set the report correction indicator.
	 * 
	 * @param corIndicator
	 *            The corIndicator.
	 */
	public void setCorIndicator(String corIndicator) {
		this.corIndicator = corIndicator;
	}

	/**
	 * Get the report data for this observation.
	 * 
	 * @return The Report data.
	 */
	public String getReportData() {
		String s = null;
		if (messageData instanceof String) {
			s = (String) messageData;
		}
		return s;
	}

	/**
	 * Set the report data for this observation.
	 * 
	 * @param reportData
	 *            The Report data.
	 */
	public void setReportData(String reportData) {
		messageData = reportData;
	}

	/**
	 * Get this observation's geometry.
	 * 
	 * @return The geometry for this observation.
	 */
	public Geometry getGeometry() {
		return location.getGeometry();
	}

	/**
	 * Get the geometry latitude.
	 * 
	 * @return The geometry latitude.
	 */
	public double getLatitude() {
		return location.getLatitude();
	}

	/**
	 * Get the geometry longitude.
	 * 
	 * @return The geometry longitude.
	 */
	public double getLongitude() {
		return location.getLongitude();
	}

	/**
	 * Is the location defined in the spatial tables.
	 * 
	 * @return Is the location defined in the spatial tables.
	 */
	public Boolean getLocationDefined() {
		return location.getLocationDefined();
	}

	/**
	 * Get the station identifier for this observation.
	 * 
	 * @return the stationId
	 */
	public String getStationId() {
		return location.getStationId();
	}

	/**
	 * // * Get the elevation, in meters, of the observing platform or location.
	 * Get the elevation, in feet, of the observing platform or location.
	 * 
	 * // * @return The observation elevation, in meters.
	 * 
	 * @return The observation elevation, in feet.
	 */
	public Integer getFlightLevel() {
		return location.getFlightLevel();
	}

	/**
	 * @return the reportType
	 */
	public String getReportType() {
		return reportType;
	}

	/**
	 * @param reportType
	 *            the reportType to set
	 */
	public void setReportType(String reportType) {
		this.reportType = reportType;
	}

	/**
	 * @return the timeObs
	 */
	public Calendar getTimeObs() {
		if (this.dataTime == null)
			return null;
		return this.dataTime.getRefTimeAsCalendar();
	}

	/**
	 * @param timeObs
	 *            the timeObs to set
	 */
	public void setTimeObs(Calendar timeObs) {
		this.timeObs = timeObs;
	}

	/**
	 * @return the refHour
	 */
	public Calendar getRefHour() {
		return refHour;
	}

	/**
	 * @param refHour
	 *            the refHour to set
	 */
	public void setRefHour(Calendar refHour) {
		this.refHour = refHour;
	}

	public String getSuspectTimeFlag() {
		return suspectTimeFlag;
	}

	public void setSuspectTimeFlag(String suspectTimeFlag) {
		this.suspectTimeFlag = suspectTimeFlag;
	}

	/**
	 * @param aircraftType
	 *            the aircraftType to set
	 */
	public void setAircraftType(String aircraftType) {
		this.aircraftType = aircraftType;
	}

	/**
	 * @return the aircraftType
	 */
	public String getAircraftType() {
		return aircraftType;
	}

	/**
	 * @return the temp
	 */
	// public Double getTemp() {
	public float getTemp() {
		return temp;
	}

	/**
	 * @param temp
	 *            the temp to set
	 */
	// public void setTemp(Double temp) {
	public void setTemp(float temp) {
		this.temp = temp;
	}

	/**
	 * @return the windDirection
	 */
	// public Integer getWindDirection() {
	public float getWindDirection() {
		return windDirection;
	}

	/**
	 * @param windDirection
	 *            the windDirection to set
	 */
	// public void setWindDirection(Integer windDirection) {
	public void setWindDirection(float windDirection) {
		this.windDirection = windDirection;
	}

	/**
	 * @return the windspeed
	 */
	// public Double getWindSpeed() {
	public float getWindSpeed() {
		return windSpeed;
	}

	/**
	 * @param windspeed
	 *            the windspeed to set
	 */
	// public void setWindSpeed(Double windSpeed) {
	public void setWindSpeed(float windSpeed) {
		this.windSpeed = windSpeed;
	}

	/**
	 * @return the horzVisibility
	 */
	public Integer getHorzVisibility() {
		return horzVisibility;
	}

	/**
	 * @param horzVisibility
	 *            the horzVisibility to set
	 */
	public void setHorzVisibility(Integer horzVisibility) {
		this.horzVisibility = horzVisibility;
	}

	/**
	 * @return the obsText
	 */
	public String getObsText() {
		return obsText;
	}

	/**
	 * @param obsText
	 *            the obsText to set
	 */
	public void setObsText(String obsText) {
		this.obsText = obsText;
	}

	/**
	 * @return the weatherGroup
	 */
	public String getWeatherGroup() {
		return weatherGroup;
	}

	/**
	 * @param weatherGroup
	 *            the weatherGroup to set
	 */
	public void setWeatherGroup(String weatherGroup) {
		this.weatherGroup = weatherGroup;
	}

	/**
	 * @return the ancPirepData
	 */
	public Set<NcPirepLayerData> getAncPirepData() {
		return ancPirepData;
	}

	/**
	 * @param ancPirepData
	 *            the ancPirepData to set
	 */
	public void setAncPirepData(Set<NcPirepLayerData> ancPirepData) {
		this.ancPirepData = ancPirepData;
	}

	/**
	 * 
	 * @param cloud
	 */
	public void addLayer(NcPirepLayerData layer) {
		layer.setParent(this);
		if (ancPirepData == null) {
			ancPirepData = new HashSet<NcPirepLayerData>();
		}
		ancPirepData.add(layer);
	}

	@Override
	public void setDataURI(String dataURI) {
		identifier = dataURI;
	}

	/**
	 * Get the IDecoderGettable reference for this record.
	 * 
	 * @return The IDecoderGettable reference for this record.
	 */
	@Override
	public IDecoderGettable getDecoderGettable() {
		return this;
	}

	/**
	 * Get the value of a parameter that is represented as a String.
	 * 
	 * @param paramName
	 *            The name of the parameter value to retrieve.
	 * @return The String value of the parameter. If the parameter is unknown, a
	 *         null reference is returned.
	 */
	@Override
	public String getString(String paramName) {
		String retData = null;
		if ("STA".matches(paramName)) {
			retData = getStationId();
		} else if ("TEXT".equals(paramName)) {
			retData = obsText;
		}
		return retData;
	}

	/**
	 * Get the value and units of a named parameter within this observation.
	 * 
	 * @param paramName
	 *            The name of the parameter value to retrieve.
	 * @return An Amount with value and units. If the parameter is unknown, a
	 *         null reference is returned.
	 */
	@Override
	public Amount getValue(String paramName) {
		Amount a = null;

		String pName = PARM_MAP.get(paramName);
		if (display) {
			if (SFC_TEMP.equals(pName) && (temp != null)) {
				a = new Amount(temp, TEMPERATURE_UNIT);
			} else if (SFC_WNDSPD.equals(pName) && (windSpeed != null)) {
				a = new Amount(windSpeed, WIND_SPEED_UNIT);
			} else if (SFC_WNDDIR.equals(pName) && (windDirection != null)) {
				a = new Amount(windDirection, WIND_DIR_UNIT);
			} else if (STA_LAT.equals(pName)) {
				a = new Amount(this.getLatitude(), LOCATION_UNIT);
			} else if (STA_LON.equals(pName)) {
				a = new Amount(this.getLongitude(), LOCATION_UNIT);
			} else if (UA_FLTLVL.equals(pName) && getFlightLevel() != null) {
				a = new Amount(this.getFlightLevel().intValue(), ALTITUDE_UNIT);
				// if used, need to modify raytheon code
				// } else if (UA_TOPHGT.equals(pName) && maxPirepLayerData !=
				// null
				// && maxPirepLayerData.getTopLayerHeight() != null) {
				// a = new
				// Amount(maxPirepLayerData.getTopLayerHeight().intValue(),
				// ALTITUDE_UNIT);
				// } else if (UA_BOTHGT.equals(pName) && maxPirepLayerData !=
				// null
				// && maxPirepLayerData.getBaseLayerHeight() != null) {
				// a = new
				// Amount(maxPirepLayerData.getBaseLayerHeight().intValue(),
				// ALTITUDE_UNIT);
			}
		}
		return a;
	}

	/**
	 * Get the value of a parameter that is represented as a String.
	 * 
	 * @param paramName
	 *            The name of the parameter value to retrieve.
	 * @return The String value of the parameter. If the parameter is unknown, a
	 *         null reference is returned.
	 */
	@Override
	public Collection<Amount> getValues(String paramName) {
		return null;
	}

	@Override
	public String[] getStrings(String paramName) {
		if ("ICI".matches(paramName)) {
			int rank = -1;
			String iceIntensity = null;
			for (NcPirepLayerData layer : this.ancPirepData) {
				String intensity = "";
				if (layer.getLayerType().equals(
						NcPirepLayerData.LAYER_TYP_ICING)) {
					if (layer.getIceInten() != null) {
						intensity = layer.getIceInten();
					}
					// if (layer.getSecondValue() != null) {
					// intensity += layer.getSecondValue();
					// }

					if (ICING_MAP.get(intensity).intValue() > rank) {
						rank = ICING_MAP.get(intensity).intValue();
						iceIntensity = intensity;
						maxPirepLayerData = layer;
					}
				}
			}
			if (iceIntensity != null) {
				String[] maxIntensity = { iceIntensity };
				return maxIntensity;
			} else {
				display = false;
			}
		} else if ("ICT".matches(paramName) && maxPirepLayerData != null) {
			String[] maxType = { maxPirepLayerData.getIceType() };
			return maxType;
		} else if ("TBI".matches(paramName)) {
			int rank = -1;
			String turbIntensity = null;
			for (NcPirepLayerData layer : this.ancPirepData) {
				String intensity = "";
				if (layer.getLayerType().equals(
						NcPirepLayerData.LAYER_TYP_TURBC)) {
					if (layer.getTurbInten() != null) {
						intensity = layer.getTurbInten();
					}
					// if (layer.getSecondValue() != null) {
					// intensity += layer.getSecondValue();
					// }

					if (TURB_MAP.get(intensity).intValue() > rank) {
						rank = TURB_MAP.get(intensity).intValue();
						turbIntensity = intensity;
						maxPirepLayerData = layer;
					}
				}
			}
			if (turbIntensity != null) {
				String[] maxIntensity = { turbIntensity };
				return maxIntensity;
			} else {
				display = false;
			}
		} else if ("TBT".matches(paramName) && maxPirepLayerData != null) {
			String[] maxType = { maxPirepLayerData.getTurbType() };
			return maxType;
		} else if ("TBF".matches(paramName) && maxPirepLayerData != null) {
			// Turbulence Frequency Types do not get stored.
		}
		return null;
	}

	@Override
	public AircraftObsLocation getSpatialObject() {
		return location;
	}

	public AircraftObsLocation getLocation() {
		return location;
	}

	public void setLocation(AircraftObsLocation location) {
		this.location = location;
	}

	@Override
	public String getMessageData() {
		return getObsText();
	}

	/**
	 * Returns the hashCode for this object. This implementation returns the
	 * hashCode of the generated dataURI.
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((getDataURI() == null) ? 0 : getDataURI().hashCode());
		return result;
	}

	/**
	 * Checks if this record is equal to another by checking the generated
	 * dataURI.
	 * 
	 * @param obj
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		NcPirepRecord other = (NcPirepRecord) obj;
		if (getDataURI() == null) {
			if (other.getDataURI() != null) {
				return false;
			}
		} else if (!getDataURI().equals(other.getDataURI()))
			return false;
		return true;
	}

	@Override
	public Date getPersistenceTime() {
		return this.dataTime.getRefTime();
	}

	@Override
	public void setPersistenceTime(Date persistTime) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.common.pointdata.IPointData#getPointDataView()
	 */
	@Override
	public PointDataView getPointDataView() {
		return this.pointDataView;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.common.pointdata.IPointData#setPointDataView(com.raytheon
	 * .uf.common.pointdata.PointDataView)
	 */
	@Override
	public void setPointDataView(PointDataView pointDataView) {
		this.pointDataView = pointDataView;
	}

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
