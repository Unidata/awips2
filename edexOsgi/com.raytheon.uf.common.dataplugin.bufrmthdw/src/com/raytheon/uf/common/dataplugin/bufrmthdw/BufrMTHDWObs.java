/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.dataplugin.bufrmthdw;

import java.util.Calendar;

import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Index;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * PluginDataObject for MTSAT high density winds data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 26, 2010            jkorman     Initial creation
 * Apr 04, 2013 1846       bkowal      Added an index on refTime and
 *                                     forecastTime
 * Apr 12, 2013 1857       bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * May 17, 2013 1869       bsteffen    Remove DataURI column from sat plot
 *                                     types.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "bufrmthdwseq")
@Table(name = "bufrmthdw", uniqueConstraints = { @UniqueConstraint(columnNames = {
        "stationid", "refTime", "sattype", "pressure", "latitude", "longitude" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "bufrmthdw",
		indexes = {
				@Index(name = "bufrmthdw_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@DynamicSerialize
public class BufrMTHDWObs extends PersistablePluginDataObject implements
        ISpatialEnabled, IPointData, IPersistable {

	private static final long serialVersionUID = 1L;

	// The observation time.
	@DataURI(position = 1)
	@DynamicSerializeElement
	private String satType;

	@DataURI(position = 2)
	@DynamicSerializeElement
	private Double pressure;

	@Embedded
	@DataURI(position = 3, embedded = true)
	@DynamicSerializeElement
	private SurfaceObsLocation location;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	// Text of the WMO header
	@Column(length = 32)
	@DynamicSerializeElement
	private String wmoHeader;

	// The observation time.
	@Column
	@DynamicSerializeElement
	private Calendar validTime;

	@DynamicSerializeElement
	@Transient
	private Double windDir;

	@DynamicSerializeElement
	@Transient
	private Double windSpd;

	@DynamicSerializeElement
	@Transient
	private Double satelliteID;

	@DynamicSerializeElement
	@Transient
	private Double originatingID;

	@DynamicSerializeElement
	@Transient
	private Double satelliteClass;

	@DynamicSerializeElement
	@Transient
	private Double sgmtSzX;

	@DynamicSerializeElement
	@Transient
	private Double sgmtSzY;

	@DynamicSerializeElement
	@Transient
	private Integer satelliteInstr;

	@DynamicSerializeElement
	@Transient
	private Integer satelliteWindMethod;

	@DynamicSerializeElement
	@Transient
	private Double satelliteFreq;

	@DynamicSerializeElement
	@Transient
	private Double satelliteBandWidth;

	@DynamicSerializeElement
	@Transient
	private Double coldestTemp;

	@DynamicSerializeElement
	@Transient
	private Integer heightMethod;

	@DynamicSerializeElement
	@Transient
	private Integer tracerCorrelation;

	@DynamicSerializeElement
	@Transient
	private Integer landSea;

	@DynamicSerializeElement
	@Transient
	private Double satelliteZenith;

	@DynamicSerializeElement
	@Transient
	private Integer firstGuess;

	@DynamicSerializeElement
	@Transient
	private Integer timeSignificance;

	/**
	 * Empty constructor.
	 */
	public BufrMTHDWObs() {
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
	public BufrMTHDWObs(String uri) {
		super(uri);
	}

	/**
	 * Get the observation time for this data.
	 * 
	 * @return The data observation time.
	 */
	public Calendar getValidTime() {
		return validTime;
	}

	/**
	 * Set the observation time for this data.
	 * 
	 * @param timeObs
	 *            The data observation time.
	 */
	public void setValidTime(Calendar time) {
		validTime = time;
	}

	/**
	 * @return the satType
	 */
	public String getSatType() {
		return satType;
	}

	/**
	 * @param satType
	 *            the satType to set
	 */
	public void setSatType(String type) {
		satType = type;
	}

	/**
	 * @param satType
	 *            the satType to set
	 */
	public void setSatType(BUFRMTHDWSatType type) {
		satType = type.toString();
	}

	/**
	 * @return the location
	 */
	public SurfaceObsLocation getLocation() {
		return location;
	}

	/**
	 * @param location
	 *            the location to set
	 */
	public void setLocation(SurfaceObsLocation location) {
		this.location = location;
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
	 * @return the windDir
	 */
	public Double getWindDir() {
		return windDir;
	}

	/**
	 * @param windDir
	 *            the windDir to set
	 */
	public void setWindDir(Double direction) {
		windDir = direction;
	}

	/**
	 * @return the windSpd
	 */
	public Double getWindSpd() {
		return windSpd;
	}

	/**
	 * @param windSpd
	 *            the windSpd to set
	 */
	public void setWindSpd(Double speed) {
		windSpd = speed;
	}

	/**
	 * @return the pressure
	 */
	public Double getPressure() {
		return pressure;
	}

	/**
	 * @param pressure
	 *            the pressure to set
	 */
	public void setPressure(Double pressure) {
		this.pressure = pressure;
	}

	/**
	 * @return the satelliteID
	 */
	public Double getSatelliteID() {
		return satelliteID;
	}

	/**
	 * @param satelliteID
	 *            the satelliteID to set
	 */
	public void setSatelliteID(Double satelliteID) {
		this.satelliteID = satelliteID;
	}

	/**
	 * @return the originatingID
	 */
	public Double getOriginatingID() {
		return originatingID;
	}

	/**
	 * @param originatingID
	 *            the originatingID to set
	 */
	public void setOriginatingID(Double originatingID) {
		this.originatingID = originatingID;
	}

	/**
	 * @return the satelliteClass
	 */
	public Double getSatelliteClass() {
		return satelliteClass;
	}

	/**
	 * @param satelliteClass
	 *            the satelliteClass to set
	 */
	public void setSatelliteClass(Double satelliteClass) {
		this.satelliteClass = satelliteClass;
	}

	/**
	 * @return the sgmtSzX
	 */
	public Double getSgmtSzX() {
		return sgmtSzX;
	}

	/**
	 * @param sgmtSzX
	 *            the sgmtSzX to set
	 */
	public void setSgmtSzX(Double sgmtSzX) {
		this.sgmtSzX = sgmtSzX;
	}

	/**
	 * @return the sgmtSzY
	 */
	public Double getSgmtSzY() {
		return sgmtSzY;
	}

	/**
	 * @param sgmtSzY
	 *            the sgmtSzY to set
	 */
	public void setSgmtSzY(Double sgmtSzY) {
		this.sgmtSzY = sgmtSzY;
	}

	/**
	 * @return the satelliteInstr
	 */
	public Integer getSatelliteInstr() {
		return satelliteInstr;
	}

	/**
	 * @param satelliteInstr
	 *            the satelliteInstr to set
	 */
	public void setSatelliteInstr(Integer satelliteInstr) {
		this.satelliteInstr = satelliteInstr;
	}

	/**
	 * @return the satelliteWindMethod
	 */
	public Integer getSatelliteWindMethod() {
		return satelliteWindMethod;
	}

	/**
	 * @param satelliteWindMethod
	 *            the satelliteWindMethod to set
	 */
	public void setSatelliteWindMethod(Integer satelliteWindMethod) {
		this.satelliteWindMethod = satelliteWindMethod;
	}

	/**
	 * @return the satelliteFreq
	 */
	public Double getSatelliteFreq() {
		return satelliteFreq;
	}

	/**
	 * @param satelliteFreq
	 *            the satelliteFreq to set
	 */
	public void setSatelliteFreq(Double satelliteFreq) {
		this.satelliteFreq = satelliteFreq;
	}

	/**
	 * @return the satelliteBandWidth
	 */
	public Double getSatelliteBandWidth() {
		return satelliteBandWidth;
	}

	/**
	 * @param satelliteBandWidth
	 *            the satelliteBandWidth to set
	 */
	public void setSatelliteBandWidth(Double satelliteBandWidth) {
		this.satelliteBandWidth = satelliteBandWidth;
	}

	/**
	 * @return the coldestTemp
	 */
	public Double getColdestTemp() {
		return coldestTemp;
	}

	/**
	 * @param coldestTemp
	 *            the coldestTemp to set
	 */
	public void setColdestTemp(Double coldestTemp) {
		this.coldestTemp = coldestTemp;
	}

	/**
	 * @return the heightMethod
	 */
	public Integer getHeightMethod() {
		return heightMethod;
	}

	/**
	 * @param heightMethod
	 *            the heightMethod to set
	 */
	public void setHeightMethod(Integer heightMethod) {
		this.heightMethod = heightMethod;
	}

	/**
	 * @return the tracerCorrelation
	 */
	public Integer getTracerCorrelation() {
		return tracerCorrelation;
	}

	/**
	 * @param tracerCorrelation
	 *            the tracerCorrelation to set
	 */
	public void setTracerCorrelation(Integer tracerCorrelation) {
		this.tracerCorrelation = tracerCorrelation;
	}

	/**
	 * @return the landSea
	 */
	public Integer getLandSea() {
		return landSea;
	}

	/**
	 * @param landSea
	 *            the landSea to set
	 */
	public void setLandSea(Integer landSea) {
		this.landSea = landSea;
	}

	/**
	 * @return the satelliteZenith
	 */
	public Double getSatelliteZenith() {
		return satelliteZenith;
	}

	/**
	 * @param satelliteZenith
	 *            the satelliteZenith to set
	 */
	public void setSatelliteZenith(Double satelliteZenith) {
		this.satelliteZenith = satelliteZenith;
	}

	/**
	 * @return the firstGuess
	 */
	public Integer getFirstGuess() {
		return firstGuess;
	}

	/**
	 * @param firstGuess
	 *            the firstGuess to set
	 */
	public void setFirstGuess(Integer firstGuess) {
		this.firstGuess = firstGuess;
	}

	/**
	 * @return the timeSignificance
	 */
	public Integer getTimeSignificance() {
		return timeSignificance;
	}

	/**
	 * @param timeSignificance
	 *            the timeSignificance to set
	 */
	public void setTimeSignificance(Integer timeSignificance) {
		this.timeSignificance = timeSignificance;
	}

	@Override
	public ISpatialObject getSpatialObject() {
		return null;
	}

	/**
         * 
         */
	@Override
	public PointDataView getPointDataView() {
		return pointDataView;
	}

	/**
     * 
     */
	@Override
	public void setPointDataView(PointDataView pointDataView) {
		this.pointDataView = pointDataView;
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
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		BufrMTHDWObs other = (BufrMTHDWObs) obj;
		if (getDataURI() == null) {
			if (other.getDataURI() != null) {
				return false;
			}
		} else if (!getDataURI().equals(other.getDataURI())) {
			return false;
		}
		return true;
	}

}
