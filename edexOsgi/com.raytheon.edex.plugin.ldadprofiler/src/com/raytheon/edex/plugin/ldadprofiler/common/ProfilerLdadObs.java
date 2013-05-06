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
package com.raytheon.edex.plugin.ldadprofiler.common;

import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

import javax.measure.quantity.Angle;
import javax.measure.quantity.Velocity;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
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
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Record implementation for ldadprofiler plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 *                     
 * ate          Ticket#     Engineer    Description
 * -----------  ----------  ----------- --------------------------
 * 10/07/09                 vkorolev    Initial creation
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale     Added SequenceGenerator annotation.
 * </pre>
 * 
 * @author vkorolev
 * @version 1
 */

@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "ldadprofilerseq")
@Table(name = "ldadprofiler", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "ldadprofiler",
		indexes = {
				@Index(name = "ldadprofiler_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ProfilerLdadObs extends PersistablePluginDataObject implements
		ISpatialEnabled, IDecoderGettable, IPointData, IPersistable {

	public static final String PLUGIN_NAME = "ldadprofiler";

	private static final long serialVersionUID = 1L;

	public static final Unit<Angle> LOCATION_UNIT = NonSI.DEGREE_ANGLE;

	public static final Unit<Velocity> WIND_SPEED_UNIT = SI.METERS_PER_SECOND;

	public static final Unit<Angle> WIND_DIR_UNIT = NonSI.DEGREE_ANGLE;

	private static final HashMap<String, String> PARM_MAP = new HashMap<String, String>();

	static {
		PARM_MAP.put("NLAT", STA_LAT);
		PARM_MAP.put("NLON", STA_LON);
		PARM_MAP.put("WS", SFC_WNDSPD);
		PARM_MAP.put("WD", SFC_WNDDIR);
	}

	private static final String PRESS = "PRESS";

	private static final String AGL = "AGL";

	public static final String PRESS_PARAM_PTRN = ".*:" + PRESS + "=\\d{2,4}";

	public static final String AGL_PARAM_PTRN = ".*:" + AGL + "=\\d{2,4}";

	@Transient
	private String parameterName = null;

	@DataURI(position = 1)
	@XmlAttribute
	@DynamicSerializeElement
	private Integer reportType;

	// Location
	@Embedded
	@DataURI(position = 2, embedded = true)
	@XmlElement
	@DynamicSerializeElement
	private SurfaceObsLocation location; // latitude, longitude, elevation,

	// stationId

	// Base time in Epoch "seconds since 1970-01-01 00:00:00 UTC"
	@Column
	@DynamicSerializeElement
	@XmlElement
	int base_time;

	// Consensus start time offset from base_time
	// "seconds since 2009/10/07 00:00:00 UTC"
	@Column
	@DynamicSerializeElement
	@XmlElement
	double start_time_offset;

	// Consensus end time offset from base_time
	// "seconds since 2009/10/07 00:00:00 UTC"
	@Column
	@DynamicSerializeElement
	@XmlElement
	double end_time_offset;

	// nhts Number of heights?
	@Column
	@DynamicSerializeElement
	@XmlElement
	int nhts;

	// the level data
	@XmlElement
	@DynamicSerializeElement
	@Transient
	private List<ProfilerLdadLevel> levels;

	// The profiler observation time.
	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private Calendar timeObs;

	@Column
	@XmlAttribute
	@DynamicSerializeElement
	private String stationName;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	/**
	 * @return the base_time
	 */
	public int getBase_time() {
		return base_time;
	}

	/**
	 * @param base_time
	 *            the base_time to set
	 */
	public void setBase_time(int base_time) {
		this.base_time = base_time;
	}

	public Calendar getTimeObs() {
		return timeObs;
	}

	public void setTimeObs(Calendar timeObs) {
		this.timeObs = timeObs;
	}

	/**
	 * @return the start_time_offset
	 */
	public double getStart_time_offset() {
		return start_time_offset;
	}

	/**
	 * @param start_time_offset
	 *            the start_time_offset to set
	 */
	public void setStart_time_offset(double start_time_offset) {
		this.start_time_offset = start_time_offset;
	}

	/**
	 * @return the end_time_offset
	 */
	public double getEnd_time_offset() {
		return end_time_offset;
	}

	/**
	 * @param end_time_offset
	 *            the end_time_offset to set
	 */
	public void setEnd_time_offset(double end_time_offset) {
		this.end_time_offset = end_time_offset;
	}

	/**
	 * @return the nhts
	 */
	public int getNhts() {
		return nhts;
	}

	/**
	 * @param nhts
	 *            the nhts to set
	 */
	public void setNhts(int nhts) {
		this.nhts = nhts;
	}

	/**
	 * @return the levels
	 */
	public List<ProfilerLdadLevel> getLevels() {
		return levels;
	}

	/**
	 * @param levels
	 *            the levels to set
	 */
	public void setLevels(List<ProfilerLdadLevel> levels) {
		this.levels = levels;
	}

	/**
	 * @return the pointDataView
	 */
	@Override
	public PointDataView getPointDataView() {
		return pointDataView;
	}

	public ProfilerLdadObs() {
	}

	/**
	 * @param pointDataView
	 *            the pointDataView to set
	 */
	@Override
	public void setPointDataView(PointDataView pointDataView) {
		this.pointDataView = pointDataView;
	}

	// ----------------------------------------------------

	@Override
	public IDecoderGettable getDecoderGettable() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ISpatialObject getSpatialObject() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getString(String paramName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String[] getStrings(String paramName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Amount getValue(String paramName) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<Amount> getValues(String paramName) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * @param location
	 *            the location to set
	 */
	public void setLocation(SurfaceObsLocation location) {
		this.location = location;
	}

	/**
	 * @return the location
	 */
	public SurfaceObsLocation getLocation() {
		return location;
	}

	/**
	 * @param stationName
	 *            the stationName to set
	 */
	public void setStationName(String stationName) {
		this.stationName = stationName;
	}

	/**
	 * @return the stationName
	 */
	public String getStationName() {
		return stationName;
	}

	/**
	 * @param parameterName
	 *            the parameterName to set
	 */
	public void setParameterName(String parameterName) {
		this.parameterName = parameterName;
	}

	/**
	 * @return the parameterName
	 */
	public String getParameterName() {
		return parameterName;
	}

	/**
	 * @param reportType
	 *            the reportType to set
	 */
	public void setReportType(Integer reportType) {
		this.reportType = reportType;
	}

	/**
	 * @return the reportType
	 */
	public Integer getReportType() {
		return reportType;
	}

}
