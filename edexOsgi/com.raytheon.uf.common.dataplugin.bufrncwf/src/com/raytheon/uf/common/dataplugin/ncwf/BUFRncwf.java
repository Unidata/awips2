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
package com.raytheon.uf.common.dataplugin.ncwf;

import java.util.Collection;

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
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.pointdata.IPointData;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 17, 2009            jkorman     Initial creation
 * Apr 4, 2013        1846 bkowal      Added an index on refTime and forecastTime
 * Apr 12, 2013       1857 bgonzale    Added SequenceGenerator annotation.
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "bufrncwfseq")
@Table(name = "bufrncwf", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
/*
 * Both refTime and forecastTime are included in the refTimeIndex since
 * forecastTime is unlikely to be used.
 */
@org.hibernate.annotations.Table(
		appliesTo = "bufrncwf",
		indexes = {
				@Index(name = "bufrncwf_refTimeIndex", columnNames = { "refTime", "forecastTime" } )
		}
)
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class BUFRncwf extends PersistablePluginDataObject implements
		ISpatialEnabled, IDecoderGettable, IPointData, IPersistable {

	private static final long serialVersionUID = 1L;

	@Embedded
	@DynamicSerializeElement
	private PointDataView pointDataView;

	@Embedded
	@DataURI(position = 1, embedded = true)
	@XmlElement
	@DynamicSerializeElement
	private SurfaceObsLocation location;

	@Transient
	@XmlElement
	@DynamicSerializeElement
	private NCWFFeature detection;

	@Transient
	@XmlElement
	@DynamicSerializeElement
	private NCWFFeature forecast;

	@Transient
	@XmlAttribute
	@DynamicSerializeElement
	private Double stormDir;

	@Transient
	@XmlAttribute
	@DynamicSerializeElement
	private Double stormSpeed;

	@Transient
	@XmlAttribute
	@DynamicSerializeElement
	private Double stormTop;

	/**
	 * Empty constructor.
	 */
	public BUFRncwf() {
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
	public BUFRncwf(String uri) {
		super(uri);
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
	 * @return the detection
	 */
	public NCWFFeature getDetection() {
		return detection;
	}

	/**
	 * @param detection
	 *            the detection to set
	 */
	public void setDetection(NCWFFeature detection) {
		this.detection = detection;
	}

	/**
	 * @return the forecast
	 */
	public NCWFFeature getForecast() {
		return forecast;
	}

	/**
	 * @param forecast
	 *            the forecast to set
	 */
	public void setForecast(NCWFFeature forecast) {
		this.forecast = forecast;
	}

	/**
	 * @return the stormDir
	 */
	public Double getStormDir() {
		return stormDir;
	}

	/**
	 * @param stormDir
	 *            the stormDir to set
	 */
	public void setStormDir(Double stormDir) {
		this.stormDir = stormDir;
	}

	/**
	 * @return the stormSpeed
	 */
	public Double getStormSpeed() {
		return stormSpeed;
	}

	/**
	 * @param stormSpeed
	 *            the stormSpeed to set
	 */
	public void setStormSpeed(Double stormSpeed) {
		this.stormSpeed = stormSpeed;
	}

	/**
	 * @return the stormTop
	 */
	public Double getStormTop() {
		return stormTop;
	}

	/**
	 * @param stormTop
	 *            the stormTop to set
	 */
	public void setStormTop(Double stormTop) {
		this.stormTop = stormTop;
	}

	@Override
	public IDecoderGettable getDecoderGettable() {
		return null;
	}

	@Override
	public ISpatialObject getSpatialObject() {
		return location;
	}

	@Override
	public String getString(String paramName) {
		return null;
	}

	@Override
	public String[] getStrings(String paramName) {
		return null;
	}

	@Override
	public Amount getValue(String paramName) {
		return null;
	}

	@Override
	public Collection<Amount> getValues(String paramName) {
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
    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }
}
