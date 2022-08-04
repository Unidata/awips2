/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2013 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 2120 South 72nd Street
 * Omaha Tower, Suite 900
 * Omaha, NE 68124 USA
 * 402.291.0100
 *
 */
package com.raytheon.uf.common.dataplugin.nswrc;

import java.text.ParsePosition;

import javax.measure.Unit;
import javax.persistence.Access;
import javax.persistence.AccessType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.hibernate.annotations.Index;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.impl.CoordinateArraySequence;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.crs.ProjectedCRS;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.geospatial.CRSCache;
import com.raytheon.uf.common.geospatial.ISpatialEnabled;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

import tec.uom.se.AbstractUnit;
import tec.uom.se.format.SimpleUnitFormat;

/**
 * Persistence record to store NextGen Surveillance and Weather Radar 
 * Capability (NSWRC) data.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 11, 2013            ekladstrup  Initial creation
 * Apr 22, 2014  3048      mweeks      Updates for peer review and 13.5.4 baseline.
 *
 * </pre>
 *
 * @author ekladstrup
 * @version 1.0
 */
@Entity
@SequenceGenerator(initialValue = 1, name = PluginDataObject.ID_GEN, sequenceName = "nswrcradialseq")
@Table(name = "nswrcradial", uniqueConstraints = { @UniqueConstraint(columnNames = { "dataURI" }) })
@org.hibernate.annotations.Table(appliesTo = "nswrcradial", indexes = { 
		@Index(name = "nswrcradial_refTimeIndex", columnNames = { "refTime", "forecastTime" }) })
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class NSWRCRadialRecord extends PersistablePluginDataObject implements
        ISpatialEnabled, ISpatialObject {

    private static final long serialVersionUID = 6475720118144609096L;

    @Transient
    protected float[] data = null;

    @Transient
    protected float[] signal_to_noise = null;

    @Transient
    protected float[] normalized_coherent_power = null;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    protected Integer numRadials = null;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    protected Integer numBins = null;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    /**
     * bin size in meters
     */
    protected Integer binWidth = null;

    @Column
    @DataURI(position = 1)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    protected String locationName = null;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    protected Float latitude = null;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    protected Float longitude = null;

    @Column
    @DataURI(position = 2)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    protected String productName = null;

    @Column
    @DataURI(position = 3)
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    protected String angleIdentifier = null;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    protected Double elevationAngle = null;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    protected String unit = null;

    @Column
    @DynamicSerializeElement
    @XmlElement(nillable = false)
    protected Integer jStart = null;

    @Transient
    protected float[] angleData = null;

    public NSWRCRadialRecord() {

    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.common.geospatial.ISpatialEnabled#getSpatialObject()
     */

    @Override
    public ISpatialObject getSpatialObject() {
        return this;
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.common.geospatial.ISpatialObject#getGeometry()
     */
    @Override
    public Geometry getGeometry() {
        CoordinateArraySequence caq = new CoordinateArraySequence(
                new Coordinate[] { new Coordinate(longitude, latitude) });
        Point p = new Point(caq, new GeometryFactory());
        return p;
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.common.geospatial.ISpatialObject#getCrs()
     */
    @Override
    public CoordinateReferenceSystem getCrs() {
        // return null, just like RadarStation
        return null;
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.common.geospatial.ISpatialObject#getNx()
     */
    @Override
    public Integer getNx() {
        // like RadarStation
        return 0;
    }

    /*
     * (non-Javadoc)
     *
     * @see com.raytheon.uf.common.geospatial.ISpatialObject#getNy()
     */
    @Override
    public Integer getNy() {
        // like RadarStation
        return 0;
    }

    public float getDataValue(int radial, int bin) {
        if ((radial < numRadials) && (bin < numBins)) {
            return data[radial * numBins + bin];
        }
        return 0.0f;
    }

    public float[] getDataNoisePower(int radial, int bin) {
        if ((radial < numRadials) && (bin < numBins)) {
            int index = radial * numBins + bin;
            float snr = NSWRCConstants.FILL_VALUE;
            float ncp = NSWRCConstants.FILL_VALUE;

            if (signal_to_noise != null) {
                snr = signal_to_noise[index];
            }
            if (normalized_coherent_power != null) {
                ncp = normalized_coherent_power[index];
            }

            return new float[] { data[index], snr, ncp };
        }
        return new float[] { 0.0f, 0.0f, 0.0f };
    }

    /**
     * Gets the parameter unit as a javax.measure.Unit<?> object. If the
     * parameter unit string cannot be successfully converted to a
     * javax.measure.Unit<?> object, AbstractUnit.ONE is returned
     *
     * @return The parameter unit as a javax.measure.Unit<?> object
     */
    public Unit<?> getUnitObject() {
        Unit<?> retVal = AbstractUnit.ONE;

        if (unit != null && !unit.isEmpty()) {
            try {
                retVal = SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).parseProductUnit(unit,
                        new ParsePosition(0));
            } catch (Exception e) {
                // Unable to parse
                retVal = AbstractUnit.ONE;
            }
        }

        return retVal;
    }

    /**
     * @return the data
     */
    public float[] getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(float[] data) {
        this.data = data;
    }

    /**
     * @return the numRadials
     */
    public Integer getNumRadials() {
        return numRadials;
    }

    /**
     * @param numRadials
     *            the numRadials to set
     */
    public void setNumRadials(Integer numRadials) {
        this.numRadials = numRadials;
    }

    /**
     * @return the numBins
     */
    public Integer getNumBins() {
        return numBins;
    }

    /**
     * @param numBins
     *            the numBins to set
     */
    public void setNumBins(Integer numBins) {
        this.numBins = numBins;
    }

    /**
     * @return the binWidth
     */
    public Integer getBinWidth() {
        return binWidth;
    }

    /**
     * @param binWidth
     *            the binWidth to set
     */
    public void setBinWidth(Integer binWidth) {
        this.binWidth = binWidth;
    }

    /**
     * @return the locationName
     */
    public String getLocationName() {
        return locationName;
    }

    /**
     * @param locationName
     *            the locationName to set
     */
    public void setLocationName(String locationName) {
        this.locationName = locationName;
    }

    /**
     * @return the latitude
     */
    public Float getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     *            the latitude to set
     */
    public void setLatitude(Float latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the longitude
     */
    public Float getLongitude() {
        return longitude;
    }

    /**
     * @param longitude
     *            the longitude to set
     */
    public void setLongitude(Float longitude) {
        this.longitude = longitude;
    }

    /**
     * @return the productName
     */
    public String getProductName() {
        return productName;
    }

    /**
     * @param productName
     *            the productName to set
     */
    public void setProductName(String productName) {
        this.productName = productName;
    }

    /**
     * @return the angleIdentifier
     */
    public String getAngleIdentifier() {
        return angleIdentifier;
    }

    /**
     * @param angleIdentifier
     *            the angleIdentifier to set
     */
    public void setAngleIdentifier(String angleIdentifier) {
        this.angleIdentifier = angleIdentifier;
    }

    /**
     * @return the elevationAngle
     */
    public Double getElevationAngle() {
        return elevationAngle;
    }

    /**
     * @param elevationAngle
     *            the elevationAngle to set
     */
    public void setElevationAngle(Double elevationAngle) {
        this.elevationAngle = elevationAngle;
    }

    /**
     * @return the unit
     */
    public String getUnit() {
        return unit;
    }

    /**
     * @param unit
     *            the unit to set
     */
    public void setUnit(String unit) {
        this.unit = unit;
    }

    /**
     * @return the angleData
     */
    public float[] getAngleData() {
        return angleData;
    }

    /**
     * @param angleData
     *            the angleData to set
     */
    public void setAngleData(float[] angleData) {
        this.angleData = angleData;
    }

    /**
     * @return
     */
    public ProjectedCRS getProjectedCrs() {
        return CRSCache.getInstance().constructStereographic(
                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                this.latitude, this.longitude);
    }

    /**
     * @return the jStart
     */
    public Integer getjStart() {
        return jStart;
    }

    /**
     * @param jStart
     *            the jStart to set
     */
    public void setjStart(Integer jStart) {
        this.jStart = jStart;
    }

    /**
     * @return the signal_to_noise
     */
    public float[] getSignal_to_noise() {
        return signal_to_noise;
    }

    /**
     * @param signal_to_noise
     *            the signal_to_noise to set
     */
    public void setSignal_to_noise(float[] signal_to_noise) {
        this.signal_to_noise = signal_to_noise;
    }

    /**
     * @return the normalized_coherent_power
     */
    public float[] getNormalized_coherent_power() {
        return normalized_coherent_power;
    }

    /**
     * @param normalized_coherent_power
     *            the normalized_coherent_power to set
     */
    public void setNormalized_coherent_power(float[] normalized_coherent_power) {
        this.normalized_coherent_power = normalized_coherent_power;
    }

    public NSWRCRadialRecord cloneNoData() {
        NSWRCRadialRecord newRecord = new NSWRCRadialRecord();
        // from parent classes
        newRecord.setDataTime(dataTime);

        // from this class
        newRecord.setAngleData(angleData);
        newRecord.setAngleIdentifier(angleIdentifier);
        newRecord.setBinWidth(binWidth);
        newRecord.setElevationAngle(elevationAngle);
        newRecord.setjStart(jStart);
        newRecord.setLatitude(latitude);
        newRecord.setLocationName(locationName);
        newRecord.setLongitude(longitude);
        newRecord.setNumBins(numBins);
        newRecord.setNumRadials(numRadials);

        return newRecord;
    }

    @Override
    public String getPluginName() {
        return NSWRCConstants.NSWRC_RADIAL;
    }

    @Override
    @Column
    @Access(AccessType.PROPERTY)
    public String getDataURI() {
        return super.getDataURI();
    }

}
