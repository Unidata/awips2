/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */

package com.raytheon.uf.edex.plugin.grib.ogc;

import java.util.Date;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.ogc.common.db.SimpleLayer;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public abstract class GribLayer extends SimpleLayer<GribDimension> {

    private static final long serialVersionUID = 6934503885157257766L;

    @XmlElement
    @DynamicSerializeElement
    protected String coverageName;

    @XmlElement
    @DynamicSerializeElement
    protected String crsWkt;

    @XmlElement
    @DynamicSerializeElement
    protected double nativeMinX;

    @XmlElement
    @DynamicSerializeElement
    protected double nativeMinY;

    @XmlElement
    @DynamicSerializeElement
    protected double nativeMaxX;

    @XmlElement
    @DynamicSerializeElement
    protected double nativeMaxY;

    @XmlElement
    @DynamicSerializeElement
    protected boolean vertical = true;

	public GribLayer() {
	}

    public GribLayer(GribLayer other) {
        super(other);
        this.times = new TreeSet<Date>(other.getTimes());
        this.coverageName = other.getCoverageName();
        this.crsWkt = other.getCrsWkt();
        this.nativeMaxX = other.nativeMaxX;
        this.nativeMaxY = other.nativeMaxY;
        this.nativeMinX = other.nativeMinX;
        this.nativeMinY = other.nativeMinY;
        this.vertical = other.vertical;
    }

    /**
     * @return the coverageName
     */
    public String getCoverageName() {
        return coverageName;
    }

    /**
     * @param coverageName
     *            the coverageName to set
     */
    public void setCoverageName(String coverageName) {
        this.coverageName = coverageName;
    }

    /**
     * @return the crsWkt
     */
    public String getCrsWkt() {
        return crsWkt;
    }

    /**
     * @param crsWkt
     *            the crsWkt to set
     */
    public void setCrsWkt(String crsWkt) {
        this.crsWkt = crsWkt;
    }

    /**
     * @return the nativeMinX
     */
    public double getNativeMinX() {
        return nativeMinX;
    }

    /**
     * @param nativeMinX
     *            the nativeMinX to set
     */
    public void setNativeMinX(double nativeMinX) {
        this.nativeMinX = nativeMinX;
    }

    /**
     * @return the nativeMinY
     */
    public double getNativeMinY() {
        return nativeMinY;
    }

    /**
     * @param nativeMinY
     *            the nativeMinY to set
     */
    public void setNativeMinY(double nativeMinY) {
        this.nativeMinY = nativeMinY;
    }

    /**
     * @return the nativeMaxX
     */
    public double getNativeMaxX() {
        return nativeMaxX;
    }

    /**
     * @param nativeMaxX
     *            the nativeMaxX to set
     */
    public void setNativeMaxX(double nativeMaxX) {
        this.nativeMaxX = nativeMaxX;
    }

    /**
     * @return the nativeMaxY
     */
    public double getNativeMaxY() {
        return nativeMaxY;
    }

    /**
     * @param nativeMaxY
     *            the nativeMaxY to set
     */
    public void setNativeMaxY(double nativeMaxY) {
        this.nativeMaxY = nativeMaxY;
    }

    /**
     * @return the vertical
     */
    public boolean isVertical() {
        return vertical;
    }

    /**
     * @param vertical
     *            the vertical to set
     */
    public void setVertical(boolean vertical) {
        this.vertical = vertical;
    }

}
