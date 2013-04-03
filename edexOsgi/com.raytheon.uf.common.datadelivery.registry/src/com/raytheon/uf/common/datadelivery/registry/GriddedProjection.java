package com.raytheon.uf.common.datadelivery.registry;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@XmlRootElement(name = "griddedProjection")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GriddedProjection extends Projection {

    public GriddedProjection() {

    }

    public GriddedProjection(Projection v) {

    }

    /** Latitude of first grid point */
    @XmlElement
    @DynamicSerializeElement
    private double la1;

    /** Longitude of the first grid point */
    @XmlElement
    @DynamicSerializeElement
    private double lo1;

    /** Corner of the first grid point */
    @XmlElement
    @DynamicSerializeElement
    // TODO This comes from an enumeration that is in the Corner class
    private String firstGridPointCorner;

    /** Number of points along I direction */
    @XmlElement
    @DynamicSerializeElement
    private Integer nx;

    /** Number of points along J direction */
    @XmlElement
    @DynamicSerializeElement
    private Integer ny;

    /** I direction increment */
    @XmlElement
    @DynamicSerializeElement
    private double dx;

    /** J direction increment */
    @XmlElement
    @DynamicSerializeElement
    private double dy;

    /** Spacing unit of dx and dy */
    @XmlElement
    @DynamicSerializeElement
    private String spacingUnit;

    public double getLa1() {
        return la1;
    }

    public void setLa1(double la1) {
        this.la1 = la1;
    }

    public double getLo1() {
        return lo1;
    }

    public void setLo1(double lo1) {
        this.lo1 = lo1;
    }

    public String getFirstGridPointCorner() {
        return firstGridPointCorner;
    }

    public void setFirstGridPointCorner(String firstGridPointCorner) {
        this.firstGridPointCorner = firstGridPointCorner;
    }

    public Integer getNx() {
        return nx;
    }

    public void setNx(Integer nx) {
        this.nx = nx;
    }

    public Integer getNy() {
        return ny;
    }

    public void setNy(Integer ny) {
        this.ny = ny;
    }

    public double getDx() {
        return dx;
    }

    public void setDx(double dx) {
        this.dx = dx;
    }

    public double getDy() {
        return dy;
    }

    public void setDy(double dy) {
        this.dy = dy;
    }

    public String getSpacingUnit() {
        return spacingUnit;
    }

    public void setSpacingUnit(String spacingUnit) {
        this.spacingUnit = spacingUnit;
    }

    // TODO We override here because much more goes into a grid projection than
    // just a standard point data projection
}
