package com.raytheon.uf.common.dataplugin.ffmp;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class SourceBinEntry {

    /** sourceName and dataKey **/
    @DynamicSerializeElement
    @XmlElement
    public Coordinate coor;

    @DynamicSerializeElement
    @XmlElement
    public double area;

    public Coordinate getCoor() {
        return coor;
    }

    public void setCoor(Coordinate coor) {
        this.coor = coor;
    }

    public double getArea() {
        return area;
    }

    public void setArea(double area) {
        this.area = area;
    }

}
