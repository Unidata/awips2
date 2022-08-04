package com.raytheon.uf.common.monitor.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.locationtech.jts.geom.Coordinate;

import com.raytheon.uf.common.geospatial.adapter.CoordAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
public class FreezingLevelEntry {

    public FreezingLevelEntry() {

    }

    public FreezingLevelEntry(Coordinate coor, Float freezingLevel) {
        this.coor = coor;
        this.freezingLevel = freezingLevel;
    }

    /** sourceName and dataKey **/
    @DynamicSerializeElement
    @XmlElement
    @XmlJavaTypeAdapter(value = CoordAdapter.class)
    public Coordinate coor;

    /** sourceName and dataKey **/
    @DynamicSerializeElement
    @XmlElement
    public float freezingLevel;

    public void setCoor(Coordinate coor) {
        this.coor = coor;
    }

    public Coordinate getCoordinate() {
        return coor;
    }

    public void setFreezingLevel(float freezingLevel) {
        this.freezingLevel = freezingLevel;
    }

    public float getFreezingLevel() {
        return freezingLevel;
    }

}
