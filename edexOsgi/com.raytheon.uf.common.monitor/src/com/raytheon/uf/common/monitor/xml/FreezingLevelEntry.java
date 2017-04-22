package com.raytheon.uf.common.monitor.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

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
