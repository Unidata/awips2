package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;
import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class FreezingLevelXML {

    @DynamicSerializeElement
    @XmlElement(name = "forecastHour")
    protected int forecastHour;

    @DynamicSerializeElement
    @XmlElement(name = "date")
    protected Date date;

    /** sourceName and dataKey **/
    @DynamicSerializeElement
    @XmlElement
    public double[] lats;

    @DynamicSerializeElement
    @XmlElement
    public double[] lons;

    @DynamicSerializeElement
    @XmlElement
    public float[] freezingLevels;

    public ArrayList<FreezingLevelEntry> entries = null;

    public FreezingLevelXML() {

    }

    public FreezingLevelXML(ArrayList<Coordinate> coors,
            ArrayList<Float> fLevels) {
        this.lats = new double[coors.size()];
        this.lons = new double[coors.size()];
        this.freezingLevels = new float[coors.size()];
        for (int i = 0; i < coors.size(); i++) {
            lons[i] = coors.get(i).x;
            lats[i] = coors.get(i).y;
            freezingLevels[i] = fLevels.get(i);
        }
    }

    public FreezingLevelXML(ArrayList<FreezingLevelEntry> fles) {
        this.lats = new double[fles.size()];
        this.lons = new double[fles.size()];
        this.freezingLevels = new float[fles.size()];
        for (int i = 0; i < fles.size(); i++) {
            FreezingLevelEntry fle = fles.get(i);
            lons[i] = fle.getCoordinate().x;
            lats[i] = fle.getCoordinate().y;
            freezingLevels[i] = fle.getFreezingLevel();
        }
    }

    public void setForecastHour(int forecastHour) {
        this.forecastHour = forecastHour;
    }

    public int getForecastHour() {
        return forecastHour;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public Date getDate() {
        return date;
    }

    public double[] getLats() {
        return lats;
    }

    public void setLats(double[] lats) {
        this.lats = lats;
    }

    public double[] getLons() {
        return lons;
    }

    public void setLons(double[] lons) {
        this.lons = lons;
    }

    public float[] getFreezingLevels() {
        return freezingLevels;
    }

    public void setFreezingLevels(float[] freezingLevels) {
        this.freezingLevels = freezingLevels;
    }

    /**
     * Gets the entries for the freezing levels
     * 
     * @return
     */
    public ArrayList<FreezingLevelEntry> getEntries() {

        if (entries == null) {
            entries = new ArrayList<FreezingLevelEntry>(freezingLevels.length);
            for (int i = 0; i < freezingLevels.length; i++) {
                FreezingLevelEntry fle = new FreezingLevelEntry();
                fle.setCoor(new Coordinate(lons[i], lats[i]));
                fle.setFreezingLevel(freezingLevels[i]);
                entries.add(fle);
            }
        }

        return entries;
    }

    /**
     * Get entry at coordinate
     * 
     * @return
     */
    public FreezingLevelEntry getEntry(Coordinate coor) {

        for (FreezingLevelEntry fle : getEntries()) {
            Coordinate flecoor = fle.getCoordinate();
            if (flecoor.x == coor.x && flecoor.y == coor.y) {
                return fle;
            }
        }

        return null;
    }

    public Coordinate[] getCoordinates() {
        Coordinate[] coors = new Coordinate[lats.length];
        for (int i = 0; i < lats.length; i++) {
            coors[i] = new Coordinate(lons[i], lats[i]);
        }
        return coors;
    }

}
