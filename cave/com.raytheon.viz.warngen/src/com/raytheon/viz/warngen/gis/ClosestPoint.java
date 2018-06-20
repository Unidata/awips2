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
package com.raytheon.viz.warngen.gis;

import java.util.Date;
import java.util.List;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;

/**
 * 
 * ClosestPoint - Provides a descriptor of where a closest point to an event is
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Dec 11, 2007 #601        chammack    Initial Creation.
 *    APr 18, 2012 #14733      Qinglu Lin  David's fix is used, which adds 
 *                                         a copy constructor.
 *    Sep 25, 2012 #15425      Qinglu Lin  Updated two ClosestPoint() and added getGid().
 *    Oct 17, 2012             jsanchez    Added setter methods.
 *    Feb 12, 2013  1600       jsanchez    Removed adjustAngle method.
 *    Mar 25, 2013  1605       jsanchez    Added prepGeom if an urban bound area.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ClosestPoint implements Comparable<ClosestPoint> {
    protected String name;

    protected String area;

    protected String parentArea;

    protected Coordinate point;

    protected double distance;

    protected int roundedDistance;

    protected double azimuth;

    protected double roundedAzimuth;

    protected double oppositeAzimuth;

    protected double oppositeRoundedAzimuth;

    protected int population;

    protected int warngenlev;

    protected Date time;

    protected List<String> partOfArea;

    protected int gid;

    protected PreparedGeometry prepGeom;

    public ClosestPoint() {

    }

    public ClosestPoint(ClosestPoint o) {
        this.name = o.name;
        this.area = o.area;
        this.parentArea = o.parentArea;
        this.point = o.point;
        this.distance = o.distance;
        this.roundedDistance = o.roundedDistance;
        this.azimuth = o.azimuth;
        this.roundedAzimuth = o.roundedAzimuth;
        this.oppositeAzimuth = o.oppositeAzimuth;
        this.oppositeRoundedAzimuth = o.oppositeRoundedAzimuth;
        this.population = o.population;
        this.warngenlev = o.warngenlev;
        this.time = o.time;
        this.partOfArea = o.partOfArea;
        this.gid = o.gid;
    }

    public ClosestPoint(String name, Coordinate point) {
        this(name, point, 0, 0, null, 0);
    }

    public ClosestPoint(String name, Coordinate point, int population,
            int warngenlev, List<String> partOfArea, int gid) {
        this.name = name;
        this.point = point;
        this.population = population;
        this.warngenlev = warngenlev;
        this.partOfArea = partOfArea;
        this.gid = gid;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    public Coordinate getPoint() {
        return point;
    }

    /**
     * @return the distance
     */
    public double getDistance() {
        return distance;
    }

    /**
     * @return the azimuth
     */
    public double getAzimuth() {
        return azimuth;
    }

    /**
     * @return the roundedDistance
     */
    public int getRoundedDistance() {
        return roundedDistance;
    }

    /**
     * @return the roundedAzimuth
     */
    public double getRoundedAzimuth() {
        return roundedAzimuth;
    }

    public double getOppositeAzimuth() {
        return oppositeAzimuth;
    }

    public double getOppositeRoundedAzimuth() {
        return oppositeRoundedAzimuth;
    }

    public int getPopulation() {
        return population;
    }

    public int getWarngenlev() {
        return warngenlev;
    }

    public String getArea() {
        return area;
    }

    public String getParentArea() {
        return parentArea;
    }

    public List<String> getPartOfArea() {
        return partOfArea;
    }

    public int getGid() {
        return gid;
    }

    public Date getTime() {
        return time;
    }

    public void setTime(Date time) {
        this.time = time;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setArea(String area) {
        this.area = area;
    }

    public void setParentArea(String parentArea) {
        this.parentArea = parentArea;
    }

    public void setPoint(Coordinate point) {
        this.point = point;
    }

    public void setDistance(double distance) {
        this.distance = distance;
    }

    public void setRoundedDistance(int roundedDistance) {
        this.roundedDistance = roundedDistance;
    }

    public void setAzimuth(double azimuth) {
        this.azimuth = azimuth;
    }

    public void setRoundedAzimuth(double roundedAzimuth) {
        this.roundedAzimuth = roundedAzimuth;
    }

    public void setOppositeAzimuth(double oppositeAzimuth) {
        this.oppositeAzimuth = oppositeAzimuth;
    }

    public void setOppositeRoundedAzimuth(double oppositeRoundedAzimuth) {
        this.oppositeRoundedAzimuth = oppositeRoundedAzimuth;
    }

    public void setPopulation(int population) {
        this.population = population;
    }

    public void setWarngenlev(int warngenlev) {
        this.warngenlev = warngenlev;
    }

    public void setPartOfArea(List<String> partOfArea) {
        this.partOfArea = partOfArea;
    }

    public void setGid(int gid) {
        this.gid = gid;
    }

    public void setPrepGeom(PreparedGeometry prepGeom) {
        this.prepGeom = prepGeom;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(ClosestPoint o) {
        if (o == null)
            return 1;

        return Double.compare(this.distance, o.distance);
    }

    public static ClosestPoint min(ClosestPoint cp1, ClosestPoint cp2) {
        if (cp1 == null)
            return cp2;
        else if (cp2 == null)
            return cp1;
        else if (cp1.compareTo(cp2) <= 0)
            return cp1;
        else
            return cp2;
    }

}