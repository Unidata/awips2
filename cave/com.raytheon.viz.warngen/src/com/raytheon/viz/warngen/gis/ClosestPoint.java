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

import com.vividsolutions.jts.geom.Coordinate;

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
	}

	public ClosestPoint(String name, Coordinate point) {
        this(name, point, 0, 0);
    }

    public ClosestPoint(String name, Coordinate point, int population,
            int warngenlev) {
        this.name = name;
        this.point = point;
        this.population = population;
        this.warngenlev = warngenlev;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
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

    /**
     * Adjusts the angle from -360/360 to be between -180/180
     * 
     * @param angle
     * @return
     */
    public static double adjustAngle(double angle) {
        double newVal = angle % 360;
        if (newVal > 180) {
            newVal -= 360;
        } else if (newVal < -180) {
            newVal += 360;
        }

        if (newVal < 0) {
            newVal += 360;
        }
        return newVal;
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