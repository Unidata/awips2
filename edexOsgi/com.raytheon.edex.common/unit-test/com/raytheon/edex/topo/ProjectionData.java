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
package com.raytheon.edex.topo;

import java.awt.Point;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;

public class ProjectionData {

    private static float COMPARISON_THRESHOLD = 0.005f;

    public static enum ProjectionType {
        NONE, LAMBERT_CONFORMAL, MERCATOR, POLAR_STEREOGRAPHIC, LATLON
    };

    private String projectionID;

    private ProjectionType projectionType;

    private Coordinate latLonLL;

    private Coordinate latLonUR;

    private Coordinate latLonOrigin;

    private double stdParallelOne;

    private double stdParallelTwo;

    private Point gridPointLL;

    private Point gridPointUR;

    private double latIntersect;

    private double lonCenter;

    private double lonOrigin;

    public ProjectionData() {
        latLonLL = new Coordinate();
        latLonUR = new Coordinate();
        latLonOrigin = new Coordinate();
        gridPointLL = new Point();
        gridPointUR = new Point();
    }

    public ProjectionData(String projID, int projType, Coordinate latLonLL,
            Coordinate latLonUR, Coordinate latLonOrig, float stdPar1,
            float stdPar2, Point gridLL, Point gridUR, float latInt,
            float lonCenter, float lonOrig) {
        this();

        this.projectionID = projID;
        this.projectionType = ProjectionType.values()[projType];
        this.latLonLL = latLonLL;
        this.latLonUR = latLonUR;
        this.latLonOrigin = latLonOrig;
        this.stdParallelOne = stdPar1;
        this.stdParallelTwo = stdPar2;
        this.gridPointLL = gridLL;
        this.gridPointUR = gridUR;
        this.latIntersect = latInt;
        this.lonCenter = lonCenter;
        this.lonOrigin = lonOrig;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        String result = "{";

        result += projectionType + ", ";
        result += latLonLL + ", ";
        result += latLonUR + ", ";
        result += latLonOrigin + ", ";
        result += stdParallelOne + ", ";
        result += stdParallelTwo + ", ";
        result += gridPointLL + ", ";
        result += gridPointUR + ", ";
        result += latIntersect + ", ";
        result += lonCenter + ", ";
        result += lonOrigin;

        result += "}";
        return result;
    }

    /**
     * @return
     */
    public CoordinateReferenceSystem getCRS() {
        // construct the appropriate CRS based on the projection type
        CoordinateReferenceSystem crs = null;
        switch (this.projectionType) {
        case LAMBERT_CONFORMAL:
            crs = MapUtil.constructLambertConformal(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, this.stdParallelOne,
                    this.stdParallelTwo, this.latLonOrigin.x);
            break;

        case MERCATOR:
            crs = MapUtil.constructMercator(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, this.stdParallelOne,
                    this.lonCenter);
            break;

        case POLAR_STEREOGRAPHIC:
            crs = MapUtil.constructNorthPolarStereo(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, 60.0, this.lonOrigin);
            break;

        case LATLON:
            crs = MapUtil.LATLON_PROJECTION;
            break;

        case NONE:
        default:
            System.out.println("ERROR: unknown projection type: "
                    + this.projectionType);
        }

        return crs;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ProjectionData)) {
            return false;
        }

        ProjectionData rhs = (ProjectionData) obj;
        if (!this.getProjectionType().equals(rhs.getProjectionType())) {
            return false;
        }

        if (Math.abs(this.latLonLL.y - rhs.latLonLL.y) > COMPARISON_THRESHOLD
                || Math.abs(this.latLonLL.x - rhs.latLonLL.x) > COMPARISON_THRESHOLD
                || Math.abs(this.latLonUR.y - rhs.latLonUR.y) > COMPARISON_THRESHOLD
                || Math.abs(this.latLonLL.x - rhs.latLonLL.x) > COMPARISON_THRESHOLD) {
            return false;
        }

        // specific projection comparisons
        switch (this.projectionType) {
        case LAMBERT_CONFORMAL:
            return (Math.abs(this.latLonOrigin.y - rhs.latLonOrigin.y) < COMPARISON_THRESHOLD
                    || Math.abs(this.latLonOrigin.x - rhs.latLonOrigin.x) < COMPARISON_THRESHOLD
                    || Math.abs(this.stdParallelOne - rhs.stdParallelOne) < COMPARISON_THRESHOLD || Math
                    .abs(this.stdParallelTwo - rhs.stdParallelTwo) < COMPARISON_THRESHOLD);
        case POLAR_STEREOGRAPHIC:
            return (Math.abs(this.lonOrigin - rhs.lonOrigin) < COMPARISON_THRESHOLD);
        case MERCATOR:
            return (Math.abs(this.lonCenter - rhs.lonCenter) < COMPARISON_THRESHOLD);
        case LATLON:
            return true;
        default:
            return false;
        }
    }

    public String getProjectionID() {
        return projectionID;
    }

    public ProjectionType getProjectionType() {
        return projectionType;
    }

    public Coordinate getLatLonLL() {
        return latLonLL;
    }

    public Coordinate getLatLonUR() {
        return latLonUR;
    }

    public Coordinate getLatLonOrigin() {
        return latLonOrigin;
    }

    public double getStdParallelOne() {
        return stdParallelOne;
    }

    public double getStdParallelTwo() {
        return stdParallelTwo;
    }

    public Point getGridPointLL() {
        return gridPointLL;
    }

    public Point getGridPointUR() {
        return gridPointUR;
    }

    public double getLatIntersect() {
        return latIntersect;
    }

    public double getLonCenter() {
        return lonCenter;
    }

    public double getLonOrigin() {
        return lonOrigin;
    }

    public void setProjectionID(String projectionID) {
        this.projectionID = projectionID;
    }

    public void setProjectionType(ProjectionType projectionType) {
        this.projectionType = projectionType;
    }

    public void setLatLonLL(Coordinate latLonLL) {
        this.latLonLL = latLonLL;
    }

    public void setLatLonUR(Coordinate latLonUR) {
        this.latLonUR = latLonUR;
    }

    public void setLatLonOrigin(Coordinate latLonOrigin) {
        this.latLonOrigin = latLonOrigin;
    }

    public void setStdParallelOne(double stdParallelOne) {
        this.stdParallelOne = stdParallelOne;
    }

    public void setStdParallelTwo(double stdParallelTwo) {
        this.stdParallelTwo = stdParallelTwo;
    }

    public void setGridPointLL(Point gridPointLL) {
        this.gridPointLL = gridPointLL;
    }

    public void setGridPointUR(Point gridPointUR) {
        this.gridPointUR = gridPointUR;
    }

    public void setLatIntersect(double latIntersect) {
        this.latIntersect = latIntersect;
    }

    public void setLonCenter(double lonCenter) {
        this.lonCenter = lonCenter;
    }

    public void setLonOrigin(double lonOrigin) {
        this.lonOrigin = lonOrigin;
    }
}
