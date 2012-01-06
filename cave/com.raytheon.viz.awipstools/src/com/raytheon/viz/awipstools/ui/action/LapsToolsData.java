package com.raytheon.viz.awipstools.ui.action;

import java.awt.geom.Point2D;

import org.geotools.referencing.GeodeticCalculator;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;

public class LapsToolsData {

    private Coordinate gridCenter = new Coordinate();

    private Integer nx;

    private Integer ny;

    private Integer nz;

    private Double gridSpacing;

    private Double dp;

    private Double lowp;

    private Double lat;

    private Double lat2;

    private Double lon;

    private Coordinate[] corners;

    private Envelope cwaArea;

    private Envelope validArea;

    private Envelope gridArea;

    public Coordinate getGridCenter() {
        return gridCenter;
    }

    public Coordinate getCwaCenter() {
        return cwaArea.centre();
    }

    public Integer getNx() {
        return nx;
    }

    public Integer getNy() {
        return ny;
    }

    public Integer getNz() {
        return nz;
    }

    public Double getGridSpacing() {
        return gridSpacing;
    }

    public Double getDp() {
        return dp;
    }

    public Double getLowp() {
        return lowp;
    }

    public Double getLat() {
        return lat;
    }

    public Double getLat2() {
        return lat2;
    }

    public Double getLon() {
        return lon;
    }

    public Envelope getCwaArea() {
        return cwaArea;
    }

    public Envelope getValidArea() {
        if (validArea == null) {
            double halfHeight = Math.abs(corners[3].x - corners[0].x) / 2;
            double halfWidth = Math.abs(corners[0].y - corners[3].y) / 2;
            double cwa_dist_lat = halfHeight - cwaArea.getHeight() / 2;
            double cwa_dist_lon = halfWidth - cwaArea.getWidth() / 2;
            validArea = new Envelope(cwaArea.centre());
            validArea.expandBy(cwa_dist_lon, cwa_dist_lat);
        }
        return validArea;
    }

    public Envelope getGridArea() {
        if (gridArea == null) {
            double width = gridSpacing * nx;
            double height = gridSpacing * ny;
            GeodeticCalculator gc = new GeodeticCalculator();
            gc.setStartingGeographicPoint(gridCenter.x, gridCenter.y);
            gc.setDirection(0.0, height / 2);
            Point2D top = gc.getDestinationGeographicPoint();
            gc.setStartingGeographicPoint(gridCenter.x, gridCenter.y);
            gc.setDirection(180.0, height / 2);
            Point2D bot = gc.getDestinationGeographicPoint();
            gc.setStartingGeographicPoint(top);
            gc.setDirection(90.0, width / 2);
            Point2D NE = gc.getDestinationGeographicPoint();
            gc.setStartingGeographicPoint(top);
            gc.setDirection(-90.0, width / 2);
            Point2D NW = gc.getDestinationGeographicPoint();
            gc.setStartingGeographicPoint(bot);
            gc.setDirection(90.0, width / 2);
            Point2D SE = gc.getDestinationGeographicPoint();
            gc.setStartingGeographicPoint(bot);
            gc.setDirection(-90.0, width / 2);
            Point2D SW = gc.getDestinationGeographicPoint();

            gridArea = new Envelope();
            gridArea.expandToInclude(NE.getX(), NE.getY());
            gridArea.expandToInclude(NW.getX(), NW.getY());
            gridArea.expandToInclude(SE.getX(), SE.getY());
            gridArea.expandToInclude(SW.getX(), SW.getY());
        }
        return gridArea;
    }

    public Coordinate[] getCorners() {
        return corners;
    }

    public void setCorners(Coordinate[] corners) {
        validArea = null;
        this.corners = corners;
    }

    public void setCwaArea(Envelope cwaArea) {
        validArea = null;
        this.cwaArea = cwaArea;
    }

    public void setGridCenterLon(double lon) {
        gridArea = null;
        gridCenter.x = lon;
    }

    public void setGridCenterLat(double lat) {
        gridArea = null;
        gridCenter.y = lat;
    }

    public void setGridCenter(Coordinate gridCenter) {
        gridArea = null;
        this.gridCenter = gridCenter;
    }

    public void setNx(Integer nx) {
        gridArea = null;
        this.nx = nx;
    }

    public void setNy(Integer ny) {
        gridArea = null;
        this.ny = ny;
    }

    public void setGridSpacing(Double gridSpacing) {
        gridArea = null;
        this.gridSpacing = gridSpacing;
    }

    public void setNz(Integer nz) {
        this.nz = nz;
    }

    public void setDp(Double dp) {
        this.dp = dp;
    }

    public void setLowp(Double lowp) {
        this.lowp = lowp;
    }

    public void setLat(Double lat) {
        this.lat = lat;
    }

    public void setLat2(Double lat2) {
        this.lat2 = lat2;
    }

    public void setLon(Double lon) {
        this.lon = lon;
    }

}
