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
package com.raytheon.uf.edex.plugin.fog.common;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.monitor.data.AdjacentWfoMgr;
import com.raytheon.uf.edex.cpgsrv.CompositeProductGenerator;
import com.raytheon.uf.edex.plugin.fog.FogDbUtils;
import com.raytheon.uf.edex.plugin.fog.FogGenerator;
import com.raytheon.uf.edex.plugin.fog.FogURIFilter;
import com.raytheon.uf.edex.plugin.fog.FogURIGenerateMessage;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

public class FogConfig {

    private final int[] minIndex = null;

    private final int[] maxIndex = null;

    /** VIS URI */
    private String visURI = null;

    /** IR 3.9 URI */
    private String ir3_9URI = null;

    /** IR 10.7 URI */
    private String ir10_7URI = null;

    /** VIS Sat Rec */
    private int[] visRec = null;

    /** D VIL Rec */
    private int[] ir3_9Rec = null;

    /** ET Rec */
    private int[] ir10_7Rec = null;

    /** Our generator reference */
    private FogGenerator foggen = null;

    /** the nader point for the satellite */
    private Coordinate satPos = null;

    /** the center of the monitoring area */
    private Coordinate center = null;

    /** The distance in km to the satellite */
    private double satHeight = 0.0;

    /** the cwa */
    private String cwa = null;

    /** dimensions of the data */
    public static final int dimensions = 2;

    /** dx for this Area **/
    public float dx = 0;

    /** dy for this Area **/
    public float dy = 0;

    /** number of x's for this monitor area **/
    public int nx = 0;

    /** number of y's for this monitor area **/
    public int ny = 0;

    /** visible nx **/
    public int visNx = 0;

    /** visible ny **/
    public int visNy = 0;

    /** ir nx **/
    public int irNx = 0;

    /** ir ny **/
    public int irNy = 0;

    public Coordinate upperLeftCorner = null;

    public Coordinate lowerRightCorner = null;

    public FogConfig(FogURIGenerateMessage genMessage, FogGenerator generator)
            throws Exception {

        this.foggen = generator;
        setCwa(genMessage.getCwa());
        visURI = genMessage.getURI(FogURIFilter.vis).replaceAll("\\\\", "");
        ir3_9URI = genMessage.getURI(FogURIFilter.ir3_9).replaceAll("\\\\", "");
        ir10_7URI = genMessage.getURI(FogURIFilter.ir10_7).replaceAll("\\\\",
                "");

        try {
            SatelliteRecord irSatRec = FogDbUtils.getSatRecord(ir3_9URI);
            SatelliteRecord visSatRec = FogDbUtils.getSatRecord(visURI);

            setSatHeight(irSatRec.getSatHeight());
            setSatPos(new Coordinate(irSatRec.getSatSubPointLon(),
                    irSatRec.getSatSubPointLat()));
            setDx(irSatRec);
            setDy(irSatRec);

            // big grid coverage
            GridGeometry2D visGridGeo = FogDbUtils.getGridGeometry(visSatRec);
            GridGeometry2D irGridGeo = FogDbUtils.getGridGeometry(irSatRec);

            // our new coverage boundaries including adjacent areas
            Geometry monitorAreaGeo = AdjacentWfoMgr.getAdjacentAreas(getCwa());
            // (SK) Geometry monitorAreaGeo =
            // FogDbUtils.getMonitoringAreaGeometry(filter.getCwaGeometry(),getCwa());
            setCenter(monitorAreaGeo.getCentroid().getCoordinate());
            Coordinate[] coords = monitorAreaGeo.getEnvelope().getCoordinates();
            this.upperLeftCorner = coords[1];
            this.lowerRightCorner = coords[3];

            FogGeometry irFogGeometry = getSubGribCoverage(irGridGeo);
            FogGeometry visFogGeometry = getSubGribCoverage(visGridGeo);

            // down sample vis grid to 4km res
            int[] fullArray = FogDbUtils.getSatGridData(visURI,
                    visFogGeometry.getMinIndex(), visFogGeometry.getMaxIndex());
            this.visRec = FogDbUtils.reSizeIntGrid(fullArray,
                    irFogGeometry.getNx(), irFogGeometry.getNy(),
                    visFogGeometry.getNx(), visFogGeometry.getNy());
            this.ir3_9Rec = FogDbUtils.getSatGridData(ir3_9URI,
                    irFogGeometry.getMinIndex(), irFogGeometry.getMaxIndex());
            this.ir10_7Rec = FogDbUtils.getSatGridData(ir10_7URI,
                    irFogGeometry.getMinIndex(), irFogGeometry.getMaxIndex());

            this.nx = irFogGeometry.getNx();
            this.ny = irFogGeometry.getNy();

        } catch (Exception e) {
            e.printStackTrace();
            throw new Exception("FogConfig: FogGenerator cannot run....");
        }
    }

    public CompositeProductGenerator getGenerator() {
        return foggen;
    }

    /**
     * sets the main cwa for this fog monitor
     * 
     * @param icao
     */
    public void setCwa(String cwa) {
        this.cwa = cwa;
    }

    /**
     * gets the cwa
     * 
     * @return
     */
    public String getCwa() {
        return cwa;
    }

    /**
     * Gets the Lat/Lon coord
     * 
     * @return
     */
    public Coordinate getSatPos() {
        return satPos;
    }

    /**
     * set the center lat lon
     * 
     * @param lat
     * @param lon
     */
    public void setSatPos(Coordinate satPos) {
        this.satPos = satPos;
    }

    /**
     * Sets the satellite height above the ground
     * 
     * @param satHeight
     */
    public void setSatHeight(double satHeight) {
        this.satHeight = satHeight;
    }

    /**
     * Gets the satellite height above the ground
     * 
     * @return
     */
    public double getSatHeight() {
        return satHeight;
    }

    /**
     * Data record for the vis array
     * 
     * @return
     */
    public int[] getVis() {
        return visRec;
    }

    /**
     * data record for the 3.9 um array
     * 
     * @return
     */
    public int[] getIR3_9() {
        return ir3_9Rec;
    }

    /**
     * data record for the 10.7 um array
     * 
     * @return
     */
    public int[] getIR10_7() {
        return ir10_7Rec;
    }

    /**
     * Sets the max and min indexes for the subgrid
     */
    public FogGeometry getSubGribCoverage(GridGeometry2D gridGeo) {
        // under assumption this works from upper left around and back
        ReferencedCoordinate ulrc = new ReferencedCoordinate(new Coordinate(
                upperLeftCorner.x, upperLeftCorner.y));
        ReferencedCoordinate lrrc = new ReferencedCoordinate(new Coordinate(
                lowerRightCorner.x, lowerRightCorner.y));

        Coordinate upperLeft = null;
        Coordinate lowerRight = null;

        try {
            upperLeft = ulrc.asGridCell(gridGeo, PixelInCell.CELL_CENTER);
            lowerRight = lrrc.asGridCell(gridGeo, PixelInCell.CELL_CENTER);
        } catch (TransformException e) {
            e.printStackTrace();
        } catch (FactoryException e) {
            e.printStackTrace();
        }

        int minx = (int) upperLeft.x;
        int maxx = (int) lowerRight.x;
        int miny = (int) upperLeft.y;
        int maxy = (int) lowerRight.y;

        FogGeometry fogGeometry = new FogGeometry();
        // set our total number of x/y's
        fogGeometry.setNx(maxx - minx);
        fogGeometry.setNy(maxy - miny);

        fogGeometry.setMinIndex(new int[] { minx, miny });
        fogGeometry.setMaxIndex(new int[] { maxx, maxy });

        return fogGeometry;
    }

    /**
     * Gets the DX value in meters
     * 
     * @param rec
     * @return
     */
    private void setDx(SatelliteRecord rec) {

        dx = rec.getCoverage().getDx();
    }

    /**
     * Gets the DX for this fog monitor area
     * 
     * @return
     */
    public float getDx() {
        return dx;
    }

    /**
     * Gets the DX value in meters
     * 
     * @param rec
     * @return
     */
    private void setDy(SatelliteRecord rec) {

        dy = rec.getCoverage().getDy();
    }

    /**
     * Gets the Dy for this fog monitor area
     * 
     * @return
     */
    public float getDy() {
        return dy;
    }

    /**
     * number of total x's
     * 
     * @return
     */
    public int getNx() {
        return nx;
    }

    /**
     * total number of y's
     * 
     * @return
     */
    public int getNy() {
        return ny;
    }

    /**
     * min extents
     * 
     * @return
     */
    public int[] getMinExtent() {
        return minIndex;
    }

    /**
     * max extents
     * 
     * @return
     */
    public int[] getMaxExtent() {
        return maxIndex;
    }

    /**
     * Gets you the center point for the monitoring area
     * 
     * @return
     */
    public Coordinate getCenter() {
        return center;
    }

    public void setCenter(Coordinate center) {
        this.center = center;
    }

}
