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
package com.raytheon.uf.edex.plugin.ffmp.common;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.ffmp.FFMPConfigurationException;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPUtils;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBin;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBinEntry;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.plugin.ffmp.FFMPGenerator;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Polygon;

/**
 * Processes grid data for FFMP. Logic extracted from FFMPProcessor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 07, 2018 6560       njensen     Initial creation
 * Aug 14, 2018 6720       njensen     Use simplified enums
 *
 * </pre>
 *
 * @author njensen
 */
public class GridFFMPProcessor extends FFMPProcessor {

    protected GridRecord gribRec = null;

    protected float[] gribData = null;

    /** FFG geometry **/
    protected int ffgNx = 0;

    /** FFG geometry **/
    protected int ffgNy = 0;

    /** FFG geometry **/
    protected GridGeometry2D ffgGeometry = null;

    public GridFFMPProcessor(FFMPConfig config, FFMPGenerator generator,
            FFMPRecord ffmpRec, FFMPTemplates template) {
        super(config, generator, ffmpRec, template);
    }

    @Override
    protected Date initialSetup(Object data) throws Exception {
        Date recdate = null;

        try {
            gribRec = (GridRecord) data;
            ffgNx = gribRec.getSpatialObject().getNx();
            ffgNy = gribRec.getSpatialObject().getNy();
            ffgGeometry = MapUtil.getGridGeometry(gribRec.getSpatialObject());
            gribData = this.retrieveGridData(gribRec);
            recdate = getDataDate();
        } catch (Exception e) {
            logAndThrowConfigurationException(getDataType(), e);
        }

        statusHandler.handle(Priority.INFO,
                "Source Expiration: " + source.getExpirationMinutes(siteKey));

        return recdate;
    }

    /**
     * Process the grib types
     * 
     * @param pfaf
     * @param geo
     * @return
     */
    private float processGrib(Long pfaf, String cwa) throws Exception {
        List<SourceBinEntry> entries = null;
        float arealWeight = 0.0f;
        float val = 0.0f;

        if (existingSBL) {
            SourceBin bin = sbl.getMap(pfaf);
            if (bin != null) {
                entries = bin.getEntries();
            }
        } else {
            if (cwaGeometries == null || cwaGeometries.isEmpty()) {
                cwaGeometries = template.getRawGeometries(siteKey, cwa);
            }

            Geometry geo = cwaGeometries.get(pfaf);

            if (geo != null) {
                List<SourceBinEntry> newPoints = new ArrayList<>();
                ReferencedCoordinate rc = new ReferencedCoordinate(
                        geo.getCentroid().getCoordinate());
                Coordinate center = null;
                try {
                    center = rc.asGridCell(getGridGeometry(),
                            PixelInCell.CELL_CORNER);
                } catch (TransformException | FactoryException e) {
                    statusHandler.handle(Priority.ERROR,
                            "Error in geometry, pfafId: " + pfaf);
                    throw e;
                }

                if (((int) center.x >= 0) && ((int) center.x < getNx())
                        && ((int) center.y >= 0)
                        && ((int) center.y < getNy())) {
                    // add at least the center point
                    SourceBinEntry sbe = new SourceBinEntry();
                    sbe.setCoor(center);
                    sbe.setArea(1.0);
                    newPoints.add(sbe);
                    // keep processing nest's until exhausted
                    int p = 1;
                    while (p > 0) {
                        List<SourceBinEntry> nestPoints = processNest(geo, p,
                                getNx(), getNy(), (int) center.x,
                                (int) center.y, false);

                        if (nestPoints != null && !nestPoints.isEmpty()) {
                            newPoints.addAll(nestPoints);
                            p++;
                        } else {
                            p = 0;
                        }
                    }

                    SourceBin bin = new SourceBin(newPoints);
                    entries = bin.getEntries();
                    sbl.addBin(pfaf, bin);
                }
            }
        }

        if (entries != null) {
            // process the values for the points
            for (SourceBinEntry sbe : entries) {
                float thisVal = processGrib(sbe.getCoor(), sbe.getArea());
                if (thisVal != FFMPUtils.MISSING) {
                    val += thisVal;
                    arealWeight += sbe.getArea();
                }
            }
        }

        /*
         * Sparse data conditions differ when acting as a GUIDANCE source or
         * QPE/QPF
         */
        if (val == FFMPUtils.MISSING) {
            if (source.isGuidance()) {
                return FFMPUtils.MISSING;
            } else {
                return 0.0f;
            }
        }
        // don't waste time calculating, it's zero regardless.
        if (val == 0.0f) {
            return val;
        }

        return (val / arealWeight);
    }

    /**
     * Process the grib coor
     * 
     * @param geo
     * @return
     */
    private float processGrib(Coordinate coor, double area) {
        double val = FFMPUtils.MISSING;

        if (((int) coor.x >= 0) && ((int) coor.x <= getNx())
                && ((int) coor.y >= 0) && ((int) coor.y <= getNy())) {
            int dataBin = (getNx() * (int) coor.y) + (int) coor.x;
            if (gribData[dataBin] > 0.0) {
                val = (gribData[dataBin] * source.getConversion(siteKey));
                val = val * area;
            }
        }

        return (float) val;
    }

    /**
     * Gets FFG geometry
     * 
     * @return
     */
    @Override
    protected GridGeometry2D getGridGeometry() {
        return ffgGeometry;
    }

    /**
     * Gets FFG nx
     * 
     * @return
     */
    private int getNx() {
        return ffgNx;
    }

    /**
     * Gets FFG ny
     * 
     * @return
     */
    private int getNy() {
        return ffgNy;
    }

    /**
     * point cache for Grids
     * 
     * @throws FactoryException
     * @throws TransformException
     */
    private void setupGridPointCache()
            throws TransformException, FactoryException {
        // pointCache = new Point[getNx()][getNy()];
        pointGeometries = new Geometry[getNx()][getNy()];
        for (int x = 0; x < getNx(); x++) {
            for (int y = 0; y < getNy(); y++) {
                Coordinate[] coors = new Coordinate[5];

                ReferencedCoordinate rc1 = new ReferencedCoordinate(
                        new Coordinate(x, y), getGridGeometry(),
                        Type.GRID_CORNER);
                coors[0] = rc1.asLatLon();

                ReferencedCoordinate rc2 = new ReferencedCoordinate(
                        new Coordinate(x + 1, y), getGridGeometry(),
                        Type.GRID_CORNER);
                coors[1] = rc2.asLatLon();

                ReferencedCoordinate rc3 = new ReferencedCoordinate(
                        new Coordinate(x + 1, y + 1), getGridGeometry(),
                        Type.GRID_CORNER);
                coors[2] = rc3.asLatLon();

                ReferencedCoordinate rc4 = new ReferencedCoordinate(
                        new Coordinate(x, y + 1), getGridGeometry(),
                        Type.GRID_CORNER);
                coors[3] = rc4.asLatLon();
                // complete the square
                coors[4] = coors[0];

                Polygon poly = geomFactory.createPolygon(coors);
                pointGeometries[x][y] = poly;
            }
        }
    }

    /**
     * Retrieve the float array from the GridRecord
     * 
     * @param rec
     * @return
     */
    public float[] retrieveGridData(GridRecord rec) {
        return ((FloatDataRecord) rec.getMessageData()).getFloatData();
    }

    /**
     * Log bad configuration message and throw Configuration Exception
     * 
     * @param type
     * @param e
     * @throws FFMPConfigurationException
     */
    @Override
    protected void addExtraLogging(StringBuilder sb) {
        if (gribRec != null) {
            sb.append("Record: ").append(gribRec.getDataURI()).append("\n");
        }
    }

    @Override
    protected float processBasin(Long pfaf, String cwa) throws Exception {
        return processGrib(pfaf, cwa);
    }

    @Override
    protected float processCoordinate(Coordinate coord, double area) {
        return processGrib(coord, area);
    }

    @Override
    protected void setupCache() throws Exception {
        setupGridPointCache();
    }

    @Override
    protected Date getDataDate() {
        return gribRec.getDataTime().getRefTime();
    }

}
