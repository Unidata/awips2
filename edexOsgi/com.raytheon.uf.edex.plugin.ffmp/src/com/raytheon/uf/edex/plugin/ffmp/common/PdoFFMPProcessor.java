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

import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBin;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBinEntry;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.monitor.processing.IMonitorProcessing;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.plugin.ffmp.FFMPGenerator;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Polygon;

/**
 * Processes PDO data for FFMP. Logic extracted from FFMPProcessor.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 07, 2018 6560       njensen     Initial creation
 *
 * </pre>
 *
 * @author njensen
 */

public class PdoFFMPProcessor extends FFMPProcessor {

    protected IMonitorProcessing imp = null;

    /**
     * Public constructor
     * 
     * @param config
     */
    public PdoFFMPProcessor(FFMPConfig config, FFMPGenerator generator,
            FFMPRecord ffmpRec, FFMPTemplates template) {
        super(config, generator, ffmpRec, template);
    }

    @Override
    protected Date initialSetup(Object data) throws Exception {
        Date recdate = null;

        try {
            imp = (IMonitorProcessing) data;
            recdate = getDataDate();
        } catch (Exception e) {
            logAndThrowConfigurationException(getDataType(), e);
        }

        statusHandler.handle(Priority.INFO,
                "Source Expiration: " + source.getExpirationMinutes(siteKey));

        return recdate;
    }

    /**
     * Process the PDO for a geometry
     * 
     * @param pfaf
     * @param geo
     * @return
     */
    private float processPDO(Long pfaf, String cwa) throws Exception {
        List<SourceBinEntry> entries = null;
        float arealWeight = 0;
        float val = 0.0f;

        if (existingSBL) {
            SourceBin bin = sbl.getMap(pfaf);
            if (bin != null) {
                entries = bin.getEntries();
            }
        } else {

            if (cwaGeometries == null || cwaGeometries.isEmpty()) {
                cwaGeometries = template.getRawGeometries(dataKey, cwa);
            }

            Geometry geo = cwaGeometries.get(pfaf);

            if (geo != null) {
                List<SourceBinEntry> newPoints = new ArrayList<>();
                ReferencedCoordinate rc = new ReferencedCoordinate(
                        geo.getCoordinate());
                Coordinate center = null;
                try {
                    center = rc.asGridCell(imp.getGridGeometry(),
                            PixelInCell.CELL_CORNER);
                } catch (TransformException | FactoryException e) {
                    statusHandler.error("PDO error", e);
                    throw e;
                }

                if ((center.x >= 0) && (center.x < imp.getNx())
                        && (center.y >= 0) && (center.y < imp.getNy())) {
                    // add the center one at least
                    SourceBinEntry sbe = new SourceBinEntry();
                    sbe.setCoor(center);
                    sbe.setArea(1.0);
                    newPoints.add(sbe);
                    // keep processing nest's until exhausted
                    int p = 1;
                    while (p > 0) {
                        List<SourceBinEntry> nestPoints = processNest(geo, p,
                                imp.getNx(), imp.getNy(), (int) center.x,
                                (int) center.y, false);

                        if (nestPoints != null && !nestPoints.isEmpty()) {
                            p++;
                            // nestPoints are already in grid coordinates
                            newPoints.addAll(nestPoints);
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
                val += processPDO(sbe.getCoor(), sbe.getArea());
                arealWeight += sbe.getArea();
            }
        }

        return (val / arealWeight);
    }

    /**
     * Process the PDO for a geometry
     * 
     * @param geo
     * @return
     */
    private float processPDO(Coordinate coor, double area) {
        double val = 0.0f;

        if (((int) coor.x >= 0) && (coor.x < imp.getNx()) && ((int) coor.y >= 0)
                && ((int) coor.y < imp.getNy())) {
            val = (imp.getDataArray()[(imp.getNx() * (int) coor.y)
                    + (int) coor.x] * source.getConversion(siteKey));

            val = (val * area);
        }

        return (float) val;
    }

    /**
     * point cache for general PDOs
     * 
     * @throws FactoryException
     * @throws TransformException
     */
    private void setupPDOPointCache()
            throws TransformException, FactoryException {
        pointGeometries = new Geometry[imp.getNx()][imp.getNy()];
        for (int x = 0; x < imp.getNx(); x++) {
            for (int y = 0; y < imp.getNy(); y++) {
                Coordinate[] coors = new Coordinate[5];

                ReferencedCoordinate rc1 = new ReferencedCoordinate(
                        new Coordinate(x, y), imp.getGridGeometry(),
                        Type.GRID_CORNER);
                coors[0] = rc1.asLatLon();

                ReferencedCoordinate rc2 = new ReferencedCoordinate(
                        new Coordinate(x + 1, y), imp.getGridGeometry(),
                        Type.GRID_CORNER);
                coors[1] = rc2.asLatLon();

                ReferencedCoordinate rc3 = new ReferencedCoordinate(
                        new Coordinate(x + 1, y + 1), imp.getGridGeometry(),
                        Type.GRID_CORNER);
                coors[2] = rc3.asLatLon();

                ReferencedCoordinate rc4 = new ReferencedCoordinate(
                        new Coordinate(x, y + 1), imp.getGridGeometry(),
                        Type.GRID_CORNER);
                coors[3] = rc4.asLatLon();
                // complete the square
                coors[4] = coors[0];

                Polygon poly = geomFactory.createPolygon(coors);
                pointGeometries[x][y] = poly;
            }
        }
    }

    @Override
    protected void addExtraLogging(StringBuilder sb) {
        if (imp != null) {
            sb.append("PDO Record: ").append(imp.getClass().getName())
                    .append(" Size: ").append(imp.getDataArray().length)
                    .append("\n");
        }
    }

    @Override
    protected float processBasin(Long pfaf, String cwa) throws Exception {
        return processPDO(pfaf, cwa);
    }

    @Override
    protected float processCoordinate(Coordinate coord, double area) {
        return processPDO(coord, area);
    }

    @Override
    protected void setupCache() throws Exception {
        setupPDOPointCache();
    }

    @Override
    protected Date getDataDate() {
        return imp.getDataTime().getRefTime();
    }

    @Override
    protected GridGeometry2D getGridGeometry() {
        return imp.getGridGeometry();
    }

}
