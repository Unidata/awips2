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

import java.awt.Rectangle;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.dataplugin.exception.MalformedDataException;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPRecord;
import com.raytheon.uf.common.dataplugin.ffmp.FFMPTemplates;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBin;
import com.raytheon.uf.common.dataplugin.ffmp.SourceBinEntry;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.ReferencedObject.Type;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.xmrg.XmrgFile;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.common.xmrg.hrap.HrapConversionException;
import com.raytheon.uf.edex.plugin.ffmp.FFMPGenerator;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Polygon;

/**
 * Processes xmrg data for FFMP. Logic extracted from FFMPProcessor.
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

public class XmrgFFMPProcessor extends FFMPProcessor {

    /** HRAP subGrid **/
    private HRAPSubGrid hrapgrid = null;

    private XmrgFile xmrg = null;

    private short[][] xmrgData = null;

    private Rectangle extent = null;

    /**
     * Public constructor
     * 
     * @param config
     */
    public XmrgFFMPProcessor(FFMPConfig config, FFMPGenerator generator,
            FFMPRecord ffmpRec, FFMPTemplates template) {
        super(config, generator, ffmpRec, template);
    }

    @Override
    protected Date initialSetup(Object data) throws Exception {
        Date recdate = null;

        try {
            xmrg = (XmrgFile) data;
            xmrgData = getXMRGData();
            ffmpRec.setMetaData(xmrg.getFile().getName());
        } catch (Exception e) {
            logAndThrowConfigurationException(getDataType(), e);
        }

        if (getDataDate().getTime() > 0) {
            recdate = getDataDate();
        } else {
            if (source.getDateFormat() != null) {
                SimpleDateFormat formatter = new SimpleDateFormat(
                        source.getDateFormat());
                int length = source.getDateFormat().length();

                String dateString = xmrg.getFile().getName().substring(
                        (xmrg.getFile().getName().length() - 1) - length,
                        (xmrg.getFile().getName().length() - 1));

                recdate = formatter.parse(dateString);
            } else {
                statusHandler.handle(Priority.ERROR,
                        "Source: " + ffmpRec.getSourceName() + " sitekey: "
                                + siteKey + " File: " + xmrg.getFile().getName()
                                + " : Invalid date header");
            }
        }

        statusHandler.handle(Priority.INFO,
                "Source Expiration: " + source.getExpirationMinutes(siteKey));

        return recdate;
    }

    /**
     * Process XMRG at a basin
     * 
     * @param pfaf
     * @param geo
     * @return
     */
    private float processXMRG(Long pfaf, String cwa) throws Exception {
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
                cwaGeometries = template.getRawGeometries(dataKey, cwa);
            }

            Geometry geo = cwaGeometries.get(pfaf);

            if (geo != null) {
                List<SourceBinEntry> newPoints = new ArrayList<>();
                ReferencedCoordinate rc = new ReferencedCoordinate(
                        geo.getCentroid().getCoordinate());
                Coordinate center = null;
                try {
                    center = rc.asGridCell(
                            getHRAPSubGrid().getHRAP().getGridGeometry(),
                            PixelInCell.CELL_CORNER);

                } catch (Exception e) {
                    statusHandler.error("SourceBinList exception: ", e);
                    throw e;
                }

                double xx = center.x - getHRAPSubGrid().getExtent().x;
                double yy = center.y - getHRAPSubGrid().getExtent().y;

                if (((int) xx >= 0) && ((int) xx < xmrgData[0].length)
                        && ((int) yy >= 0) && ((int) yy < xmrgData[1].length)) {

                    List<SourceBinEntry> nestPoints = null;
                    // add the center one at least
                    SourceBinEntry sbe = new SourceBinEntry();
                    Geometry ptGeo = null;
                    ptGeo = pointGeometries[(int) yy][(int) xx];
                    sbe.setCoor(ptGeo.getCentroid().getCoordinate());
                    sbe.setArea(1.0);
                    newPoints.add(sbe);
                    // keep processing nest's until exhausted
                    int p = 1;
                    while (p > 0) {

                        try {
                            nestPoints = processNest(geo, p,
                                    getHRAPSubGrid().getNy(),
                                    getHRAPSubGrid().getNx(), (int) yy,
                                    (int) xx, true);

                            if (nestPoints != null && !nestPoints.isEmpty()) {
                                p++;
                                newPoints.addAll(nestPoints);
                            } else {
                                p = 0;
                            }
                        } catch (Exception e) {
                            p = 0;
                            statusHandler.handle(Priority.INFO,
                                    "Source: " + ffmpRec.getSourceName()
                                            + " sitekey: " + siteKey
                                            + " domain: " + cwa
                                            + " : Data outside of domain");
                        }
                    }

                    SourceBin bin = new SourceBin(newPoints);
                    entries = bin.getEntries();
                    sbl.addBin(pfaf, bin);
                }
            }
        }

        if (entries != null) {
            for (SourceBinEntry sbe : entries) {
                val += processXMRG(sbe.getCoor(), sbe.getArea());
                arealWeight += sbe.getArea();
            }
        }

        return (val / arealWeight);
    }

    /**
     * Process XMRG at a Coordinate
     * 
     * @param geo
     * @return
     */
    private float processXMRG(Coordinate coor, double area) {
        double val = 0.0;
        Coordinate gridCoor = getHRAPXY(coor);
        gridCoor.x = (int) (gridCoor.x + 0.5);
        gridCoor.y = (int) (gridCoor.y + 0.5);
        if ((gridCoor.x >= 0) && (gridCoor.x < xmrgData[0].length)
                && (gridCoor.y >= 0) && (gridCoor.y < xmrgData.length)) {

            if (xmrgData[(int) gridCoor.y][(int) gridCoor.x] != -899) {
                val = ((xmrgData[(int) gridCoor.y][(int) gridCoor.x])
                        * source.getConversion(siteKey));
                val = val * area;
            }
        }

        return (float) val;
    }

    /**
     * The HRAP sub grid
     * 
     * @return
     */
    private HRAPSubGrid getHRAPSubGrid() {
        return hrapgrid;
    }

    /**
     * gets the XMRG local extents
     * 
     * @return
     */
    private Rectangle getExtents(int hrapGridFactor) {
        Rectangle rect = null;

        try {
            rect = HRAPCoordinates.getHRAPCoordinates();
            rect.setBounds(rect.x * hrapGridFactor, rect.y * hrapGridFactor,
                    rect.width * hrapGridFactor, rect.height * hrapGridFactor);
        } catch (Exception e) {
            statusHandler.error("Error getting HRAP extents", e);
        }
        return rect;
    }

    /**
     * Get HRAP coordinate
     * 
     * @param latLon
     * @return
     */
    private Coordinate getHRAPXY(Coordinate latLon) {
        Coordinate gridCell = null;
        try {
            ReferencedCoordinate rc = new ReferencedCoordinate(latLon);
            HRAPSubGrid subGrid = getHRAPSubGrid();
            Coordinate gridCell2 = rc.asGridCell(
                    subGrid.getHRAP().getGridGeometry(),
                    PixelInCell.CELL_CORNER);
            // gridCell is in terms of parent HRAP, need to modify by extent
            // Rectangle extent = subGrid.getExtent();
            // gridCell.x += extent.x;
            // gridCell.y += extent.y;
            int x = (int) gridCell2.x;
            int y = (int) gridCell2.y;

            x = x - subGrid.getExtent().x;
            y = (subGrid.getExtent().y + subGrid.getNy()) - y;
            gridCell = new Coordinate(x, y, 0.0);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to translate lat lon coordinate: " + latLon, e);
        }
        return gridCell;
    }

    /**
     * Gets the gribPoint
     * 
     * @param gridPoint
     * @return
     */
    private Coordinate getHRAPLatLon(Coordinate gridPoint) {
        try {
            ReferencedCoordinate rc = new ReferencedCoordinate(gridPoint,
                    getHRAPSubGrid().getHRAP().getGridGeometry(),
                    Type.GRID_CORNER);
            gridPoint = rc.asLatLon();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to translate grid coordinate: " + gridPoint, e);
        }
        return gridPoint;
    }

    /**
     * point cache for HRAPs
     * 
     * @param extent
     */
    private void setupXMRGPointCache(Rectangle extent) {
        pointGeometries = new Geometry[extent.height][extent.width];

        for (int y = 0; y < extent.height; y++) {
            for (int x = 0; x < extent.width; x++) {
                int xx = getHRAPSubGrid().getExtent().x + x;
                int yy = getHRAPSubGrid().getExtent().y + y;

                Coordinate[] coors = new Coordinate[5];

                coors[0] = getHRAPLatLon(new Coordinate(xx, yy));
                coors[1] = getHRAPLatLon(new Coordinate(xx + 1, yy));
                coors[2] = getHRAPLatLon(new Coordinate(xx + 1, yy + 1));
                coors[3] = getHRAPLatLon(new Coordinate(xx, yy + 1));

                // complete the square
                coors[4] = coors[0];

                Polygon poly = geomFactory.createPolygon(coors);
                pointGeometries[y][x] = poly;
            }
        }
    }

    @Override
    protected void addExtraLogging(StringBuilder sb) {
        if (xmrg != null) {
            sb.append("XMRG File: ").append(xmrg.getFile().getAbsolutePath())
                    .append("\n");
        }
    }

    /**
     * Gets the XMRG data array, checks HRAP/XMRG config for sanity.
     * 
     * @return
     */
    private short[][] getXMRGData() throws Exception {
        String fileName = "MISSING";

        if (xmrg.getFile() != null) {
            fileName = xmrg.getFile().getAbsolutePath();
        }

        int hrapGridFactor = source.getHrapGridFactor();
        this.extent = getExtents(hrapGridFactor);
        try {
            hrapgrid = new HRAPSubGrid(extent, hrapGridFactor);
        } catch (HrapConversionException e) {
            statusHandler.error("Error loading HRAP sub grid", e);
        }

        if (xmrg.getHrapExtent() != null) {
            xmrgData = xmrg.getData(extent);
        } else {
            throw new MalformedDataException(
                    "The XMRG data is malformed or the file is non-readable. "
                            + fileName);
        }

        return xmrgData;
    }

    @Override
    protected float processBasin(Long pfaf, String cwa) throws Exception {
        return processXMRG(pfaf, cwa);
    }

    @Override
    protected float processCoordinate(Coordinate coord, double area) {
        return processXMRG(coord, area);
    }

    @Override
    protected void setupCache() throws Exception {
        /*
         * all the HPE sources use an HRAP grid so, create it only once.
         */
        setupXMRGPointCache(extent);
    }

    @Override
    protected Date getDataDate() {
        return xmrg.getHeader().getValidDate();
    }

    @Override
    protected GridGeometry2D getGridGeometry() {
        // not supported
        return null;
    }

}
