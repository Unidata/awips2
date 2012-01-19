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
package com.raytheon.viz.core.contours.rsc.displays;

import java.nio.FloatBuffer;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.viz.core.contours.util.VectorGraphicsRenderable;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Performs same functions as the original GriddedVectorDisplay using wireframe
 * shapes instead of svg for much faster performance. This is still slightly
 * experimental but seems to work well. It should also have the drawing code
 * extracted to a class similar to PointWindDisplay so wireframe shape barbs and
 * arrows can be used elsewhere.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 22, 2010            bsteffen     Initial creation
 * Feb 07, 2011 7948       bkowal       added a public method to get
 *                                      the direction.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GriddedVectorDisplay extends AbstractGriddedDisplay<Coordinate> {

    private final FloatBuffer magnitude;

    private final FloatBuffer direction;

    private int lineWidth;

    private LineStyle lineStyle;

    private double scale = 0.6;

    private IExtent lastExtent;

    private VectorGraphicsRenderable vectorRenderable;

    private DisplayType displayType;

    private GeodeticCalculator gc;

    /**
     * @param magnitude
     * @param direction
     * @param mode
     * @param descriptor
     * @param gridGeometryOfGrid
     * @param imageSize
     * @param gridLocation
     * @param forceCircle
     * @param plotLocations
     *            Pre-configured plot locations. If null, they will be created.
     */
    public GriddedVectorDisplay(FloatBuffer magnitude, FloatBuffer direction,
            IMapDescriptor descriptor, GeneralGridGeometry gridGeometryOfGrid,
            int size, DisplayType displayType) {
        super(descriptor, gridGeometryOfGrid, size);
        this.magnitude = magnitude;
        this.direction = direction;
        this.displayType = displayType;
        this.gc = new GeodeticCalculator(descriptor.getCRS());
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        if (lastExtent == null
                || !lastExtent.equals(paintProps.getView().getExtent())) {
            disposeResources();
            lastExtent = paintProps.getView().getExtent().clone();
        }
        if (vectorRenderable == null) {
            vectorRenderable = new VectorGraphicsRenderable(descriptor, target,
                    this.size, this.scale);
            super.paint(target, paintProps);
        }
        vectorRenderable.setColor(this.color);
        vectorRenderable.setLineWidth(lineWidth);
        vectorRenderable.setLineStyle(lineStyle);
        vectorRenderable.paint(target);
    }

    @Override
    protected void issueRefresh() {
        lastExtent = null;
        super.issueRefresh();
    }

    protected void paint(Coordinate ijcoord, PaintProperties paintProps,
            Coordinate plotLoc, double adjSize) throws VizException {
        int idx = (int) (ijcoord.x + (ijcoord.y * this.gridDims[0]));

        float spd = this.magnitude.get(idx);
        float dir = this.direction.get(idx) - 180;

        if (dir < -999999 || dir > 9999999) {
            // perhaps this check should limit +/- 180
            return;
        }

        if (Float.isNaN(spd) || Float.isNaN(dir)) {
            return;
        }

        try {
            ReferencedCoordinate rCoord = new ReferencedCoordinate(
                    gridGeometryOfGrid, ijcoord);
            Coordinate latLon = rCoord.asLatLon();
            latLon.x = MapUtil.correctLon(latLon.x);
            double[] stationLocation = { latLon.x, latLon.y };
            double[] stationPixelLocation = this.descriptor
                    .worldToPixel(stationLocation);

            if (stationPixelLocation != null) {
                stationPixelLocation[1]--;
                double[] newWorldLocation = this.descriptor
                        .pixelToWorld(stationPixelLocation);
                this.gc.setStartingGeographicPoint(stationLocation[0],
                        stationLocation[1]);
                this.gc.setDestinationGeographicPoint(newWorldLocation[0],
                        newWorldLocation[1]);
            }

            dir = dir
                    + (float) MapUtil.rotation(latLon,
                            GridGeometry2D.wrap(gridGeometryOfGrid));
            dir -= this.gc.getAzimuth();
        } catch (Exception e) {
            throw new VizException(e);
        }

        dir = (float) Math.toRadians(dir);
        switch (displayType) {
        case ARROW:
            vectorRenderable.paintArrow(plotLoc, adjSize, spd, dir);
            break;
        case BARB:
            vectorRenderable.paintBarb(plotLoc, adjSize, spd, dir);
            break;
        case DUALARROW:
            vectorRenderable.paintDualArrow(plotLoc, adjSize, spd, dir);
            break;
        default:
            throw new VizException("Unsupported disply type: " + displayType);
        }
    }

    /**
     * 
     * @param color
     */
    public void setScale(double scale) {
        this.scale = scale;
    }

    public void setLineWidth(int lineWidth) {
        this.lineWidth = lineWidth;
    }

    /**
     * @param lineStyle
     */
    public void setLineStyle(LineStyle lineStyle) {
        this.lineStyle = lineStyle;
    }

    /**
     * @param density
     *            the density to set
     */
    public boolean setDensity(double density) {
        if (super.setDensity(density)) {
            disposeResources();
            if (this.target != null) {
                this.target.setNeedsRefresh(true);
            }
            return true;
        }
        return false;
    }

    /**
     * @param magnification
     *            the magnification to set
     */
    public boolean setMagnification(double magnification) {
        if (super.setMagnification(magnification)) {
            disposeResources();
            if (this.target != null) {
                this.target.setNeedsRefresh(true);
            }
            return true;
        }
        return false;
    }

    /**
     * @return the magnitude
     */
    public FloatBuffer getMagnitude() {
        return magnitude;
    }

    public FloatBuffer getDirection() {
        return direction;
    }

    protected void disposeResources() {
        if (vectorRenderable != null) {
            vectorRenderable.dispose();
            vectorRenderable = null;
        }
    }

    protected Coordinate createResource(Coordinate coord) throws VizException {
        return coord;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.contours.rsc.displays.AbstractGriddedImageDisplay
     * #getImage(com.raytheon.uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    protected Coordinate getResource(Coordinate coord) {
        return coord;
    }

}
