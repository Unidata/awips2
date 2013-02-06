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
package com.raytheon.viz.dataaccess.rsc.geometry;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.core.rsc.jts.JTSCompiler.PointStyle;
import com.raytheon.viz.dataaccess.rsc.AbstractDataAccessResource;

/**
 * Renders various geometric entities based on geometry data that is retrieved
 * using the Data Access Framework.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2013            bkowal     Initial creation
 * Feb 6, 2013  #1555      bkowal     Improve Geometry Loop
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class GenericGeometryResource extends
        AbstractDataAccessResource<GenericGeometryResourceData> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GenericGeometryResource.class);

    private static final String GENERIC_LEGEND_TEXT = "Generic Geometry ";

    private boolean geometriesReady;

    private List<double[]> pointsToRender;

    private IWireframeShape shape;

    /**
     * Constructor
     * 
     * @param resourceData
     * @param loadProperties
     */
    protected GenericGeometryResource(GenericGeometryResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, GENERIC_LEGEND_TEXT);
        this.geometriesReady = false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.dataaccess.rsc.AbstractDataAccessResource#disposeResource
     * ()
     */
    @Override
    protected void disposeInternal() {
        this.purgeDrawableStorage();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (this.geometriesReady == false) {
            this.prepareData(target);
        }

        // First, draw the points
        if (this.pointsToRender.size() > 0) {
            target.drawPoints(this.pointsToRender,
                    getCapability(ColorableCapability.class).getColor(),
                    IGraphicsTarget.PointStyle.CIRCLE,
                    getCapability(MagnificationCapability.class)
                            .getMagnification().floatValue());
        }

        OutlineCapability outlineCapability = getCapability(OutlineCapability.class);
        // Finally, draw the shape
        if (this.shape != null && outlineCapability.isOutlineOn()) {
            target.drawWireframeShape(this.shape,
                    getCapability(ColorableCapability.class).getColor(),
                    outlineCapability.getOutlineWidth(),
                    outlineCapability.getLineStyle());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.dataaccess.rsc.AbstractDataAccessResource#reprojectResource
     * (org.opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        this.purgeDrawableStorage();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.dataaccess.rsc.AbstractDataAccessResource#prepareData
     * (com.raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void prepareData(IGraphicsTarget target) throws VizException {
        this.initDrawableStorage();

        int numberOfPoints = 0;

        for (IGeometryData geometryData : this.resourceData.getData()) {
            Geometry geometry = geometryData.getGeometry();
            if (geometry instanceof Point) {
                double[] pixels = this.descriptor
                        .worldToPixel(new double[] {
                                geometry.getCoordinate().x,
                                geometry.getCoordinate().y });
                this.pointsToRender.add(pixels);
            } else {
                // Calculate the number of points.

                /*
                 * Theoretically, this should also work for GeometryCollection
                 * because the Multi Geometry types returned by the getGeometryN
                 * method of GeometryCollection support the getNumPoints method
                 * because they (the Multi Geometry Types) extend
                 * GeometryCollection which extends Geometry and getNumPoints is
                 * a method defined by Geometry.
                 */
                for (int i = 0; i < geometry.getNumGeometries(); i++) {
                    Geometry _geometry = geometry.getGeometryN(i);
                    numberOfPoints += _geometry.getNumPoints();
                }
            }
        }

        if (numberOfPoints > 0) {
            // create the wireframe shape
            this.shape = target.createWireframeShape(false, this.descriptor,
                    0.0f);

            JTSCompiler jtsCompiler = new JTSCompiler(null, this.shape,
                    this.descriptor, PointStyle.CROSS);
            this.shape.allocate(numberOfPoints);
            // add the geometries
            for (IGeometryData geometryData : this.resourceData.getData()) {
                try {
                    jtsCompiler.handle((Geometry) geometryData.getGeometry()
                            .clone());
                } catch (VizException e1) {
                    statusHandler.handle(UFStatus.Priority.ERROR,
                            "Failed to handle Geometry "
                                    + geometryData.getGeometry().getClass()
                                            .getName(), e1);
                }
            }

            // compile
            this.shape.compile();
        }
        this.geometriesReady = true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.dataaccess.rsc.AbstractDataAccessResource#buildLegendText
     * ()
     */
    @Override
    protected String buildLegendTextInternal() {
        return StringUtils.EMPTY;
    }

    /**
     * Initializes the lists that will track the geometric entities that we will
     * be drawing.
     */
    private void initDrawableStorage() {
        this.pointsToRender = new ArrayList<double[]>();
    }

    /**
     * Normally invoked when the resource is disposed; purges all saved
     * geometric entities
     */
    public void purgeDrawableStorage() {
        this.pointsToRender = null;
        if (this.shape != null) {
            this.shape.dispose();
        }
        this.shape = null;
        this.geometriesReady = false;
    }
}
