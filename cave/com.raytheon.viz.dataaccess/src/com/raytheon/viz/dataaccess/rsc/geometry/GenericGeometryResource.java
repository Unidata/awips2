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
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataaccess.geom.IGeometryData;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
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
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

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

    private static class FrameData {

        public final List<double[]> pointsToRender;

        public final IWireframeShape shape;

        public FrameData(List<double[]> pointsToRender, IWireframeShape shape) {
            this.pointsToRender = pointsToRender;
            this.shape = shape;
        }

    }

    private Map<DataTime, FrameData> renderableData = new HashMap<DataTime, FrameData>();

    /**
     * Constructor
     * 
     * @param resourceData
     * @param loadProperties
     */
    protected GenericGeometryResource(GenericGeometryResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, GENERIC_LEGEND_TEXT);
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
        DataTime timeToPaint = null;
        if (!isTimeAgnostic()) {
            timeToPaint = paintProps.getDataTime();
            if (timeToPaint == null) {
                return;
            }
        }
        if (!renderableData.containsKey(timeToPaint)) {
            this.prepareData(target, timeToPaint);
        }

        FrameData frameData = renderableData.get(timeToPaint);

        // First, draw the points
        if (frameData.pointsToRender.size() > 0) {
            target.drawPoints(frameData.pointsToRender,
                    getCapability(ColorableCapability.class).getColor(),
                    IGraphicsTarget.PointStyle.CIRCLE,
                    getCapability(MagnificationCapability.class)
                            .getMagnification().floatValue());
        }

        OutlineCapability outlineCapability = getCapability(OutlineCapability.class);
        // Finally, draw the shape
        if (frameData.shape != null && outlineCapability.isOutlineOn()) {
            target.drawWireframeShape(frameData.shape,
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
    protected void prepareData(IGraphicsTarget target, DataTime time)
            throws VizException {
        IWireframeShape shape = null;
        List<double[]> pointsToRender = new ArrayList<double[]>();

        int numberOfPoints = 0;

        for (IGeometryData geometryData : this.resourceData.getData(time)) {
            Geometry geometry = geometryData.getGeometry();
            if (geometry instanceof Point) {
                double[] pixels = this.descriptor
                        .worldToPixel(new double[] {
                                geometry.getCoordinate().x,
                                geometry.getCoordinate().y });
                pointsToRender.add(pixels);
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
            shape = target.createWireframeShape(false, this.descriptor, 0.0f);

            JTSCompiler jtsCompiler = new JTSCompiler(null, shape,
                    this.descriptor, PointStyle.CROSS);
            shape.allocate(numberOfPoints);
            // add the geometries
            for (IGeometryData geometryData : this.resourceData.getData(time)) {
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
            shape.compile();
        }
        renderableData.put(time, new FrameData(pointsToRender, shape));
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
     * Normally invoked when the resource is disposed; purges all saved
     * geometric entities
     */
    public void purgeDrawableStorage() {
        Collection<FrameData> frames = renderableData.values();
        renderableData = new HashMap<DataTime, FrameData>();
        for (FrameData frame : frames) {
            if (frame.shape != null) {
                frame.shape.dispose();
            }
        }
    }
}
