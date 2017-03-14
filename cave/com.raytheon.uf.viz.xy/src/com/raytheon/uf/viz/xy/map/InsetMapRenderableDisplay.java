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
package com.raytheon.uf.viz.xy.map;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.maps.display.PlainMapRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource.ResourceStatus;
import com.raytheon.uf.viz.core.rsc.ResourceList.AddListener;
import com.raytheon.uf.viz.core.rsc.ResourceList.RemoveListener;
import com.raytheon.uf.viz.xy.map.rsc.IInsetMapResource;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * Inset renderable display, descriptor resources include map resources only,
 * parent display is used to find IInsetMapResources and paint them.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2009            mschenke    Initial creation
 * Jun 18, 2014 3242       njensen     Synchronized resources
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class InsetMapRenderableDisplay extends PlainMapRenderableDisplay
        implements AddListener, RemoveListener {

    /** The color of the map background */
    private static final RGB GREY = new RGB(155, 155, 155);

    /** default width in meters */
    private static final int DEFAULT_WIDTH = 2000000;

    /** The inset map container display */
    private IRenderableDisplay parentDisplay;

    /** the IInsetMapResources */
    private List<IInsetMapResource> resources;

    /** recalculate the extent/projection boolean */
    private boolean recalc = true;

    /** GeometryFactory for construction Geometries */
    private static GeometryFactory factory = new GeometryFactory();

    /** multiply by .8 to give some room for stereo so it doesn't look terrible */
    private static final double APPROX_HALF_EARTH_CIR_m = MapUtil.AWIPS_EARTH_RADIUS
            * Math.PI * .8;

    public InsetMapRenderableDisplay() {
        super();
        resources = new ArrayList<IInsetMapResource>();
    }

    public InsetMapRenderableDisplay(IRenderableDisplay parentDisplay) {
        this.parentDisplay = parentDisplay;
    }

    public IRenderableDisplay getParentDisplay() {
        return parentDisplay;
    }

    public void setParentDisplay(IRenderableDisplay parentDisplay) {
        synchronized (resources) {
            this.resources.clear();
        }
        if (this.parentDisplay != null) {
            this.parentDisplay.getDescriptor().getResourceList()
                    .removePostAddListener(this);
            this.parentDisplay.getDescriptor().getResourceList()
                    .removePreRemoveListener(this);
        }
        this.parentDisplay = parentDisplay;
        for (ResourcePair rp : parentDisplay.getDescriptor().getResourceList()) {
            addResource(rp);
        }
        this.parentDisplay.getDescriptor().getResourceList()
                .addPostAddListener(this);
        this.parentDisplay.getDescriptor().getResourceList()
                .addPreRemoveListener(this);
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        // Paint the border
        IExtent extent = paintProps.getView().getExtent();

        // Technically should be same color as map background resources which
        // should be same color as the graph background resource
        target.drawRect(extent, GREY, 2.0f, 1.0);

        // Recalculate the projection when we need to and we have resources
        synchronized (resources) {
            if (recalc && !resources.isEmpty()) {
                // if the IInsetMapResources are not initialized don't do
                // anything
                if (!allInitialized()) {
                    target.setNeedsRefresh(true);
                    return;
                } else {
                    recalc = false;
                    calculateProjection(target, paintProps);
                }
            }

            for (IInsetMapResource rsc : resources) {
                rsc.paintInsetMap(target, paintProps, getDescriptor());
            }
        }

        // Paint maps last
        super.paint(target, paintProps);
    }

    /**
     * Returns whether all of the inset map resources are initialized. Must be
     * externally synchronized.
     * 
     * @return
     */
    private boolean allInitialized() {
        for (IInsetMapResource rsc : resources) {
            if (rsc.getStatus() != ResourceStatus.INITIALIZED) {
                return false;
            }
        }
        return true;
    }

    @Override
    public void notifyAdd(ResourcePair rp) throws VizException {
        addResource(rp);
    }

    /**
     * If the resource added to the parent display is IInsetMapResource, add it
     * to our list
     */
    private void addResource(ResourcePair rp) {
        AbstractVizResource<?, ?> rsc = rp.getResource();
        if (rsc != null && rsc instanceof IInsetMapResource) {
            synchronized (resources) {
                resources.add((IInsetMapResource) rsc);
            }
            recalc = true;
        }
    }

    @Override
    public void notifyRemove(ResourcePair rp) throws VizException {
        synchronized (resources) {
            resources.remove(rp.getResource());
        }
        recalc = true;
    }

    /**
     * Calculate the projection of the map. Must be externally synchronized.
     * 
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void calculateProjection(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        Geometry geom = null;
        Geometry[] geometries = new Geometry[resources.size()];
        int i = 0;
        for (IInsetMapResource rsc : resources) {
            Geometry g = rsc.getInsetMapLocation();
            if (g != null) {
                geometries[i++] = g;
            } else {
                recalc = true;
                return;
            }
        }

        geom = factory.createGeometryCollection(geometries);
        Coordinate[] coords = geom.getCoordinates();
        double canvasRatio = (double) paintProps.getCanvasBounds().height
                / (double) paintProps.getCanvasBounds().width;
        double width = DEFAULT_WIDTH;
        double height = width * canvasRatio;
        CoordinateReferenceSystem crs = null;
        GridGeometry2D geom2D = null;
        if (coords.length == 1) {
            crs = MapUtil.constructStereographic(MapUtil.AWIPS_EARTH_RADIUS,
                    MapUtil.AWIPS_EARTH_RADIUS, coords[0].y, coords[0].x);
            geom2D = MapDescriptor.createGridGeometry(crs, coords[0], width,
                    height);
        } else if (coords.length > 1) {
            GeodeticCalculator gc = new GeodeticCalculator();
            Point p = geom.getCentroid();

            Coordinate[] env = geom.getEnvelope().getCoordinates();
            boolean stereo = true;
            double maxDistance = Double.MIN_VALUE;
            for (i = 0; i < env.length; ++i) {
                Coordinate toUse = env[i];
                for (int j = i + 1; j < env.length; ++j) {
                    Coordinate compareTo = env[j];
                    gc.setStartingGeographicPoint(toUse.x, toUse.y);
                    gc.setDestinationGeographicPoint(compareTo.x, compareTo.y);
                    double distance = gc.getOrthodromicDistance();
                    if (stereo && distance > APPROX_HALF_EARTH_CIR_m) {
                        stereo = false;
                    }

                    if (distance > maxDistance) {
                        maxDistance = distance;
                    }
                }
            }
            if (stereo) {
                crs = MapUtil.constructStereographic(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        p.getY(), p.getX());
            } else {
                crs = MapUtil.constructEquidistantCylindrical(
                        MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                        p.getX(), p.getY());
            }
            width = maxDistance + (DEFAULT_WIDTH * 1.5);
            height = width * canvasRatio;
            geom2D = MapDescriptor.createGridGeometry(crs, p.getCoordinate(),
                    width, height);
        }
        if (geom2D != null) {
            IMapDescriptor md = (IMapDescriptor) descriptor;
            if (md.getGridGeometry().equals(geom2D) == false) {
                md.setGridGeometry(geom2D);
                scaleAllToClientArea();
            }
        }
    }

    @Override
    public MapDescriptor getDescriptor() {
        return (MapDescriptor) descriptor;
    }

    /**
     * Scales all inset map panes on the container to their client area
     */
    public void scaleAllToClientArea() {
        // TODO: Huge hack to keep inset maps in sync, projection calculation
        // should be moved to XyPaneManager eventually and this class will be
        // responsible for calling paint on IInsetMapResources
        IDisplayPane[] insetPanes = getInsetContainer().getInsetPanes();
        for (IDisplayPane pane : insetPanes) {
            pane.getRenderableDisplay().scaleToClientArea(pane.getBounds());
        }
    }

    /**
     * Get the display pane container as an IInsetMapDisplayPaneContainer
     * 
     * @return
     */
    public IInsetMapDisplayPaneContainer getInsetContainer() {
        return (IInsetMapDisplayPaneContainer) super.getContainer();
    }

}
