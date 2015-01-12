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
package com.raytheon.viz.mpe.ui.rsc;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.operation.transform.ConcatenatedTransform;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.mpe.ui.dialogs.polygon.DrawPolygonDlg;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Polygon edit resource.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 30, 2009 2685       mpduff      Initial creation.
 * Sep 23, 2009 3069       mpduff      Changed the parent class to HydroPointResource.
 * Feb 07, 2011 5535       mschenke    Rewrote resource to fix numerous issues and follow viz standards
 * Jul 24, 2014 3429       mapeters    Updated deprecated drawLine() calls.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class MPEPolygonResource extends
        AbstractVizResource<GenericResourceData, IDescriptor> implements
        IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MPEPolygonResource.class);

    /** Input handler for polygon creation and dialog opening */
    private IInputHandler polygonInputHandler = new InputAdapter() {

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {
            return true;
        }

        @Override
        public boolean handleMouseDownMove(int x, int y, int mouseButton) {
            return true;
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (getResourceContainer().getActiveDisplayPane().getDescriptor() != descriptor) {
                return false;
            }

            IDisplayPaneContainer container = getResourceContainer();
            if (container != null) {
                Coordinate c = container.translateClick(x, y);
                if (c != null) {
                    addCoordinate(c);
                }
            }

            if ((mouseButton != 1) && (polyPoints.size() > 2)) {
                polyPoints.add(new Coordinate(polyPoints.get(0)));

                if (dialog == null) {
                    // TODO: Intersect polygon with hrap grid and pop up dialog
                    dialog = new DrawPolygonDlg(Display.getCurrent()
                            .getActiveShell(), MPEPolygonResource.this);
                    dialog.setPolygonPoints(constructPolyPoints());
                    dialog.open();

                    dialog.getShell().addDisposeListener(new DisposeListener() {
                        @Override
                        public void widgetDisposed(DisposeEvent e) {
                            dialog = null;
                            clearPolygons();
                            EditableManager.makeEditable(
                                    MPEPolygonResource.this, false);
                        }
                    });
                } else {
                    dialog.setPolygonPoints(constructPolyPoints());
                }
            }

            issueRefresh();

            return true;
        }

    };

    private List<Coordinate> polyPoints = new ArrayList<Coordinate>();

    private List<Polygon> drawnPolygons = new ArrayList<Polygon>();

    private DrawPolygonDlg dialog;

    private MathTransform llToGrid = null;

    private MathTransform gridToLL = null;

    private GridGeometry2D hrapGeometry;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public MPEPolygonResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        // Start out as white
        getCapability(ColorableCapability.class).setColor(
                new RGB(255, 255, 255));

        // Add as data update listener
        resourceData.addChangeListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        try {
            hrapGeometry = MapUtil.getGridGeometry(new HRAPSubGrid(
                    HRAPCoordinates.getHRAPCoordinates()));
            MathTransform llToHrap = MapUtil
                    .getTransformFromLatLon(hrapGeometry
                            .getCoordinateReferenceSystem());
            llToGrid = ConcatenatedTransform.create(llToHrap,
                    hrapGeometry.getCRSToGrid2D(PixelOrientation.UPPER_LEFT));
            gridToLL = llToGrid.inverse();
        } catch (Exception e) {
            throw new VizException(
                    "Could not get math transforms for HRAP grid", e);
        }
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
        synchronized (polyPoints) {
            int size = polyPoints.size();
            if (size > 1) {
                paintCoordinates(target, paintProps,
                        polyPoints.toArray(new Coordinate[polyPoints.size()]));
            }

            for (Polygon p : drawnPolygons) {
                paintCoordinates(target, paintProps, p.getCoordinates());
            }
        }
    }

    /**
     * Paints the coordinates as connected line segments
     * 
     * @param target
     * @param paintProps
     * @param coords
     * @throws VizException
     */
    private void paintCoordinates(IGraphicsTarget target,
            PaintProperties paintProps, Coordinate[] coords)
            throws VizException {
        double[] startPixels = descriptor.worldToPixel(new double[] {
                coords[0].x, coords[0].y });
        DrawableLine line = new DrawableLine();
        line.basics.color = getCapability(ColorableCapability.class).getColor();
        line.setCoordinates(startPixels[0], startPixels[1]);
        for (int i = 1; i < coords.length; ++i) {
            Coordinate curr = coords[i];
            double[] currPixels = descriptor.worldToPixel(new double[] {
                    curr.x, curr.y });
            line.addPoint(currPixels[0], currPixels[1]);
        }
        target.drawLine(line);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(polygonInputHandler);
        }
    }

    /**
     * Clear the drawn polygons from the resource
     */
    public void clearPolygons() {
        synchronized (polyPoints) {
            drawnPolygons.clear();
            polyPoints.clear();
        }
    }

    private List<Point> constructPolyPoints() {
        List<Point> points = new ArrayList<Point>();
        List<Coordinate> hrapList = new ArrayList<Coordinate>();
        HRAP hrap = HRAP.getInstance();
        Envelope env;
        GeometryFactory gf = new GeometryFactory();
        try {
            for (Coordinate coord : polyPoints) {
                Coordinate cell = hrap.latLonToGridCoordinate(coord,
                        PixelOrientation.UPPER_LEFT);
                hrapList.add(cell);
            }

            Polygon drawnPolygon;
            drawnPolygon = gf.createPolygon(gf.createLinearRing(hrapList
                    .toArray(new Coordinate[hrapList.size()])), null);
            env = drawnPolygon.getEnvelopeInternal();

            for (int i = (int) env.getMinX(); i < env.getMaxX(); ++i) {
                for (int j = (int) env.getMinY(); j < env.getMaxY(); ++j) {
                    Coordinate ul = new Coordinate(i, j);
                    if (drawnPolygon.contains(gf.createPoint(ul))) {
                        Coordinate lr = new Coordinate(i + 1, j + 1);
                        if (drawnPolygon.contains(gf.createPoint(lr))) {
                            if (env.contains(i + 1, j)) {
                                if (env.contains(i, j + 1)) {
                                    points.add(new Point(i, j));
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception e1) {
            statusHandler
                    .handle(Priority.PROBLEM, e1.getLocalizedMessage(), e1);
        }

        synchronized (polyPoints) {
            drawnPolygons.add(gf.createPolygon(gf.createLinearRing(polyPoints
                    .toArray(new Coordinate[polyPoints.size()])), null));
            polyPoints.clear();
        }

        return points;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY) {
            if (object instanceof EditableCapability) {
                EditableCapability cap = (EditableCapability) object;
                if (cap.isEditable()) {
                    getResourceContainer().registerMouseHandler(
                            polygonInputHandler);
                } else {
                    getResourceContainer().unregisterMouseHandler(
                            polygonInputHandler);
                }
            }
        }
        issueRefresh();
    }

    /**
     * Attempt to add the lat/lon coordinate to the list of clicked points
     * 
     * @param c2
     *            coordinate to check
     * @return
     */
    private boolean addCoordinate(Coordinate c2) {

        // Get extents
        double width = hrapGeometry.getGridRange().getSpan(0);
        double height = hrapGeometry.getGridRange().getSpan(1);
        double xZero = 0;
        double yZero = 0;

        Rectangle extent = new Rectangle((int) xZero, (int) yZero,
                (int) (width + 1), (int) (height + 1));

        // Convert lat/lon point clicked into grid
        double[] in = new double[] { c2.x, c2.y };
        double[] out = new double[3];

        try {
            llToGrid.transform(in, 0, out, 0, 1);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return false;
        }

        // if lat/lon point within grid, add the point
        if (extent.contains(out[0], out[1])) {
            polyPoints.add(c2);
            return true;
        }
        // Point is outside of grid, do calculations to find closest point
        // within grid along slope

        // If no points, don't try to add
        if (polyPoints.size() == 0) {
            return false;
        }

        // calculate slope and find intersection
        Coordinate c1 = polyPoints.get(polyPoints.size() - 1);
        try {
            llToGrid.transform(new double[] { c1.x, c1.y }, 0, out, 0, 1);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return false;
        }
        Coordinate p1 = new Coordinate(out[0], out[1]);

        // Convert c2 as well
        try {
            llToGrid.transform(new double[] { c2.x, c2.y }, 0, out, 0, 1);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return false;
        }
        Coordinate p2 = new Coordinate(out[0], out[1]);

        double m = (p2.y - p1.y) / (p2.x - p1.x);
        Coordinate x0, y0, xW, yW;
        double b = p1.y - (p1.x * m);

        // Find intersection options, x=0, x=width, y=0, y=height
        x0 = new Coordinate(xZero, b);
        System.out.println("x0: " + x0);
        y0 = new Coordinate(-b / m, yZero);
        System.out.println("y0: " + y0);
        yW = new Coordinate((height - b) / m, height);
        System.out.println("yW: " + yW);
        xW = new Coordinate(width, m * width + b);
        System.out.println("xW: " + xW);

        Envelope env = new Envelope(p1, p2);
        // Use point within grid that isn't our anchor point p1
        Coordinate used = null;
        if (extent.contains(x0.x, x0.y) && env.contains(x0.x, x0.y)
                && (equals(p1, x0) == false)) {
            used = x0;
        } else if (extent.contains(y0.x, y0.y) && env.contains(y0.x, y0.y)
                && (equals(p1, y0) == false)) {
            used = y0;
        } else if (extent.contains(xW.x, xW.y) && env.contains(xW.x, xW.y)
                && (equals(p1, xW) == false)) {
            used = xW;
        } else if (extent.contains(yW.x, yW.y) && env.contains(yW.x, yW.y)
                && (equals(p1, yW) == false)) {
            used = yW;
        }

        if (used == null) {
            // Could not find a point to use, find closest in grid
            double x = p2.x;
            if (p2.x > width) {
                x = width;
            } else if (p2.x < 0) {
                x = 0;
            }
            double y = p2.y;
            if (p2.y > height) {
                y = height;
            } else if (p2.y < 0) {
                y = 0;
            }

            used = new Coordinate(x, y);
        }

        try {
            gridToLL.transform(new double[] { used.x, used.y }, 0, out, 0, 1);
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return false;
        }
        polyPoints.add(new Coordinate(out[0], out[1]));
        return true;
    }

    /**
     * Compates coordinates rounding to 4 decimal places
     * 
     * @param c1
     * @param c2
     * @return
     */
    private boolean equals(Coordinate c1, Coordinate c2) {
        double factor = 10000.0;
        double x1 = Math.round(c1.x * factor) / factor;
        double y1 = Math.round(c1.y * factor) / factor;

        double x2 = Math.round(c2.x * factor) / factor;
        double y2 = Math.round(c2.y * factor) / factor;

        return (x1 == x2) && (y1 == y2);
    }
}
