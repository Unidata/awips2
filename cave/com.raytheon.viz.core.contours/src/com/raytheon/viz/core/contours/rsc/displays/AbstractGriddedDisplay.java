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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.GeodeticCalculator;
import org.geotools.referencing.operation.transform.ConcatenatedTransform;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.NoninvertibleTransformException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * An abstract resource for displays where each grid cell is an individual
 * IImage. Handles progressive disclosure algorithm.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractGriddedDisplay<T> implements IRenderable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractGriddedDisplay.class);

    protected class GridCellRenderable {
        public T resource;

        public Coordinate plotLocation;

        public double adjustedSize;

        /**
         * @param object
         * @param plotLocation
         * @param adjustedSize
         */
        public GridCellRenderable(T resource, Coordinate plotLocation,
                double adjustedSize) {
            this.resource = resource;
            this.plotLocation = plotLocation;
            this.adjustedSize = adjustedSize;
        }

    }

    private final Queue<Coordinate> calculationQueue;

    private CalculationJob calculationJob;

    protected final IMapDescriptor descriptor;

    protected final GeneralGridGeometry gridGeometryOfGrid;

    protected final int[] gridDims;

    protected IGraphicsTarget target;

    protected final int size;

    protected RGB color;

    protected double density = 1.0;

    protected double magnification = 1.0;

    private boolean async = true;

    private double pixelSize = 1.0;

    private float[] plotLocations;

    /**
     * 
     * @param descriptor
     * @param gridGeometryOfGrid
     * @param size
     * @param plotLocations
     *            Pre-configured plot locations. If null, they will be created.
     */
    public AbstractGriddedDisplay(IMapDescriptor descriptor,
            GeneralGridGeometry gridGeometryOfGrid, int size) {
        this.calculationQueue = new ConcurrentLinkedQueue<Coordinate>();

        this.descriptor = descriptor;
        this.gridGeometryOfGrid = gridGeometryOfGrid;
        this.size = size;

        this.gridDims = new int[] {
                this.gridGeometryOfGrid.getGridRange().getSpan(0),
                this.gridGeometryOfGrid.getGridRange().getSpan(1) };
        initPlotLocations();
    }

    private void initPlotLocations() {
        plotLocations = PlotLocationCache.getInstance().getPlotLocations(
                GridGeometry2D.wrap(gridGeometryOfGrid),
                GridGeometry2D.wrap(descriptor.getGridGeometry()));
    }

    public void setASync(boolean async) {
        this.async = async;
    }

    public double getPixelWidth(GridGeometry2D gridGeometry,
            IMapDescriptor descriptor) throws VizException {
        try {
            double[] input = new double[] { 0, 0, 1, 1 };
            double[] output = new double[input.length];

            MathTransform mathTransform = gridGeometry
                    .getGridToCRS(PixelInCell.CELL_CORNER);
            // convert the point s to lat/lon
            mathTransform.transform(input, 0, output, 0, input.length / 2);
            DirectPosition2D s1 = new DirectPosition2D(output[0], output[1]);
            DirectPosition2D d1 = new DirectPosition2D(output[2], output[3]);
            GeodeticCalculator gc = new GeodeticCalculator(
                    gridGeometry.getCoordinateReferenceSystem());
            gc.setStartingPosition(s1);
            gc.setDestinationPosition(d1);

            return gc.getOrthodromicDistance();
        }

        catch (org.opengis.referencing.operation.NoninvertibleTransformException e) {
            throw new VizException(e);

        } catch (TransformException e) {
            throw new VizException(e);

        } catch (InvalidGridGeometryException e) {
            throw new VizException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.drawables.IRenderable#paint(com.raytheon.viz.core
     * .IGraphicsTarget, com.raytheon.viz.core.drawables.PaintProperties)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        this.target = target;

        MathTransform grid2grid = getGeometryToDescriptorGridTransform();

        PaintProperties pp = new PaintProperties(paintProps);
        pp.setAlpha(1.0f);

        // This distance is the diagonal distance between two gridCells in
        // pixel
        // space
        // Linear distance(between (0,0) and (0,1) makes more sense but
        // looks to sparse.
        DirectPosition2D p1 = new DirectPosition2D(0, 0);
        DirectPosition2D p2 = new DirectPosition2D(1, 1);
        try {
            grid2grid.transform(p1, p1);
            grid2grid.transform(p2, p2);
        } catch (TransformException e) {
            throw new VizException(e);
        }
        pixelSize = p1.distance(p2);

        IExtent viewPixelExtent = paintProps.getView().getExtent();
        int canvasWidth = paintProps.getCanvasBounds().width;
        double ratio = viewPixelExtent.getWidth() / canvasWidth;

        double adjSize = size * ratio * magnification;

        // Casting to an int always rounds down, this is an arbitrary
        // rounding
        // decision, actually rounding would seem to make more sense but
        // causes
        // it to look a bit to sparse
        // If it is 0, then go with 1, 0 infinite loops

        List<GridCellRenderable> renderables = new ArrayList<GridCellRenderable>();
        int increment = Math.max(
                (int) Math.ceil(adjSize * 0.75 / pixelSize / density), 1);
        for (int x = 0; x < gridDims[0]; x += increment) {
            for (int y = 0; y < gridDims[1]; y += increment) {

                double xVal = plotLocations[(x * 2) * gridDims[1] + (y * 2)];
                double yVal = plotLocations[((x * 2) * gridDims[1] + (y * 2)) + 1];
                if (!viewPixelExtent.contains(new double[] { xVal, yVal })) {
                    continue;
                }
                Coordinate plotLocCoord = new Coordinate(xVal, yVal);
                Coordinate gridCellCoord = new Coordinate(x, y);

                T oldResource = getResource(gridCellCoord);
                if (oldResource != null) {
                    renderables.add(new GridCellRenderable(oldResource,
                            plotLocCoord, adjSize));
                } else {
                    if (async) {
                        if (!this.calculationQueue.contains(gridCellCoord)) {
                            this.calculationQueue.add(gridCellCoord);
                        }
                    } else {
                        T resource = createResource(gridCellCoord);
                        if (resource != null) {
                            paint(resource, pp, plotLocCoord, adjSize);
                        }
                    }
                }
            }
        }

        if (renderables.size() > 0) {
            paint(pp, renderables);
        }

        if (calculationQueue.size() > 0) {
            if (this.calculationJob == null) {
                this.calculationJob = new CalculationJob();
                this.calculationJob.schedule();
            } else if (!this.calculationJob.isRunning()) {
                this.calculationJob.schedule();
            }
        }
    }

    /**
     * Should return a cached image if it is available, if this returns null
     * createImage will be called on the same point(possible asynchronously)
     * 
     * @param coord
     * @return
     */
    protected abstract T getResource(Coordinate coord);

    /**
     * Create an image for the given coordinate.
     * 
     * @param coord
     * @return
     * @throws VizException
     */
    protected abstract T createResource(Coordinate coord) throws VizException;

    /**
     * Should dispose of all images and clear a cache. Called whenever the color
     * is changed, or when the display is disposed.
     */
    protected abstract void disposeResources();

    /**
     * Paint a single object, this method can be overridden to paint single
     * entities or paint that takes a Collection<GridCellRenderable> can be
     * overridden for bulk rendering
     * 
     * @param image
     * @param paintProps
     * @param plotLoc
     * @param adjustedSize
     * @throws VizException
     */
    protected void paint(T image, PaintProperties paintProps,
            Coordinate plotLoc, double adjustedSize) throws VizException {
        // Does nothing by default
    }

    /**
     * Bulk render method for gridded display, can be overridden for more
     * efficient rendering. Default implementation will call paint on each
     * object
     * 
     * @param paintProps
     * @param renderables
     * @throws VizException
     */
    protected void paint(PaintProperties paintProps,
            Collection<GridCellRenderable> renderables) throws VizException {
        for (GridCellRenderable renderable : renderables) {
            paint(renderable.resource, paintProps, renderable.plotLocation,
                    renderable.adjustedSize);
        }
    }

    public void dispose() {
        disposeResources();
    }

    /**
     * x* // DirectPosition2D plotLoc = plotLocationCache[x][y]; // if (plotLoc
     * == null) { // plotLoc = new DirectPosition2D(x, y); //
     * grid2grid.transform(plotLoc, plotLoc); // plotLocationCache[x][y] =
     * plotLoc; // } Set the color of the images
     * 
     * @param color
     */
    public boolean setColor(RGB color) {
        if (this.color == null || !this.color.equals(color)) {
            this.color = color;
            return true;
        }
        return false;
    }

    /**
     * @param density
     *            the density to set
     */
    public boolean setDensity(double density) {
        if (density > DensityCapability.MAX_THRESHOLD) {
            density = DensityCapability.MAX_THRESHOLD;
        }
        if (this.density != density) {
            this.density = density;
            return true;
        }
        return false;
    }

    /**
     * @param magnification
     *            the magnification to set
     */
    public boolean setMagnification(double magnification) {
        if (this.magnification != magnification) {
            this.magnification = magnification;
            return true;
        }
        return false;
    }

    protected void issueRefresh() {
        if (target != null) {
            target.setNeedsRefresh(true);
        }
    }

    private MathTransform getGeometryToDescriptorGridTransform()
            throws VizException {
        try {

            MathTransform grid2crs = gridGeometryOfGrid.getGridToCRS();
            MathTransform crs2crs = CRS
                    .findMathTransform(gridGeometryOfGrid
                            .getCoordinateReferenceSystem(), descriptor
                            .getGridGeometry().getCoordinateReferenceSystem());
            MathTransform crs2grid = descriptor.getGridGeometry()
                    .getGridToCRS().inverse();

            return ConcatenatedTransform.create(
                    ConcatenatedTransform.create(grid2crs, crs2crs), crs2grid);
        } catch (InvalidGridGeometryException e) {
            throw new VizException(e);
        } catch (NoninvertibleTransformException e) {
            throw new VizException(e);
        } catch (FactoryException e) {
            throw new VizException(e);
        }
    }

    /**
     * Off UI Thread job for calculating the wind images
     * 
     * @author chammack
     * @version 1.0
     */
    private class CalculationJob extends Job {

        private boolean running = false;

        public CalculationJob() {
            super("Grid Image Calculation");
        }

        /*
         * (non-Javadoc)
         * 
         * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            boolean loggedError = false;
            running = true;
            while (!calculationQueue.isEmpty()) {

                Coordinate coord = calculationQueue.remove();

                try {
                    createResource(coord);
                } catch (VizException e) {
                    if (!loggedError) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Generating the grid image failed.", e);
                        loggedError = true;
                    }
                }

            }

            target.setNeedsRefresh(true);
            running = false;
            return Status.OK_STATUS;
        }

        /**
         * @return the running
         */
        public boolean isRunning() {
            return running;
        }

    }

    public void reproject() throws VizException {
        initPlotLocations();
        issueRefresh();
    }

}
