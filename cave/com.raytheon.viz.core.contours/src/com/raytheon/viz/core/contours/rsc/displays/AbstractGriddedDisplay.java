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

import java.awt.Rectangle;
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
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Apr 23, 2010           bsteffen    Initial creation
 * Aug 07, 2013  2077     bsteffen    Revise pixel size calculations.
 * Aug 27, 2013  2287     randerso    Replaced hard coded constant with
 *                                    densityFactor parameter to allow
 *                                    application specific density scaling to
 *                                    better match A1 displays
 * Sep 10, 2013  16257    MPorricelli Fix so that wind for global grids displays on
 *                                    mercator maps.
 * Sep 23, 2013  2363     bsteffen    Add more vector configuration options.
 * Feb 27, 2014  2791     bsteffen    Remove Unnecessary throws
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

    protected final double size;

    protected final double densityFactor;

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
     * @param densityFactor
     *            adjustment factor to make density match A1
     */
    public AbstractGriddedDisplay(IMapDescriptor descriptor,
            GeneralGridGeometry gridGeometryOfGrid, double size,
            double densityFactor) {
        this.calculationQueue = new ConcurrentLinkedQueue<Coordinate>();

        this.descriptor = descriptor;
        this.gridGeometryOfGrid = gridGeometryOfGrid;
        this.size = size;
        this.densityFactor = densityFactor;

        this.gridDims = new int[] {
                this.gridGeometryOfGrid.getGridRange().getSpan(0),
                this.gridGeometryOfGrid.getGridRange().getSpan(1) };
        initPlotLocations();
    }

    private void initPlotLocations() {
        plotLocations = PlotLocationCache.getInstance().getPlotLocations(
                GridGeometry2D.wrap(gridGeometryOfGrid),
                GridGeometry2D.wrap(descriptor.getGridGeometry()));

        // Calculate pixel size also.
        Rectangle descBounds = GridGeometry2D
                .wrap(descriptor.getGridGeometry()).getGridRange2D();

        double totalPixelSize = 0;
        int totalCount = 0;
        for (int x = 1; x < gridDims[0]; x += 1) {
            for (int y = 1; y < gridDims[1]; y += 1) {
                double x1 = plotLocations[(x - 1) * 2 * gridDims[1]
                        + ((y - 1) * 2)];
                double y1 = plotLocations[((x - 1) * 2 * gridDims[1] + ((y - 1) * 2)) + 1];
                double x2 = plotLocations[x * 2 * gridDims[1] + (y * 2)];
                double y2 = plotLocations[(x * 2 * gridDims[1] + (y * 2)) + 1];
                if (descBounds.contains(x1, y1) && descBounds.contains(x2, y2)) {
                    /*
                     * For every grid cell which is on the descriptor this will
                     * add the diagonal distance in display pixels.
                     */
                    totalPixelSize += Math.sqrt((x1 - x2) * (x1 - x2)
                            + (y1 - y2) * (y1 - y2));
                    totalCount += 1l;
                }
            }
        }
        if (totalCount == 0) {
            /*
             * There is one or less things to draw anyway so picking a
             * relatively large number forces full progressive disclosure.
             */
            pixelSize = descBounds.width;
        } else {
            pixelSize = totalPixelSize / totalCount;
        }
    }

    public void setASync(boolean async) {
        this.async = async;
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

        PaintProperties pp = new PaintProperties(paintProps);
        pp.setAlpha(1.0f);

        IExtent viewPixelExtent = paintProps.getView().getExtent();
        int canvasWidth = paintProps.getCanvasBounds().width;
        double ratio = viewPixelExtent.getWidth() / canvasWidth;

        double adjSize = size * ratio * magnification;

        List<GridCellRenderable> renderables = new ArrayList<GridCellRenderable>();

        /*
         * Casting to an int always rounds down, this is an arbitrary rounding
         * decision, actually rounding would seem to make more sense but causes
         * it to look a bit to sparse. If it rounds to 0, then use 1 because 0
         * infinite loops.
         */
        int increment = Math.max(
                (int) Math.ceil(adjSize * densityFactor / pixelSize / density),
                1);
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

    public void reproject() {
        initPlotLocations();
        issueRefresh();
    }

}
