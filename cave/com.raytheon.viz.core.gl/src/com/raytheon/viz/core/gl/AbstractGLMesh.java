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
package com.raytheon.viz.core.gl;

import java.util.ArrayList;
import java.util.List;

import javax.media.opengl.GL;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.util.WorldWrapChecker;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.PaintStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.viz.core.gl.GLGeometryObject2D.GLGeometryObjectData;
import com.raytheon.viz.core.gl.SharedCoordMap.SharedCoordinateKey;
import com.raytheon.viz.core.gl.SharedCoordMap.SharedCoordinates;

/**
 * Abstract GLMesh
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 1, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractGLMesh implements IMesh {

    private static final JobPool calculator = new JobPool("Mesh Calculator", 2,
            false);

    protected static enum State {
        NEW, CALCULATING, CALCULATED, COMPILED, INVALID;
    }

    private State internalState = State.NEW;

    private GLGeometryObject2D vertexCoords;

    private SharedCoordinates sharedTextureCoords;

    protected SharedCoordinateKey key;

    private Runnable calculate = new Runnable() {
        @Override
        public void run() {
            synchronized (calculate) {
                if (internalState == State.CALCULATING) {
                    // If we aren't in CALCULATING state, we were disposed while
                    // waiting to run and shouldn't run now
                    if (calculateMesh()) {
                        internalState = State.CALCULATED;
                    } else {
                        internalState = State.INVALID;
                    }
                }
            }
        }
    };

    private int geometryType;

    private MathTransform imageCRSToLatLon;

    private MathTransform latLonToTargetGrid;

    private GeneralGridGeometry targetGeometry;

    private GridGeometry2D imageGeometry;

    protected AbstractGLMesh(int geometryType) {
        this.geometryType = geometryType;
    }

    protected final void initialize(GridGeometry2D imageGeometry,
            GeneralGridGeometry targetGeometry) throws VizException {
        this.imageGeometry = imageGeometry;
        if (imageGeometry != null) {
            try {
                imageCRSToLatLon = MapUtil.getTransformToLatLon(imageGeometry
                        .getCoordinateReferenceSystem());
            } catch (Throwable t) {
                throw new VizException(
                        "Error construcing image to lat/lon transform", t);
            }
        }
        reproject(targetGeometry);
    }

    public final synchronized PaintStatus paint(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        State internalState = this.internalState;
        if (internalState == State.NEW) {
            throw new VizException(
                    "Class did not properly call initialize on construction");
        } else if (internalState == State.INVALID) {
            // Don't paint if invalid to avoid crashes
            return PaintStatus.ERROR;
        }

        IGLTarget glTarget;
        if (!(target instanceof IGLTarget)) {
            throw new VizException("IGLTarget required ");
        }

        glTarget = (IGLTarget) target;

        try {
            if (internalState == State.CALCULATED) {
                // We finished calculating the mesh, compile it
                sharedTextureCoords = SharedCoordMap.get(key, glTarget);
                vertexCoords.compile(glTarget.getGl());
                this.internalState = internalState = State.COMPILED;
            }

            if (internalState == State.COMPILED) {
                GLGeometryPainter.paintGeometries(glTarget.getGl(),
                        vertexCoords, sharedTextureCoords.getTextureCoords());
                return PaintStatus.PAINTED;
            } else if (internalState == State.CALCULATING) {
                target.setNeedsRefresh(true);
                return PaintStatus.REPAINT;
            } else {
                return PaintStatus.ERROR;
            }
        } catch (VizException e) {
            this.internalState = State.INVALID;
            throw e;
        }
    }

    @Override
    public synchronized void dispose() {
        // Synchronize on calculate so we don't dispose while running
        synchronized (calculate) {
            // Cancel calculation job from running
            calculator.cancel(calculate);
            // dispose and reset vertexCoords
            if (vertexCoords != null) {
                vertexCoords.dispose();
                vertexCoords = null;
            }
            if (sharedTextureCoords != null) {
                SharedCoordMap.remove(key);
                sharedTextureCoords = null;
            }
            internalState = State.INVALID;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IMesh#reproject(org.geotools.coverage.grid.
     * GeneralGridGeometry)
     */
    @Override
    public final void reproject(GeneralGridGeometry targetGeometry)
            throws VizException {
        if (targetGeometry.equals(this.targetGeometry) == false) {
            dispose();
            this.targetGeometry = targetGeometry;

            // Set up convenience transforms
            try {
                DefaultMathTransformFactory factory = new DefaultMathTransformFactory();
                latLonToTargetGrid = factory.createConcatenatedTransform(
                        MapUtil.getTransformFromLatLon(targetGeometry
                                .getCoordinateReferenceSystem()),
                        targetGeometry.getGridToCRS(PixelInCell.CELL_CENTER)
                                .inverse());
            } catch (Throwable t) {
                internalState = State.INVALID;
                throw new VizException("Error projecting mesh", t);
            }

            internalState = State.CALCULATING;
            calculator.schedule(calculate);
        }
    }

    private boolean calculateMesh() {
        key = generateKey(imageGeometry, imageCRSToLatLon);
        try {
            double[][][] worldCoordinates = generateWorldCoords(imageGeometry,
                    imageCRSToLatLon);
            vertexCoords = new GLGeometryObject2D(new GLGeometryObjectData(
                    geometryType, GL.GL_VERTEX_ARRAY));
            vertexCoords.allocate(worldCoordinates.length
                    * worldCoordinates[0].length);
            // Check for world wrapping
            WorldWrapChecker wwc = new WorldWrapChecker(targetGeometry);

            for (int i = 0; i < worldCoordinates.length; ++i) {
                double[][] strip = worldCoordinates[i];
                List<double[]> vSegment = new ArrayList<double[]>();
                double[] prev1 = null, prev2 = null;
                for (int j = 0; j < strip.length; ++j) {
                    double[] next = strip[j];

                    if ((prev1 != null && wwc.check(prev1[0], next[0]))
                            || (prev2 != null && wwc.check(prev2[0], next[0]))) {
                        vertexCoords.addSegment(vSegment
                                .toArray(new double[vSegment.size()][]));
                        vSegment = new ArrayList<double[]>();
                        prev1 = null;
                        prev2 = null;
                    }
                    vSegment.add(worldToPixel(next));

                    prev2 = prev1;
                    prev1 = next;
                }

                vertexCoords.addSegment(vSegment.toArray(new double[vSegment
                        .size()][]));
            }
            return true;
        } catch (Exception e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    "Error calculating mesh", e);
        }
        return false;
    }

    protected final double[] worldToPixel(double[] world) {
        double[] in = null;
        if (world.length == 2) {
            in = new double[] { world[0], world[1], 0.0 };
        } else {
            in = world;
        }
        double[] out = new double[in.length];
        try {
            latLonToTargetGrid.transform(in, 0, out, 0, 1);
        } catch (TransformException e) {
            return null;
        }
        return out;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.IMesh#intersects(com.raytheon.uf.viz.core.IExtent
     * )
     */
    @Override
    public boolean intersects(IExtent extent) {
        return false;
    }

    protected abstract SharedCoordinateKey generateKey(
            GridGeometry2D imageGeometry, MathTransform mt);

    protected abstract double[][][] generateWorldCoords(
            GridGeometry2D imageGeometry, MathTransform mt)
            throws TransformException;

}
