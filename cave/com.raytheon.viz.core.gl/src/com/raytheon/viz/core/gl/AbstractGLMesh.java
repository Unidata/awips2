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

    // For world wrapping we maintain a set of triangle strips that fill in any
    // cut segements.
    private GLGeometryObject2D wwcVertexCoords;

    private GLGeometryObject2D wwcTextureCoords;

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

    protected GeneralGridGeometry targetGeometry;

    protected GridGeometry2D imageGeometry;

    protected int refCount;

    protected AbstractGLMesh(int geometryType) {
        this.geometryType = geometryType;
        this.refCount = 1;
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
        this.targetGeometry = targetGeometry;

        // Set up convenience transforms
        try {
            DefaultMathTransformFactory factory = new DefaultMathTransformFactory();
            latLonToTargetGrid = factory.createConcatenatedTransform(MapUtil
                    .getTransformFromLatLon(targetGeometry
                            .getCoordinateReferenceSystem()), targetGeometry
                    .getGridToCRS(PixelInCell.CELL_CENTER).inverse());
        } catch (Throwable t) {
            internalState = State.INVALID;
            throw new VizException("Error projecting mesh", t);
        }

        internalState = State.CALCULATING;
        calculator.schedule(calculate);
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
                if (wwcTextureCoords != null && wwcVertexCoords != null) {
                    wwcTextureCoords.compile(glTarget.getGl());
                    wwcVertexCoords.compile(glTarget.getGl());
                }
                this.internalState = internalState = State.COMPILED;
            }

            if (internalState == State.COMPILED) {
                GLGeometryPainter.paintGeometries(glTarget.getGl(),
                        vertexCoords, sharedTextureCoords.getTextureCoords());
                if (wwcTextureCoords != null && wwcVertexCoords != null) {
                    glTarget.getGl().glColor3f(1.0f, 0.0f, 0.0f);
                    GLGeometryPainter.paintGeometries(glTarget.getGl(),
                            wwcVertexCoords, wwcTextureCoords);
                    glTarget.getGl().glColor3f(0.0f, 1.0f, 0.0f);
                }
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

    protected void use() {
        refCount += 1;
    }

    @Override
    public synchronized void dispose() {
        refCount -= 1;
        if (refCount > 0) {
            return;
        }
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
            if (wwcTextureCoords != null) {
                wwcTextureCoords.dispose();
                wwcTextureCoords = null;
            }
            if (wwcVertexCoords != null) {
                wwcVertexCoords.dispose();
                wwcVertexCoords = null;
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
    public final IMesh reproject(GeneralGridGeometry targetGeometry)
            throws VizException {
        return clone(targetGeometry);
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
                        fixWorldWrap(wwc, prev2, prev1, next, i, j);
                        if ((prev1 != null && wwc.check(prev1[0], next[0]))
                                || vSegment.size() > 1) {
                            vertexCoords.addSegment(vSegment
                                    .toArray(new double[vSegment.size()][]));
                            vSegment = new ArrayList<double[]>();
                        }
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

    private void fixWorldWrap(WorldWrapChecker wwc, double[] p2, double[] p1,
            double[] n, int i, int j) {
        // make sure we have all 3 points
        if (p2 == null || p1 == null || n == null) {
            return;
        }
        // figure out texture coordinates
        float dX = (1.0f / (key.horizontalDivisions));
        float dY = (1.0f / (key.verticalDivisions));
        double[] tp2 = { (i + ((j - 2) % 2)) * dX, (j - 2) / 2 * dY };
        double[] tp1 = { (i + ((j - 1) % 2)) * dX, (j - 1) / 2 * dY };
        double[] tn = { (i + (j % 2)) * dX, j / 2 * dY };
        // find which two sides are cut
        boolean wwcp1n = wwc.check(p1[0], n[0]);
        boolean wwcp2n = wwc.check(p2[0], n[0]);
        boolean wwcp1p2 = wwc.check(p1[0], p2[0]);
        double[] a = null;
        double[] b = null;
        double[] c = null;
        double[] ta = null;
        double[] tb = null;
        double[] tc = null;
        if (wwcp1n && wwcp2n && !wwcp1p2) {
            a = n;
            b = p1;
            c = p2;
            ta = tn;
            tb = tp1;
            tc = tp2;
        } else if (wwcp1n && !wwcp2n && wwcp1p2) {
            a = p1;
            b = p2;
            c = n;
            ta = tp1;
            tb = tp2;
            tc = tn;
        } else if (!wwcp1n && wwcp2n && wwcp1p2) {
            a = p2;
            b = n;
            c = p1;
            ta = tp2;
            tb = tn;
            tc = tp1;
        } else {
            // this occurs when a pole is within the triangle, maybe we should
            // try to cut these triangles, but its hard.
            return;
        }
        if (wwcTextureCoords == null || wwcVertexCoords == null) {
            wwcVertexCoords = new GLGeometryObject2D(new GLGeometryObjectData(
                    GL.GL_TRIANGLE_STRIP, GL.GL_VERTEX_ARRAY));
            wwcTextureCoords = new GLGeometryObject2D(new GLGeometryObjectData(
                    GL.GL_TRIANGLE_STRIP, GL.GL_TEXTURE_COORD_ARRAY));
        }
        // at this point triangle abc is a triangle in which sides ab and ac
        // are cut by the inverse central meridian. We need to find the two
        // points of intersection and use them to make a triangle with a ion one
        // side and a quad with bc on the other side. ta, tb, tc represent the
        // texture coordinates for their respective points.
        double ax = wwc.toProjectionRange(a[0]);
        double bx = wwc.toProjectionRange(b[0]);
        double cx = wwc.toProjectionRange(c[0]);
        // Get various x distances to use as weights in interpolating
        double abDist = 360 - Math.abs(ax - bx);
        double acDist = 360 - Math.abs(ax - cx);
        double amDist = ax - wwc.getLowInverseCentralMeridian();
        if (amDist > 360) {
            amDist = amDist - 360;
        }
        // x location to use for midpoints on the triangle side, should be on
        // same side of central meridian as a
        double tx = wwc.getLowInverseCentralMeridian() + 0.00001;
        // x location to use for midpoints on the quad side, should be on
        // same side of central meridian as b and c
        double qx = wwc.getHighInverseCentralMeridian() - 0.00001;
        // If a is closer to the central meridian on the other side then switch
        // amDist, tx, and qx
        if (amDist > 180) {
            amDist = 360 - amDist;
            double tmp = tx;
            tx = qx;
            qx = tmp;
        }
        // interpolated y coordinate and texture coordinates along the ab line.
        double aby = a[1] + amDist * (b[1] - a[1]) / abDist;
        double abtx = ta[0] + amDist * (tb[0] - ta[0]) / abDist;
        double abty = ta[1] + amDist * (tb[1] - ta[1]) / abDist;
        // interpolated y coordinate and texture coordinates along the ac line.
        double acy = a[1] + amDist * (c[1] - a[1]) / acDist;
        double actx = ta[0] + amDist * (tc[0] - ta[0]) / acDist;
        double acty = ta[1] + amDist * (tc[1] - ta[1]) / acDist;
        // all done with math, assemble everything into a triangle and a quad to
        // set in the geometry.
        double[][] tri = new double[3][];
        double[][] triTex = new double[3][];
        tri[0] = worldToPixel(a);
        triTex[0] = ta;
        tri[1] = worldToPixel(new double[] { tx, aby });
        triTex[1] = new double[] { abtx, abty };
        tri[2] = worldToPixel(new double[] { tx, acy });
        triTex[2] = new double[] { actx, acty };
        double[][] quad = new double[4][];
        double[][] quadTex = new double[4][];
        quad[0] = worldToPixel(b);
        quadTex[0] = tb;
        quad[1] = worldToPixel(c);
        quadTex[1] = tc;
        quad[2] = worldToPixel(new double[] { qx, aby });
        quadTex[2] = new double[] { abtx, abty };
        quad[3] = worldToPixel(new double[] { qx, acy });
        quadTex[3] = new double[] { actx, acty };
        wwcVertexCoords.addSegment(tri);
        wwcTextureCoords.addSegment(triTex);
        wwcVertexCoords.addSegment(quad);
        wwcTextureCoords.addSegment(quadTex);
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
            return new double[] { Double.NaN, Double.NaN, Double.NaN };
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
