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

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

import javax.media.opengl.GL;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.WorldWrapChecker;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;
import com.raytheon.viz.core.gl.GLGeometryObject2D.GLGeometryObjectData;
import com.raytheon.viz.core.gl.SharedCoordMap.SharedCoordinateKey;
import com.raytheon.viz.core.gl.SharedCoordMap.SharedCoordinates;
import com.vividsolutions.jts.geom.Envelope;

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

    protected boolean shouldDraw = true;

    private boolean compiled = false;

    protected GLGeometryObject2D vertexCoords;

    protected SharedCoordinateKey key;

    private SharedCoordinates sharedTextureCoords;

    protected IMapDescriptor descriptor;

    public AbstractGLMesh(int geometryType, IMapDescriptor descriptor) {
        vertexCoords = new GLGeometryObject2D(new GLGeometryObjectData(
                geometryType, GL.GL_VERTEX_ARRAY));
        this.descriptor = descriptor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.IMesh#paint(com.raytheon.viz.core.IGraphicsTarget)
     */
    @Override
    public synchronized void paint(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (!shouldDraw) {
            return;
        }

        IGLTarget glTarget;
        if (!(target instanceof IGLTarget)) {
            throw new VizException("IGLTarget required ");
        }

        glTarget = (IGLTarget) target;

        if (sharedTextureCoords == null) {
            sharedTextureCoords = SharedCoordMap.get(key, glTarget);
        }

        if (!compiled) {
            vertexCoords.compile(glTarget.getGl());
            compiled = true;
        }

        GLGeometryPainter.paintGeometries(glTarget.getGl(), vertexCoords,
                sharedTextureCoords.getTextureCoords());
    }

    @Override
    public synchronized void dispose() {
        vertexCoords.dispose();
        if (sharedTextureCoords != null) {
            SharedCoordMap.remove(key);
            sharedTextureCoords = null;
        }
        shouldDraw = false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.IMesh#calculateMesh(com.raytheon.viz.core.PixelCoverage
     * , org.opengis.coverage.grid.GridGeometry)
     */
    public void calculateMesh(PixelCoverage pc, GridGeometry2D gg) {
        MathTransform toLL = null;
        ImageTile tile = null;
        try {
            toLL = CRS.findMathTransform(gg.getCoordinateReferenceSystem(),
                    DefaultGeographicCRS.WGS84);
            tile = new ImageTile();
            tile.coverage = pc;
            tile.rect = new Rectangle(gg.getGridRange().getLow(0), gg
                    .getGridRange().getLow(1), gg.getGridRange().getHigh(0), gg
                    .getGridRange().getHigh(1));
            tile.envelope = new Envelope(gg.getEnvelope().getMinimum(0), gg
                    .getEnvelope().getMaximum(0), gg.getEnvelope()
                    .getMinimum(1), gg.getEnvelope().getMaximum(1));
        } catch (Exception e) {
        }
        calculateMesh(pc, tile, toLL);
    }

    @Override
    public void calculateMesh(PixelCoverage pc, ImageTile tile, MathTransform mt) {
        shouldDraw = false;
        key = generateKey(tile, mt);
        try {
            double[][][] worldCoordinates = generateWorldCoords(tile, mt);
            vertexCoords.allocate(worldCoordinates.length
                    * worldCoordinates[0].length);
            // Check for world wrapping
            WorldWrapChecker wwc = new WorldWrapChecker(descriptor);

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
                    vSegment.add(descriptor.worldToPixel(next));

                    prev2 = prev1;
                    prev1 = next;
                }

                vertexCoords.addSegment(vSegment.toArray(new double[vSegment
                        .size()][]));
            }

            shouldDraw = true;
        } catch (Exception e) {
            e.printStackTrace();
            shouldDraw = false;
        }

        pc.setMesh(this);
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

    protected abstract SharedCoordinateKey generateKey(ImageTile tile,
            MathTransform mt);

    protected abstract double[][][] generateWorldCoords(ImageTile tile,
            MathTransform mt) throws TransformException;

}
