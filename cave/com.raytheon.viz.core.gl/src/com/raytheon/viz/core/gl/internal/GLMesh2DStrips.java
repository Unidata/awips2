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
package com.raytheon.viz.core.gl.internal;

import javax.media.opengl.GL;

import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;
import com.raytheon.viz.core.gl.AbstractGLMesh;
import com.raytheon.viz.core.gl.SharedCoordMap.SharedCoordinateKey;
import com.vividsolutions.jts.geom.Envelope;

/**
 * 
 * GL Mesh based on triangle strips
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
public class GLMesh2DStrips extends AbstractGLMesh {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GLMesh2DStrips.class);

    public GLMesh2DStrips(IMapDescriptor descriptor) {
        super(GL.GL_TRIANGLE_STRIP, descriptor);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.IMesh#calculateMesh(com.raytheon.viz.core.PixelCoverage
     * , com.raytheon.viz.core.rsc.tiling.AbstractTileSet.HDF5Tile,
     * org.opengis.referencing.operation.MathTransform)
     */
    @Override
    public double[][][] generateWorldCoords(ImageTile tile, MathTransform mt)
            throws TransformException {
        double worldMinX = tile.envelope.getMinX();
        double worldMinY = tile.envelope.getMinY();
        double worldWidth = tile.envelope.getWidth();
        double worldHeight = tile.envelope.getHeight();

        // get dx and dy for texture points

        double dXWorld = worldWidth / (key.horizontalDivisions);
        double dYWorld = worldHeight / (key.verticalDivisions - 1);

        double[][][] worldCoordinates = new double[key.horizontalDivisions][2 * key.verticalDivisions][2];

        int width = worldCoordinates.length;
        int height = worldCoordinates[0].length;

        double horzHigh = -1, horzLow = -1;
        double vertVal, horzVal;
        double[] in = new double[2];
        double[] out = new double[3];

        for (int j = 1; j <= width; ++j) {
            int idx = j - 1;
            if (horzLow == -1) {
                horzLow = worldMinX;
            } else {
                horzLow = horzHigh;
            }
            horzHigh = worldMinX + j * dXWorld;

            for (int i = 0; i < height; ++i) {
                boolean reused = false;
                if (j > 1) {
                    if (i % 2 == 0) {
                        // we can reuse idx-1
                        worldCoordinates[idx][i][0] = worldCoordinates[idx - 1][i + 1][0];
                        worldCoordinates[idx][i][1] = worldCoordinates[idx - 1][i + 1][1];
                        reused = true;
                    }
                }

                if (!reused) {
                    if (i % 2 == 1) {
                        horzVal = horzHigh;
                    } else {
                        horzVal = horzLow;
                    }

                    vertVal = worldMinY + ((height - i - 1) / 2) * dYWorld;
                    in[0] = horzVal;
                    in[1] = vertVal;

                    if (mt != null) {
                        mt.transform(in, 0, out, 0, 1);
                    } else {
                        out[0] = in[0];
                        out[1] = in[1];
                        out[2] = 0;
                    }

                    worldCoordinates[idx][i][0] = out[0];
                    worldCoordinates[idx][i][1] = out[1];
                }
            }
        }
        return worldCoordinates;
    }

    @Override
    protected SharedCoordinateKey generateKey(ImageTile tile, MathTransform mt) {
        try {
            int maxHorzDiv = tile.rect.width / 4;
            int maxVertDiv = tile.rect.height / 4;
            Envelope envelope = tile.envelope;
            double[] tl = { envelope.getMinX(), envelope.getMaxY() };
            double[] tr = { envelope.getMaxX(), envelope.getMaxY() };
            double[] bl = { envelope.getMinX(), envelope.getMinY() };
            double[] br = { envelope.getMaxX(), envelope.getMinY() };
            // start off estimating the number of horzintal divisions by using
            // only the top and bottom.
            int horzDivTop = 1 + getNumDivisions(tl, null, tr, null,
                    maxHorzDiv, mt);
            int horzDivBot = 1 + getNumDivisions(bl, null, br, null,
                    maxHorzDiv, mt);
            int horzDiv = Math.max(horzDivTop, horzDivBot);
            // Next get the number of vertical divisions by finding the maximum
            // needed in every horizontal row.
            int vertDiv = 2;
            for (int i = 1; i <= horzDiv; i++) {
                double topX = tl[0] + (tr[0] - tl[0]) * i / horzDiv;
                double topY = tl[1] + (tr[1] - tl[1]) * i / horzDiv;
                double botX = bl[0] + (br[0] - bl[0]) * i / horzDiv;
                double botY = bl[1] + (br[1] - bl[1]) * i / horzDiv;
                double[] top = { topX, topY };
                double[] bot = { botX, botY };
                int vertDivTest = 1 + getNumDivisions(top, null, bot, null,
                        maxVertDiv, mt);
                vertDiv = Math.max(vertDiv, vertDivTest);
                if (vertDiv >= maxVertDiv) {
                    vertDiv = maxVertDiv;
                    break;
                }
            }
            // Now fill in the actual number of horzontal divisions incase
            // distortion increases towards the middle.
            for (int i = 2; i < vertDiv; i++) {
                double leftX = bl[0] + (tl[0] - bl[0]) * i / vertDiv;
                double leftY = bl[1] + (tl[1] - bl[1]) * i / vertDiv;
                double rightX = br[0] + (tr[0] - br[0]) * i / vertDiv;
                double rightY = br[1] + (tr[1] - br[1]) * i / vertDiv;
                double[] left = { leftX, leftY };
                double[] right = { rightX, rightY };
                int horzDivTest = 1 + getNumDivisions(left, null, right, null,
                        maxHorzDiv, mt);
                horzDiv = Math.max(horzDiv, horzDivTest);
                if (horzDiv >= maxHorzDiv) {
                    horzDiv = maxHorzDiv;
                    break;
                }
            }
            horzDiv = Math.max(2, horzDiv);
            vertDiv = Math.max(2, vertDiv);
            return new SharedCoordinateKey(vertDiv, horzDiv);
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Error calculating divisions needed for image, defaulting to dims/4",
                            e);
            return new SharedCoordinateKey(tile.rect.height / 4,
                    tile.rect.width / 4);
        }

    }

    private static final double THRESHOLD = 0.1;

    private int getNumDivisions(double[] p1, double[] r1, double[] p3,
            double[] r3, double maxNumDivs, MathTransform mt)
            throws TransformException {
        if (r1 == null) {
            r1 = new double[p1.length];
            mt.transform(p1, 0, r1, 0, 1);
            r1 = descriptor.worldToPixel(r1);
        }
        if (r3 == null) {
            r3 = new double[p3.length];
            mt.transform(p3, 0, r3, 0, 1);
            r3 = descriptor.worldToPixel(r3);
        }
        if (r1 == null || r3 == null) {
            // if the image has some points outside the valid range of the
            // screen then give up optimizing and assume the max number of
            // points.
            return (int) Math.ceil(maxNumDivs);
        }
        double[] p2 = { (p1[0] + p3[0]) / 2, (p1[1] + p3[1]) / 2 };
        double[] r2 = new double[p2.length];
        mt.transform(p2, 0, r2, 0, 1);
        r2 = descriptor.worldToPixel(r2);
        double[] interp2 = { (r1[0] + r3[0]) / 2, (r1[1] + r3[1]) / 2 };
        double dX = r2[0] - interp2[0];
        double dY = r2[1] - interp2[1];
        double d = Math.hypot(dX, dY);
        if (d < THRESHOLD || maxNumDivs < 1) {
            return 1;
        } else {
            int nd1 = getNumDivisions(p1, r1, p2, r2, maxNumDivs / 2, mt);
            if (nd1 * 2 >= maxNumDivs) {
                return (int) Math.ceil(maxNumDivs);
            }
            int nd2 = getNumDivisions(p2, r2, p3, r3, maxNumDivs / 2, mt);
            if (nd2 * 2 >= maxNumDivs) {
                return (int) Math.ceil(maxNumDivs);
            }
            return (Math.max(nd1, nd2) * 2);
        }
    }
}
