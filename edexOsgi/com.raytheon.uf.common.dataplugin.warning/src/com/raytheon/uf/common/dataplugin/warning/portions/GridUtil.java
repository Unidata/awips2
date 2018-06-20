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
package com.raytheon.uf.common.dataplugin.warning.portions;

import java.util.ArrayList;
import java.util.List;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.metadata.spatial.PixelOrientation;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.warning.util.GeometryUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateSequence;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

/**
 * Converts the county or zone and the intersecting warning area to grids. The
 * county or zone is also weighted to determine the northern, southern, eastern,
 * and western parts of the county or zone. Most of the code is ported from A1
 * to create an EntityData object that will be used by PortionsUtil to determine
 * the applicable impacted portions of a county or zone.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2013            jsanchez     Initial creation
 * Dec 4, 2013  2604      jsanchez     Moved out of viz.warngen.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class GridUtil {

    private int ny = 0;

    private int nx = 0;

    private int[] ewGrid;

    private int[] nsGrid;

    private byte[] warnedAreaGrid;

    private byte[] countyOrZoneGrid;

    private MathTransform latLonToContour, contourToLatLon;

    public GridUtil(GeneralGridGeometry localGridGeometry,
            MathTransform localToLatLon) throws Exception {

        GridEnvelope range = localGridGeometry.getGridRange();
        this.nx = range.getHigh(0);
        this.ny = range.getHigh(1);

        org.opengis.geometry.Envelope ge = localGridGeometry.getEnvelope();
        contourToLatLon = new DefaultMathTransformFactory()
                .createConcatenatedTransform(new GridGeometry2D(range, ge)
                        .getGridToCRS(PixelOrientation.CENTER), localToLatLon);
        latLonToContour = contourToLatLon.inverse();

        ewGrid = new int[nx * ny];
        nsGrid = new int[nx * ny];
    }

    /**
     * Converts the countyOrZone geometry and the warnedArea into grids and sets
     * the appropriate data in an EntityData object.
     * 
     * @param countyOrZone
     * @param warnedArea
     * @return
     * @throws Exception
     */
    public EntityData calculateGrids(Geometry countyOrZone, Geometry warnedArea)
            throws Exception {
        countyOrZoneGrid = toByteArray(countyOrZone);
        warnedAreaGrid = toByteArray(warnedArea);
        int[] bounds = awips1FinishAreaEntity();
        EntityData entityData = finishDefineArea(bounds);
        return entityData;
    }

    /**
     * Converts the geometry into a byte array that is expected by ported A1
     * code.
     * 
     * @param geometry
     * @return
     * @throws VizException
     */
    private byte[] toByteArray(Geometry geometry) throws Exception {
        byte[] bytes = new byte[nx * ny];
        float[][] floatData = toFloatData(geometry);

        // Rotates grid
        int k = 0;
        for (int j = ny - 1; j >= 0; j--) {
            for (int i = 0; i < nx; i++) {
                if (floatData[i][j] == 1) {
                    bytes[k] = 1;
                }
                k++;
            }
        }

        return bytes;
    }

    /**
     * Converts the geometry into a 2-D float array.
     * 
     * @param geometry
     * @return
     * @throws VizException
     */
    private float[][] toFloatData(Geometry geometry) throws Exception {
        Geometry contoured = convertGeom(geometry, latLonToContour);
        List<Geometry> geomList = new ArrayList<Geometry>(
                contoured.getNumGeometries());
        GeometryUtil.buildGeometryList(geomList, contoured);
        List<PreparedGeometry> prepped = new ArrayList<PreparedGeometry>(
                geomList.size());
        for (Geometry g : geomList) {
            prepped.add(PreparedGeometryFactory.prepare(g));
        }

        GeometryFactory gf = geometry.getFactory();
        Point point = gf.createPoint(new Coordinate(0, 0));
        CoordinateSequence pointCS = point.getCoordinateSequence();
        float[][] contourAreaData = new float[nx][ny];
        for (PreparedGeometry geom : prepped) {
            Envelope env = geom.getGeometry().getEnvelopeInternal();
            int startX = (int) env.getMinX();
            int startY = (int) env.getMinY();
            int width = (int) env.getMaxX();
            int height = (int) env.getMaxY();
            if (startX < 0 || width > nx || startY < 0 || height > ny) {
                continue;
            }
            startX = Math.max(0, startX - 1);
            startY = Math.max(0, startY - 1);
            width = Math.min(nx, width + 1);
            height = Math.min(ny, height + 1);
            for (int x = startX; x < width; ++x) {
                for (int y = startY; y < height; ++y) {

                    pointCS.setOrdinate(0, 0, x);
                    pointCS.setOrdinate(0, 1, y);
                    point.geometryChanged();
                    if (contourAreaData[x][y] == 0.0f && geom.intersects(point)) {
                        contourAreaData[x][y] = 1.0f;
                    }
                }
            }

        }

        return contourAreaData;
    }

    static private <T> T convertGeom(T geom, MathTransform transform) {
        if (geom == null) {
            return null;
        }
        try {
            if (geom instanceof Coordinate) {
                return (T) JTS.transform(
                        new GeometryFactory().createPoint((Coordinate) geom),
                        transform).getCoordinate();
            } else if (geom instanceof Geometry) {
                return (T) JTS.transform((Geometry) geom, transform);
            } else {
                throw new RuntimeException("Invalid type passed in: "
                        + geom.getClass());
            }
        } catch (Exception e) {
            throw new RuntimeException("Error transforming object, "
                    + e.getLocalizedMessage(), e);
        }
    }

    /**
     * Ported only the logic from A1 code
     * GeoEntityLookupTable::finishDefineArea() that calculates the meanMask,
     * coverageMask,and octants for an entity (i.e. county or zone).
     * 
     * @param countyOrZone
     * @param warnedArea
     * @return
     */
    private EntityData finishDefineArea(int[] bounds) {
        int meanMask = 0;
        int coverageMask = 0;
        int octants = 0;

        int ewCount = 0;
        int nsCount = 0;
        int ewTotal = 0;
        int nsTotal = 0;

        int k = 0;
        int min_i = bounds[0];
        int max_i = bounds[1];
        int min_j = bounds[2];
        int max_j = bounds[3];
        for (int j = min_j; j < max_j; j++) {
            k = nx * j + min_i;
            for (int i = min_i; i < max_i; i++, k++) {
                if (warnedAreaGrid[k] == 1) {
                    int e = countyOrZoneGrid[k];

                    int ii = ewGrid[k];
                    int jj = nsGrid[k];

                    if (ii == 0 && jj == 0) {
                        continue;
                    }
                    ewTotal += ii;
                    if (ii > 0) {
                        ewCount++;
                    }
                    nsTotal += jj;
                    if (jj > 0) {
                        nsCount++;
                    }
                    int m = CoverageConstants.EW_MASK[ii]
                            | CoverageConstants.NS_MASK[jj];
                    coverageMask |= m;
                    if ((m & CoverageConstants.CENTRAL) == CoverageConstants.CENTRAL) {
                        continue;
                    }
                    if (ii == 0) {
                        ii = 127;
                    }
                    if (jj == 0) {
                        jj = 127;
                    }
                    if (ii < 127) {
                        if (jj < 127) {
                            e = (ii > jj ? CoverageConstants.SSW
                                    : CoverageConstants.WSW);
                        } else {
                            e = (ii > 254 - jj ? CoverageConstants.NNW
                                    : CoverageConstants.WNW);
                        }
                    } else {
                        if (jj < 127) {
                            e = (ii < 254 - jj ? CoverageConstants.SSE
                                    : CoverageConstants.ESE);
                        } else {
                            e = (ii < jj ? CoverageConstants.NNE
                                    : CoverageConstants.ENE);
                        }
                    }
                    if ((m & CoverageConstants.EXTREME_NS) > 0) {
                        e <<= 8;
                    }
                    if ((m & CoverageConstants.EXTREME_EW) > 0) {
                        e <<= 8;
                    }
                    octants |= e;
                } else {
                    warnedAreaGrid[k] = 0;
                }

            }
        }

        if (ewCount > 0) {
            ewTotal = (ewTotal + ewCount / 2) / ewCount;
        }
        if (nsCount > 0) {
            nsTotal = (nsTotal + nsCount / 2) / nsCount;
        }

        meanMask = CoverageConstants.NS_MASK[nsTotal]
                | CoverageConstants.EW_MASK[ewTotal];
        return new EntityData(meanMask, coverageMask, octants);
    }

    /**
     * Calculates the _ewGrid and _nsGrid via A1 ~~ mAgIc ~~
     */
    private int[] awips1FinishAreaEntity() {

        final double EXTREME_FRAC = 0.1;
        final double MIN_EXTREME = 87;
        final double MAX_EXTREME = 167;

        int k = 0;
        int ii, jj;

        /*
         * identify those points on the boundary of the entity so we can shrink
         * from there
         */
        int i_mean = 0;
        int j_mean = 0;
        int new_tot = 0;
        int ii_mean = 0;
        int jj_mean = 0;
        int min_i = Integer.MAX_VALUE;
        int min_j = Integer.MAX_VALUE;
        int max_i = Integer.MIN_VALUE;
        int max_j = Integer.MIN_VALUE;

        for (jj = 0; jj < ny; jj++) {
            k = nx * jj;
            for (ii = 0; ii < nx; ii++, k++) {
                // If the entity is not 1 then it's not part of the county or
                // zone area.
                if (countyOrZoneGrid[k] != 1) {
                    continue;
                }

                if (ii > max_i) {
                    max_i = ii;
                }

                if (ii < min_i) {
                    min_i = ii;
                }

                if (jj > max_j) {
                    max_j = jj;
                }
                if (jj < min_j) {
                    min_j = jj;
                }
                ++new_tot;
                ii_mean += ii;
                jj_mean += jj;

            }

        }

        /*
         * restablish some things that might have changed since the first time
         * they were calculated.
         */
        // if (!outside) {
        ii_mean /= new_tot;
        jj_mean /= new_tot;
        i_mean = ii_mean;
        j_mean = jj_mean;
        // }/*endif*/

        /* assign correct base for directional computation */
        // I changed this from ii_mean to this
        double i_base = (min_i + max_i) / 2;
        // I changed this from jj_mean to this
        double j_base = (min_j + max_j) / 2;

        /*
         * calculate needed rotation factors for computation of amount each
         * point is north and east of centroid
         */
        double x_intcpg = 0.5;
        double y_intcpg = 0.5;

        double dx = 1, dy = 1;
        double x = (i_base - x_intcpg);
        double y = (j_base - y_intcpg);

        // Below is some code from the original A1 code. I assumed that x_intcpg
        // to be 0.5 to avoid porting all the methods in gelt_maker.c.
        // xy_to_ll(&x,&y,&lat,&lon,&my_proj);
        // lat01=lat+0.1;
        // ll_to_xy(&lat01,&lon,&dx,&dy,&my_proj);
        // dx -= x;

        dy -= y;

        double mag = Math.sqrt(dx * dx + dy * dy);
        dx /= mag;
        dy /= mag;
        double erot_i = -dy;
        double erot_j = -dx;

        double nrot_i = dx;
        double nrot_j = dy;

        int[] ew_hist = new int[nx * ny];
        int[] ns_hist = new int[nx * ny];

        /* Calculate north/south & east/west offsets, create histogram of these. */
        // TODO I did not fully implement the histograms as used in a1. Using
        // the histograms created index out of bounds errors. If the field is
        // unhappy with the portions, then porting the histograms needs to be
        // re-investigated.
        int ns1 = 0, ns2 = 0, ew1 = 0, ew2 = 0;
        int np_n = 0, np_s = 0, np_e = 0, np_w = 0;
        ns_hist[0] = 0;
        ew_hist[0] = 0;
        for (jj = min_j; jj < max_j; jj++) {
            k = nx * jj + min_i;
            for (ii = min_i; ii < max_i; ii++, k++) {
                // If the entity is not 1 then it's not part of the county or
                // zone area.
                if (countyOrZoneGrid[k] != 1) {
                    continue;
                }
                double di = ii - i_base;
                double dj = jj - j_base;

                double dns = (int) (nrot_i * di + nrot_j * dj);
                while (ns1 > dns) {
                    // ns_hist[--ns1] = 0;
                    --ns1;
                }

                while (ns2 < dns) {
                    // ns_hist[++ns2] = 0;
                    ++ns2;
                }
                // ns_hist[(int) dns]++;

                double dew = (int) (erot_i * di + erot_j * dj);
                while (ew1 > dew) {
                    // ew_hist[--ew1] = 0;
                    --ew1;
                }
                while (ew2 < dew) {
                    // ew_hist[++ew2] = 0;
                    ++ew2;
                }
                // ew_hist[(int) dew]++;

                if (dew < 0) {
                    np_w++;
                }
                if (dew > 0) {
                    np_e++;
                }
                if (dns < 0) {
                    np_s++;
                }
                if (dns > 0) {
                    np_n++;
                }
            }/* end for */
        }/* end for */

        /*
         * Transform n-s & e-w offsets into normalized distances north and
         * south. This is done based on a preferred fraction of each area that
         * is "extreme".
         */

        // a lot of assumptions were made here. therefore, not everything in
        // this part was fully ported.
        double target = np_w * EXTREME_FRAC;
        // for (ii = 0, k = ew1; k < -1 && ii < target; k++) {
        // ii += ew_hist[k];
        // }
        // if (ii / target > 1.5) {
        // k--;
        // }
        // if (k < ew1) {
        // k = ew1;
        // }

        k = ew1;
        double mu_w = (MIN_EXTREME - 127) / (k + 0.5);

        target = np_e * EXTREME_FRAC;
        // for (ii = 0, k = ew2; k > 1 && ii < target; k--) {
        // ii += ew_hist[k];
        // }
        // if (ii / target > 1.5) {
        // k++;
        // }
        // if (k > ew2) {
        // k = ew2;
        // }

        k = ew2;
        double mu_e = (MAX_EXTREME - 127) / (k - 0.5);

        target = np_s * EXTREME_FRAC;
        // for (ii = 0, k = ns1; k < -1 && ii < target; k++) {
        // ii += ns_hist[k];
        // }
        // if (ii / target > 1.5) {
        // k--;
        // }
        // if (k < ns1) {
        // k = ns1;
        // }

        k = ns1;// TODO - REPLACE WITH ABOVE
        double mu_s = (MIN_EXTREME - 127) / (k + 0.5);

        target = np_n * EXTREME_FRAC;
        // for (ii = 0, k = ns2; k > 1 && ii < target; k--) {
        // ii += ns_hist[k];
        // }
        // if (ii / target > 1.5) {
        // k++;
        // }
        // if (k > ns2) {
        // k = ns2;
        // }

        k = ns2;
        double mu_n = (MAX_EXTREME - 127) / (k - 0.5);

        for (jj = min_j; jj < max_j; jj++) {
            k = nx * jj + min_i;
            for (ii = min_i; ii < max_i; ii++, k++) {
                // If the entity is not 1 then it's not part of the county or
                // zone area.
                if (countyOrZoneGrid[k] != 1) {
                    ewGrid[k] = 0;
                    nsGrid[k] = 0;
                    continue;
                }

                double di = ii - i_base;
                double dj = jj - j_base;
                double dns = (int) (nrot_i * di + nrot_j * dj);
                double dew = (int) (erot_i * di + erot_j * dj);
                int c_ns2 = (int) (dns);
                int c_ew2 = (int) (dew);

                if (c_ew2 < 0) {
                    dx = c_ew2 * mu_w;
                } else {
                    dx = c_ew2 * mu_e;
                }

                if (dx > 127) {
                    dx = 127;
                }

                if (dx < -127) {
                    dx = -127;
                }

                ewGrid[k] = (int) (127 + (int) (dx));

                if (c_ns2 < 0) {
                    dy = c_ns2 * mu_s;
                } else {
                    dy = c_ns2 * mu_n;
                }

                if (dy > 127) {
                    dy = 127;
                }

                if (dy < -127) {
                    dy = -127;
                }

                nsGrid[k] = (int) (127 + (int) (dy));
            }
        }
        // System.out.println("-----------------------------------");
        // printGrids(countyOrZoneGrid, min_i, max_i, min_j, max_j);
        // System.out.println("-----------------------------------");
        // printGrids(_currentArea, min_i, max_i, min_j, max_j);
        // System.out.println("-------------- EAST WEST ---------------------");
        // printGrids(ewGrid, min_i, max_i, min_j, max_j);
        // System.out.println("-------------- NORTH SOUTH ---------------------");
        // printGrids(nsGrid, min_i, max_i, min_j, max_j);
        // System.out.println("north/south - east/west done");
        return new int[] { min_i, max_i, min_j, max_j };

    }

    // For debugging and view the grid in ascii format.
    private void printGrids(int[] grid, int min_i, int max_i, int min_j,
            int max_j) {
        int k = 0;
        for (int jj = min_j; jj < max_j; jj++) {
            k = nx * jj + min_i;
            for (int ii = min_i; ii < max_i; ii++) {
                System.out.print((int) grid[k]);
                k++;
            }
            System.out.println("-");
        }
    }

    // For debugging and view the grid in ascii format.
    private void printGrids(byte[] grid, int min_i, int max_i, int min_j,
            int max_j) {
        int k = 0;
        for (int jj = min_j; jj < max_j; jj++) {
            k = nx * jj + min_i;
            for (int ii = min_i; ii < max_i; ii++) {
                System.out.print((int) grid[k]);
                k++;
            }
            System.out.println("-");
        }
    }

}
