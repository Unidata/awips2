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
package com.raytheon.uf.common.gridcoverage.subgrid;

import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;

/**
 * Provides utility methods for trimming grids into subgrids.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 30, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class TrimUtil {

    /**
     * Uses the lat/lon values in subGrid to calculate a valid overlapping area
     * to the parent grid and then sets the nx,ny and the lower Left x and y in
     * the subgrid object.
     * 
     * @param parentLLLat
     * @param parentLLLon
     * @param subGrid
     * @param nx
     * @param ny
     * @param dxMeter
     * @param dyMeter
     * @param fromLatLon
     * @param toLatLon
     * @param checkGridWrap
     * @throws Exception
     */
    public static void trimMeterSpace(double parentLLLat, double parentLLLon,
            SubGrid subGrid, int nx, int ny, double dxMeter, double dyMeter,
            MathTransform fromLatLon, MathTransform toLatLon,
            boolean checkGridWrap) throws Exception {
        double[] lonLats = new double[8];
        double[] lonLatsInMeters = new double[8];
        lonLats[0] = parentLLLon;
        lonLats[1] = parentLLLat;
        lonLats[4] = subGrid.getLowerLeftLon();
        lonLats[5] = subGrid.getLowerLeftLat();
        lonLats[6] = subGrid.getUpperRightLon();
        lonLats[7] = subGrid.getUpperRightLat();

        if (checkGridWrap) {
            // determine UR of parent
            fromLatLon.transform(lonLats, 0, lonLatsInMeters, 0, 1);
            // size minus one so boundaries are inclusive
            lonLatsInMeters[2] = lonLatsInMeters[0] + (nx - 1) * dxMeter;
            lonLatsInMeters[3] = lonLatsInMeters[1] + (ny - 1) * dyMeter;
            toLatLon.transform(lonLatsInMeters, 2, lonLats, 2, 1);

            validateLongitudes(lonLats);
        }

        // adjust all longitudes to be offset from LL of parent grid
        if (lonLats[4] < lonLats[0]) {
            lonLats[4] += 360;
        }
        if (lonLats[6] < lonLats[0]) {
            lonLats[6] += 360;
        }

        fromLatLon.transform(lonLats, 0, lonLatsInMeters, 0, 4);
        // recalculate anyway
        lonLatsInMeters[2] = lonLatsInMeters[0] + dxMeter * (nx - 1);
        lonLatsInMeters[3] = lonLatsInMeters[1] + dyMeter * (ny - 1);

        // sanity check lat bounds
        if (lonLatsInMeters[5] < lonLatsInMeters[1]) {
            lonLatsInMeters[5] = lonLatsInMeters[1];
        }
        if (lonLatsInMeters[7] > lonLatsInMeters[3]) {
            lonLatsInMeters[7] = lonLatsInMeters[3];
        }

        // if grid wrap was not checked then need to constrain lons
        if (!checkGridWrap) {
            if (lonLatsInMeters[4] < lonLatsInMeters[0]) {
                lonLatsInMeters[4] = lonLatsInMeters[0];
            }
            if (lonLatsInMeters[6] > lonLatsInMeters[2]) {
                lonLatsInMeters[6] = lonLatsInMeters[2];
            }
            if (lonLatsInMeters[6] < lonLatsInMeters[4]) {
                throw new GridCoverageException(
                        "Model does not contain area defined by sub grid.");
            }
        }

        // need to determine exact LL grid point, round up to be just inside
        // subGrid def
        // X/Y is 0/0 in Upper Left
        int leftX = (int) (Math.ceil((lonLatsInMeters[4] - lonLatsInMeters[0])
                / dxMeter));
        int lowerY = ny
                - (int) (Math.ceil((lonLatsInMeters[5] - lonLatsInMeters[1])
                        / dyMeter));

        // determine exact LL in meter
        lonLatsInMeters[4] = lonLatsInMeters[0] + leftX * dxMeter;
        lonLatsInMeters[5] = lonLatsInMeters[1] + (ny - lowerY) * dyMeter;

        // determine number points, round down to be inside sub grid, inclusive
        subGrid.setNX((int) ((lonLatsInMeters[6] - lonLatsInMeters[4]) / dxMeter) + 1);
        subGrid.setNY((int) ((lonLatsInMeters[7] - lonLatsInMeters[5]) / dyMeter) + 1);

        // just double check possible rounding error, in case of using
        // subgridding to shift a world wide grid
        if (subGrid.getNX() > nx) {
            subGrid.setNX(nx);
        }
        if (subGrid.getNY() > ny) {
            subGrid.setNY(ny);
        }

        // sub gridding needs the upper left x/y to pull out the data
        // X/Y is 0/0 at UR and NX/NY at LL
        subGrid.setUpperLeftX(leftX);
        subGrid.setUpperLeftY(lowerY - subGrid.getNY());

        // determine exact UR in meter
        lonLatsInMeters[6] = lonLatsInMeters[4] + (subGrid.getNX() - 1)
                * dxMeter;
        lonLatsInMeters[7] = lonLatsInMeters[5] + (subGrid.getNY() - 1)
                * dyMeter;

        toLatLon.transform(lonLatsInMeters, 4, lonLats, 4, 1);
        subGrid.setLowerLeftLon(MapUtil.correctLon(lonLats[4]));
        subGrid.setLowerLeftLat(MapUtil.correctLat(lonLats[5]));
        subGrid.setUpperRightLon(MapUtil.correctLon(lonLats[6]));
        subGrid.setUpperRightLat(MapUtil.correctLat(lonLats[7]));
    }

    /**
     * Uses the lat/lon values in subGrid to calculate a valid overlapping area
     * to the parent grid and then sets the nx,ny and the lower Left x and y in
     * the subgrid object.
     * 
     * @param parentLLLat
     * @param parentLLLon
     *            * @param subGrid
     * @param nx
     * @param ny
     * @param dx
     * @param dy
     * @throws Exception
     */
    public static void trimLatLonSpace(double parentLLLat, double parentLLLon,
            SubGrid subGrid, int nx, int ny, double dx, double dy)
            throws Exception {

        double lonLats[] = new double[8];
        lonLats[0] = parentLLLon;
        lonLats[1] = parentLLLat;
        // size minus 1 so boundaries are inclusive
        lonLats[2] = lonLats[0] + (nx - 1) * dx;
        lonLats[3] = lonLats[1] + (ny - 1) * dy;
        lonLats[4] = subGrid.getLowerLeftLon();
        lonLats[5] = subGrid.getLowerLeftLat();
        lonLats[6] = subGrid.getUpperRightLon();
        lonLats[7] = subGrid.getUpperRightLat();

        // check grid wrap
        validateLongitudes(lonLats);

        // sanity check lats
        if (lonLats[5] < lonLats[1]) {
            lonLats[5] = lonLats[1];
        }
        if (lonLats[7] > lonLats[3]) {
            lonLats[7] = lonLats[3];
        }

        // adjust all longitudes to be offset from LL of parent grid
        if (lonLats[2] < lonLats[0]) {
            lonLats[2] += 360;
        }
        if (lonLats[4] < lonLats[0]) {
            lonLats[4] += 360;
        }
        if (lonLats[6] < lonLats[0]) {
            lonLats[6] += 360;
        }

        // if subGrid wraps, need to add 360
        if (lonLats[6] < lonLats[4]) {
            lonLats[6] += 360;
        }

        // need to determine exact LL grid point
        // X/Y is 0/0 in Upper Left
        int leftX = (int) ((lonLats[4] - lonLats[0]) / dx);
        int lowerY = ny - (int) ((lonLats[5] - lonLats[1]) / dy);

        // determine exact LL
        lonLats[4] = lonLats[0] + leftX * dx;
        lonLats[5] = lonLats[1] + (ny - lowerY) * dy;

        // determine number points, round up and inclusive
        subGrid.setNX((int) ((lonLats[6] - lonLats[4]) / dx + 0.5) + 1);
        subGrid.setNY((int) ((lonLats[7] - lonLats[5]) / dy + 0.5) + 1);

        // just double check possible rounding error, in case of using
        // subgridding to shift a world wide grid
        if (subGrid.getNX() > nx) {
            subGrid.setNX(nx);
        }
        if (subGrid.getNY() > ny) {
            subGrid.setNY(ny);
        }

        // sub gridding needs the upper left x/y to pull out the data
        // X/Y is 0/0 at UR and NX/NY at LL
        subGrid.setUpperLeftX(leftX);
        subGrid.setUpperLeftY(lowerY - subGrid.getNY());

        // determine exact UR
        lonLats[6] = lonLats[4] + (subGrid.getNX() - 1) * dx;
        lonLats[7] = lonLats[5] + (subGrid.getNY() - 1) * dy;

        subGrid.setLowerLeftLon(MapUtil.correctLon(lonLats[4]));
        subGrid.setLowerLeftLat(MapUtil.correctLat(lonLats[5]));
        subGrid.setUpperRightLon(MapUtil.correctLon(lonLats[6]));
        subGrid.setUpperRightLat(MapUtil.correctLat(lonLats[7]));
    }

    private static void validateLongitudes(double lonLats[])
            throws GridCoverageException {
        // check > 180 and wrap back around
        if (lonLats[2] > 180) {
            lonLats[2] -= 360;
        }

        // rough guess if its world wide
        boolean parentCoverageWraps = lonLats[2] < lonLats[0];
        boolean isWorldWide = (parentCoverageWraps && ((lonLats[2] - lonLats[0]) < 2.5))
                || ((lonLats[2] - lonLats[0]) > 357);

        // no need to constrain sub grid if parent is world wide
        if (!isWorldWide) {
            boolean subCoverageWraps = lonLats[6] < lonLats[4];

            if ((parentCoverageWraps && subCoverageWraps)
                    || (!parentCoverageWraps && !subCoverageWraps)) {
                // compare lons normally
                if (lonLats[4] < lonLats[0]) {
                    lonLats[4] = lonLats[0];
                }
                if (lonLats[6] > lonLats[2]) {
                    lonLats[6] = lonLats[2];
                }
            } else if (parentCoverageWraps) {
                // parent coverage wraps, sub coverage doesn't wrap
                if (lonLats[6] > lonLats[2]) {
                    // UR is valid, compare LL normally
                    if (lonLats[4] < lonLats[0]) {
                        lonLats[4] = lonLats[0];
                    }
                } else {
                    // verify LL is before UR of parent
                    if (lonLats[4] > lonLats[2]) {
                        // invalid grid
                        throw new GridCoverageException(
                                "Model does not contain area defined by sub grid.");
                    }

                    // UR needs to be checked normally
                    if (lonLats[6] > lonLats[2]) {
                        lonLats[6] = lonLats[2];
                    }
                }
            } else {
                // parent coverage doesn't wrap, sub coverage wraps
                if (lonLats[6] < lonLats[4]) {
                    // LL is valid, constrain UR as normal
                    if (lonLats[6] > lonLats[2]) {
                        lonLats[6] = lonLats[2];
                    }
                } else {
                    // verify UR is after LL of parent
                    if (lonLats[6] < lonLats[0]) {
                        // invalid grid
                        throw new GridCoverageException(
                                "Model does not contain area defined by sub grid.");
                    }

                    // sub LL was beyond grib, set it to LL of parent inside
                    // the wrap
                    lonLats[4] = lonLats[0];

                    // UR needs to be checked normally
                    if (lonLats[6] > lonLats[2]) {
                        lonLats[6] = lonLats[2];
                    }
                }
            }
        }
    }
}
