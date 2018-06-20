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
package com.raytheon.uf.common.dataplugin.grid.util;

import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.Map;

import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A class for calculating and caching static data for grids.
 * 
 * Orignally ported from GridAccessor5.C
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------	----------	-----------	--------------------------
 * Jul 24, 2008             brockwoo    Initial creation
 * Oct 21, 2014 3721        dlovely     Optimized for reduced memory usage
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */

public class StaticGridData {
    private static Map<GridCoverage, SoftReference<StaticGridData>> instanceMap = new HashMap<GridCoverage, SoftReference<StaticGridData>>();

    private static final double R_EARTH = 6370.0;

    private FloatDataRecord dx;

    private FloatDataRecord dy;

    private FloatDataRecord coriolis;

    private StaticGridData(GridCoverage gridCoverage) {
        initStaticData(gridCoverage);
    }

    public static synchronized StaticGridData getInstance(
            GridCoverage gridCoverage) {
        SoftReference<StaticGridData> data = instanceMap.get(gridCoverage);

        StaticGridData rval = null;
        if (null != data) {
            rval = data.get();
        }

        if (null == data || null == rval) {
            rval = new StaticGridData(gridCoverage);
            data = new SoftReference<StaticGridData>(rval);
            instanceMap.put(gridCoverage, data);
        }

        return rval;
    }

    public FloatDataRecord getCoriolis() {
        return this.coriolis;
    }

    public FloatDataRecord getDx() {
        return this.dx;
    }

    public FloatDataRecord getDy() {
        return this.dy;
    }

    /**
     * Initializes the Dx, Dy and Coriolis data from the provided
     * {@link GridCoverage}.
     * 
     * @param gridCoverage
     *            Grid Coverage.
     */
    private void initStaticData(GridCoverage gridCoverage) {
        int nx = gridCoverage.getNx();
        int ny = gridCoverage.getNy();
        int n = nx * ny;
        float[] _coriolis = new float[n];
        float[] dxPtr = new float[n];
        float[] dyPtr = new float[n];
        float[] avgPtr = new float[n];
        float[] xxU = new float[nx];
        float[] xxC = new float[nx];
        float[] xxD = new float[nx];
        float[] yyU = new float[nx];
        float[] yyC = new float[nx];
        float[] yyD = new float[nx];
        float[] zzU = new float[nx];
        float[] zzC = new float[nx];
        float[] zzD = new float[nx];

        float[] tmpXX, tmpYY, tmpZZ;

        int i, j, k;

        // Populate Up rows.
        for (i = 0; i < nx; i++) {
            Coordinate location = new Coordinate(i, 1);
            Coordinate latLon = MapUtil.gridCoordinateToLatLon(location,
                    PixelOrientation.CENTER, gridCoverage);
            latLon.x = Math.toRadians(latLon.x);
            latLon.y = Math.toRadians(latLon.y);
            xxU[i] = (float) Math.cos(latLon.y);
            yyU[i] = (float) (xxU[i] * Math.sin(latLon.x));
            xxU[i] *= Math.cos(latLon.x);
            zzU[i] = (float) Math.sin(latLon.y);
        }

        // Populate Current rows.
        for (i = 0; i < nx; i++) {
            Coordinate location = new Coordinate(i, 0);
            Coordinate latLon = MapUtil.gridCoordinateToLatLon(location,
                    PixelOrientation.CENTER, gridCoverage);
            latLon.x = Math.toRadians(latLon.x);
            latLon.y = Math.toRadians(latLon.y);
            xxC[i] = (float) Math.cos(latLon.y);
            yyC[i] = (float) (xxC[i] * Math.sin(latLon.x));
            xxC[i] *= Math.cos(latLon.x);
            zzC[i] = (float) Math.sin(latLon.y);
        }

        // Init Down as a copy of Current
        System.arraycopy(xxC, 0, xxD, 0, nx);
        System.arraycopy(yyC, 0, yyD, 0, nx);
        System.arraycopy(zzC, 0, zzD, 0, nx);

        int lft, rgt;
        double d;
        double icomp, jcomp, kcomp;
        double dmax = 0.0;

        for (j = k = 0; j < ny; j++) {

            lft = 0;
            for (i = 0; i < nx; i++, k++) {
                _coriolis[k] = (float) (zzC[i] * 1.458e-4);
                rgt = (i < nx - 1 ? i + 1 : i);
                icomp = yyC[lft] * zzC[rgt] - zzC[lft] * yyC[rgt];
                jcomp = zzC[lft] * xxC[rgt] - xxC[lft] * zzC[rgt];
                kcomp = xxC[lft] * yyC[rgt] - yyC[lft] * xxC[rgt];
                d = Math.sqrt(icomp * icomp + jcomp * jcomp + kcomp * kcomp);
                dxPtr[k] = (float) (Math.asin(d) * 1000.0 * R_EARTH / (rgt - lft));
                icomp = yyD[i] * zzU[i] - zzD[i] * yyU[i];
                jcomp = zzD[i] * xxU[i] - xxD[i] * zzU[i];
                kcomp = xxD[i] * yyU[i] - yyD[i] * xxU[i];
                d = Math.sqrt(icomp * icomp + jcomp * jcomp + kcomp * kcomp);
                dyPtr[k] = (float) (Math.asin(d) * 1000.0 * R_EARTH * (j == 0
                        || j == (ny - 1) ? 1 : 0.5));
                avgPtr[k] = (dxPtr[k] + dyPtr[k]) / 2.0f;
                d = dxPtr[k] - dyPtr[k];
                if (d < 0) {
                    d = -d;
                }
                d /= avgPtr[k];
                if (d > dmax) {
                    dmax = d;
                }
                if (i != 0) {
                    lft++;
                }
            }

            // Move Current to Down and Up to Current.
            tmpXX = xxD;
            xxD = xxC;
            xxC = xxU;

            tmpYY = yyD;
            yyD = yyC;
            yyC = yyU;

            tmpZZ = zzD;
            zzD = zzC;
            zzC = zzU;

            // Construct the next Up row with new data unless this is the last
            // pass then duplicate the current row.
            if (j < ny - 2) {
                // Populate the next Up row.
                xxU = tmpXX;
                yyU = tmpYY;
                zzU = tmpZZ;
                for (i = 0; i < nx; i++) {
                    Coordinate location = new Coordinate(i, j + 2);
                    Coordinate latLon = MapUtil.gridCoordinateToLatLon(
                            location, PixelOrientation.CENTER, gridCoverage);
                    latLon.x = Math.toRadians(latLon.x);
                    latLon.y = Math.toRadians(latLon.y);
                    xxU[i] = (float) Math.cos(latLon.y);
                    yyU[i] = (float) (xxU[i] * Math.sin(latLon.x));
                    xxU[i] *= Math.cos(latLon.x);
                    zzU[i] = (float) Math.sin(latLon.y);
                }
            } else {
                // If the last run, Duplicate the Current row to the Up row.
                xxU = xxC;
                yyU = yyC;
                zzU = zzC;
            }
        }

        this.coriolis = newRecord(_coriolis, nx, ny);

        if (dmax > 0.01) {
            this.dx = newRecord(dxPtr, nx, ny);
            this.dy = newRecord(dyPtr, nx, ny);
        } else {
            this.dx = this.dy = newRecord(avgPtr, nx, ny);
        }
    }

    private FloatDataRecord newRecord(float[] data, int nx, int ny) {
        return new FloatDataRecord("DATA", "", data, 2, new long[] { nx, ny });
    }

}
