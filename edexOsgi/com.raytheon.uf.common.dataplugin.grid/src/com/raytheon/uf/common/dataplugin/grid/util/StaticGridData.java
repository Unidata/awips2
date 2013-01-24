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
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 24, 2008				brockwoo	Initial creation
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */

public class StaticGridData {
    private static Map<GridCoverage, StaticGridData> instanceMap = new HashMap<GridCoverage, StaticGridData>();

    private static final double R_EARTH = 6370.0;

    private FloatDataRecord dx;

    private FloatDataRecord dy;

    private FloatDataRecord coriolis;

    private StaticGridData(GridCoverage gridCoverage) {
        initStaticData(gridCoverage);
    }

    public static synchronized StaticGridData getInstance(
            GridCoverage gridCoverage) {
        StaticGridData rval = instanceMap.get(gridCoverage);

        if (rval == null) {
            rval = new StaticGridData(gridCoverage);
            instanceMap.put(gridCoverage, rval);
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

    private void initStaticData(GridCoverage gridCoverage) {
        int nx = gridCoverage.getNx();
        int ny = gridCoverage.getNy();
        int n = nx * ny;
        float[] _coriolis = new float[n];
        float[] dxPtr = new float[n];
        float[] dyPtr = new float[n];
        float[] avgPtr = new float[n];
        double[] xx = new double[n];
        double[] yy = new double[n];
        double[] zz = new double[n];
        int i, j, k;

        for (j = k = 0; j < ny; j++) {
            for (i = 0; i < nx; i++, k++) {
                Coordinate location = new Coordinate(i, j);
                Coordinate latLon = MapUtil.gridCoordinateToLatLon(location,
                        PixelOrientation.CENTER, gridCoverage);
                latLon.x = Math.toRadians(latLon.x);
                latLon.y = Math.toRadians(latLon.y);
                xx[k] = Math.cos(latLon.y);
                yy[k] = xx[k] * Math.sin(latLon.x);
                xx[k] *= Math.cos(latLon.x);
                zz[k] = Math.sin(latLon.y);
                _coriolis[k] = (float) (zz[k] * 1.458e-4);
            }
        }

        this.coriolis = newRecord(_coriolis, nx, ny);

        int up, dn, lft, rgt;
        long _nxm = nx - 1;
        double d;
        double icomp, jcomp, kcomp;
        double dmax = 0.0;
        dn = 0;
        up = nx;
        for (j = k = 0; j < ny; j++) {
            if (up >= n) {
                up -= nx;
            }
            lft = k;
            for (i = 0; i < nx; i++, k++) {
                rgt = (i < _nxm ? k + 1 : k);
                icomp = yy[lft] * zz[rgt] - zz[lft] * yy[rgt];
                jcomp = zz[lft] * xx[rgt] - xx[lft] * zz[rgt];
                kcomp = xx[lft] * yy[rgt] - yy[lft] * xx[rgt];
                d = Math.sqrt(icomp * icomp + jcomp * jcomp + kcomp * kcomp);
                dxPtr[k] = (float) (Math.asin(d) * 1000.0 * R_EARTH / (rgt - lft));
                icomp = yy[dn] * zz[up] - zz[dn] * yy[up];
                jcomp = zz[dn] * xx[up] - xx[dn] * zz[up];
                kcomp = xx[dn] * yy[up] - yy[dn] * xx[up];
                d = Math.sqrt(icomp * icomp + jcomp * jcomp + kcomp * kcomp);
                dyPtr[k] = (float) (Math.asin(d) * 1000.0 * R_EARTH * nx / (up - dn));
                avgPtr[k] = (dxPtr[k] + dyPtr[k]) / 2.0f;
                d = dxPtr[k] - dyPtr[k];
                if (d < 0) {
                    d = -d;
                }
                d /= avgPtr[k];
                if (d > dmax) {
                    dmax = d;
                }
                dn++;
                up++;
                lft = k;
            }
            if (j == 0) {
                dn = 0;
            }
        }
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
