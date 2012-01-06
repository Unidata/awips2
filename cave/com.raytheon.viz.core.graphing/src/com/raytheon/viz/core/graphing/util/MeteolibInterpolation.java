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
package com.raytheon.viz.core.graphing.util;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;

import com.raytheon.edex.meteoLib.Controller;
import com.raytheon.uf.viz.core.interp.IInterpolation;
import com.raytheon.uf.viz.core.interp.InterpolationRequest;
import com.raytheon.uf.viz.core.interp.InterpolationResult;
import com.raytheon.viz.core.slice.request.HeightScale.ScaleType;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class MeteolibInterpolation implements IInterpolation {

    private ScaleType scale;

    public MeteolibInterpolation(ScaleType scale) {
        this.scale = scale;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.interp.IInterpolation#interpolate(
     * com.raytheon.uf.viz.core.interp.InterpolationRequest,
     * org.geotools.coverage.grid.GridGeometry2D)
     */
    @Override
    public InterpolationResult interpolate(InterpolationRequest request) {
        if (request.getXData().length == 0) {
            throw new IllegalArgumentException(
                    "No Data Available for Interpolation");
        }
        float maxX = request.getMaxX();
        float minX = request.getMinX();
        float maxY = request.getMaxY();
        float minY = request.getMinY();
        float gridX = request.getGridX();
        float gridY = request.getGridY();
        float[] x = request.getXData();
        float[] y = request.getYData();
        float[] z = request.getZData();
        float logMinY = (float) Math.log10(minY);
        float logMaxY = (float) Math.log10(maxY);
        for (int i = 0; i < x.length; i++) {
            if (scale == ScaleType.LOG) {
                y[i] = (float) ((Math.log10(y[i]) - logMinY)
                        / (logMaxY - logMinY) * (gridY - 1));
            } else {
                y[i] = (y[i] - minY) / (maxY - minY) * (gridY - 1);
            }
            x[i] = (x[i] - minX) / (maxX - minX) * (gridX - 1);
            if (x[i] < 0 || y[i] < 0 || x[i] > gridX - 1 || y[i] > gridY - 1) {
                z[i] = 1e37f;
            }
        }
        float[] grid = new float[(int) (gridX * gridY)];
        Controller.scaleless_analysis(x, y, z, x.length, (int) gridX,
                (int) gridY, grid);

        float[] newgrid = Controller.dist_filter(grid, 5.0f, (int) gridX, 0, 0,
                (int) gridX, (int) gridY);

        for (int i = 0; i < grid.length; i++) {
            if (newgrid[i] < -999999 || newgrid[i] > 999999) {
                newgrid[i] = grid[i];
            }
            if (newgrid[i] < -999999 || newgrid[i] > 999999) {
                newgrid[i] = -999999;
            }
        }
        grid = newgrid;
        InterpolationResult result = new InterpolationResult();

        result.setValues(grid);
        double nminX = Math.min(minX, maxX);
        double nmaxX = Math.max(minX, maxX);
        double nminY = Math.min(minY, maxY);
        double nmaxY = Math.max(minY, maxY);
        double xinc = (nmaxX - nminX) / gridX;
        double yinc = (nmaxY - nminY) / gridY;
        nminX -= xinc / 2;
        nmaxX += xinc / 2;
        nminY -= yinc / 2;
        nmaxY += yinc / 2;
        GeneralEnvelope env = new GeneralEnvelope(
                new double[] { nminX, nminY }, new double[] { nmaxX, nmaxY });
        GeneralGridEnvelope range = new GeneralGridEnvelope(new int[] { 0, 0 },
                new int[] { (int) gridX, (int) gridY }, false);
        result.setGeometry(new GridGeometry2D(range, env));
        return result;

    }

}
