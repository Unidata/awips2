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
package com.raytheon.viz.gfe.edittool.contour;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.viz.core.contours.util.ContourContainer;
import com.raytheon.viz.core.contours.util.FortConBuf;
import com.raytheon.viz.core.contours.util.FortConConfig;
import com.raytheon.viz.gfe.contours.util.CLine;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Contouring support for GFE grids
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 29, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ContourGrid {

    private ContourGrid() {
    }

    public static List<CLine> createContours(Grid2DFloat grid,
            float[] contourValues) {
        float[] data1D = grid.getFloats();
        int nx = grid.getXdim();
        int ny = grid.getYdim();

        float[][] contourArrayData = new float[nx][ny];

        for (int j = 0; j < ny; j++) {
            for (int i = 0; i < nx; i++) {
                contourArrayData[i][j] = data1D[nx * j + i];
            }
        }

        FortConConfig config = new FortConConfig();
        config.badlo = Util.GRID_FILL_VALUE - 1;
        config.badhi = Util.GRID_FILL_VALUE + 1;
        config.xOffset = 0;
        config.yOffset = 0;
        config.labelSpacingLine = 1;
        config.generateMaxes = false;
        config.generateMins = false;
        config.mode = contourValues.length;
        config.seed = contourValues;
        ContourContainer contours = FortConBuf
                .contour(contourArrayData, config);

        List<CLine> rval = new ArrayList<CLine>();

        float contourValue = 0;

        int size = contours.xyContourPoints.size();

        for (int i = 0; i < size; i++) {
            float[] contourGridPoints = contours.xyContourPoints.get(i);
            int points = contourGridPoints.length / 2;
            Coordinate[] coordinates = new Coordinate[points];
            int index = 0;
            for (int j = 0; j < points; j++) {
                coordinates[j] = new Coordinate(contourGridPoints[index++],
                        contourGridPoints[index++]);
            }

            // TODO: Should we be rounding to nearest tenth?
            // Round to the nearest tenth
            contourValue = ((int) (contours.contourVals.get(i) * 10)) / 10.0f;
            rval.add(new CLine(coordinates, contourValue, false));
        }

        return rval;
    }
}
