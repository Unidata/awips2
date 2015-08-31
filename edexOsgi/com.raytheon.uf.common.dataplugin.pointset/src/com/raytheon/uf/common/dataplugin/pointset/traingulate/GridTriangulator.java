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
package com.raytheon.uf.common.dataplugin.pointset.traingulate;

import java.nio.IntBuffer;

/**
 * Given the dimensions of a grid this will create a triangle indices for the
 * vertices of the grid. For data that is a well formed grid this is a much
 * faster alternative to the {@link DelauneyTriangulator}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 24, 2015  4709     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class GridTriangulator {

    public IntBuffer triangulate(int nx, int ny) {
        IntBuffer buffer = IntBuffer.allocate(6 * (nx - 1) * (ny - 1));

        for (int j = 1; j < ny; j += 1) {
            for (int i = 1; i < nx; i += 1) {
                /*-
                 * c0---c1       c0---c1
                 *  |   |         |  /|
                 *  |   |  ====>  | / |
                 *  |   |         |/  |
                 * c2---c3       c2---c3
                 */
                int c0 = (j - 1) * nx + (i - 1);
                int c1 = j * nx + (i - 1);
                int c2 = (j - 1) * nx + i;
                int c3 = j * nx + i;

                buffer.put(c0);
                buffer.put(c1);
                buffer.put(c2);

                buffer.put(c3);
                buffer.put(c1);
                buffer.put(c2);

            }
        }
        buffer.rewind();
        return buffer;
    }

}
