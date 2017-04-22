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

package com.raytheon.uf.common.dataplugin.gfe.grid;

import java.awt.Point;

/**
 * Grid Slice Interface
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2008 879        rbell       Initial Creation.
 * 
 * </pre>
 * 
 * @author rbell
 * @version 1.0
 */
public interface IGrid2D {

    public static final int makeContiguousArrayXComponent[] = new int[] { 0, 1,
            0, -1, -1, 1, 1, -1 };

    public static final int makeContiguousArrayYComponent[] = new int[] { 1, 0,
            -1, 0, 1, 1, -1, -1 };

    /**
     * @return whether or not the grid is valid
     */
    public boolean isValid();

    /**
     * @param x
     *            x coordinate of the point
     * @param y
     *            y coordinate of the point
     * @return returns whether or not the specified coordinate is valid
     */
    public boolean isValid(int x, int y);

    /**
     * 
     * Returns the Grid2DBit specified by the supplied sub dimensions.
     * 
     * @param minX
     *            x coordinate of upper left corner of subgrid
     * @param minY
     *            y coordinate of upper left corner of subgrid
     * @param maxX
     *            x coordinate of lower right corner of subgrid
     * @param maxY
     *            y coordinate of lower right corner of subgrid
     * @return a new Grid2D which is the specified subgrid
     */
    public IGrid2D subGrid(int minX, int minY, int maxX, int maxY);

    /**
     * 
     * Clone override function.
     * 
     * @return an IGrid2D object that is equal to this IGrid2D object
     * @throws CloneNotSupportedException
     */
    public IGrid2D clone() throws CloneNotSupportedException;

    /**
     * Copies the input grid onto this grid, but only for coordinates that are 1
     * in the maskGrid.
     * 
     * @param sourceGrid
     *            grid to copy data from
     * @param maskGrid
     *            grid to control which coordinates get copied
     */
    public void copyWithMask(IGrid2D sourceGrid, Grid2DBit maskGrid);

    /**
     * toString method
     * 
     * @return string value
     */
    public String toString();

    /**
     * @return the x dimension
     */
    public int getXdim();

    /**
     * @return the y dimension
     */
    public int getYdim();

    /**
     * @return the grid size
     */
    public Point getGridSize();

}
