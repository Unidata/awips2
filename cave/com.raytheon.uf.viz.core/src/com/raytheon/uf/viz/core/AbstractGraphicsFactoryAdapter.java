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
package com.raytheon.uf.viz.core;

import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.opengis.coverage.grid.GridEnvelope;

import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Graphics Factory adapter for constructing graphics types
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public abstract class AbstractGraphicsFactoryAdapter {

    /**
     * Construct the view
     * 
     * @return the view
     */
    public abstract IView constructView();

    /**
     * Create a graphics target
     * 
     * @param canvas
     *            TODO
     * @param width
     *            TODO
     * @param height
     *            TODO
     * @return
     * @throws UnsupportedOperationException
     * @throws VizException
     */
    public abstract IGraphicsTarget constructTarget(Canvas canvas, float width,
            float height) throws VizException;

    /**
     * Create an extent
     * 
     * @param coords
     * @return
     * @throws VizException
     */
    public abstract IExtent constructExtent(Coordinate[] coords)
            throws VizException;

    /**
     * Create an extent.
     * 
     * @param aMinX
     * @param aMaxX
     * @param aMinY
     * @param aMaxY
     */
    public abstract IExtent constructExtent(double aMinX, double aMaxX,
            double aMinY, double aMaxY) throws VizException;

    /**
     * Create an extent.
     * 
     * @param rect
     *            a rectangle to build the extent from
     */
    public abstract IExtent constructExtent(Rectangle rect) throws VizException;

    /**
     * Create an extent.
     * 
     * @param range
     *            a grid range to build the extent from
     */
    public abstract IExtent constructExtent(GridEnvelope range)
            throws VizException;

    /**
     * Construct a new canvas to be used with the graphics context
     * 
     * @param canvasComp
     * @throws VizException
     */
    public abstract Canvas constrcutCanvas(Composite canvasComp)
            throws VizException;

}
