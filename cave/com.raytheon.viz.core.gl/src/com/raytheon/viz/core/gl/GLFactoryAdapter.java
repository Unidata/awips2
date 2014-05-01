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
package com.raytheon.viz.core.gl;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.opengl.GLCanvas;
import org.eclipse.swt.opengl.GLData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.opengis.coverage.grid.GridEnvelope;

import com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.gl.internal.GLTarget;
import com.raytheon.viz.core.gl.internal.GLView2D;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * GL Graphics adapter for 2D.
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
public class GLFactoryAdapter extends AbstractGraphicsFactoryAdapter {

    public GLFactoryAdapter() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.IGraphicsFactoryAdapter#constructTarget(java.lang
     * .String)
     */
    @Override
    public IGraphicsTarget constructTarget(Canvas canvas, float width,
            float height) throws VizException {
        return new GLTarget(canvas, width, height);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.IGraphicsFactoryAdapter#constructView(java.lang
     * .String)
     */
    @Override
    public IView constructView() {
        return new GLView2D();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.IGraphicsFactoryAdapter#constructExtent(double,
     * double, double, double)
     */
    @Override
    public IExtent constructExtent(double minX, double maxX, double minY,
            double maxY) throws VizException {
        return new PixelExtent(minX, maxX, minY, maxY);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.IGraphicsFactoryAdapter#constructExtent(java.lang
     * .String, com.vividsolutions.jts.geom.Coordinate[])
     */
    @Override
    public IExtent constructExtent(Coordinate[] coords) throws VizException {
        return new PixelExtent(coords);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.IGraphicsFactoryAdapter#constructExtent(org.eclipse
     * .swt.graphics.Rectangle)
     */
    @Override
    public IExtent constructExtent(Rectangle rect) throws VizException {
        return new PixelExtent(rect);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.IGraphicsFactoryAdapter#constructExtent(org.opengis
     * .coverage.grid.GridRange)
     */
    @Override
    public IExtent constructExtent(GridEnvelope range) throws VizException {
        return new PixelExtent(range);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.AbstractGraphicsFactoryAdapter#constrcutCanvas
     * (org.eclipse.swt.widgets.Composite)
     */
    @Override
    public Canvas constrcutCanvas(Composite canvasComp) throws VizException {
        GLCanvas canvas;
        GLData data = GLContextBridge.getGLData();
        canvas = new GLCanvas(canvasComp, SWT.NONE, data);

        canvas.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        return canvas;
    }

}
