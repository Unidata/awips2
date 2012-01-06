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
package com.raytheon.viz.core.gl.internal;

import javax.media.opengl.GL;
import javax.vecmath.Vector3d;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.geom.Plane;
import com.raytheon.uf.viz.core.geom.Ray;
import com.raytheon.viz.core.gl.IGLTarget;

/**
 * @author estrabal
 * 
 */
/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author estrabal
 * @version 1.0
 */
public class GLView2D extends GLAbstractView {
    private final Plane mapPlane = new Plane(0.0, 0.0, 1.0, 0.0, false);

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#getDisplayType()
     */
    @Override
    public String getDisplayType() {
        return "2D";
    }

    public GLView2D() {

    }

    /**
     * @param minX
     * @param maxX
     * @param minY
     * @param maxY
     */
    public GLView2D(double minX, double maxX, double minY, double maxY) {
        super(new PixelExtent(minX, maxX, minY, maxY));
    }

    /**
     * @param rect
     */
    public GLView2D(Rectangle rect) {

        this(rect.x, rect.x + rect.width, rect.y, rect.y + rect.height);

    }

    public GLView2D(PixelExtent pe) {
        super(pe);
    }

    /**
     * 
     */
    public void scale(double factor) {
        this.extent.scale(factor);
    }

    /**
     * 
     * @param factor
     * @param xCenter
     * @param yCenter
     */
    public void scaleAndBias(double factor, double screenX, double screenY,
            IGraphicsTarget target) {
        double[] grid = screenToGrid(screenX, screenY, 0, target);

        this.extent.scaleAndBias(factor, grid[0], grid[1]);
    }

    public double recalcZoomLevel(int[] dimensions) {
        int worldWidth = dimensions[0];
        int worldHeight = dimensions[1];

        return Math.min((extent.getMaxX() - extent.getMinX()) / worldWidth,
                (extent.getMaxY() - extent.getMinY()) / worldHeight);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object arg0) {
        if (arg0 == null || !(arg0 instanceof GLView2D)) {
            return false;
        }

        return this.getExtent().equals(((GLView2D) arg0).getExtent());
    }

    public boolean isVisible(double[] pixel) {
        return getExtent().contains(pixel);
    }

    /**
     * Determine if the getExtent() is in view
     * 
     * @param pe
     * @return
     */
    public boolean isVisible(IExtent pe) {

        return this.getExtent().intersects(pe);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    // @Override
    // public Object clone() {
    // return new GLView2D(this.getExtent().clone());
    // }
    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "GLView2D { " + this.getExtent().toString() + " }";
    }

    // /**
    // * Serialize a pixel extent
    // *
    // * @param view
    // * @return the serialized form
    // */
    // public static String serialize(DisplayView view) {
    // if (view == null)
    // return null;
    //
    // StringBuffer sb = new StringBuffer();
    // sb.append(view.getMinX());
    // sb.append(" ");
    // sb.append(view.getMaxX());
    // sb.append(" ");
    // sb.append(view.getMinY());
    // sb.append(" ");
    // sb.append(view.getMaxY());
    // return sb.toString();
    // }
    //
    // /**
    // * Deserialize a pixel extent from a string
    // *
    // * @param data
    // * the serialized form fo the pixel extent
    // * @return the pixel extent object
    // */
    // public static DisplayView deserialize(String data) {
    // if (data == null) {
    // return null;
    // }
    //		
    // String[] parts = data.split(" ");
    // if (parts.length != 4) {
    // return null;
    // }
    //
    // double[] vals = new double[4];
    // for (int i = 0; i < vals.length; i++) {
    // vals[i] = Double.parseDouble(parts[i]);
    // }
    //
    // return new DisplayView(vals[0], vals[1], vals[2], vals[3]);
    //
    // }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.internal.GLAbstractView#setProjectionMatrix()
     */
    @Override
    protected void setProjectionMatrix(IGLTarget target) {

        // We "flip" y-axis for sanity reasons
        target.getGlu().gluOrtho2D(this.extent.getMinX(),
                this.extent.getMaxX(), this.extent.getMaxY(),
                this.extent.getMinY());

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.internal.GLAbstractView#setModelViewMatrix(IGLTarget
     * )
     */
    @Override
    protected void setModelViewMatrix(IGLTarget target) {

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#screenToGrid(double, double, double)
     */
    @Override
    public double[] screenToGrid(double x, double y, double depth,
            IGraphicsTarget target) {
        Rectangle bounds = asIGLTarget(target).getBounds();
        double correctedX = (x * (extent.getMaxX() - extent.getMinX()) / bounds.width)
                + extent.getMinX();
        double correctedY = (y * (extent.getMaxY() - extent.getMinY()) / bounds.height)
                + extent.getMinY();
        // z bounds are 0 to 1
        double correctedZ = (depth * (extent.getMax().z - extent.getMin().z))
                + extent.getMin().z;
        return new double[] { correctedX, correctedY, correctedZ };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#gridToScreen(double[])
     */
    @Override
    public double[] gridToScreen(double[] grid, IGraphicsTarget target) {
        Rectangle bounds = asIGLTarget(target).getBounds();
        double x = ((grid[0] - extent.getMinX()) * bounds.width)
                / (extent.getMaxX() - extent.getMinX());
        double y = ((grid[1] - extent.getMinY()) * bounds.height)
                / (extent.getMaxY() - extent.getMinY());
        // z bounds are 0 to 1
        double z = (grid[2] - extent.getMin().z)
                / (extent.getMax().z - extent.getMin().z);

        return new double[] { x, y, z };
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.gl.internal.GLAbstractView#getMapCoords(com.raytheon
     * .viz.core.Ray)
     */
    @Override
    public double[] getDisplayCoords(double[] screenCoordinate,
            IGraphicsTarget target) {

        Ray r = computeRay(screenCoordinate, target);
        if (r == null) {
            return null;
        }

        Vector3d i = this.mapPlane.intersection(r);
        if (i == null) {
            return null;
        }
        return new double[] { i.x, i.y, i.z };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#setClippingPlanes()
     */
    @Override
    public void setClippingPlanes() throws VizException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.View#getClippingPlanes()
     */
    @Override
    public Plane[] getClippingPlanes() {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#setExtent(com.raytheon.viz.core.IExtent)
     */
    @Override
    public void setExtent(IExtent e) {
        extent = e;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public Object clone() {

        GLView2D v = new GLView2D((PixelExtent) this.extent.clone());
        return v;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#shiftPOV(double[], double[])
     */
    @Override
    public boolean shiftPOV(double[] lastMouse, double[] currentMouse,
            POVShiftType shiftType, IGraphicsTarget target) {
        // TODO Auto-generated method stub
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#setMapCenter(javax.vecmath.Vector3d)
     */
    @Override
    public void setCenter(Vector3d point) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.IView#createExtent(com.raytheon.viz.core.PixelCoverage
     * )
     */
    @Override
    public IExtent createExtent(PixelCoverage pc) {
        return new PixelExtent(pc.getMinX(), pc.getMaxX(), pc.getMinY(), pc
                .getMaxY());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#shiftExtent(double[])
     */
    @Override
    public void shiftExtent(double[] startScreen, double[] endScreen,
            IGraphicsTarget target) {
        double[] start = screenToGrid(startScreen[0], startScreen[1], 0, target);
        double[] end = screenToGrid(endScreen[0], endScreen[1], 0, target);

        this.extent.shift(end[0] - start[0], end[1] - start[1]);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.IView#scaleToClientArea(org.eclipse.swt.graphics
     * .Rectangle, int[])
     */
    @Override
    public void scaleToClientArea(Rectangle clientArea, int[] dims) {
        double screenRatio;

        if ((clientArea.width == 0) || (clientArea.height == 0)) {
            screenRatio = 1.0;
        } else {
            screenRatio = (double) clientArea.width
                    / (double) clientArea.height;
        }
        int f_worldWidth = dims[0];
        int f_worldHeight = dims[1];

        double worldRatio = (double) f_worldWidth / (double) f_worldHeight;

        // set pixel extent to show entire map
        if (screenRatio > worldRatio) {
            this.extent = new PixelExtent(0, f_worldHeight * screenRatio, 0,
                    f_worldHeight);
        } else {
            this.extent = new PixelExtent(0, f_worldWidth, 0, f_worldWidth
                    / screenRatio);
        }

        this.extent.shift((f_worldWidth - extent.getWidth()) / 2,
                (f_worldHeight - extent.getHeight()) / 2);

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.internal.GLAbstractView#setViewArea()
     */
    @Override
    protected void setViewArea(IGLTarget target) {
        target.getGl().glDisable(GL.GL_DEPTH_TEST);

    }

}
