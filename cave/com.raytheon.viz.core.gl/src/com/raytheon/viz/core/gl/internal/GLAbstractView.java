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
/**
 * 
 */
package com.raytheon.viz.core.gl.internal;

import javax.media.opengl.GL;
import javax.vecmath.Matrix4d;
import javax.vecmath.Vector3d;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IView;
import com.raytheon.uf.viz.core.geom.Plane;
import com.raytheon.uf.viz.core.geom.Ray;
import com.raytheon.viz.core.gl.IGLTarget;

/**
 * @author estrabal
 * 
 */
public abstract class GLAbstractView implements IView, Cloneable {

    // private static final double MIN_ZOOM_REQUEST = 0.01;
    //
    // private static final double ZOOM_ANIMATION_FACTOR = 2.0;

    protected IExtent extent;

    // protected double[] eye = new double[] { 0.0, 0.0, 7000.0, 0 };

    protected double elevationExaggeration = 1.0;

    protected double tilt = 0.0;

    static final protected Matrix4d IDENTITY_MATRIX = new Matrix4d(1.0, 0.0,
            0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
            1.0);

    protected Matrix4d projection = new Matrix4d(IDENTITY_MATRIX);

    protected Matrix4d modelView = new Matrix4d(IDENTITY_MATRIX);

    protected Rectangle bounds;

    public GLAbstractView() {
        // this.extent = e;
    }

    public GLAbstractView(IExtent pe) {
        this.extent = pe;
    }

    /**
     * 
     * @param pm
     */
    public void calcFrustum(Matrix4d pm) {

    }

    public double getZoom() {

        return getExtent().getScale();
    }

    public double[] applyRotation(double[] c) {
        return null;
    }

    /**
     * Rotation around left vector.
     */
    public void rotateX(double angle) {

    }

    /**
     * Rotation around up vector.
     */
    public void rotateY(double angle) {
    }

    public void rotateZ(double angle) {
    }

    /**
     * 
     * @param gl
     * @param glu
     */
    public void setupView(IGraphicsTarget target) {
        IGLTarget glTarget = asIGLTarget(target);

        glTarget.makeContextCurrent();
        glTarget.getGl().glMatrixMode(GL.GL_PROJECTION);
        glTarget.getGl().glLoadIdentity();
        setProjectionMatrix(glTarget);

        glTarget.getGl().glMatrixMode(GL.GL_MODELVIEW);
        glTarget.getGl().glLoadIdentity();
        setModelViewMatrix(glTarget);

        setModelView(glTarget.getModelView());
        setProjection(glTarget.getProjection());

        setViewArea(glTarget);
        // glTarget.setupClippingPlane(getClippingPlanes());
    }

    protected IGLTarget asIGLTarget(IGraphicsTarget target) {
        if (target instanceof IGLTarget) {
            return (IGLTarget) target;
        } else {
            throw new IllegalArgumentException("Require type IGLTarget got "
                    + target);
        }
    }

    protected abstract void setViewArea(IGLTarget target);

    /**
     * 
     * @param glu
     */
    protected abstract void setModelViewMatrix(IGLTarget target);

    protected abstract void setProjectionMatrix(IGLTarget target);

    @Override
    public abstract Object clone();

    /**
     * 
     * @return projection matrix
     */
    public Matrix4d getProjection() {
        return this.projection;
    }

    /**
     * 
     * @return modelview matrix
     */
    public Matrix4d getModelView() {

        return this.modelView;
    }

    /**
     * Set the modelview matrix
     * 
     * @param mv
     */
    protected void setModelView(double[] mv) {
        modelView = new Matrix4d(mv);

        int index = 0;
        // values of mv are "column major" 0-3 are first column ...
        for (int i = 0; i < 4; ++i) {
            for (int j = 0; j < 4; ++j) {
                modelView.setElement(j, i, mv[index]);
                ++index;
            }

        }

    }

    /**
     * Set the projection matrix
     * 
     * @param mv
     */
    protected void setProjection(double[] mv) {
        // Matrix4d rot
        projection = new Matrix4d();

        int index = 0;
        // values of mv are "column major" 0-3 are first column ...
        for (int i = 0; i < 4; ++i) {
            for (int j = 0; j < 4; ++j) {
                projection.setElement(j, i, mv[index]);
                ++index;
            }

        }

    }

    /**
     * 
     * @param m
     * @return
     */
    public double[] matrixToArray(Matrix4d m) {

        // OpenGL matrices are column major
        double[] mArray = new double[16];
        int index = 0;
        for (int i = 0; i < 4; ++i) {
            for (int j = 0; j < 4; ++j) {
                mArray[index] = m.getElement(j, i);
                ++index;
            }
        }
        return mArray;
    }

    /**
     * Serialize a pixel extent
     * 
     * @param view
     * @return the serialized form
     */
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
    /**
     * Deserialize a pixel extent from a string
     * 
     * @param data
     *            the serialized form fo the pixel extent
     * @return the pixel extent object
     */
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
    //
    public boolean setCameraPos(Vector3d newEye) {
        return true;
    }

    /**
     * Get the clipping planes
     * 
     * @return
     */
    abstract public Plane[] getClippingPlanes();

    public void setExtent(IExtent pixelExtent) {
        this.extent = pixelExtent;
    }

    public IExtent getExtent() {
        return extent;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#setFocalPoint(javax.vecmath.Vector3d)
     */
    @Override
    public boolean setFocalPoint(double[] currentMouse, IGraphicsTarget target) {
        return false;

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#setTilt(double)
     */
    @Override
    public void setTilt(double delta) {
        this.tilt = delta;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#getEyeDistance(javax.vecmath.Vector3d)
     */
    @Override
    public double getEyeDistance(Vector3d point) {
        // TODO Auto-generated method stub
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#getTilt()
     */
    @Override
    public double getTilt() {
        return this.tilt;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#getEye()
     */
    @Override
    public double[] getEye() {
        return new double[] { 0, 0, -1 };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#setEye(double[])
     */
    @Override
    public void setEye(double[] eye) {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#getFocalPoint()
     */
    @Override
    public double[] getFocalPoint() {
        return new double[] { 0, 0, 0 };
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#setElevationExaggeration(double)
     */
    @Override
    public void setElevationExaggeration(double factor) {
        elevationExaggeration = factor;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#getElevationExaggeration()
     */
    @Override
    public double getElevationExaggeration() {
        return elevationExaggeration;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#screenToGrid(double, double, double)
     */
    @Override
    public abstract double[] screenToGrid(double x, double y, double depth,
            IGraphicsTarget target);

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.IView#gridToScreen(double[])
     */
    @Override
    public abstract double[] gridToScreen(double[] grid, IGraphicsTarget target);

    /**
     * Create a Ray starting a the near plane with direction towards the far
     * 
     * @param mouse
     *            mouse x,y
     * @return Ray
     */
    public Ray computeRay(double[] mouse, IGraphicsTarget target) {

        Vector3d far = new Vector3d(screenToGrid(mouse[0], mouse[1], 1, target));
        Vector3d near = new Vector3d(
                screenToGrid(mouse[0], mouse[1], 0, target));
        if (near == null || far == null) {
            return null;
        }

        far.sub(near);
        far.normalize();

        return new Ray(near, far);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.drawables.IRenderableDisplay#zoom(double)
     */
    @Override
    public void zoom(double factor) {
        // zoom = Math.max(Math.min(factor, IRenderableDisplay.MAX_ZOOM_LEVEL),
        // IRenderableDisplay.MIN_ZOOM_LEVEL);
        this.extent.scale(factor);
    }
}
