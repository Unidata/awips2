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
import javax.media.opengl.glu.GLU;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.AbstractView;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.viz.core.gl.IGLTarget;

/**
 * View that represents a 2 dimensional GL world
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
public class GLView2D extends AbstractView {

    public GLView2D() {
        this(0, 0, 0, 0);
    }

    /**
     * @param minX
     * @param maxX
     * @param minY
     * @param maxY
     */
    public GLView2D(double minX, double maxX, double minY, double maxY) {
        this(new PixelExtent(minX, maxX, minY, maxY));
    }

    /**
     * @param rect
     */
    public GLView2D(Rectangle rect) {
        this(rect.x, rect.x + rect.width, rect.y, rect.y + rect.height);
    }

    public GLView2D(IExtent pe) {
        super(pe);
    }

    protected IGLTarget asIGLTarget(IGraphicsTarget target) {
        if (target instanceof IGLTarget) {
            return (IGLTarget) target;
        } else {
            throw new IllegalArgumentException("Require type IGLTarget got "
                    + target);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return "GLView2D { " + this.getExtent().toString() + " }";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.IView#setupView(com.raytheon.uf.viz.core.
     * IGraphicsTarget)
     */
    @Override
    public void setupView(IGraphicsTarget target) {
        IGLTarget glTarget = asIGLTarget(target);
        GL gl = glTarget.getGl();
        GLU glu = glTarget.getGlu();

        boolean release = glTarget.makeContextCurrent();
        gl.glMatrixMode(GL.GL_PROJECTION);
        gl.glLoadIdentity();
        // We "flip" y-axis for sanity reasons
        glu.gluOrtho2D(this.extent.getMinX(), this.extent.getMaxX(),
                this.extent.getMaxY(), this.extent.getMinY());

        gl.glMatrixMode(GL.GL_MODELVIEW);
        gl.glLoadIdentity();

        gl.glDisable(GL.GL_DEPTH_TEST);
        if (release) {
            glTarget.releaseContext();
        }
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

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#clone()
     */
    @Override
    public GLView2D clone() {
        return new GLView2D((PixelExtent) this.extent.clone());
    }

    @Override
    public Rectangle getCanvasBounds(IGraphicsTarget target) {
        return asIGLTarget(target).getBounds();
    }

}
