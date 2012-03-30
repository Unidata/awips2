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

import javax.media.opengl.GL;
import javax.media.opengl.glu.GLU;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.viz.core.gl.objects.GLTextureObject;

public interface IGLTarget extends IGraphicsTarget {

    public abstract GL getGl();

    public abstract GLU getGlu();

    public abstract void pushGLState();

    public abstract void popGLState();

    /**
     * 
     * @return
     */
    public abstract boolean makeContextCurrent();

    /**
     * 
     * @return
     */
    public abstract void releaseContext();

    /**
     * Get the modle view matrix settings
     * 
     * @return
     */
    public abstract double[] getModelView();

    /**
     * Get the projection matrix settings
     * 
     * @return
     */
    public abstract double[] getProjection();

    /**
     * Get the view port settings
     * 
     * @return
     */
    public abstract int[] getViewPort();

    /**
     * Convert pixel coordinates to window.
     * 
     * @param p
     * @return
     */
    public abstract double[] project(double[] p);

    /**
     * Convert screen x,y to pixel space
     * 
     * @param x
     *            screen x value
     * @param y
     *            screen y value
     * @param z
     *            0 near 1 far plane
     * @return pixel value
     */
    public abstract double[] unproject(double[] pos);

    /**
     * @return the bounds for the target pane.
     */
    public abstract Rectangle getBounds();

    /**
     * Get a colormap texture id for the specified parameters
     * 
     * @param cmapParams
     * @return
     */
    public abstract GLTextureObject getColorMapTexture(
            ColorMapParameters cmapParams);

    /**
     * Checks the glError state and does a UFStatus message
     */
    public abstract void handleError(int errorid);

}
