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
package com.raytheon.viz.core.gl.objects;

import javax.media.opengl.GL;

/**
 * 
 * A simple wrapper around a GL renderBuffer id that manages creation and
 * disposal of renderBuffer ids.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GLRenderBuffer extends GLIdWrapper {

    /**
     * Create a new renderBuffer id in gl and register this renderBuffer to be
     * disposed when parent is garbage collected.
     * 
     * @param parent
     *            - the object that will be using the texture.
     * @param gl
     */
    public GLRenderBuffer(Object parent) {
        super(parent);
    }

    /**
     * Create a new renderBuffer id in gl
     * 
     * @param gl
     */
    public GLRenderBuffer() {
        super();
    }

    @Override
    protected void genId(GL gl, int[] arr) {
        gl.glGenRenderbuffersEXT(1, arr, 0);
    }

    @Override
    protected void deleteId(GL gl, int[] arr) {
        gl.glDeleteRenderbuffersEXT(1, arr, 0);
    }

    public void bind(GL gl) {
        gl.glBindRenderbufferEXT(GL.GL_RENDERBUFFER_EXT, id);
    }

    public void createStorage(GL gl, int internalFormat, int width, int height) {
        gl.glRenderbufferStorageEXT(GL.GL_RENDERBUFFER_EXT, internalFormat,
                width, height);
    }

}
