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
 * A simple wrapper around a GLTextureId that manages creation and disposal of
 * texture ids.
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
public class GLTextureObject extends GLIdWrapper {

    /**
     * Create a new texture id in gl and register this texture to be disposed
     * when parent is garbage collected.
     * 
     * @param parent
     *            - the object that will be using the texture.
     * @param gl
     */
    public GLTextureObject(Object parent) {
        super(parent);
    }

    /**
     * Create a new texture id in gl
     * 
     * @param gl
     */
    public GLTextureObject() {
        super();
    }

    @Override
    protected void genId(GL gl, int[] arr) {
        gl.glGenTextures(1, arr, 0);

    }

    @Override
    protected void deleteId(GL gl, int[] arr) {
        gl.glDeleteTextures(1, arr, 0);
    }

    /**
     * 
     * @param gl
     * @param target
     *            Specifies the target to which the texture is bound. Must be
     *            either GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_3D, or
     *            GL_TEXTURE_CUBE_MAP.
     */
    public void bind(GL gl, int target) {
        gl.glBindTexture(target, id);
    }

}
