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
 * A wrapper around a vbo id from gl. This object contains the logic to create
 * and dispose vbo ids.
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
public class GLVertexBufferObject extends GLIdWrapper {

    /**
     * Create a new vboId in gl and register this vbo to be disposed when parent
     * is garbage collected.
     * 
     * @param parent
     *            - the object that will be using the vbo.
     * @param gl
     */
    public GLVertexBufferObject(Object parent) {
        super();
    }

    /**
     * Create a new vboId in gl
     * 
     * @param gl
     */
    public GLVertexBufferObject() {
        super();
    }

    @Override
    protected void genId(GL gl, int[] arr) {
        gl.glGenBuffers(1, arr, 0);
    }

    @Override
    protected void deleteId(GL gl, int[] arr) {
        gl.glDeleteBuffers(1, arr, 0);
    }

    /**
     * 
     * @param gl
     * @param target
     *            Specifies the target to which the buffer object is bound. The
     *            symbolic constant must be GL_ARRAY_BUFFER,
     *            GL_ELEMENT_ARRAY_BUFFER, GL_PIXEL_PACK_BUFFER, or
     *            GL_PIXEL_UNPACK_BUFFER
     */
    public void bind(GL gl, int target) {
        gl.glBindBuffer(target, id);
    }

}
