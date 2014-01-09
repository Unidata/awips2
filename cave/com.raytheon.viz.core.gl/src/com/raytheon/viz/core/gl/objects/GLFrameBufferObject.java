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
 * A simple wrapper around a GL frameBuffer id that manages creation and
 * disposal of frameBuffer ids.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2012            bsteffen    Initial creation
 * Jan  9, 2014 2680       mschenke    Added default error message handling
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GLFrameBufferObject extends GLIdWrapper {

    /**
     * Create a new frameBuffer id in gl and register this frameBuffer to be
     * disposed when parent is garbage collected.
     * 
     * @param parent
     *            - the object that will be using the texture.
     * @param gl
     */
    public GLFrameBufferObject(Object parent) {
        super(parent);
    }

    /**
     * Create a new frameBuffer id in gl
     * 
     * @param gl
     */
    public GLFrameBufferObject() {
        super();
    }

    @Override
    protected void genId(GL gl, int[] arr) {
        gl.glGenFramebuffersEXT(1, arr, 0);
    }

    @Override
    protected void deleteId(GL gl, int[] arr) {
        gl.glDeleteFramebuffersEXT(1, arr, 0);

    }

    public void bind(GL gl) {
        gl.glBindFramebufferEXT(GL.GL_FRAMEBUFFER_EXT, id);
    }

    public String checkStatus(GL gl) {
        String errorMessage = null;

        switch (gl.glCheckFramebufferStatusEXT(GL.GL_FRAMEBUFFER_EXT)) {
        case GL.GL_FRAMEBUFFER_COMPLETE_EXT: {
            // Everything is ok.
            break;
        }
        case GL.GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT: {
            errorMessage = "Error: Framebuffer incomplete, fbo attachement is NOT complete";
            break;
        }
        case GL.GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT: {
            errorMessage = "Error: Framebuffer incomplete, no image is attached to FBO";
            break;
        }
        case GL.GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT: {
            errorMessage = "Error: Framebuffer incomplete, attached images have different dimensions";
            break;
        }
        case GL.GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT: {
            errorMessage = "Error: Framebuffer incomplete, color attached images have different internal formats";
            break;
        }
        case GL.GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT: {
            errorMessage = "Error: Framebuffer incomplete, draw buffer";
            break;
        }
        case GL.GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT: {
            errorMessage = "Error: Framebuffer incomplete, read buffer";
            break;
        }
        case GL.GL_FRAMEBUFFER_UNSUPPORTED_EXT: {
            errorMessage = "Error: Framebuffer not supported by hardware/drivers";
            break;
        }
        default: {
            errorMessage = "Framebuffer is not complete, unknown reason";
            break;
        }
        }
        return errorMessage;
    }
}
